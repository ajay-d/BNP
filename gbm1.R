library(xgboost)

set.seed(666)

LogLoss <- function(actual, predicted, eps=1e-15) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

######
#Split Data
######

####Sample .25 OOS test, .25 Watch and .5 model####

train.model <- train.imp %>%
  sample_frac(.5) %>%
  select(ID, target, one_of(var.list)) %>%
  arrange(ID)

train.watch <- train.imp %>%
  anti_join(train.model, by='ID') %>%
  sample_frac(.25) %>% 
  select(ID, target, one_of(var.list)) %>%
  arrange(ID)

test.oos <- train.imp %>%
  anti_join(train.model, by='ID') %>%
  anti_join(train.watch, by='ID') %>%
  select(ID, target, one_of(var.list)) %>%
  arrange(ID)

train.model.y <- train.model %>%
  select(target) %>%
  as.matrix
train.watch.y <- train.watch %>%
  select(target) %>%
  as.matrix
test.oos.y <- test.oos %>%
  select(target) %>%
  as.matrix

train.model <- train.model %>%
  select(-ID, -target) %>%
  as.matrix
train.watch <- train.watch %>%
  select(-ID, -target) %>%
  as.matrix
test.oos <- test.oos %>%
  select(-ID, -target) %>%
  as.matrix

dtrain <- xgb.DMatrix(data = train.model, label = train.model.y)
dtest <- xgb.DMatrix(data = train.watch, label = train.watch.y)

watchlist <- list(train=dtrain, test=dtest)

bst.1 <- xgb.train(data = dtrain, 
                   watchlist = watchlist,
                   max.depth = 5, eta = .05, nround = 100,
                   verbose = 1,
                   nthread = 8,
                   eval_metric = "logloss",
                   objective = "binary:logistic")
bst.2 <- xgb.train(data = dtrain, 
                   watchlist = watchlist,
                   max.depth = 9, eta = .03, nround = 200,
                   verbose = 1,
                   nthread = 8,
                   eval_metric = "logloss",
                   objective = "binary:logistic")

xgb.1 <- predict(bst.1, test.oos)
xgb.2 <- predict(bst.2, test.oos)

ggplot(as.data.frame(xgb.1)) + geom_histogram(aes(xgb.1), binwidth = .001)
ggplot(as.data.frame(xgb.2)) + geom_histogram(aes(xgb.2), binwidth = .001)

quantile(xgb.1, c(0,.01, .05, .25, .5, .75, .95, .99, 1))
quantile(xgb.2, c(0,.01, .05, .25, .5, .75, .95, .99, 1))

#test set
head(test.oos.y)
head(xgb.1)
LogLoss(test.oos.y, xgb.1)
LogLoss(test.oos.y, xgb.2)
