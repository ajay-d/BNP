rm(list=ls(all=TRUE))

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lazyeval)

options(mc.cores = parallel::detectCores(),
        stringsAsFactors = FALSE,
        scipen = 10) 

#' coalesce function
`%||%` <- function(a, b) ifelse(!is.na(a), a, b)

train.full <- read_csv("data/train.csv.zip")
test.full <- read_csv("data/test.csv.zip")
sample_submission <- read_csv("data/sample_submission.csv.zip")

train.full %>% count(target)
#train.full %>% map_chr(class)

classes <- train.full %>% map(class)
classes <- classes[3:length(classes)] %>% 
  as.data.frame() %>%
  gather(Variable, Class)

classes %>% 
  count(Class)


####Create Stats on Character Variables####

vars <- classes %>%
  filter(Class=='character')

all.cat.vars <- train.full %>%
  select(one_of(vars$Variable))

summary.cat <- NULL
for(i in names(all.cat.vars)) {

  #print(all.cat.vars %>% count_(i))
  
  #get highest category, and count
  var.max <- all.cat.vars %>%
    count_(i, sort=TRUE) %>%
    filter(row_number()==1)
  
  dots <- list(interp(~ifelse(is.na(var), 1, 0), var = as.name(paste0(i))))
  
  #Count number of NA columns
  na.cnt <- all.cat.vars %>% 
    mutate_(.dots = setNames(dots, paste("na.var"))) %>% 
    count(na.var)
  
  #Count cardinality
  df <- data_frame(var = i,
                   categories = nrow(all.cat.vars %>% count_(i)),
                   populated = na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric,
                   pct.populated = (na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric)/nrow(all.cat.vars),
                   max.cat.value = as.character(var.max[[1,1]]),
                   max.cat.population = var.max[[1,2]])
  
  summary.cat <- bind_rows(summary.cat, df)
}
summary.cat <- summary.cat %>%
  arrange(desc(categories))

all.cat.vars <- test.full %>%
  select(one_of(vars$Variable))

####Create Stats on Character Variables _Test Set_####

summary.cat.test <- NULL
for(i in names(all.cat.vars)) {

  #print(all.cat.vars %>% count_(i))
  
  #get highest category, and count
  var.max <- all.cat.vars %>%
    count_(i, sort=TRUE) %>%
    filter(row_number()==1)
  
  dots <- list(interp(~ifelse(is.na(var), 1, 0), var = as.name(paste0(i))))
  
  #Count number of NA columns
  na.cnt <- all.cat.vars %>% 
    mutate_(.dots = setNames(dots, paste("na.var"))) %>% 
    count(na.var)
  
  #Count cardinality
  df <- data_frame(var = i,
                   categories = nrow(all.cat.vars %>% count_(i)),
                   populated = na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric,
                   pct.populated = (na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric)/nrow(all.cat.vars),
                   max.cat.value = as.character(var.max[[1,1]]),
                   max.cat.population = var.max[[1,2]])
  
  summary.cat.test <- bind_rows(summary.cat.test, df)
}
summary.cat.test <- summary.cat.test %>%
  arrange(desc(categories))

####Create Stats on Numeric Variables####

vars <- classes %>%
  filter(Class!='character')

all.num.vars <- train.full %>%
  select(one_of(vars$Variable))

summary.num <- NULL
for(i in names(all.num.vars)) {
  
  dots <- list(interp(~ifelse(is.na(var), 1, 0), var = as.name(paste0(i))))
  
  #Count number of NA columns
  na.cnt <- all.num.vars %>% 
    mutate_(.dots = setNames(dots, paste("na.var"))) %>% 
    count(na.var)
  
  stat.var <- all.num.vars %>% select_(i) %>% na.omit() %>%
    setNames("var")

  q <- quantile(stat.var$var, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
  
  df <- data_frame(var = i,
                   populated = na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric,
                   pct.populated = (na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric)/nrow(all.num.vars),
                   mean = mean(stat.var$var),
                   median = median(stat.var$var),
                   q5 = q[['5%']],
                   q95 = q[['95%']]
                   )
  
  summary.num <- bind_rows(summary.num, df)
}
summary.num <- summary.num %>%
  arrange(desc(populated))

####Create Stats on Numeric Variables _Test Set_####

vars <- classes %>%
  filter(Class!='character')

all.num.vars <- test.full %>%
  select(one_of(vars$Variable))

summary.num.test <- NULL
for(i in names(all.num.vars)) {
  
  dots <- list(interp(~ifelse(is.na(var), 1, 0), var = as.name(paste0(i))))
  
  #Count number of NA columns
  na.cnt <- all.num.vars %>% 
    mutate_(.dots = setNames(dots, paste("na.var"))) %>% 
    count(na.var)
  
  stat.var <- all.num.vars %>% select_(i) %>% na.omit() %>%
    setNames("var")

  q <- quantile(stat.var$var, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
  
  df <- data_frame(var = i,
                   populated = na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric,
                   pct.populated = (na.cnt %>% filter(na.var==0) %>% select(n) %>% as.numeric)/nrow(all.num.vars),
                   mean = mean(stat.var$var),
                   median = median(stat.var$var),
                   q5 = q[['5%']],
                   q95 = q[['95%']]
                   )
  
  summary.num.test <- bind_rows(summary.num.test, df)
}
summary.num.test <- summary.num.test %>%
  arrange(desc(populated))

#######Check Variables#########
sort(table(all.cat.vars$v56), decreasing = TRUE)
sort(table(all.cat.vars$v125), decreasing = TRUE)
sort(table(all.cat.vars$v113), decreasing = TRUE)

##Total Pcts 1s
mean(train.full$target)

train.full %>%
  select(v113, target) %>%
  filter(!is.na(v113)) %>%
  group_by(v113) %>%
  summarise(pct.1 = mean(target),
            n=n()) %>%
  arrange(desc(n))

##Check NA numeric values
train.full %>%
  filter(is.na(v21)) %>%
  summarise(pct.1 = mean(target),
            n=n())

t1 <- train.full %>%
  filter(is.na(v8))

t1 %>% summarise(pct.1 = mean(target))

t1 <- train.full %>%
  filter(v38==1)
t1 %>% summarise(pct.1 = mean(target))

t2 <- train.full %>%
   filter(v62==2)
t2 %>% summarise(pct.1 = mean(target))

table(train.full$v129)
table(train.full$v72)
table(train.full$v62)
table(train.full$v38)
