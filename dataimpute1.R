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


####Split NA and non-NA numeric vars####

#Also fully populated numeric vars
vars <- summary.num %>%
  filter(pct.populated > .99, pct.populated < 1)
var.list <- vars$var

train.imp <- train.full
for(var.i in var.list) {
  
  m <- summary.num %>%
    filter(var==var.i)
  
  ind <- which(is.na(train.imp[[var.i]]), arr.ind=TRUE)
  train.imp[ind, var.i] <- m[['median']]
  
}

test.imp <- test.full
for(var.i in var.list) {
  
  m <- summary.num.test %>%
    filter(var==var.i)
  
  ind <- which(is.na(test.imp[[var.i]]), arr.ind=TRUE)
  test.imp[ind, var.i] <- m[['median']]
  
}




