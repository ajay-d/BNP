rm(list=ls(all=TRUE))

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


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
