library(mice)

source('clean.R')
train <- read.csv('train.csv') %>% russia_clean

# Benchmark: how long does this take on my laptop?
t1 <- Sys.time()
imp1 <- mice(train)
t2 <- Sys.time()
t2 - t1
