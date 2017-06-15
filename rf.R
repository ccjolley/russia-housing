# Movin' on up: random forests

###############################################################################
# Initial setup
###############################################################################
source('load_data.R')
load_RDS()
predictors <- setdiff(names(train_pca2),'ln_price_doc')

###############################################################################
# Simplest test - seems to take a really long time, so test on subset
###############################################################################
library(randomForest)
library(ggplot2)

rf_subset <- function(df,numrow) {
  ind <- 1:nrow(df) %>% sample %>% head(numrow) %>% sort
  df_sub <- df[ind,]
  train_rf <- randomForest(ln_price_doc ~ .,data=df_sub)
  (df_sub$ln_price_doc - train_rf$predicted)^2 %>% mean %>% sqrt
}

for (i in c(10,20,50,100,200,500,1000,2000,5000,10000,20000)) {
  x <- rf_subset(train_pca2,i)
  paste0(i," : ",x) %>% print
}

# seems to slow down a lot after 2000 rows, but results look pretty consistent
# TODO: How does time scale with the number of rows? Would training on the 
#       entire dataset be completely impractical?
# TODO: set random seed for reproducibility

###############################################################################
# 10x cross-validation with default parameters
###############################################################################
rf_rmsle <- function(df,in_test) {
  df_test <- df[in_test,]
  df_train <- df[!in_test,]
  df_rf <- randomForest(ln_price_doc ~ .,data=train_pca2)
  (df_test$ln_price_doc - df_rf$predicted)^2 %>% mean %>% sqrt
}

rf_xval <- function(df,verbose=FALSE,fold=10) {
  xval_ind <- rep(1:fold,times=nrow(df)/fold+1)[1:nrow(df)] %>% sample
  res <- rep(NA,fold)
  for (i in 1:fold) {
    rmsle <- rf_rmsle(df,xval_ind==i)
    if (verbose) {
      paste0('x-val ',i,': RMSLE = ',rmsle) %>% print
    }
    res[i] <- rmsle
  }
  mean(res)
}

knn_xval(train_pca2,verbose=TRUE)
# 0.5398897 - darn close to in-sample