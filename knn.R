# Next simplest algorithm I know: knn

###############################################################################
# Initial setup
###############################################################################
source('load_data.R')
load_RDS()

###############################################################################
# Simplest test, with k = 3 
###############################################################################
library(FNN)
library(ggplot2)
predictors <- setdiff(names(train_pca2),'ln_price_doc')
train_knn <- knn.reg(train_pca2[,predictors],y=train_pca2$ln_price_doc,
                     k=3,algorithm='kd_tree')
qplot(train_pca2$ln_price_doc,train_knn$pred)
(train_pca2$ln_price_doc - train_knn$pred)^2 %>% mean %>% sqrt
# 0.5398862 - not actually better than linear at this point

###############################################################################
# 10x cross-validation, with k=3
###############################################################################
knn_rmsle <- function(df,in_test,k=3) {
  df_test <- df[in_test,]
  df_train <- df[!in_test,]
  df_knn <- knn.reg(df_train[,predictors],df_test[,predictors],
                    y=df_train$ln_price_doc,k=k,algorithm='kd_tree')
  (df_test$ln_price_doc - df_knn$pred)^2 %>% mean %>% sqrt
}

knn_xval <- function(df,verbose=FALSE,fold=10,k=3) {
  xval_ind <- rep(1:fold,times=nrow(df)/fold+1)[1:nrow(df)] %>% sample
  res <- rep(NA,fold)
  for (i in 1:fold) {
    rmsle <- knn_rmsle(df,xval_ind==i,k)
    if (verbose) {
      paste0('x-val ',i,': RMSLE = ',rmsle) %>% print
    }
    res[i] <- rmsle
  }
  mean(res)
}

knn_xval(train_pca2,verbose=TRUE)
# 0.5398897 - darn close to in-sample

###############################################################################
# Scan for a better value of k. I might also be able to improve this by 
# varying my set of predictors.
###############################################################################
k_rmsle <- sapply(1:15,function(i) {
  knn_i <- knn.reg(train_pca2[,predictors],y=train_pca2$ln_price_doc,
               k=i,algorithm='kd_tree')
  rmsle <- (train_pca2$ln_price_doc - knn_i$pred)^2 %>% mean %>% sqrt
  paste0(i,' neighbors, RMSLE = ',rmsle) %>% print
  rmsle
})
qplot(1:15,k_rmsle)
# best was at k=15, RMSLE=0.50121
# not sure how much higher I need to go before it starts getting worse again

###############################################################################
# Prepare submission
###############################################################################
# Do this Monday!

