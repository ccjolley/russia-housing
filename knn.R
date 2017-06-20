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
# Scan for a better value of k. 
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
# Actually, the minimum is around k=15 even when I go higher.

###############################################################################
# Try varying predictors
###############################################################################
limit_pcs <- function(df,k=15,n_loc=38,n_build=9) {
  l <- sapply(1:n_loc, function(i) 
    paste0('loc_',i,'$') %>% grep(names(df),value=TRUE))
  b <- sapply(1:n_build, function(i) 
    paste0('build_',i,'$') %>% grep(names(df),value=TRUE))
  knn_lb <- knn.reg(df[,c(l,b)],y=df$ln_price_doc,
                    k=k,algorithm='kd_tree')
  (df$ln_price_doc - knn_lb$pred)^2 %>% mean %>% sqrt
}

limit_pcs(train_pca2,n_loc=38,n_build=9) # 0.501208
limit_pcs(train_pca2,n_loc=9,n_build=9) # 0.4930363
limit_pcs(train_pca2,n_loc=8,n_build=8)

best <- 100
for (i in 1:38) {
  for (j in 1:9) {
    rmsle <- limit_pcs(train_pca2,n_loc=i,n_build=j)
    if (rmsle < best) {
      paste0('(i,j) = ',i,',',j,' rmsle = ',rmsle) %>% print
      best <- rmsle
    }
  }
}

# best: (i,j) = 5,2 rmsle = 0.487482030525468
# comparable to linear regression

###############################################################################
# Re-optimize k
###############################################################################
n_loc=5
n_build=2
l <- sapply(1:n_loc, function(i) 
  paste0('loc_',i,'$') %>% grep(names(train_pca2),value=TRUE))
b <- sapply(1:n_build, function(i) 
  paste0('build_',i,'$') %>% grep(names(train_pca2),value=TRUE))
k_rmsle <- sapply(1:50,function(i) {
  knn_i <- knn.reg(train_pca2[,c(l,b)],y=train_pca2$ln_price_doc,
                   k=i,algorithm='kd_tree')
  rmsle <- (train_pca2$ln_price_doc - knn_i$pred)^2 %>% mean %>% sqrt
  paste0(i,' neighbors, RMSLE = ',rmsle) %>% print
  rmsle
})
qplot(1:30,k_rmsle)
# 30 neighbors, RMSLE = 0.484813381579831
###############################################################################
# Prepare submission
###############################################################################
k=30
all_train <- all_pca2 %>% filter(!is.na(ln_price_doc))
all_test <- all_pca2 %>% filter(is.na(ln_price_doc))
knn_all <- knn.reg(train=all_train[,c(l,b)],test=all_test[,c(l,b)],
                   y=all_train$ln_price_doc,
                   k=k,algorithm='kd_tree')

pred_knn <- all_pca2 
pred_knn$id <- alldata$id
pred_knn <- pred_knn %>% filter(is.na(ln_price_doc))
# NOTE: not sure if this is the right way to do it for KNN
pred_knn$y <- knn_all$pred
submit <- pred_knn %>%
  mutate(price_doc=exp(y)-1) %>%
  select(id,price_doc) %>%
  arrange(id)

write.csv(submit,'submit-0614.csv',row.names=FALSE)
# New leaderboard score is 0.36721; current best is 0.30075