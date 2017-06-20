# Even better: support vector machines

###############################################################################
# Initial setup
###############################################################################
source('load_data.R')
load_RDS()
predictors <- setdiff(names(train_pca2),'ln_price_doc')

###############################################################################
# Simplest test - seems to take a really long time, so test on subset
###############################################################################
library(e1071)
library(ggplot2)

svm_subset <- function(df,numrow) {
  ind <- 1:nrow(df) %>% sample %>% head(numrow) %>% sort
  df_sub <- df[ind,]
  train_svm <- svm(ln_price_doc ~ .,data=df_sub)
  (df_sub$ln_price_doc - predict(train_svm,df_sub))^2 %>% mean %>% sqrt
}

res <- data.frame()
for (i in c(10,20,50,100,200,500,1000,2000,5000,10000,20000)) {
  ti <- Sys.time()
  x <- svm_subset(train_pca2,i)
  t <- Sys.time()-ti
  paste0(i," : ",x,' : ',t) %>% print
  add <- data.frame(i=i,x=x,t=t)
  res <- rbind(res,add)
}

# Execution time seems to scale as ~N^1.5 in the number of rows
# 30471 rows = 10.1 min 

# 10x cross-validation will take almost 2h.
# rmsle = 0.4618; better than my re-optimized KNN

###############################################################################
# 10x cross-validation with default parameters
###############################################################################
svm_rmsle <- function(df,in_test) {
  df_test <- df[in_test,]
  df_train <- df[!in_test,]
  df_svm <- svm(ln_price_doc ~ .,data=df_train)
  (df_test$ln_price_doc - predict(df_svm,df_test))^2 %>% mean %>% sqrt
}

svm_xval <- function(df,verbose=FALSE,fold=10) {
  xval_ind <- rep(1:fold,times=nrow(df)/fold+1)[1:nrow(df)] %>% sample
  res <- rep(NA,fold)
  for (i in 1:fold) {
    rmsle <- svm_rmsle(df,xval_ind==i)
    if (verbose) {
      paste0('x-val ',i,': RMSLE = ',rmsle) %>% print
    }
    res[i] <- rmsle
  }
  mean(res)
}

ss <- 1:nrow(train_pca2) %>% sample %>% head(1000)
train_subset <- train_pca2[ss,]
svm_xval(train_subset,verbose=TRUE) # 0.5234664, individual 0.386-0.579

# when I have more time...
svm_xval(train_pca2,verbose=TRUE) # 0.4930099, individual 0.474-0.519

###############################################################################
# Are there other parameters I can adjust?
# - epsilon: default = 0.1
# - cost of constraint violation: default = 1
# https://www.svm-tutorial.com/2014/10/support-vector-regression-r/ 
# recommends searching over 2^(2:9)
#
# use tune() function to do this, use subset of 5000
#
# - kernel: could also try linear, polynomial, sigmoid
###############################################################################
tuneResult <- tune(svm, ln_price_doc ~ .,  data = train_subset,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
) # started at 12:42pm, finished before 1:07pm
print(tuneResult) # best epsilon=0.2, cost=4
plot(tuneResult)
tuneResult2 <- tune(svm, ln_price_doc ~ .,  data = train_subset,
                   ranges = list(epsilon = seq(0,0.6,0.05), cost = 1:4)
)
print(tuneResult2) # best is now eps=0.3, cost=1
plot(tuneResult2)

###############################################################################
# Prepare submission
###############################################################################
svm_all <- svm(ln_price_doc ~ .,data=train_pca2,epsilon=0.3,cost=1)

pred_svm <- all_pca2 
pred_svm$id <- alldata$id
pred_svm <- pred_svm %>% filter(is.na(ln_price_doc))
pred_svm$y <- predict(svm_all,all_pca2 %>% filter(is.na(ln_price_doc)))
submit <- pred_svm %>%
  mutate(price_doc=exp(y)-1) %>%
  select(id,price_doc) %>%
  arrange(id)

write.csv(submit,'submit-0620.csv',row.names=FALSE)

# leaderboard score = 0.55811, not an improvement over RF
