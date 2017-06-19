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

res <- data.frame()
for (i in c(10,20,50,100,200,500,1000,2000,5000,10000,20000)) {
  ti <- Sys.time()
  x <- rf_subset(train_pca2,i)
  t <- Sys.time()-ti
  paste0(i," : ",x,' : ',t) %>% print
  add <- data.frame(i=i,x=x,t=t)
  res <- rbind(res,add)
}

# Execution time seems to scale as ~N^1.5 in the number of rows
# 30471 rows = 16.9 min
# 38133 rows = 23.5 min

# Not unreasonable, but cross-validation might be overkill.
# rmsle = 0.4755; better than my re-optimized KNN

###############################################################################
# 10x cross-validation with default parameters
###############################################################################
rf_rmsle <- function(df,in_test) {
  df_test <- df[in_test,]
  df_train <- df[!in_test,]
  x_train <- df_train %>% select(-ln_price_doc)
  x_test <- df_test %>% select(-ln_price_doc)
  df_rf <- randomForest(x=x_train,y=df_train$ln_price_doc,
                        xtest=x_test,ytest=df_test$ln_price_doc)
  (df_test$ln_price_doc - df_rf$test$predicted)^2 %>% mean %>% sqrt
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

ss <- 1:nrow(train_pca2) %>% sample %>% head(1000)
train_subset <- train_pca2[ss,]
rf_xval(train_subset,verbose=TRUE) # 0.4405848 -- better than KNN

# when I have more time...
rf_xval(train_pca2,verbose=TRUE)

###############################################################################
# Prepare submission
###############################################################################
all_train <- all_pca2 %>% filter(!is.na(ln_price_doc))
all_test <- all_pca2 %>% filter(is.na(ln_price_doc))
x_train <- all_train %>% select(-ln_price_doc)
x_test <- all_test %>% select(-ln_price_doc)
rf_all <- randomForest(x=x_train,y=all_train$ln_price_doc,
                       xtest=x_test)

pred_rf <- all_pca2 
pred_rf$id <- alldata$id
pred_rf <- pred_rf %>% filter(is.na(ln_price_doc))
pred_rf$y <- rf_all$test$predicted
submit <- pred_rf %>%
  mutate(price_doc=exp(y)-1) %>%
  select(id,price_doc) %>%
  arrange(id)

write.csv(submit,'submit-0619.csv',row.names=FALSE)
# Leaderboard score: 0.35994 -- improved over KNN