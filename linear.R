# Trying the simplest thing I can think of -- linear regressions
# no imputation or PCA yet

library(ggplot2)

###############################################################################
# Initial setup
###############################################################################
source('load_data.R')
load_RDS()

###############################################################################
# Simplest test
###############################################################################
lm_pca <- lm(ln_price_doc ~ .,train_pca2)
pred_pca <- train_pca2 %>% select(ln_price_doc)
pred_pca <- merge(pred_pca,predict(lm_pca),by='row.names')
(pred_pca$ln_price_doc - pred_pca$y)^2 %>% mean %>% sqrt
# 0.488407
qplot(pred_pca$ln_price_doc,pred_pca$y)

###############################################################################
# 10x cross-validation for linear model
###############################################################################
xval_ind <- rep(1:10,times=nrow(train)/10+1)[1:nrow(train)] %>% sample
res <- rep(NA,10)
for (i in 1:10) {
  train_i <- train_pca2
  train_i$ln_price_doc <- train$ln_price_doc
  train_i$ln_price_doc[xval_ind==i] <- NA
  lm_i <- lm(ln_price_doc ~ .,train_i)
  pred_i <- train_i
  pred_i$id <- train$id
  pred_i$ln_price_doc <- train$ln_price_doc # put the known prices back in
  pred_i <- pred_i[xval_ind==i,] # keep only the "test" set
  pred_i <- merge(pred_i,predict(lm_i,newdata=pred_i),by='row.names')
  rmsle <- (pred_i$ln_price_doc - pred_i$y)^2 %>% mean %>% sqrt
  paste0('x-val ',i,': RMSLE = ',rmsle) %>% print
  res[i] <- rmsle
}
mean(res) # 0.489141

###############################################################################
# Prepare submission with linear model
###############################################################################
lm_all <- lm(ln_price_doc ~ .,all_pca2)
pred_pca <- all_pca2 
pred_pca$id <- alldata$id
pred_pca <- pred_pca %>% filter(is.na(ln_price_doc))
pred_pca <- merge(pred_pca,predict(lm_pca,newdata=pred_pca),by='row.names')
submit <- pred_pca %>%
  mutate(price_doc=exp(y)-1) %>%
  select(id,price_doc) %>%
  arrange(id)

write.csv(submit,'submit-0608b.csv',row.names=FALSE)
# Leaderboard: 0.57660