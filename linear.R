# Trying the simplest thing I can think of -- linear regressions
# no imputation or PCA yet

source('clean.R')
train <- read.csv('train.csv') %>% russia_clean

glimpse(train)

# What are my strongest correlations? Work directly with price_doc
res <- data.frame()
for (x in colnames(train)) {
  colx <- train[,x]
  if (!class(colx) %in% c('factor','Date') & x != 'id') {
    rowx <- data.frame(name=x,c=cor(colx,train$price_doc,use='complete.obs'))
    res <- rbind(res,rowx)
  }
}
res <- res %>%
  mutate(absc = abs(c)) %>%
  arrange(desc(absc))

head(res,22)

# Model with the top 20 correlations
lm20 <- lm(price_doc ~ ln_full_sq + num_room + ln_life_sq + sport_count_5000 + 
             sport_count_3000 + trc_count_5000 + zd_vokzaly_avto_km + 
             sadovoe_km + kremlin_km + bulvar_ring_km + sport_count_2000 + 
             ttk_km + office_sqm_5000 + trc_sqm_5000 + sport_count_1500 + 
             nuclear_reactor_km + sport_objects_raion + ln_big_church_km + 
             trc_count_3000 + cafe_count_5000_price_1000,train)
summary(lm20) # R^2 = 0.4858; not terrible

# Get RSME instead; see where this would sit on leaderboard
pred20 <- train %>% select(id,price_doc)
pred20 <- merge(pred20,predict(lm20),by='row.names')
qplot(pred20$price_doc,pred20$y)

# TODO: what happens if I work with log price instead?
# TODO: incorporate cross-validation for better estimate
# TODO: instead of strongest 20 correlations, look at strongest n
# TODO: also look at impact of missing data here
