# Trying the simplest thing I can think of -- linear regressions
# no imputation or PCA yet

library(ggplot2)
source('clean.R')
train <- read.csv('train.csv') %>% russia_clean

glimpse(train)

# What are my strongest correlations? Work directly with price_doc
res <- data.frame()
for (x in colnames(train)) {
  colx <- train[,x]
  if (!class(colx) %in% c('factor','Date') & x != 'id') {
    c <- cor(colx,train$price_doc,use='complete.obs')
    logc <- cor(colx,train$ln_price_doc,use='complete.obs')
    rowx <- data.frame(name=x,c=c,logc=logc)
    res <- rbind(res,rowx)
  }
}
res <- res %>%
  mutate(absc = abs(c),
         abslnc = abs(logc)) %>%
  arrange(desc(absc))

head(res,22)

###############################################################################
# Model with the top 20 correlations
###############################################################################
lm20 <- lm(price_doc ~ ln_full_sq + num_room + ln_life_sq + sport_count_5000 + 
             sport_count_3000 + trc_count_5000 + zd_vokzaly_avto_km + 
             sadovoe_km + kremlin_km + bulvar_ring_km + sport_count_2000 + 
             ttk_km + office_sqm_5000 + trc_sqm_5000 + sport_count_1500 + 
             nuclear_reactor_km + sport_objects_raion + ln_big_church_km + 
             trc_count_3000 + cafe_count_5000_price_1000,train)
summary(lm20) # R^2 = 0.4858; not terrible

# Get RSMLE instead; see where this would sit on leaderboard
pred20 <- train %>% select(id,price_doc)
pred20 <- merge(pred20,predict(lm20),by='row.names')
# some came out < 0; fix those
pred20$y[pred20$y < 0] <- 0
qplot(pred20$price_doc,pred20$y)

rsmle <- function(x,y) {
  (log1p(x) - log1p(y))^2 %>% mean %>% sqrt
}

rsmle(pred20$price_doc,pred20$y)
# 1.529661 - this would put me at 2025 out of 2037. Not great.

###############################################################################
# What happens if I work with log price instead?
###############################################################################
tmp <- res %>% 
  arrange(desc(abslnc)) %>%
  filter(!name %in% c('price_doc','ln_price_doc')) %>%
  head(20) 
form <- tmp$name %>% as.character %>% paste(collapse=' + ') %>%
  paste0('ln_price_doc ~ ',.) %>% as.formula
my_lm <- lm(form,train)
lnpred <- train %>% select(id,ln_price_doc)
lnpred <- merge(lnpred,predict(my_lm),by='row.names')
qplot(lnpred$ln_price_doc,lnpred$y)
# get RSME
(lnpred$ln_price_doc - lnpred$y)^2 %>% mean %>% sqrt
# 0.5151 - quite a bit better. Would put me at 1877 on leaderboard

###############################################################################
# Instead of strongest 20 correlations, look at strongest n
###############################################################################
n_best <- function(n,df,use_log=TRUE) {
  # assumes pre-calculated res table
  if (use_log) {
    tmp <- res %>% 
      arrange(desc(abslnc)) 
  } else {
    tmp <- res %>% 
      arrange(desc(absc))
  }
  tmp <- tmp %>%
    filter(!name %in% c('price_doc','ln_price_doc')) %>%
    head(n)
  form <- tmp$name %>% as.character %>% paste(collapse=' + ') 
  if (use_log) {
    form <- form %>% paste0('ln_price_doc ~ ',.) %>% as.formula
  } else {
    form <- form %>% paste0('price_doc ~ ',.) %>% as.formula
  }
  my_lm <- lm(form,df)
  pred <- df %>% select(id,price_doc,ln_price_doc)
  pred <- merge(pred,predict(my_lm),by='row.names')
  if (use_log) {
    pred$pred <- exp(pred$y) - 1
  } else {
    pred$pred <- ifelse(pred$y<0,0,pred$y)
  }
  rsmle(pred$price_doc,pred$pred)
}

trylist <- (1:40)*5
plotme <- sapply(trylist,function(i) n_best(i,train,TRUE))
qplot(trylist,plotme)
# so... this approach really stops working for more than ~100 variables

###############################################################################
# Incorporate cross-validation for better estimate
###############################################################################
xval <- function(nreps,fn,df) {
  # fn should take a single function, which is the data frame it is 
  # evaluating
  split <- runif(nrow(df),0,nreps) %>% ceiling
  sapply(1:nreps,function(i) {
    tmp <- df[split==i,]
    resi <- fn(tmp)
    paste0('Round ',i,': ',resi) %>% print
    resi
  }) %>% mean
}

xval(10,function(x) n_best(100,x,TRUE),train)

###############################################################################
# Also look at impact of missing data here
###############################################################################
nmis <- function(n) {
  tmp <- res %>% 
    arrange(desc(abslnc)) %>%
    filter(!name %in% c('price_doc','ln_price_doc')) %>%
    head(n)
  nlist <- tmp$name %>% as.character
  if (n > 1) {
    train[,nlist] %>% na.omit %>% nrow
  } else {
    train[,nlist] %>% na.omit %>% length
  }
}

trylist2 <- c(1:10,5*(3:40))
plotme2 <- sapply(trylist2,nmis)
qplot(trylist2,plotme2)
# this sort of explains why things go to pot after ~120 variables;
# we're working with less and less data. This is why we need imputation!

