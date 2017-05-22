# Initial data exploration

library(dplyr)
library(ggplot2)
library(lubridate)
library(mice)
library(moments)

# import and light cleaning
train <- read.csv('train.csv') %>%
  mutate(ts=ymd(timestamp),
         build_year=ifelse(build_year < 1600 | build_year > 2018,NA,build_year),
         eco_scale=NA,
         eco_scale=ifelse(ecology=='poor',0,eco_scale),
         eco_scale=ifelse(ecology=='satisfactory',1,eco_scale),
         eco_scale=ifelse(ecology=='good',2,eco_scale),
         eco_scale=ifelse(ecology=='excellent',3,eco_scale))
glimpse(train)

# these look like data entry errors; replace with NA
train[13547,c('full_sq','life_sq')]
train[3528,c('full_sq','life_sq')]
train$life_sq[train$life_sq > 5000] <- NA
train$full_sq[train$full_sq > 5000] <- NA
# looks like build year sometimes got mixed up with kitchen square footage
train[c(10369,13118,21416,28735),c('full_sq','life_sq','kitch_sq','build_year')]
train$kitch_sq[train$kitch_sq > 1900] <- NA

# what's the missing data look like?
colSums(is.na(train))

# impute missing data based on all other fields; should probably wait, do this
# later and save the result(s)
train_imp <- mice(train,m=1)

# workhorse plotting function
plot2 <- function(x,y) {
  plotme <- data.frame(x=train[,x],y=train[,y])
  ggplot(plotme,aes(x=x,y=y)) +
    geom_point(size=2,alpha=0.2,color='seagreen4') +
    xlab(x) +
    ylab(y) +
    theme_classic()
}

plot2('ts','logprice')
plot2('full_sq','logprice') + xlim(0,1000)



# what are my strongest correlations with price_doc? ignore factors for now
res <- data.frame()
for (x in colnames(train)) {
  colx <- train[,x]
  if (!class(colx) %in% c('factor','Date') & x != 'id') {
    s <- na.omit(colx) %>% skewness
    k <- na.omit(colx) %>% kurtosis
    nv <- unique(colx) %>% length
    logs <- na.omit(colx) %>% log1p %>% skewness
    logk <- na.omit(colx) %>% log1p %>% kurtosis
    nn <- abs(s) + abs(k-3)
    lognn <- abs(logs) + abs(logk-3)
    logc <- log1p(colx) %>% cor(train$price_doc,use='complete.obs')
    rowx <- data.frame(name=x,nval=nv,c=cor(colx,train$price_doc,use='complete.obs'),
                       logc=logc,skew=s,kurt=k,non_norm=nn,logskew=logs,logkurt=logk,
                       lognn=lognn)
    res <- rbind(res,rowx)
  }
}
res <- res %>%
  mutate(absc = abs(c)) %>%
  arrange(desc(absc))

head(res,20)
# so I've got a fairly large set of moderately-strong predictors
# number of rooms and area are pretty clear
# location matters; especially sports facilities, shopping malls, landmarks

# Test for normality -- do I need to transform some variables?
myhist <- function(x,nb=30) {
  sx <- na.omit(x)
  sx <- sx[sx > -Inf & sx < Inf] %>% scale
  sk <- paste0('skewness = ',round(skewness(sx),3))
  kur <- paste0('kurtosis = ',round(kurtosis(sx),3))
  print(sk)
  print(kur)
  data.frame(x=sx) %>%
    ggplot(aes(x=x)) +
      geom_histogram(bins=nb,fill='seagreen4',color='grey70') +
      geom_vline(xintercept=0,color='tomato') +
      annotate('text',x=min(sx),y=6000,color='midnightblue',label=sk,hjust=0) +
      annotate('text',x=min(sx),y=5000,color='midnightblue',label=kur,hjust=0) +
      theme_classic()
}

loghist <- function(x,nb=30) {
  myhist(log10(x),nb)
}

# extremely high kurtosis could be a sign of data entry errors
res %>% arrange(desc(non_norm)) %>% head(20)
train$mosque_count_500 %>% table # no good way to normalize that; remaining are probably OK

logme <- res %>% 
  filter(nval>100) %>%
  mutate(nndiff=non_norm-lognn) %>%
  arrange(desc(nndiff)) 


# only bother log-transforming the ones that really stick out
qplot(logme$nndiff,1:nrow(logme)) + geom_vline(xintercept=40,color='red')

logme %>% filter(nndiff>40)

# TODO: move this to something that handles cleaning
train <- train %>%
  mutate(ln_life_sq=log1p(life_sq),
         ln_public_transport_station_min_walk=log1p(public_transport_station_min_walk),
         ln_public_healthcare_km=log1p(public_healthcare_km),
         ln_public_transport_station_km=log1p(public_transport_station_km),
         ln_full_sq=log1p(full_sq),
         ln_kindergarten_km=log1p(kindergarten_km),
         ln_preschool_km=log1p(preschool_km),
         ln_school_km=log1p(school_km),
         ln_fitness_km=log1p(fitness_km),
         ln_hospice_morgue_km=log1p(hospice_morgue_km),
         ln_big_church_km=log1p(big_church_km),
         ln_additional_education_km=log1p(additional_education_km),
         ln_church_synagogue_km=log1p(church_synagogue_km),
         ln_price_doc=log1p(price_doc),
         ln_trc_sqm_500=log1p(trc_sqm_500))

# sanity checks: is life_sq always <= full_sq? 
qplot(sort(na.omit(train$full_sq)),1:length(na.omit(train$full_sq))) + xlim(0,50)
qplot(sort(na.omit(train$life_sq)),1:length(na.omit(train$life_sq))) + xlim(0,50)

train %>% filter(full_sq<=1) %>% select(id,life_sq,full_sq) # not sure I believe 1m^2 apartments
train %>% filter(life_sq<=1) %>% select(id,life_sq,full_sq)
train$full_sq[train$full_sq<=1] <- NA
train$life_sq[train$life_sq<=1] <- NA
train %>% filter(life_sq > full_sq) %>% select(id,life_sq,full_sq) # 31 problem cases
# not sure whether to believe full_sq or life_sq in these cases; go with the one closer to median
fix_us <- train %>% 
  filter(life_sq > full_sq) %>%
  select(id,life_sq,full_sq) %>%
  mutate(life_diff=abs(life_sq-median(train$life_sq,na.rm=TRUE)),
         full_diff=abs(full_sq-median(train$full_sq,na.rm=TRUE))) 

full_na <- fix_us %>%
  filter(life_diff < full_diff) %>%
  select(id)
train$full_sq[train$id %in% full_na$id] <- NA
         
life_na <- fix_us %>%
  filter(life_diff > full_diff) %>%
  select(id)
train$life_sq[train$id %in% life_na$id] <- NA

# weird kitchen sizes
train %>% filter(kitch_sq > full_sq) %>% select(id,kitch_sq,full_sq) # 8 problem cases
train %>% filter(kitch_sq > life_sq) %>% select(id,kitch_sq,life_sq,full_sq) # 39 problem cases
bad_kitch <- train %>% filter(kitch_sq > full_sq | kitch_sq > life_sq)
train$kitch_sq[train$id %in% bad_kitch$id] <- NA

# are close distances always < longer ones?

tn <- names(train)
tn[grep('count',tn)] %>% sort 
tn[grep('count',tn)] %>% sort %>% sub('count_.*','count',.) %>% unique
train %>% select(starts_with('big_church_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('cafe_count'),-contains('price')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('church_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('leisure_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('market_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('mosque_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('office_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('sport_count')) %>% apply(1,is.unsorted) %>% sum
train %>% select(starts_with('trc_count')) %>% apply(1,is.unsorted) %>% sum

# TODO:
# remind myself how to do GitHub commits from RStudio...
# do something similar for my factor variables -- which matter the most?
# make a quick eval function that will put together a linear model from a 
#   set of variables and calculate my objective function 
# how well can I do with knn?
# maybe separate variables into groups -- building-specific, location, and temporal
# PCA on each variable group -- a lot of features will co-vary
# can I do anything interesting by geolocating raions?

