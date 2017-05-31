library(mice)
library(ggplot2)

source('clean.R')
train <- read.csv('train.csv') %>% russia_clean

# Just running MICE on the whole thing is too slow -- I need something more
# efficient. Which variables are actually missing the most often?

nmis <- train %>% is.na %>% colSums %>% sort(decreasing=TRUE)
nmis <- nmis[nmis > 0]
length(nmis) # 53 variables have missing values
qplot(1:length(nmis),nmis/nrow(train))

# Strongest correlations with my missing variables?
# TODO: instead of full_only, have this take a minimum number of complete 
# variables
best_cor <- function(varname,n=10,full_only=FALSE) {
  res <- data.frame()
  for (x in colnames(train)) {
    colx <- train[,x]
    nm <- is.na(colx) %>% sum
    if (!class(colx) %in% c('factor','Date') & 
        !(x %in% c('id','price_doc','ln_price_doc',varname)) &
        (!full_only | nm==0)) {
      c <- cor(colx,train[,varname],use='complete.obs')
      rowx <- data.frame(name=x,c=c,nm=nm)
      res <- rbind(res,rowx)
    }
  }
  res <- res %>%
    mutate(absc = abs(c)) %>%
    arrange(desc(absc)) %>% select(-absc)
  head(res,n)
}

best_cor('build_year')
best_cor('max_floor')

# Build an imputation matrix based on these correlations. Not sure how to make
# this work for factor variables yet.

imp_matrix <- function(df,all_n=5,full_n=5) {
  tn <- ncol(df)
  paste0('tn = ',tn) %>% print
  res <- matrix(0,nrow=tn,ncol=tn)
  cn <- colnames(df)
  for (i in intersect(names(nmis),cn)) {
    if (class(df[,i]) %in% c('integer','numeric')) {
      paste0('i = ',i) %>% print
      bc_all <- best_cor(i,all_n,FALSE) # could make this faster by not calling best_cor twice
      bc_full <- best_cor(i,full_n,TRUE)
      add_us <- c(as.character(bc_all$name),as.character(bc_full$name)) 
      ni <- which(colnames(df)==i)
      for (j in add_us) {
        nj <- which(colnames(df)==j)    
        # rows are target variable, columns are predictors
        res[ni,nj] <- 1
      }
    }
  }
  res
}

m <- imp_matrix(train,1,1) 
sub_train <- train[c(which(colSums(m) > 0),which(rowSums(m) > 0)) %>% unique]
sub_m <- imp_matrix(sub_train,1,1)
l <- which(names(train) %in% names(sub_train))
sub_m2 <- m[l,l]
# this seems to be working but takes a while, even with just 1-2 predictors each
t1 <- Sys.time()
imp <- mice(sub_train,predictorMatrix=sub_m)
t2 <- Sys.time()
t2-t1

# test this out with just one variable
best_cor('build_year')
small <- train[,c('build_year','zd_vokzaly_avto_km')] # 1 to impute, 1 complete
t1 <- Sys.time()
small_imp <- mice(small)
t2 <- Sys.time()
t2 - t1 # takes ~2min -- minimum of 106min for 53 vars

# TODO: maybe the thing to do here is just to start with PCA for complete variables and use that for
# imputation

loc_complete <- setdiff(names(train),names(nmis)) %>% 
  setdiff(c('price_doc','ln_price_doc','ts','id','timestamp','product_type','ecology'))
# dummy variables didn't help much
train_loc <- train[,loc_complete] %>%
  select(-sub_area,-ID_metro,-ID_railroad_station_avto,-ID_big_road1,
         -ID_big_road2,-ID_railroad_terminal,-ID_bus_terminal)
# Now it's PCA time
pr <- prcomp(train_loc,center=TRUE,scale=TRUE)
qplot(pr$x[,1],pr$x[,2])
data.frame(sd=pr$sdev) %>%
  mutate(var=sd^2,
         prop=var/sum(var),
         cum_prop=cumsum(prop),
         pc=row_number()) %>%
  ggplot(aes(x=pc,y=cum_prop)) +
    geom_point(color='cornflowerblue') +
    theme_classic() +
    geom_hline(yintercept=0.9,color='tomato')
# about 30 vars gets me to 90% of the location-related variance

########################################
# Benchmark speeds of different imputation methods
########################################
test <- cbind(train$build_year,pr$x[,1:30])

system.time(imp <- mice(test,m=1,method='sample')) # 1.22s
system.time(imp <- mice(test,m=1,method='mean')) # 0.85
system.time(imp <- mice(test,m=1,method='pmm')) # 26.16s
system.time(imp <- mice(test,m=1,method='fastpmm')) # 23.97s

test2 <- cbind(train[,c('build_year','state')],pr$x[,1:30])
system.time(imp2 <- mice(test2,m=1,method='pmm')) # 46.69s

# TODO: can I bring factor variables back in? what happens then?

test3 <- cbind(train$material,pr$x[,1:30])
system.time(imp <- mice(test3,m=1,method='pmm')) # 19.57s
# factor variables don't seem to take longer to impute
test4 <- cbind(train[,c('build_year','ID_metro')],pr$x[,1:30]) 
system.time(imp <- mice(test4,m=1,method='pmm')) # start at 16:13

# TODO: come up with a reasonable set of dependencies between variables
# and establish an imputation matrix

# These only depend on location; impute them based on my 30 PCA variables
# There are 43 of them; I could expect this part to take ~20 min.
loc_dep <- c('preschool_quota','school_quota','cafe_sum_1000_min_price_avg',
             'cafe_sum_1000_max_price_avg','cafe_avg_price_1000',
             'raion_build_count_with_material_info','build_count_block',
             'build_count_wood','build_count_frame','build_count_brick',
             'build_count_monolith','build_count_panel','build_count_foam',
             'build_count_slag','build_count_mix',
             'raion_build_count_with_builddate_info','build_count_before_1920',
             'build_count_1921.1945','build_count_1946.1970',
             'build_count_1971.1995','build_count_after_1995',
             'cafe_sum_1500_min_price_avg','cafe_sum_1500_max_price_avg',
             'cafe_avg_price_1500','cafe_sum_2000_min_price_avg',
             'cafe_sum_2000_max_price_avg','cafe_avg_price_2000',
             'cafe_sum_3000_min_price_avg','cafe_sum_3000_max_price_avg',
             'cafe_avg_price_3000','cafe_sum_5000_min_price_avg',
             'cafe_sum_5000_max_price_avg','cafe_avg_price_5000',
             'prom_part_5000','metro_min_walk','metro_km_walk',
             'railroad_station_walk_km','railroad_station_walk_min',
             'ID_railroad_station_walk','hospital_beds_raion',
             'cafe_sum_500_min_price_avg','cafe_sum_500_max_price_avg',
             'cafe_avg_price_500')

# These concern specific buildings -- they'll be informed by location, but 
# also depend on each other.
building <- c('build_year','state','kitch_sq','max_floor','material',
               'num_room','ln_life_sq','floor','ln_full_sq')

# don't impute target variables (price), irrelevant variables (timestamp),
# or variables that have been replaced (ecology)
# also don't impute factor variables with lots of levels (ID_*)
impute_me <- train[,!(names(train) %in% loc_complete)] %>%
  select(-id,-timestamp,-price_doc,-ln_price_doc,-ts,-ecology,
         -ID_railroad_station_walk) %>%
  cbind(pr$x[,1:30])
               
nim <- names(impute_me)
m <- matrix(0,nrow=length(nim),ncol=length(nim))
for (ni in nim) {
  i <- which(nim==ni)
  # location for everything
  if (ni %in% names(nmis)) {
    m[i,(length(nim)-29):length(nim)] <- 1
  }
  # building also depend on each other
  if (ni %in% building) {
    other_building <- which(nim %in% building & nim != ni)
    m[i,other_building] <- 1
  }
}

# merge imputed variables back into train

imputed <- mice(impute_me,m=1,predictorMatrix=m)

saveRDS(imputed, file="imputed.rds")
#imputed <- readRDS("imputed.rds")

ci <- complete(imputed)
train_imp <- train 
for (n in nim[1:(length(nim)-30)]) {
  train_imp[,n] <- ci[,n]
}
# fix factor variables with few levels

is.na(train_imp) %>% colSums 
# only ID_railroad_station_walk still has NA's, but I'm not going to use
# those ID variables in PCA anyway.

# separate building and location-related variables, do PCA separately

nti <- names(train_imp)
dont_use <- c('id','timestamp','ts','price_doc','ln_price_doc','ecology')
factors <- c('sub_area',grep('^ID',nti,value=TRUE))
build_all <- c(building,'product_type')
loc_all <- c('eco_scale',loc_dep,loc_complete) %>% setdiff(factors)
setdiff(nti,c(dont_use,factors,build_all,loc_all))

# TODO: why wasn't eco_scale in loc_dep?

pc_loc <- prcomp(train_imp[,loc_all],center=TRUE,scale=TRUE)
# I now need 38 out of 271 components to get 90% of variance
pc_loc_x <- pc_loc$x[,1:38]
colnames(pc_loc_x) <- sub('PC','LOC',colnames(pc_loc_x))

buildvars <- train_imp[,build_all] %>%
  mutate(product_type=as.numeric(product_type)) %>%
  cbind(model.matrix(~train_imp$material)) %>% 
  select(-material,-`(Intercept)`)
pc_build <- prcomp(buildvars,center=TRUE,scale=TRUE)
# I can get 90% of variance with 9 of these 14 variables
pc_build_x <- pc_build$x[,1:9]
colnames(pc_build_x) <- sub('PC','B',colnames(pc_build_x))

# combine back into one data frame with PCA results and ln_price_doc

train_pc <- train %>% 
  select(ln_price_doc) %>%
  cbind(pc_build_x,pc_loc_x)

# how well do linear models do now?

lm_pca <- lm(ln_price_doc ~ .,train_pc)
pred_pca <- train_pc %>% select(ln_price_doc)
pred_pca <- merge(pred_pca,predict(lm_pca),by='row.names')

(pred_pca$ln_price_doc - pred_pca$y)^2 %>% mean %>% sqrt
# 0.4888297, compared to 0.5151 in linear.R
qplot(pred_pca$ln_price_doc,pred_pca$y)

# does test set have any missing values in it?

test_raw <- read.csv('test.csv') # this does contain some NA's
test <- test_raw %>% russia_clean

# TODO: I need a function that, given an original data row with possible 
# missing values (as in the test set), will do the following:
# 1. initial location-based PCA projection (based on data from training set)
# 2. imputation based on data from training data
# 3. PCA projection using imputed values
# 4. return a row with just the needed PCA components

# then I can deploy this on a linear model (or whatever else I fancy)
