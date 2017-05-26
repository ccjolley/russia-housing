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
data.frame(sd=pr2$sdev) %>%
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

# TODO: come up with a reasonable set of dependencies between variables
# and establish an imputation matrix


