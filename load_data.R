source('clean.R')
source('impute.R')

make_RDS <- function() {
  # First, process training set only
  train <- read.csv('train.csv') %>% russia_clean
  train_pca1 <- clean_to_pca1(train,30,c(dont_use,factors,building))
  fd <- setdiff(names(train),c('ln_price_doc','price_doc',dont_use,factors))
  m <- make_imp_matrix(train_pca1,full_dep=fd,groups=building)
  train_imp <- pca1_to_imputed(train_pca1,train,m,12345)
  saveRDS(train_imp,'train_imp.rds')

  # Append test set
  test <- read.csv('test.csv') %>% russia_clean
  alldata <- rbind(train,test)
  all_pca1 <- clean_to_pca1(alldata,30,c(dont_use,factors,building))
  all_m <- make_imp_matrix(all_pca1,full_dep=fd,groups=building)
  all_imp <- pca1_to_imputed(all_pca1,alldata,all_m,123456)
  saveRDS(all_imp,'all_imp.rds')
}

load_RDS <- function() {
  train <<- read.csv('train.csv') %>% russia_clean
  train_imp <<- readRDS('train_imp.rds')
  train_pca2 <<- group_pca(train_imp,list(loc,building),nvars=c(38,9),
                          labels=c('loc_','build_'))
  train_pca2$ln_price_doc <<- train$ln_price_doc
  
  test <<- read.csv('test.csv') %>% russia_clean
  alldata <<- rbind(train,test)
  all_imp <<- readRDS('all_imp.rds')
  all_pca2 <<- group_pca(all_imp,list(loc,building),nvars=c(38,9),
                        labels=c('loc_','build_'))
  all_pca2$ln_price_doc <<- alldata$ln_price_doc # this will be NA for items in test set
}