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

###############################################################################
# Categories of variables in the Russia housing dataset
###############################################################################
nt <- names(train)
dont_use <- c('id','timestamp','ts','price_doc','ln_price_doc','ecology')
building <- c('build_year','state','kitch_sq','max_floor','material',
              'num_room','ln_life_sq','floor','ln_full_sq','product_type')
factors <- c(grep('^ID_',nt,value=TRUE),'sub_area')
loc <- c(grep('_count_',nt,value=TRUE),
         grep('_km',nt,value=TRUE),
         grep('_part',nt,value=TRUE),
         grep('_sqm_',nt,value=TRUE),
         grep('raion',nt,value=TRUE),
         grep('male',nt,value=TRUE),
         grep('female',nt,value=TRUE),
         grep('_all',nt,value=TRUE),
         grep('_walk',nt,value=TRUE),
         grep('_avto',nt,value=TRUE),
         grep('_price_',nt,value=TRUE),
         grep('school',nt,value=TRUE),
         grep('_1line',nt,value=TRUE),
         'eco_scale','area_m','culture_objects_top_25') %>%
  setdiff(factors)
nt %>% setdiff(c(dont_use,building,loc,factors))

###############################################################################
# Given a cleaned dataset, perform PCA on the location-based variables with
# no missing values. Return a dataframe containing the non-complete variables
# along with the first nvars principal components of the complete ones.
###############################################################################
clean_to_pca1 <- function(df,nvars=30,never_use=NULL) {
  nmis <- df %>% is.na %>% colSums
  missing <- names(df)[nmis > 0]
  use_vars <- names(df) %>% setdiff(c(missing,never_use))
  pr <- prcomp(df[,use_vars],center=TRUE,scale=TRUE)
  diag <- data.frame(sd=pr$sdev) %>%
    mutate(var=sd^2,
           prop=var/sum(var),
           cum_prop=cumsum(prop))
  paste0('First ',nvars,' components contain ',diag[nvars,'cum_prop'],
         ' of variance') %>% print
  df[,!(names(df) %in% use_vars)] %>%
    cbind(pr$x[,1:nvars])
}

train_pca1 <- clean_to_pca1(train,30,c(dont_use,factors,building))

###############################################################################
# Given output from clean_to_pca1, generate an imputation matrix that can
# be used by MICE. I want to make something general enough that it will be 
# useful for other projects after this, so I'll have it take the following:
# df - the dataframe that requires imputation, with the PCA columns
#      named PC1, PC2, etc.
# full_dep - an array of variables that depend on the principal components, or 
#            NULL if we're making all incomplete variables depend on them.
# groups - this is a list of arrays, each containing a group of interdependent
#          variables that should depend on each other during imputation. In
#          this case, these are my building-related variables.
###############################################################################
make_imp_matrix <- function(df,full_dep=NULL,groups) {
  nim <- names(df)
  if (is.null(full_dep)) {
    nmis <- df %>% is.na %>% colSums
    full_dep <- names(nmis)[nmis > 0]
  }
  # I never want to impute factor variables with more than 50 levels
  bad_facs <- names(train_pca1)[sapply(train_pca1,function(x) length(levels(x)))>50]
  full_dep <- setdiff(full_dep,bad_facs)
  # now we're ready to go
  pc_indices <- grep('^PC[0-9]+',nim)
  if (class(groups) != 'list') { # passed array instead of list
    groups <- list(groups)
  }
  # set up matrix
  m <- matrix(0,nrow=length(nim),ncol=length(nim))
  for (ni in nim) {
    i <- which(nim==ni)
    if (ni %in% full_dep) { # all missing vars depend on PCs
      m[i,pc_indices] <- 1
    }
    for (g in groups) { # members of each group also depend on each other
      if (ni %in% g & nmis[ni] > 0) {
        other_g <- which(nim %in% g & nim != ni)
        m[i,other_g] <- 1
      }
    }
  }
  m
}

m <- make_imp_matrix(train_pca1,groups=building)
# sanity checks
#nim <- names(train_pca1)
#nim[9]
#nim[m[,9]==1] # what depends on #9
#nim[m[9,]==1] # what does #9 depend on?
#colSums(m)
#rowSums(m)

###############################################################################
# Given a dataframe and a factor variable, expand it into binary dummy 
# variables. Returns the dataframe without the original factor but with
# the dummy variables added.
###############################################################################
add_dummy <- function(df,varname) {
  res <- df
  #res[,varname] <- as.numeric(res[,varname])
  options(na.action='na.pass')
  bindme <- model.matrix(~res[,varname]) 
  options(na.action='na.omit') # back to default
  l <- levels(res[,varname])
  colnames(bindme) <- c('(Intercept)',paste(varname,'_',l[2:length(l)],sep=''))
  res <- cbind(res,bindme) 
  res[,!(names(res) %in% c(varname,'(Intercept)'))]
}

add_dummy(train_pca1,'material') %>% glimpse

###############################################################################
# Given output from clean_to_pca1(), impute missing values. This will also
# take a matrix generated by make_imp_matrix(). Pass in full_df to copy all
# imputed values into a dataframe with the same structure as the original.
###############################################################################
pca1_to_imputed <- function(pca_df,full_df,m,seed=NA) {
  # drop the variables that don't do anything so MICE doesn't choke on them
  ignore_vars <- names(pca_df)[rowSums(m) == 0 & colSums(m)==0]
  keep_var_id <- which((!names(pca_df) %in% ignore_vars))
  drop_df <- pca_df[,keep_var_id]
  drop_m <- m[keep_var_id,keep_var_id]
  # now run MICE
  mice_out <- mice(drop_df,m=1,predictorMatrix=drop_m,seed=seed) %>% complete
  # copy MICE results back into full_df format
  if (is.null(full_df)) { full_df <- pca_df }
  nim <- names(drop_df)
  nim <- setdiff(nim,grep('^PC[0-9]+',nim,value=TRUE))
  res <- full_df
  for (n in nim) {
    res[,n] <- mice_out[,n]
  }
  res
}

train_imp <- pca1_to_imputed(train_pca1,train,m,12345)
# ID_railroad_station_walk was in there. Why?
names(train_pca1)
  
###############################################################################
# Given a fully-imputed dataframe with no missing values, separate variables
# into pre-specified groups, run PCA on each, and output a dataframe where
# the specified number of PC's from each category has been kept.
# 
# groups should be a list of character arrays containing names of variables
# in each group
#
# nvars should be a numeric array giving the number of components to keep 
# from each group
#
# labels should be a character array used to rename PC variables
###############################################################################
group_pca <- function(df,groups,nvars,labels) {
  if (length(groups) != length(nvars) | length(nvars) != length(labels)) {
    print('ERROR: groups and nvars should have the same length')
    return(NULL)
  }
  res <- data.frame()
  for (i in 1:length(nvars)) {
    # convert factors to dummy variables and add
    pr_me <- df[,groups[[i]]]
    flist <- names(pr_me)[lapply(pr_me,class)=='factor']
    for (f in flist) {
      pr_me <- add_dummy(pr_me,f)
    }
    pr_i <- prcomp(pr_me,center=TRUE,scale=TRUE)
    pr_i_x <- pr_i$x[,1:nvars[i]]
    colnames(pr_i_x) <- sub('PC',labels[i],colnames(pr_i_x))
    diag <- data.frame(sd=pr_i$sdev) %>%
      mutate(var=sd^2,
             prop=var/sum(var),
             cum_prop=cumsum(prop))
    paste0('First ',nvars[i],' components of group ',i,' contain ',
           diag[nvars[i],'cum_prop'],' of variance') %>% print
    if (nrow(res) == 0) {
      res <- data.frame(pr_i_x)
    } else {
      res <- cbind(res,pr_i_x)
    }
  }
  res
}

train_pca2 <- group_pca(train_imp,list(loc,building),nvars=c(38,9),
                        labels=c('loc_','build_'))

###############################################################################
# Test how well linear models do now
###############################################################################

train_pca2$ln_price_doc <- train$ln_price_doc

lm_pca <- lm(ln_price_doc ~ .,train_pca2)
pred_pca <- train_pca2 %>% select(ln_price_doc)
pred_pca <- merge(pred_pca,predict(lm_pca),by='row.names')
(pred_pca$ln_price_doc - pred_pca$y)^2 %>% mean %>% sqrt
# 0.2814239, compared to 0.5151 in linear.R
qplot(pred_pca$ln_price_doc,pred_pca$y)

###############################################################################
# Now it's time to get real
###############################################################################

test <- read.csv('test.csv') %>% russia_clean
alldata <- rbind(train,test)

all_pca1 <- clean_to_pca1(alldata,30,c(dont_use,factors,building))
all_m <- make_imp_matrix(all_pca1,groups=building)
all_imp <- pca1_to_imputed(all_pca1,alldata,m,12345)
# apparently didn't succeed at imputing everything in one go
names(all_imp)[colSums(is.na(all_imp))>0]
# do cheap imputation here
x <- is.na(all_imp$cafe_avg_price_5000)
all_imp$cafe_avg_price_5000[x] <- median(all_imp$cafe_avg_price_5000,na.rm=TRUE)
x2 <- is.na(all_imp$ln_full_sq)
all_imp$ln_full_sq[x2] <- median(all_imp$ln_full_sq,na.rm=TRUE)

all_pca2 <- group_pca(all_imp,list(loc,building),nvars=c(38,9),
                        labels=c('loc_','build_'))

all_pca2$ln_price_doc <- alldata$ln_price_doc # this will be NA for items in test set
lm_all <- lm(ln_price_doc ~ .,all_pca2)
pred_pca <- all_pca2 
pred_pca$id <- alldata$id
pred_pca <- pred_pca %>% filter(is.na(ln_price_doc))
pred_pca <- merge(pred_pca,predict(lm_pca,newdata=pred_pca),by='row.names')
submit <- pred_pca %>%
  mutate(price_doc=exp(y)-1) %>%
  select(id,price_doc) %>%
  arrange(id)

write.csv(submit,'submit-0608.csv',row.names=FALSE)

# Leaderboard result wasn't spectacular -- 0.75032
# I wonder if it's just not a great model or if I did something wrong at the end...


