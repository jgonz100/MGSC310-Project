#install.packages("glmnet")
#install.packages("glmnetUtils")

library(glmnet)
library(glmnetUtils)

setwd("~/Desktop/MGSC310-Project-master")
nycAB <-  read.csv("AB_NYC_2019.csv")

set.seed(310)

#data cleaning and feature transformation
nycAB <-  nycAB[ , !(names(nycAB) %in% c("name","id", "host_id", "host_name", "latitude", "longitude", "last_review", "calculated_host_listings_count"))]
nycAB <- na.omit(nycAB)
sum(is.na(nycAB))
nycAB$log_price = log1p(nycAB$price)
nycAB <-  nycAB[!(nycAB$log_price < 3),]
nycAB <-  nycAB[!(nycAB$log_price > 7),]
nycAB$log_num_reviews <- log1p(nycAB$number_of_reviews)
nycAB$reviews_per_month_log <-  log1p(nycAB$reviews_per_month)
nycAB$minimum_nights_log <-  log1p(nycAB$minimum_nights)

#for efficiency, take a sample of 30 percent 
sample_idx <- sample(1:nrow(nycAB), size = floor(0.3*nrow(nycAB)))
nycAB <- nycAB[sample_idx,]
#remove these variables; we don't need them
nycAB <- subset(nycAB, select = -c(neighbourhood, price))
#convert "neighbourhood_group" & "room_type" to numerical factors
#NOTE: Bronx == 1, Brookyln == 2, Manhattan == 3, Queens == 4, Staten Island == 5
#NOTE: Entire home/apt == 1, Private room == 2, Shared room == 3
nycAB$neighbourhood_group <- as.factor(as.numeric(as.factor(nycAB$neighbourhood_group)))
nycAB$room_type <- as.factor(as.numeric(as.factor(nycAB$room_type)))

#create train and test from sample
set.seed(310)
train_idx <- sample(1:nrow(nycAB), size = floor(0.75*nrow(nycAB)))
AB_train <- nycAB[train_idx,]
AB_test <- nycAB[-train_idx,]

#run linear model 
AB_lm <- lm(log_price ~ ., AB_train)
summary(AB_lm)

lm_trainpreds <- predict(AB_lm)
lm_testpreds <- predict(AB_lm, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lmtrain <- MSE(lm_trainpreds, AB_train$log_price)
MSE_lmtest <- MSE(lm_testpreds, AB_test$log_price)

df.1 <- data.frame(MSE_train_LM = as.matrix(MSE_lmtrain),
                   MSE_test_LM = as.matrix(MSE_lmtest)) 
df.1

#run lasso model
AB_lasso <- cv.glmnet(log_price ~ ., data = AB_train, alpha = 1)

#finding the best lambda
best_lambda <- AB_lasso$lambda.min
best_lambda

#retrain lasso model & make predictions
lasso_best <- glmnet(log_price ~ ., data = AB_train, alpha = 1, lambda = best_lambda)
lasso_trainpreds <- predict(lasso_best, newdata = AB_train)
lasso_testpreds <- predict(lasso_best, s = best_lambda, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lassotrain <- MSE(lasso_trainpreds,AB_train$log_price)
MSE_lassotest <- MSE(lasso_testpreds, AB_test$log_price)

df.2 <- data.frame(MSE_train_LASSO = as.matrix(MSE_lassotrain),
                 MSE_test_LASSO = as.matrix(MSE_lassotest))
df.2

#finding the important coefficients
coef(lasso_best)




