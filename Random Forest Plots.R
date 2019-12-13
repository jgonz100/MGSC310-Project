#install.packages("glmnet")
#install.packages("glmnetUtils")
#install.packages("tree")
#install.packages("randomForest")

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
nycAB <- subset(nycAB, select = c(price, log_price, neighbourhood_group, number_of_reviews,
                                  log_num_reviews, minimum_nights, minimum_nights_log,
                                  reviews_per_month, reviews_per_month_log, room_type,
                                  availability_365))
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

#run linear model using price (ARIANA)
AB_lm1 <- lm(price ~ ., AB_train)
summary(AB_lm1)

lm1_trainpreds <- predict(AB_lm1)
lm1_testpreds <- predict(AB_lm1, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lm1train <- MSE(lm1_trainpreds, AB_train$price)
MSE_lm1test <- MSE(lm1_testpreds, AB_test$price)

df.1 <- data.frame(MSE_train_LM1 = as.matrix(MSE_lm1train),
                   MSE_test_LM1 = as.matrix(MSE_lm1test)) 
df.1

#run linear model using log_price (ARIANA)
AB_lm2 <- lm(log_price ~ ., AB_train)
summary(AB_lm2)

lm2_trainpreds <- predict(AB_lm2)
lm2_testpreds <- predict(AB_lm2, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lm2train <- MSE(lm2_trainpreds, AB_train$log_price)
MSE_lm2test <- MSE(lm2_testpreds, AB_test$log_price)

df.2 <- data.frame(MSE_train_LM2 = as.matrix(MSE_lm2train),
                   MSE_test_LM2 = as.matrix(MSE_lm2test)) 
df.2

#run lasso model using price (ARIANA)
AB_lasso1 <- cv.glmnet(price ~ ., data = AB_train, alpha = 1)

#finding the best lambda
best_lambda1 <- AB_lasso1$lambda.min
best_lambda1

#retrain lasso model & make predictions
lasso_best1 <- glmnet(price ~ ., data = AB_train, alpha = 1, lambda = best_lambda1)
lasso1_trainpreds <- predict(lasso_best1, newdata = AB_train)
lasso1_testpreds <- predict(lasso_best1, s = best_lambda1, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lasso1train <- MSE(lasso1_trainpreds,AB_train$price)
MSE_lasso1test <- MSE(lasso1_testpreds, AB_test$price)

df.3 <- data.frame(MSE_train_LASSO1 = as.matrix(MSE_lasso1train),
                   MSE_test_LASSO1 = as.matrix(MSE_lasso1test))
df.3

#finding the important coefficients
coef(lasso_best1)

#run lasso model using log_price (ARIANA)
AB_lasso2 <- cv.glmnet(log_price ~ ., data = AB_train, alpha = 1)

#finding the best lambda
best_lambda2 <- AB_lasso2$lambda.min
best_lambda2

#retrain lasso model & make predictions
lasso_best2 <- glmnet(log_price ~ ., data = AB_train, alpha = 1, lambda = best_lambda2)
lasso2_trainpreds <- predict(lasso_best2, newdata = AB_train)
lasso2_testpreds <- predict(lasso_best2, s = best_lambda1, newdata = AB_test)

MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_lasso2train <- MSE(lasso2_trainpreds,AB_train$log_price)
MSE_lasso2test <- MSE(lasso2_testpreds, AB_test$log_price)

df.4 <- data.frame(MSE_train_LASSO2 = as.matrix(MSE_lasso2train),
                   MSE_test_LASSO2 = as.matrix(MSE_lasso2test))
df.4

#finding the important coefficients
coef(lasso_best2)


#run a decision tree using price (JADYN)
library(tree)
regMod = tree(price ~ neighbourhood_group + room_type 
              + minimum_nights + number_of_reviews + reviews_per_month
              + availability_365 + log_num_reviews + reviews_per_month_log 
              + minimum_nights_log,
              data = AB_train)

#run a decision tree using log_price (JADYN)
logMod = tree(log_price ~ neighbourhood_group + room_type 
              + minimum_nights + number_of_reviews + reviews_per_month
              + availability_365 + log_num_reviews + reviews_per_month_log 
              + minimum_nights_log,
              data = AB_train)

#we'll plot both
plot(regMod)
text(regMod, pretty=0)

plot(logMod)
text(logMod, pretty = 0)

#using cross validation to find the best tree size for pruning
cvTreeR = cv.tree(regMod)
bestIdx = which.min(cvTreeR$dev)
cvTreeR$size[bestIdx]
#best size is 6

cvTreeL = cv.tree(logMod)
bestIdx = which.min(cvTreeL$dev)
cvTreeL$size[bestIdx]
#best size is 4

#ince we have our best tree sizes, we can prune each tree and generate predictions
prunedTreeR = prune.tree(regMod, best = 5)
predsTrainR = predict(prunedTreeR)
predsTestR = predict(prunedTreeR, newdata = AB_test)

prunedTreeL = prune.tree(logMod, best = 4)
predsTrainL = predict(prunedTreeL)
predsTestL = predict(prunedTreeL, newdata = AB_test)

#we'll output the MSE for each model
MSE = function(p,t){
  mean((t-p)^2)
}

MSE(predsTrainR, AB_train$price) #9440.481
MSE(predsTestR, AB_test$price) #8881.773

MSE(predsTrainL, AB_train$log_price) #0.2186872
MSE(predsTestL, AB_test$log_price) #0.2087743

#The MSE is much lower when we use the log transformation.
#We do get a lower MSE in the test set which may be an indication that 
#the model is overfitting the data.


#MY CODE STARTING HERE 
#run a RF model using log_price (Bryce & Matt)
library(randomForest)
set.seed(2019) #run the model with the set.seed!
#setting mtry to 5 as cross-validated best number
#maxnodes == 160  and ntree == 500 to minimize MSE while still optimizing efficiency
bag_nycAB <- randomForest(log_price ~ neighbourhood_group + room_type 
                          + availability_365 + log_num_reviews + reviews_per_month_log 
                          + minimum_nights_log,
                          data = AB_train,
                          mtry = 5,
                          maxnodes = 160,
                          ntree = 500,
                          importance = TRUE)

install.packages("forecast")
library(forecast)

#d
#generating residuals for test and train
residual_train <- AB_train$log_price - preds_bag_nycAB
residual_test <- AB_test$log_price - preds_bag_nycAB_test

#e
#creating data frame of residuals and predicted values
mod3_train_df <- data.frame(
  resids_train <- residual_train,
  pred_train <- preds_bag_nycAB)

mod3_test_df <- data.frame(
  resids_test<- residual_test,
  pred_test <- preds_bag_nycAB_test)

#plotting residuals against predicted values from training set 
#This plot is heteroskedatic, as the variance in error term is non-constant throughout 
ggplot(mod3_train_df,aes(x=pred_train,y=resids_train)) + geom_point(alpha=0.5) + geom_smooth(color="red")
ggplot(mod3_test_df,aes(x=pred_test,y=resids_test)) + geom_point(alpha=0.5) + geom_smooth(color="red")

bag_nycAB
importance(bag_nycAB)
varImpPlot(bag_nycAB)

plot(MSE_test, AB_test$log_price)
abline(0,1,col="red")

#prediction of train data
preds_bag_nycAB <- predict(bag_nycAB, newdata = AB_train)
#prediction of test data
preds_bag_nycAB_test<- predict(bag_nycAB, newdata = AB_test)

plot(preds_bag_nycAB, AB_train$log_price)
abline(0,1,col="red")
plot(preds_bag_nycAB_test, AB_test$log_price)
abline(0,1,col="red")

#MSE test
MSE_test <- MSE(preds_bag_nycAB_test, AB_test$log_price)
#.1888732

#MSE train
MSE_train <- MSE(preds_bag_nycAB, AB_train$log_price)
#.1635333

#Random forest model has lowest error, so we will use this one
