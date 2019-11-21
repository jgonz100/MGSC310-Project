install.packages("glmnet")
install.packages("glmnetUtils")

library(glmnet)
library(glmnetUtils)

#whole code must be run with seed
set.seed(310)

#create train and test
train_idx <- sample(1:nrow(nycAB), size = floor(0.75*nrow(nycAB)))
AB_train <- nycAB[train_idx,]
AB_test <- nycAB[-train_idx,]

AB_lasso <- cv.glmnet(price ~ ., data = AB_train, alpha = 1)

coef_mat <- data.frame(Lasso_min = as.matrix(
  round(coef(AB_lasso, s = AB_lasso$lambda.min),3)),
  Lasso_1se = as.matrix(round(coef(
  AB_lasso, s = AB_lasso$lambda.1se), 3)))
colnames(coef_mat) <- c("Lasso_min", "Lasso_1se")

coef_mat

#make predictions
preds_train <- predict(AB_lasso)
preds_test <- predict(AB_lasso, newdata = AB_test)
MSE <- function(p,t){
  mean((t-p)^2)
}

MSE_train <- MSE(preds_train,AB_train$price)
MSE_test <- MSE(preds_test, AB_test$price)
df <- data.frame(MSE_train = as.matrix(MSE_train),
                 MSE_test = as.matrix(MSE_test))







