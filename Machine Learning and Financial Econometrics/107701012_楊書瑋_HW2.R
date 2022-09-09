library(data.table)
library(tidyverse)  
library(plm)
#Data preparation
#1
load("~/Documents/R/data_ml.RData")
data <- data_ml
data <- data.table(data)

#2
data <- data[date %between% c("2006-12-31","2010-01-01")] %>%
  select("date","R12M_Usd", "Mkt_Cap_12M_Usd", "Pb", "Sales_Ps",
         "Mom_11M_Usd", "Vol1Y_Usd", "Roa", "Mom_Sharp_11M_Usd",
         "Ebit_Noa", "Roe", "Share_Turn_12M", "Ev_Ebitda", "Ebitda_Margin",
         "Asset_Turnover", "Capex_Sales", "Total_Debt_Capital", "Op_Prt_Margin") %>%
  filter(date == "2006-12-31" | date == "2007-12-31" | date == "2008-12-31" | date == "2009-12-31")

#3
training_sample <- data[date %between% c("2006-12-31","2008-01-01")]
validation_sample <- data[date == "2008-12-31"]


#Estimation and forecast evaluation
#1 Ridge regression and lasso
#a
grid <- 10^seq(3, -3,length=100)
x <- model.matrix(R12M_Usd ~ ., training_sample)[, -1][, -1]
y <- training_sample$R12M_Usd
#Ridge Regression
library(glmnet)
ridge <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge)
#lasso
lasso <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso)

#b
x_test <- model.matrix(R12M_Usd ~ ., validation_sample)[, -1][, -1]
y_test <- validation_sample$R12M_Usd
#Ridge Regression
ridge.pred <- predict(ridge, newx = x_test)
ridge.pred

low_mse <- mean((ridge.pred[1] - y_test)^2)
for(i in c(1:100)){ 
  mse <- mean((ridge.pred[i] - y_test)^2)
  if(mse <= low_mse){
    ridge_lambda <- ridge$lambda[i]
    low_mse <- mean((ridge.pred[i] - y_test)^2)
  }
}
ridge_lambda #the lambda which has the lowest MSE for ridge regression is 0.06579332

#lasso
lasso.pred <- predict(lasso, newx = x_test)
lasso.pred

low_mse <- mean((lasso.pred[100] - y_test)^2)
for(i in c(1:100)){ 
  mse <- mean((lasso.pred[i] - y_test)^2)
  if(mse <= low_mse){
    lasso_lambda <- lasso$lambda[i]
    low_mse <- mean((lasso.pred[i] - y_test)^2)
  }
}
lasso_lambda #the lambda which has the lowest MSE for lasso regression is 0.001

#c
full_sample <- data[date %between% c("2006-12-31","2009-01-01")]
x_full <- model.matrix(R12M_Usd ~ ., full_sample)[, -1][, -1]
y_full <- full_sample$R12M_Usd
#ridge
ridge_full <- glmnet(x_full, y_full, alpha = 0, lambda = ridge_lambda)
#lasso
lasso_full <- glmnet(x_full, y_full, alpha = 1, lambda = lasso_lambda)

#d
fore_sample <- data[date == "2009-12-31"]
x_fore <- model.matrix(R12M_Usd ~ ., fore_sample)[, -1][, -1]
y_fore <- fore_sample$R12M_Usd
#ridge
ridge.fore <- predict(ridge_full, newx = x_fore)
RMSE_ridge <- sqrt(mean((ridge.fore - y_fore)^2))
PCSP_ridge <- mean(sign(y_fore) == sign(ridge.fore))
SpearmanCor_ridge <- cor(y_fore, ridge.fore, method="spearman")

#lasso
lasso.fore <- predict(lasso_full, newx = x_fore)
RMSE_lasso <- sqrt(mean((lasso.fore - y_fore)^2))
PCSP_lasso <- mean(sign(y_fore) == sign(lasso.fore))
SpearmanCor_lasso <- cor(y_fore, lasso.fore, method="spearman")


#2 Principal component regression and partial least squares
#a
train_sample <- data[date %between% c("2006-12-31","2008-01-01")]
valid_sample <- data[date == "2008-12-31"]
fore_sample <- data[date == "2009-12-31"]

x_train <- model.matrix(R12M_Usd ~ ., train_sample)[, -1][, -1]
y_train <- train_sample$R12M_Usd
x_test <- model.matrix(R12M_Usd ~ ., valid_sample)[, -1][, -1]
y_test <- valid_sample$R12M_Usd
x_fore <- model.matrix(R12M_Usd ~ ., fore_sample)[, -1][, -1]
y_fore <- fore_sample$R12M_Usd

#b
library(pls)

pcr_result <- c()
for(i in c(1:5)){ 
  pcr.fit <- pcr(y_train ~ x_train, scale = TRUE, ncomp = i)
  pcr_pred <- predict(pcr.fit, x_test)
  pcr_result <- c(pcr_result, mean((pcr_pred-y_test)^2))
}
min(pcr_result)
match(min(pcr_result), pcr_result)
#When component is 4, it has better result.

#Use ncomp = 4 to forecast
full_sample <- data[date %between% c("2006-12-31","2009-01-01")]
x_full <- model.matrix(R12M_Usd ~ ., full_sample)[, -1][, -1]
y_full <- full_sample$R12M_Usd
pcr.fit <- pcr(y_full ~ x_full, scale = TRUE, ncomp = 4)
pcr_pred <- predict(pcr.fit, x_fore)

#Analysis its performance
RMSE <- sqrt(mean((pcr_pred - y_fore)^2))
PCSP <- mean(sign(y_fore) == sign(pcr_pred))
SpearmanCor <- cor(y_fore, yhat, method="spearman")

#4 Boosted regression tree
train_sample <- data[date %between% c("2006-12-31","2008-01-01")]
valid_sample <- data[date == "2008-12-31"]
fore_sample <- data[date == "2009-12-31"]
full_sample <- data[date %between% c("2006-12-31","2009-01-01")]

x_train <- model.matrix(R12M_Usd ~ ., train_sample)[, -1][, -1]
y_train <- train_sample$R12M_Usd
x_test <- model.matrix(R12M_Usd ~ ., valid_sample)[, -1][, -1]
y_test <- valid_sample$R12M_Usd
x_fore <- model.matrix(R12M_Usd ~ ., fore_sample)[, -1][, -1]
y_fore <- fore_sample$R12M_Usd
x_full <- model.matrix(R12M_Usd ~ ., full_sample)[, -1][, -1]
y_full <- full_sample$R12M_Usd

library(xgboost)

trainData <- xgb.DMatrix(data = x_train, label = y_train) 

result <- c()
for(i in c(1:6)){ 
  boosted_tree <- xgb.train(list(max_depth = i,
                                 eta = 0.01,
                                 colsample_bytree = 0.3,
                                 objective = "reg:squarederror"),
                            trainData, nrounds = 100)
  yhat <- predict(boosted_tree, x_test)
  result <- c(result, mean((y_test - yhat)^2))
}
min(result)
match(min(result), result)
# So, when the max depth is 6, it has the best result.

#Use max_depth = 6 to forecast
trainData <- xgb.DMatrix(data = x_full, label = y_full)
boosted_tree <- xgb.train(list(max_depth = 6,
                               eta = 0.01,
                               colsample_bytree = 0.3,
                               objective = "reg:squarederror"),
                          trainData, nrounds = 100)
yhat <- predict(boosted_tree, x_fore)

#Analysis its performance
RMSE <- sqrt(mean((yhat - y_fore)^2))
PCSP <- mean(sign(y_fore) == sign(yhat))
SpearmanCor <- cor(y_fore, yhat, method="spearman")
