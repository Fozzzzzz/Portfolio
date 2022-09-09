library(readr)
library(data.table)
library(tidyverse)  
library(caret)
library(glmnet)

## Data preparation
data <- read_csv("~/Documents/R/Final Project/data.csv", col_names = FALSE, skip = 1) 
data <- as.data.table(data)

## Pre-process
any(is.na(data))  #check na

colnames(data)[1]<- "bankrupt"
summary(data)

## train dataset 80%, test dataset 20%
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))
train = data[train.index, ]
test = data[-train.index, ]

#---------------------------#
#  Full model
#---------------------------#
## Use all parameters to predict
model_by_full <- glm(bankrupt ~ ., data = train, family = 'binomial')

full_result <- as.vector(ifelse(predict(model_by_full, test[, -1], type = "response")>0.5, 1, 0))
full_table <- as.matrix(table(as.matrix(test[, 1]), full_result))
confusionMatrix(full_table) # to see its result and accuracy

## Furthermore, use cv.glmnet to obtain precise probability of bankruptcy
model_cv_full <- cv.glmnet(as.matrix(train[, -1]),
                           as.matrix(train[, 1]),
                           family = 'binomial')
cv_full_prob <- predict(model_cv_full, s = "lambda.min", as.matrix(test[, -1]), type = "response")
# if setting over 50% as bankrupt standard and its result
cv_full_result <- as.vector(ifelse(cv_full_prob>0.5, 1, 0))
cv_full_table <- as.matrix(table(as.matrix(test[, 1]), cv_full_result))
confusionMatrix(cv_full_table)

#---------------------------#
#  Correlation - 5
#---------------------------#
## Choose the top 5 absolute correlation to predict
cor.matrix<-as.matrix(cor(data))
correlation<-order(abs(cor.matrix[2:96]), decreasing = T)

## Take the data with the parameters we need
cor5_vari <- colnames(data)[correlation[1:5]+1]
cor5_train <- train[, c("bankrupt", cor5_vari), with=F]
cor5_test <- test[, c("bankrupt", cor5_vari), with=F]

model_by_cor5 <- glm(bankrupt ~ ., data = cor5_train, family = 'binomial')

cor5_result <- as.vector(ifelse(predict(model_by_cor5, cor5_test[, -1], type = "response")>0.5, 1, 0))
cor5_table <- as.matrix(table(as.matrix(cor5_test[, 1]), cor5_result))
confusionMatrix(cor5_table) # to see its result and accuracy

## Furthermore, use cv.glmnet to obtain precise probability of default
model_cv_cor5 <- cv.glmnet(as.matrix(cor5_train[, -1]),
                           as.matrix(cor5_train[, 1]),
                           family = 'binomial')
cv_cor5_prob <- predict(model_cv_cor5, s = "lambda.min", as.matrix(cor5_test[, -1]), type = "response")
# if setting over 50% as bankrupt standard and its result
cv_cor5_result <- as.vector(ifelse(cv_cor5_prob>0.5, 1, 0))
cv_cor5_table <- as.matrix(table(as.matrix(cor5_test[, 1]), cv_cor5_result))
confusionMatrix(cv_cor5_table)

#---------------------------#
#  Correlation - 15
#---------------------------#
## Choose the top 15 absolute correlation to predict
cor.matrix<-as.matrix(cor(data))
correlation<-order(abs(cor.matrix[2:96]), decreasing = T)
## Take the data with the parameters we need
cor15_vari <- colnames(data)[correlation[1:15]+1]
cor15_train <- train[, c("bankrupt", cor15_vari), with=F]
cor15_test <- test[, c("bankrupt", cor15_vari), with=F]

model_by_cor15 <- glm(bankrupt ~ ., data = cor15_train, family = 'binomial')

cor15_result <- as.vector(ifelse(predict(model_by_cor15, cor15_test[, -1], type = "response")>0.5, 1, 0))
cor15_table <- as.matrix(table(as.matrix(cor15_test[, 1]), cor15_result))
confusionMatrix(cor15_table)

## Furthermore, use cv.glmnet to obtain precise probability of default
model_cv_cor15 <- cv.glmnet(as.matrix(cor15_train[, -1]),
                            as.matrix(cor15_train[, 1]),
                            family = 'binomial')
cv_cor15_prob <- predict(model_cv_cor15, s = "lambda.min", as.matrix(cor15_test[, -1]), type = "response")
# if setting over 50% as bankrupt standard and its result
cv_cor15_result <- as.vector(ifelse(cv_cor15_prob>0.5, 1, 0))
cv_cor15_table <- as.matrix(table(as.matrix(cor15_test[, 1]), cv_cor15_result))
confusionMatrix(cv_cor15_table)

#---------------------------#
#  Correlation - 25
#---------------------------#
## Choose the top 25 absolute correlation to predict
cor.matrix<-as.matrix(cor(data))
correlation<-order(abs(cor.matrix[2:96]), decreasing = T)
## Take the data with the parameters we need
cor25_vari <- colnames(data)[correlation[1:25]+1]
cor25_train <- train[, c("bankrupt", cor25_vari), with=F]
cor25_test <- test[, c("bankrupt", cor25_vari), with=F]

model_by_cor25 <- glm(bankrupt ~ ., data = cor25_train, family = 'binomial')

cor25_result <- as.vector(ifelse(predict(model_by_cor25, cor25_test[, -1], type = "response")>0.5, 1, 0))
cor25_table <- as.matrix(table(as.matrix(cor25_test[, 1]), cor25_result))
confusionMatrix(cor25_table)

## Futher more, use cv.glmnet to obtain precise probability of default
model_cv_cor25 <- cv.glmnet(as.matrix(cor25_train[, -1]),
                           as.matrix(cor25_train[, 1]),
                           family = 'binomial')
cv_cor25_prob <- predict(model_cv_cor25, s = "lambda.min", as.matrix(cor25_test[, -1]), type = "response")
# if setting over 50% as bankrupt standard and its result
cv_cor25_result <- as.vector(ifelse(cv_cor25_prob>0.5, 1, 0))
cv_cor25_table <- as.matrix(table(as.matrix(cor25_test[, 1]), cv_cor25_result))
confusionMatrix(cv_cor25_table)

#---------------------------#
#  Ordinary least square
#---------------------------#
## Use OLS to find redundant parameters
ols_para_select <- lm(bankrupt ~ ., data = data)
ols_para <- which(abs(coef(ols_para_select)) >= 0.0001)[-1]
select.vari <- colnames(data)[ols_para]
## Take the data without the redundant parameters
ols_train <- train[, c("bankrupt", select.vari), with=F]
ols_test <- test[, c("bankrupt", select.vari), with=F]

model_by_ols <- glm(bankrupt ~ ., data = ols_train, family = 'binomial')

ols_result <- as.vector(ifelse(predict(model_by_ols, ols_test[, -1], type = "response")>0.5, 1, 0))
ols_table <- as.matrix(table(as.matrix(ols_test[, 1]), ols_result))
confusionMatrix(ols_table)

## Furthermore, use cv.glmnet to obtain precise probability of default
model_cv_ols <- cv.glmnet(as.matrix(ols_train[, -1]),
                          as.matrix(ols_train[, 1]),
                          family = 'binomial')
cv_ols_prob <- predict(model_cv_ols, s = "lambda.min", as.matrix(ols_test[, -1]), type = "response")
# if setting over 50% as bankrupt standard and its result
cv_ols_result <- as.vector(ifelse(cv_ols_prob>0.5, 1, 0))
cv_ols_table <- as.matrix(table(as.matrix(ols_test[, 1]), cv_ols_result))
confusionMatrix(cv_ols_table)

#---------------------------#
#  Forward Stepwise
#---------------------------#
## Set null(lower) model and full(upper) model
null = lm(bankrupt ~ 1, data = train)  
full = lm(bankrupt ~ ., data = train)

## Begin from null model, add one by one
## The biggest won't exceed the full model
forward.lm = step(null,
                  scope=list(lower=null, upper=full), 
                  direction="forward")

model_by_forward <- glm(formula = formula(forward.lm), 
                        data = train,
                        family = 'binomial')

forward_result <- as.vector(ifelse(predict(model_by_forward, test[, -1], type = "response")>0.5, 1, 0))
forward_table <- as.matrix(table(as.matrix(test[, 1]), forward_result))
confusionMatrix(forward_table)

## Futher more, use cv.glmnet to obtain precise probability of default
for_train <- as.matrix(train[, c("bankrupt", names(forward.lm$coefficient)[-1]), with=F])
for_test <- as.matrix(test[, c("bankrupt", names(forward.lm$coefficient)[-1]), with=F])

model_cv_for <- cv.glmnet(for_train[, -1],
                          for_train[, 1],
                          family = 'binomial')
cv_for_prob <- predict(model_cv_for, s = "lambda.min", for_test[, -1], type = "response")
# if setting over 50% as bankrupt standard and its result
cv_for_result <- as.vector(ifelse(cv_for_prob>0.5, 1, 0))
cv_for_table <- as.matrix(table(for_test[, 1], cv_for_result))
confusionMatrix(cv_for_table)

#---------------------------#
#  Backward Stepwise
#---------------------------#
## Set full(upper) model
full = lm(bankrupt ~ ., data = train)

## Begin from full model, remove one by one
backward.lm = step(full, 
                   scope = list(upper=full), 
                   direction="backward")  

model_by_backward <- glm(formula = formula(backward.lm), 
                        data = train,
                        family = 'binomial')
backward_result <- predict(model_by_backward, test[, -1], type = "response")
backward_result <- as.vector(ifelse(predict(model_by_backward, test[, -1], type = "response")>0.5, 1, 0))
backward_table <- as.matrix(table(as.matrix(test[, 1]), backward_result))
confusionMatrix(backward_table)

## Futher more, use cv.glmnet to obtain precise probability of default
back_train <- as.matrix(train[, c("bankrupt", names(backward.lm$coefficient)[-1]), with=F])
back_test <- as.matrix(test[, c("bankrupt", names(backward.lm$coefficient)[-1]), with=F])

model_cv_back <- cv.glmnet(back_train[, -1],
                           back_train[, 1],
                           family = 'binomial')
cv_back_prob <- predict(model_cv_back, s = "lambda.min", back_test[, -1], type = "response")
# if setting over 50% as bankrupt standard and its result
cv_back_result <- as.vector(ifelse(cv_back_prob>0.5, 1, 0))
cv_back_table <- as.matrix(table(back_test[, 1], cv_back_result))
confusionMatrix(cv_back_table)

#---------------------------#
#  Both Stepwise
#---------------------------#
## Set null(lower) model and full(upper) model
null = lm(bankrupt ~ 1, data = train)  
full = lm(bankrupt ~ ., data = train)

## Begin from null model, add or remove one by one
## The biggest won't exceed the full model
both.lm = step(null,
               scope=list(upper=full), 
               direction="both")

model_by_both <- glm(formula = formula(both.lm), 
                         data = train,
                         family = 'binomial')
both_result <- predict(model_by_both, test[, -1], type = "response")
both_result <- as.vector(ifelse(predict(model_by_both, test[, -1], type = "response")>0.5, 1, 0))
both_table <- as.matrix(table(as.matrix(test[, 1]), both_result))
confusionMatrix(both_table)

## Futher more, use cv.glmnet to obtain precise probability of default
both_train <- as.matrix(train[, c("bankrupt", names(both.lm$coefficient)[-1]), with=F])
both_test <- as.matrix(test[, c("bankrupt", names(both.lm$coefficient)[-1]), with=F])

model_cv_both <- cv.glmnet(both_train[, -1],
                           both_train[, 1],
                           family = 'binomial')
cv_both_prob <- predict(model_cv_both, s = "lambda.min", both_test[, -1], type = "response")
# if setting over 50% as bankrupt standard and its result
cv_both_result <- as.vector(ifelse(cv_both_prob>0.5, 1, 0))
cv_both_table <- as.matrix(table(both_test[, 1], cv_both_result))
confusionMatrix(cv_both_table)


## The parameters used in each selection
para_used <- c
para_used <- data.frame(Full = length(test)-1,
                        Cor5 = length(cor5_test)-1,
                        Cor15 = length(cor15_test)-1,
                        Cor25 = length(cor25_test)-1,
                        OLS = length(ols_test)-1,
                        forward = length(names(forward.lm$coefficient)),
                        backward = length(names(backward.lm$coefficient)),
                        both = length(names(both.lm$coefficient)))
view(para_used)

## The result from each selection is print on this table
result_table <- mutate(test[, 1], full_result) %>% mutate(cor5_result) %>%
  mutate(cor15_result) %>% mutate(cor25_result) %>% mutate(ols_result) %>% 
  mutate(forward_result) %>% mutate(backward_result) %>% mutate(both_result)

## Make a table with model accuracy for each selection and bankruptcy standard
prob_result_table <- mutate(test[, 1], cv_full_prob) %>% mutate(cv_cor5_prob) %>%
  mutate(cv_cor15_prob) %>% mutate(cv_cor25_prob) %>% mutate(cv_ols_prob) %>% 
  mutate(cv_for_prob) %>% mutate(cv_back_prob) %>% mutate(cv_both_prob)

## In this function, the input is bankruptcy standard(percentage)
## And the output is the accuracy corresponding to each selection
prob_calculate <- function(p){
  N <- length(filter(prob_result_table, bankrupt == 1)$bankrupt)
  numb = numeric(8)
  numb[1] <- length(filter(prob_result_table, cv_full_prob > p & bankrupt == 1)$bankrupt)
  numb[2] <- length(filter(prob_result_table, cv_cor5_prob > p & bankrupt == 1)$bankrupt)
  numb[3] <- length(filter(prob_result_table, cv_cor15_prob > p & bankrupt == 1)$bankrupt)
  numb[4] <- length(filter(prob_result_table, cv_cor25_prob > p & bankrupt == 1)$bankrupt)
  numb[5] <- length(filter(prob_result_table, cv_ols_prob > p & bankrupt == 1)$bankrupt)
  numb[6] <- length(filter(prob_result_table, cv_for_prob > p & bankrupt == 1)$bankrupt)
  numb[7] <- length(filter(prob_result_table, cv_back_prob > p & bankrupt == 1)$bankrupt)
  numb[8] <- length(filter(prob_result_table, cv_both_prob > p & bankrupt == 1)$bankrupt)
  return(round(numb / N * 100, digits = 2))
}
prob_acc <- data.frame(prob_calculate(0.5), prob_calculate(0.25), prob_calculate(0.10))
names(prob_acc) <- c(">50%", ">25%", ">10%")
row.names(prob_acc) <- c("Full", "Cor5", "Cor15", "Cor25",
                         "OLS", "Forward", "Backward", "Both")
view(t(prob_acc))

library(AICcmodavg)
models <- list(model_by_full, model_by_cor5, model_by_cor15, model_by_cor25,
               model_by_ols, model_by_forward, model_by_backward, model_by_both)

mod.names <- c('Full', 'Cor5', 'Cor15', 'Cor25', 'OLS', 'Forward', 'Backward', 'Both')

aictab(cand.set = models, modnames = mod.names)
