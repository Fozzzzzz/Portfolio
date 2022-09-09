library(data.table)
library(tidyverse)  
library(plm)
#Data preparation
load("~/Documents/R/data_ml.RData")
data <- data_ml
data <- data.table(data) %>%
  filter(stock_id == "567" | stock_id == "737" |
           stock_id == "829" | stock_id == "391" | stock_id == "1013")

#Portfolio construction
#a. 1/N portfolio
#set weight for each stock
w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#create the sub-sample with date from 2018-01-31 to 2019-01-31
subsample <- data[date %between% c("2018-02-01","2019-01-31")]

#create the df of monthly individual stock returns
Rt <- dcast(subsample, date~stock_id, value.var="R1M_Usd")
#compute the time series of portfolio returns
Rt <- data.matrix(Rt, rownames.force = NA)[, -1]
Rpt <- Rt %*% w

N = length(Rpt)
#Portfolio concentration measure
port_con <- sum(w * w)             #which is 0.2
#Annualized mean returns
mean_ret <- N * mean(Rpt)         #which is 0.1092
#Annualized volatility
vol <- sqrt(N) * sd(Rpt)          #which is 0.18752
#mean-volatility ratio
mean_vol_ratio <- mean_ret / vol   #which is 0.58234

#b. Inverse-volatility-weighted portfolio
#create the sub-sample with date from 1998-11-30 to 2017-12-31
subsample <- data[date %between% c("1998-12-01","2017-12-31")]
#create the df of monthly individual stock returns
Rt <- dcast(subsample, date~stock_id, value.var="R1M_Usd")

#calculate each inverse volatility
inv_vol <- c(sd(Rt$`391`)^-1, sd(Rt$`567`)^-1, sd(Rt$`737`)^-1, sd(Rt$`829`)^-1, sd(Rt$`1013`)^-1)
#set weight for each stock
w <- c()
for(i in c(1:5)){ 
  w <- c(w, inv_vol[i]/sum(inv_vol))
}
#compute the time series of portfolio returns
Rt <- data.matrix(Rt, rownames.force = NA)[, -1]
Rpt <- Rt %*% w

N = length(Rpt)
#Portfolio concentration measure
port_con <- sum(w * w)             #which is 0.23098
#Annualized mean returns
mean_ret <- N * mean(Rpt)         #which is 2.37214
#Annualized volatility
vol <- sqrt(N) * sd(Rpt)          #which is 0.8795
#mean-volatility ratio
mean_vol_ratio <- mean_ret / vol   #which is 2.69715

#c. Mean-variance portfolio
#create the sub-sample with date from 1998-11-30 to 2017-12-31
subsample <- data[date %between% c("1998-12-01","2017-12-31")]
#create the df of monthly individual stock returns
Rt <- dcast(subsample, date~stock_id, value.var="R1M_Usd")

#Estimate the vector of mean returns and return variance matrix
mean_vec = colMeans(Rt[,-1])
var_mat = cov(Rt[,-1])

port_con <- c()
mean_ret <- c()
vol <- c()
Rt <- data.matrix(Rt, rownames.force = NA)[, -1]
#For each 𝜇p, find its weight
for (i in 8:15) {
  w = Variable(5) 
  portf_ret = t(mean_vec) %*% w
  portf_var = quad_form(w, var_mat)
  obj = 0.5*portf_var
  constr = list(sum(w) == 1,
                w >= 0,
                portf_ret >= i*(10^-3))
  prob = Problem(Minimize(obj), constr) # Define the problem
  result = solve(prob) 
  
  w = result$getValue(w)
  Rpt <- Rt %*% w
  
  N = length(Rpt)
  #calculate the parameters
  #Portfolio concentration measure
  port_con <- c(port_con, sum(w * w))
  #Annualized mean returns
  mean_ret <- c(mean_ret, N * mean(Rpt))
  #Annualized volatility
  vol <- c(vol, sqrt(N) * sd(Rpt))
}

#mean-volatility ratio
mean_vol_ratio <- c()
for (i in c(1:8)) {
  mean_vol_ratio <- c(mean_vol_ratio, mean_ret[i] / vol[i])
}

for (i in 1:8) {
  sprintf('For 𝜇p = %.3f 
          The portfolio concentration measure is %.5f
          The annualized mean returns is %5f
          The annualized volatility is %5f
          The mean-volatility ratio is %5f
          ',
          (i+7)*10^-3, port_con[i], mean_ret[i], vol[i], mean_vol_ratio[i]) %>%
    cat()
}


#d. Mean-variance portfolio + boosted tree mean forecast
#create the sub-sample with date from 1998-11-30 to 2017-12-31 and the factors we need
subsample <- data[date %between% c("1998-12-01","2017-12-31")] %>%
  select("date", "stock_id", "R1M_Usd", "R12M_Usd", "Vol1Y_Usd", "Mom_11M_Usd", "Mkt_Cap_12M_Usd")
#set train data and forecast data
train_data <- subsample[date %between% c("1998-12-01","2016-12-31")]
fore_data <- subsample[date %between% c("2017-01-01","2017-12-31")]
x_train <- model.matrix(R12M_Usd ~ ., train_data)[, -1:-4]
y_train <- train_data$R12M_Usd
x_fore <- model.matrix(R12M_Usd ~ ., fore_data)[, -1:-4]

#build the boosted tree model
library(xgboost)

trainData <- xgb.DMatrix(data = x_train, label = y_train)
xgb_mdl <- xgb.train(list(max_depth = 4, 
                          eta = 0.01, 
                          colsample_bytree = 1, 
                          objective = "reg:squarederror"),
                          trainData, nrounds = 100)
#construct the forecasts
mean_vec_f <- predict(xgb_mdl, x_fore)/12
fore_data <- mutate(fore_data, mean_vec_f)

#create the df of monthly individual stock returns
Rt_f <- dcast(fore_data, date~stock_id, value.var="mean_vec_f")
Rt <- dcast(fore_data, date~stock_id, value.var="R1M_Usd")
#Estimate the vector of mean returns and return variance matrix
mean_vec_f = colMeans(Rt_f[,-1])
var_mat = cov(Rt[,-1])
#set the optimal portfolio
mean_vec_p <- c(0.01, 0.014, 0.018, 0.022)

port_con <- c()
mean_ret <- c()
vol <- c()
Rt <- data.matrix(Rt, rownames.force = NA)[, -1]
for (i in mean_vec_p) {
  w = Variable(5) 
  portf_ret = t(mean_vec_f) %*% w
  portf_var = quad_form(w, var_mat)
  obj = 0.5*portf_var
  constr = list(sum(w) == 1,
                w >= 0,
                portf_ret >= i)
  prob = Problem(Minimize(obj), constr) # Define the problem
  result = solve(prob) 
  
  w = result$getValue(w)
  Rpt <- Rt %*% w
  
  N = length(Rpt)
  #calculate the parameters
  #Portfolio concentration measure
  port_con <- c(port_con, sum(w * w))
  #Annualized mean returns
  mean_ret <- c(mean_ret, N * mean(Rpt))
  #Annualized volatility
  vol <- c(vol, sqrt(N) * sd(Rpt))
}

#mean-volatility ratio
mean_vol_ratio <- c()
for (i in c(1:4)) {
  mean_vol_ratio <- c(mean_vol_ratio, mean_ret[i] / vol[i])
}

for (i in 1:4) {
  sprintf('For 𝜇p = %.3f
          The portfolio concentration measure is %.5f
          The annualized mean returns is %5f
          The annualized volatility is %5f
          The mean-volatility ratio is %5f
          ',
          mean_vec_p[i], port_con[i], mean_ret[i], vol[i], mean_vol_ratio[i]) %>%
    cat()
}
