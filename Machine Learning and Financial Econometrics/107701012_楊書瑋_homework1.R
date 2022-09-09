#1
library(data.table)
library(tidyverse)  
library(plm)
load("~/Documents/R/data_ml.RData")
data <- data_ml
data <- data.table(data)
subdata <- data[date %between% c("2006-12-31","2019-01-01")]

#2
#get estimate sample from sub-sample
train_data <- subdata[date < "2012-01-01"]

#estimate the Fama-MacBeth regression
fpmg <- pmg(R12M_Usd ~ Mkt_Cap_12M_Usd + Pb + Mom_11M_Usd +
              Vol1Y_Usd + Roa + Op_Prt_Margin
            , train_data, index=c("date","stock_id"))
summary(fpmg)

#collect the coefficients of FM
coe <- fpmg$coefficients

#3
#get the test data and return the forecast results
test_data <- data[date %between% c("2012-01-01", "2019-12-31")] %>% 
  mutate(RetForecast_FM = coe[1] + Mkt_Cap_12M_Usd*coe[2] + Pb*coe[3] 
         + Mom_11M_Usd*coe[4] + Vol1Y_Usd*coe[5] + Roa*coe[6] + Op_Prt_Margin*coe[7])

#4
#Portfolio M
Port_M <- test_data[, .(Portfolio_M = weighted.mean(R1M_Usd, w = Mkt_Cap_3M_Usd)), by=date]

#Portfolio B
Port_B <- test_data[, .(Portfolio_B = weighted.mean(R1M_Usd, w = (1 + RetForecast_FM) * Mkt_Cap_3M_Usd)),
                    by=date]

Portfolio <- merge(Port_M, Port_B)
head(Portfolio)

#5
#a
mean(Portfolio[, ifelse(Portfolio_B > Portfolio_M, 1, 0)])
#We can get that there is about 54% monthly return which portfolio B's is greater than portfolio M's.

#b
#mean of portfolio M's
mean(Portfolio$Portfolio_M)
#mean of portfolio B's
mean(Portfolio$Portfolio_B)
Portfolio[,.(ExcessReturn = mean(Portfolio_B - Portfolio_M),
             Vol = sd(Portfolio_B),
             MarkVol = sd(Portfolio_M))]
#c
reg <- lm(Portfolio_B~Portfolio_M, data = Portfolio)
summary(reg)

#the alpha of this linear regression
reg$coefficients[1]
