library(margins)
library(stargazer)
library(sandwich)


Default = read.csv("Default.csv")

head(Default)

Default[,1] = ifelse(Default[,1]=="Yes", 1, 0)
Default[,2] = ifelse(Default[,2]=="Yes", 1, 0)


mdl1 = glm(default ~ balance, data=Default, family=binomial(link = "logit"))
CovMat = vcovHC(mdl1, type = "HC")
mdl1_HC = sqrt(diag(CovMat))

stargazer(mdl1, mdl1, type="text", se=list(mdl1_HC, NULL), 
          column.labels=c("Heteroskedasticity","Homoskedasticity"), align=T)

# obtain the predicted value, 
# choose type="response", the default is log-odds
predict(mdl1, newdata=list(balance=c(1000,2000)), type="response")

mdl2 = glm(default ~ balance + student + income, data=Default, 
           family=binomial(link = "logit"))

stargazer(mdl1, mdl2, type="text", align=T)


# marginal effects
margins(mdl2)  # at means

# at a specified values
margins(mdl2, at=list(balance=c(1000,2000),
                      student=c(0,1) ) )




# Contingency table
Prob_Prediction = predict(mdl2, type="response")
Model_Prediction = ifelse(Prob_Prediction > 0.5, "Default", "No default")
Actual_Outcome = ifelse(Default[,1]==1, "Default", "No default")
Comparison = data.frame(Model_Prediction,Actual_Outcome)
table(Comparison)



