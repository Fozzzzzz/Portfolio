import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn import linear_model

LoanData = pd.read_csv("Task 3 and 4_Loan_Data.csv")

x = LoanData.drop(["customer_id","default"], axis=1)
y = LoanData["default"]

logistic_reg = linear_model.LogisticRegression()
logistic_reg.fit(x, y)

def ExpectedLoss(credit_lines_outstanding, loan_amt_outstanding, total_debt_outstanding, 
                 income, years_employed, fico_score):
    coef = logistic_reg.coef_[0]
    PD = logistic_reg.intercept_[0]
    PD += credit_lines_outstanding*coef[0] + loan_amt_outstanding*coef[1]+ total_debt_outstanding*coef[2] + income*coef[3] + years_employed*coef[4] + fico_score*coef[5]
    PD = 1/(1+np.exp(-PD))
    return loan_amt_outstanding * PD * 0.9

credit_lines_outstanding = 5
loan_amt_outstanding = 1958.928726
total_debt_outstanding = 8228.752520
income = 26648.43525
years_employed = 2
fico_score = 572

EL = ExpectedLoss(credit_lines_outstanding, loan_amt_outstanding, total_debt_outstanding,income, years_employed, fico_score)

print(f'The expected loss would be {round(EL, 7)}')