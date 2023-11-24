import numpy as np
import pandas as pd
from math import log

df = pd.read_csv('Task 3 and 4_Loan_Data.csv')
df = df.sort_values(['fico_score'],ascending=True)

x = df['default'].to_list()
y = df['fico_score'].to_list()
n = len(x)

y1 = [i for i in y if i <= 600]
y2 = [i for i in y if i > 600]
n1 = len(y1)
n2 = len(y2)
x1 = x[:n1]
x2 = x[n1:]

def log_likelihood(n, k):
    p = k/n
    if (p==0 or p==1):
        return 0
    return k*np.log(p) + (n-k)*np.log(1-p)

def Bucketing(x, y, bucket):
    num = max(y) - min(y)
    default = [0 for i in range(num + 1)]
    total = [0 for i in range(num + 1)]

    for i in range(n1):
        default[y[i]-min(y)] += x[i]
        total[y[i]-min(y)] += 1

    for i in range(len(default)):
        default[i] += default[i-1]
        total[i] += total[i-1]

    dp = [[[-10**18, 0] for i in range(num + 1)] for j in range(bucket+1)]

    for i in range(bucket+1):
        for j in range(num + 1):
            if (i==0):
                dp[i][j][0] = 0
            else:
                for k in range(j):
                    if (total[j]==total[k]):
                        continue
                    if (i==1):
                        dp[i][j][0] = log_likelihood(total[j], default[j])
                    else:
                        if (dp[i][j][0] < (dp[i-1][k][0] + log_likelihood(total[j]-total[k], default[j] - default[k]))):
                            dp[i][j][0] = log_likelihood(total[j]-total[k], default[j]-default[k]) + dp[i-1][k][0]
                            dp[i][j][1] = k

    k = num
    l = []
    while bucket >= 0:
        l.append(k+min(y))
        k = dp[bucket][k][1]
        bucket -= 1

    return l

l1 = Bucketing(x1, y1, 4)
l2 = Bucketing(x2, y2, 4)

l = l2 + l1 + [300]

print(l)