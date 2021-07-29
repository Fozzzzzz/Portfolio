import yfinance as yf
import pandas as pd
import numpy as np
import datetime as dt
from yahoo_fin import stock_info as si

from sklearn.preprocessing import Normalizer
from sklearn.pipeline import make_pipeline
from sklearn.cluster import KMeans

companies_dow30 = si.tickers_dow()

clusters = 5

start = dt.datetime.now() - dt.timedelta(days=365 * 2)
end = dt.datetime.now()

data = yf.download(list(companies_dow30), start, end)

open_values = np.array(data['Open'].T)
close_values = np.array(data['Close'].T)
daily_movements = close_values - open_values

normalizar = Normalizer()
clustering_model = KMeans(n_clusters=clusters, max_iter=1000)
pipeline = make_pipeline(normalizar, clustering_model)
pipeline.fit(daily_movements)
clusters = pipeline.predict(daily_movements)

results = pd.DataFrame({
    'clusters': clusters,
    'tickers': list(companies_dow30)
}).sort_values(by=['clusters'], axis=0)

print(results)