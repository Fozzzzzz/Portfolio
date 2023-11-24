import numpy as np
from datetime import date
import math

def ContractPrice(InDates, OutDates, InPrices, OutPrices, Rate, TotalVolume, StorageCost, InOutCostRate):
    Volume = 0
    Profit = 0
    
    DatePrice = {}
    for i in range(len(InDates)):
        DatePrice[InDates[i]] = InPrices[i]
        DatePrice[OutDates[i]] = OutPrices[i]
        
    AllDates = sorted(set(InDates + OutDates))
    
    for Date in AllDates:
        if Date in InDates:
            if Volume <= TotalVolume - Rate:
                Volume += Rate
                Profit -= (DatePrice[Date] + InOutCostRate) * Rate 
        elif Date in OutDates:
            if Volume >= Rate:
                Volume -= Rate
                Profit += (DatePrice[Date] - InOutCostRate) * Rate 
    StoreCost = math.ceil((AllDates[-1] - AllDates[0]).days // 30) * StorageCost

    return Profit - StoreCost

in_dates = [date(2022, 1, 1), date(2022, 2, 1), date(2022, 2, 21), date(2022, 4, 1)] #injection dates
in_prices = [20, 21, 20.5, 22]#prices on the injection days
out_dates = [date(2022, 1, 27), date(2022, 2, 15), date(2022, 3, 20), date(2022, 6, 1)] # extraction dates
out_prices = [23, 19, 21, 25] # prices on the extraction days
rate = 100000  # rate of gas in cubic feet per day
storage_cost_rate = 10000  # total volume in cubic feet
max_storage_volume = 500000 # maximum storage capacity of the storage facility
injection_withdrawal_cost_rate = 0.0005  # $/cf
result = ContractPrice(in_dates, out_dates, in_prices, out_prices, rate, max_storage_volume, storage_cost_rate, injection_withdrawal_cost_rate)
print()
print(f"The value of the contract is: ${result}")