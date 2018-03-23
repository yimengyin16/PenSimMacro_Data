# -*- coding: utf-8 -*-
"""
Created on Sat Mar 17 21:16:05 2018

@author: yimen
"""


#%%
import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sn
import feather


import os
os.getcwd()
os.chdir("C:/Git/PenSimMacro/PenSimMacro_Data")



#%%

'''
Load data using feather
'''

df_stock_q = feather.read_dataframe('data_out/df_stock_q.feather')
df_stock_q.index = pd.date_range('1953-01-01', '2015-10-01', freq = 'QS')
df_stock_q

dta_stock = df_stock_q['return_tot_o'].dropna()
dta_gdp   = df_stock_q['dl_gdp_o'].dropna()


dta_gdp

'''
Load recession data
'''
from pandas_datareader.data import DataReader
from datetime import datetime
usrec = DataReader('USREC', 'fred', start=datetime(1947, 1, 1), end=datetime(2013, 4, 1))


'''
Load Hamilton1989 data
'''

from statsmodels.tsa.regime_switching.tests.test_markov_autoregression import rgnp
dta_hamilton = pd.Series(rgnp, index = pd.date_range('1951-04-01', '1984-10-1', freq = 'QS'))
dta_hamilton

dta_hamilton

# Correlation between GDP and GNP
np.corrcoef(dta_gdp['1953-04-01':'1984-10-01'], dta_hamilton['1953-04-01':'1984-10-01'])


#%%
'''
The Hamilton 1989 model estimates are very sensitive to sample period used
'''

# plot the data:
dta_hamilton.plot(title = 'Growth rate of Real GNP', figsize = (12, 3))


# fit the model using 1951Q2 - 1984Q4:
np.random.seed(12345)
mod_hamilton = sm.tsa.MarkovAutoregression(dta_hamilton, k_regimes=2, order = 4, switching_ar = False)
res_hamilton = mod_hamilton.fit(search_reps = 20)
print(res_hamilton.summary())
print(res_hamilton.expected_durations)


# fit the model using 1951Q3 - 1984Q4:
np.random.seed(12345)
mod_hamilton1 = sm.tsa.MarkovAutoregression(dta_hamilton['1951-10-01':], k_regimes=2, order = 4, switching_ar = False)
res_hamilton1 = mod_hamilton1.fit(search_reps = 20)
print(res_hamilton1.summary())
print(res_hamilton1.expected_durations)

dta_hamilton


fig, axes = plt.subplots(2, figsize = (7, 7))
ax = axes[0]
ax.plot(res_hamilton.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_hamilton.index[4], dta_hamilton.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')
ax.set_xticks(pd.date_range('1950-01-01', '1984-10-1', freq = 'QS')[0::20])

fig.tight_layout()


ax = axes[1]
ax.plot(res_hamilton1.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_hamilton.index[4], dta_hamilton.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')
ax.set_xticks(pd.date_range('1950-01-01', '1984-10-1', freq = 'QS')[0::20])

fig.tight_layout()

# Comments:
  # Removing the first two observations greatly affects the smoothed probability


#%%
'''
Fit the MSAR4 model using FRED GDP growth from 1953Q2-1984Q4 
'''
dta_gdp

np.random.seed(12345)
mod_gdp = sm.tsa.MarkovAutoregression(dta_gdp['1953-04-01':'1983-10-01'], k_regimes=2, order = 4, switching_ar = False, switching_variance =  False)
res_gdp = mod_gdp.fit(search_reps = 30, search_iter = 20, maxiter=1000)
print(res_gdp.summary())


fig, axes = plt.subplots(2, figsize = (7, 7))
ax = axes[0]
ax.plot(res_gdp.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession')
ax.set_xticks(pd.date_range('1950-01-01', '1984-10-1', freq = 'QS')[0::20])

ax = axes[1]
ax.plot(res_gdp.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')
ax.set_xticks(pd.date_range('1950-01-01', '1984-10-1', freq = 'QS')[0::20])

fig.tight_layout()

print(res_gdp.expected_durations)

# Using the full sample period: 1953Q2-1984Q4 
  # Gives slightly different constant and AR parameter esitmates, and very different recenssion-to-expansion probability
  # The much higher R-to-E prob a much shorter expected duration of recession compared to the Hamilton1989 paper.  

# Estimations are very sensitive to the starting/ending observations of the sample period
  # Excluding 1984 data makes the estimation very close to the Hamilton1989
  # But non of the AR parameters are significant



#%%
'''
Fit the MSAR4 model using FRED GDP growth from 1953Q2-2015Q4 
'''
dta_gdp

np.random.seed(12349)
mod_gdp = sm.tsa.MarkovAutoregression(dta_gdp['1955-04-01':'2012-10-01'], k_regimes=2, order = 4, switching_ar = False, switching_variance =  False)
res_gdp = mod_gdp.fit(search_reps = 30, search_iter = 20, maxiter=1000)
print(res_gdp.summary())
print(res_gdp.expected_durations)


fig, axes = plt.subplots(2, figsize = (9, 8))
ax = axes[0]
ax.plot(res_gdp.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession')


ax = axes[1]
ax.plot(res_gdp.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')

fig.tight_layout()



# Estimation becomes very unstable
  # Including the Period of Great Moderation seems greatly reduce the R-to-E probability 
  # Or this may also be come local max
  # Tried a lot of seed values, but no other reasonable estimates
  # Higher order AR term insignificant

  
  


#%%
  
'''
Fit the MSAR1 model using FRED GDP growth from 1953Q2-2015Q4

Try quarterly AR1 model instead of the AR4 model in Hamilton1989
  - ARIMA model prefers AR1/2
  - higher AR terms insignificant in MSAR1 above
  - Fewer parameter may make estimation easier
'''
  

np.random.seed(1235)
mod_gdp = sm.tsa.MarkovAutoregression(dta_gdp['1953-04-01':'2015-10-01'], k_regimes=2, order = 1, switching_ar = False, switching_variance =  False)
res_gdp = mod_gdp.fit(search_reps = 30, search_iter = 20, maxiter=1000)
print(res_gdp.summary())
print(res_gdp.expected_durations)


np.random.seed(1236)
mod_gdp = sm.tsa.MarkovAutoregression(dta_gdp['1953-04-01':'2015-10-01'], k_regimes=2, order = 2, switching_ar = False, switching_variance =  False)
res_gdp = mod_gdp.fit(search_reps = 40, search_iter = 30, maxiter=1000)
print(res_gdp.summary())
print(res_gdp.expected_durations)

fig, axes = plt.subplots(2, figsize = (7, 7))
ax = axes[0]
ax.plot(res_gdp.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession')

ax = axes[1]
ax.plot(res_gdp.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')

fig.tight_layout()



# AIC/BIC: MSAR1 > MSAR2 > MSAR4
# MSAR1 may be preferred model for 1953 - 2015
# Much longer expected length of expansion and shorter length of recession compared to Hamilton1989
# Need check against offcial data



#%%

'''
Fit the MS random walk with drift model using FRED GDP growth from 1953Q2-2015Q4

What if we remove all AR terms and simply model quarterly GDP as random walk with drift

The model will become a MSDR(dynamic regression) model

Use MarkovRegression function instead of MarkovAutoregression
'''

'''
fit the model
 There are two seed values that can produce reasonable estimates 
   -seed(123): Closer to Hamilton 1989, but lower likelihood, nan values in std err
   -seed(126): lower prob to ender recession (fail to identify 1970 and 2001 recessions), but higher likelihood
   -MSDR model has slighly worse AIC and BIC 
 seed(126): seems to be the global optimum. almost always got this if we strengthen initial value search
'''


np.random.seed(123)
mod_gdp2 = sm.tsa.MarkovRegression(dta_gdp['1953-04-01':'2015-10-01'], k_regimes = 2)
res_gdp2 = mod_gdp2.fit(search_reps = 30)
print(res_gdp2.summary())
print(res_gdp2.expected_durations)


np.random.seed(127)
mod_gdp3 = sm.tsa.MarkovRegression(dta_gdp['1953-04-01':'2015-10-01'], k_regimes = 2)
res_gdp3 = mod_gdp3.fit(search_reps = 40, search_iter = 30, maxiter=1000)
print(res_gdp3.summary())
print(res_gdp3.expected_durations)


fig, axes = plt.subplots(4, figsize = (11, 9))
ax = axes[0]
ax.plot(res_gdp2.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession, seed123')
ax.set_xticks(pd.date_range('1950-01-01', '2015-10-1', freq = 'QS')[0::20])

  
ax = axes[1]
ax.plot(res_gdp2.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession, seed123')
ax.set_xticks(pd.date_range('1950-01-01', '2015-10-1', freq = 'QS')[0::20])


ax = axes[2]
ax.plot(res_gdp3.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession, seed126')
ax.set_xticks(pd.date_range('1950-01-01', '2015-10-1', freq = 'QS')[0::20])

  
ax = axes[3]
ax.plot(res_gdp3.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_gdp.index[4], dta_gdp.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession, seed126')
ax.set_xticks(pd.date_range('1950-01-01', '2015-10-1', freq = 'QS')[0::20])

fig.tight_layout()

 
#%%

mod_stock = sm.tsa.MarkovRegression(dta_stock['1953-04-01':'2015-10-01'], k_regimes = 2, switching_variance =  True)
res_stock = mod_stock.fit(search_reps = 40, search_iter = 30, maxiter=1000)
print(res_stock.summary())
print(res_stock.expected_durations)


#%%


'''
Models we may want to try in simulation
1) MSAR1: highest likelihood, but does not identify historical recessions well
2) MS random walk with drift, seed 123: visually more consistent with historical recession periods, closer to Hamilton1989
3) MS random walk with drift, seed 127: better likelihood than 2), produces expected length of recessions/expansions that best matches 
   historical data

Next steps:
    1. check if there is another local max estimate of MSAR1 that produces transition matrix similar to Hamilton1989 (Seems there is not.)
    2. Calculate length of NBER recessions and expansions and compare expected lengths from the models against them. (Model3 matches the best)

It seems that model 3 describe the data the best. 


US recessions NBER:
                  recession       expansion
Starting 1953:      3.7Q            20.2Q
Starting 1980       3.7Q            23.7Q
    

Model recessions
                  recession       expansion
MSAR4               1.6             19.9 
MSAR2               1.7             20.9      
MSAR1               1.8             20.4
RW-drift            4.4             11.4
RW-drift            3.1             20.7 
Hamilton1989        4.1             16.2


# Discussion on the modeling GDP growth as random walk with drift or mean reversion process (AR)
http://econlog.econlib.org/archives/2010/08/trend_vs_random.html
   
Need to further explore:
    1. How the modeling approach of GDP - MSAR vs. random walk w/ drift - would affect simulation results
        - What we know: MSAR implies mean reversion within regimes and smoother transition across regimes. 
    2. Do models using quarterly data simulate annual data that are consistent with historical annual data? 
    
'''



#%%

'''
Export data in feather format
'''

# Recession periods: 0/1 by month from 1953 to 2013
usrec_index = pd.DataFrame(usrec.index.values)
usrec_index

feather.write_dataframe(usrec, "data_out/usrec.feather")
feather.write_dataframe(usrec_index, "data_out/usrec_index.feather")

# Recession probabilities from models 
# random walk 

regimes_gdp2 = pd.concat((res_gdp2.filtered_marginal_probabilities[0].to_frame('p2_filtered'),
                          res_gdp2.smoothed_marginal_probabilities[0].to_frame('p2_smoothed')),
                          axis = 1
                          )

regimes_gdp3 = pd.concat((res_gdp3.filtered_marginal_probabilities[0].to_frame('p3_filtered'),
                          res_gdp3.smoothed_marginal_probabilities[0].to_frame('p3_smoothed')),
                          axis = 1
                          )

regimes_gdp_index = pd.Series(regimes_gdp2.index.values, index = regimes_gdp2.index.values, name= 'dt').to_frame()
    
regimes_gdp = pd.concat((regimes_gdp_index, regimes_gdp2,  regimes_gdp3), axis = 1)

feather.write_dataframe(regimes_gdp, "data_out/regimes_gdp.feather")


