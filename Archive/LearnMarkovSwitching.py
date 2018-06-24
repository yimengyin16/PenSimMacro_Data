# -*- coding: utf-8 -*-
"""
Created on Fri Mar 16 15:18:27 2018

@author: yimen

Learn Markov switching model through Chad Fulton's Jupyter note book:
http://www.chadfulton.com/topics/markov_autoregression.html

modules installed:
pandas_datareader    
 
# Questions:
1. how constants are handled
   

"""

#%%
#%matplotlib inline

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sn

from pandas_datareader.data import DataReader
from datetime import datetime
usrec = DataReader('USREC', 'fred', start=datetime(1947, 1, 1), end=datetime(2013, 4, 1))


#%%


'''
Hamilton(1989) switching model of GNP
'''

type(dta_hamilton)

# geth the RGNP data to replicate hamilton 
from statsmodels.tsa.regime_switching.tests.test_markov_autoregression import rgnp
dta_hamilton = pd.Series(rgnp, index = pd.date_range('1951-04-01', '1984-10-1', freq = 'QS'))
dta_hamilton

# plot the data:
dta_hamilton.plot(title = 'Growth rate of Real GNP', figsize = (12, 3))

# fit the model:
mod_hamilton = sm.tsa.MarkovAutoregression(dta_hamilton, k_regimes=2, order = 4, switching_ar = False)
res_hamilton = mod_hamilton.fit()

print(res_hamilton.summary())

# Expected duration of recession vs expansion.
print(res_hamilton.expected_durations)
# recession: one year, expansion: 2.5 years


fig, axes = plt.subplots(2, figsize = (7, 7))
ax = axes[0]
ax.plot(res_hamilton.filtered_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_hamilton.index[4], dta_hamilton.index[-1]), ylim = (0,1),
       title = 'Filtered probability of recession')

ax = axes[1]
ax.plot(res_hamilton.smoothed_marginal_probabilities[0])
ax.fill_between(usrec.index, 0, 1, where = usrec['USREC'].values, color = 'grey', alpha = 0.3)
ax.set(xlim = (dta_hamilton.index[4], dta_hamilton.index[-1]), ylim = (0,1),
       title = 'Smoothed probability of recession')

fig.tight_layout()

res_hamilton.filtered_marginal_probabilities


usrec
dta_hamilton.index[-1]


#%%

'''
Federal funds rate with switching intercept
'''

from statsmodels.tsa.regime_switching.tests.test_markov_regression import fedfunds
dta_fedfunds = pd.Series(fedfunds, index = pd.date_range('1954-07-01', '2010-10-01', freq = 'QS'))

# plot the data
dta_fedfunds.plot(title = 'Federal funds, rate', figsize = (12, 3))

# Fit the model
# a switching mean is the default of the MarkovRegression model
mod_fedfunds = sm.tsa.MarkovRegression(dta_fedfunds, k_regimes = 2)
res_fedfunds = mod_fedfunds.fit()

print(res_fedfunds.summary())

res_fedfunds.smoothed_marginal_probabilities[1].plot(
        title = 'Probability of being in the high regime', figsize = (12, 3))

print(res_fedfunds.expected_durations)



#%%%
'''
Federal funds rate with switching intercept and lagged dependent variable
'''

dta_fedfunds.iloc[1:] # remove  the 1st element
dta_fedfunds.iloc[:-1] # remove the last element

# fit the model
mod_fedfunds2 = sm.tsa.MarkovRegression(
        dta_fedfunds.iloc[1:], k_regimes = 2, exog = dta_fedfunds.iloc[:-1])
res_fedfunds2 = mod_fedfunds2.fit()

print(res_fedfunds2.summary())

res_fedfunds2.smoothed_marginal_probabilities[0].plot(
        title = 'Probability of being in the high regime', figsize = (12, 3))

print(res_fedfunds2.expected_durations)

#%%
'''
Switching Variances
'''

from statsmodels.tsa.regime_switching.tests.test_markov_regression import areturns
dta_areturns = pd.Series(areturns, index = pd.date_range('2004-05-04', '2014-05-03', freq = "W"))
dta_areturns

# plot the data
dta_areturns.plot(title = 'Absolute Returns, S&P500', figsize = (12, 5))

# fit the model
mod_areturns = sm.tsa.MarkovRegression(
        dta_areturns.iloc[1:], k_regimes = 2, exog = dta_areturns.iloc[:-1], switching_variance = True)
res_areturn = mod_areturns.fit()
print(res_areturn.summary())

res_areturn.smoothed_marginal_probabilities[0].plot(
        title = 'Probability of being in a low_variance regime', figsize = (12,3))


#%%
'''
Taylor rule with 2 or 3 regimes
'''

# Get the additional data
from statsmodels.tsa.regime_switching.tests.test_markov_regression import ogap, inf
dta_ogap = pd.Series(ogap, index =  pd.date_range('1954-07-01', '2010-10-01', freq = 'QS'))
dta_inf  = pd.Series(inf,  index =  pd.date_range('1954-07-01', '2010-10-01', freq = 'QS'))

exog = pd.concat((dta_fedfunds.shift(), dta_ogap, dta_inf), axis = 1).iloc[4:]
exog

dta_fedfunds.shift()
dta_fedfunds

# fit the model
mod_fedfunds3 = sm.tsa.MarkovRegression(
        dta_fedfunds.iloc[4:], k_regimes = 2, exog = exog)
res_fedfunds3 = mod_fedfunds3.fit()

# fit the 3-regime model

np.random.seed(12345)
mod_fedfunds4 = sm.tsa.MarkovRegression(
        dta_fedfunds.iloc[4:], k_regimes = 3, exog = exog)
res_fedfunds4 = mod_fedfunds4.fit(search_reps = 20)
print(res_fedfunds4.summary())
# results can be sensitive to init value

print(res_fedfunds3.summary())
print(res_fedfunds4.summary())


fig, axes = plt.subplots(3, figsize=(10,7))

ax = axes[0]
ax.plot(res_fedfunds4.smoothed_marginal_probabilities[0])
ax.set(title='Smoothed probability of a low-interest rate regime')

ax = axes[1]
ax.plot(res_fedfunds4.smoothed_marginal_probabilities[1])
ax.set(title='Smoothed probability of a medium-interest rate regime')

ax = axes[2]
ax.plot(res_fedfunds4.smoothed_marginal_probabilities[2])
ax.set(title='Smoothed probability of a high-interest rate regime')

fig.tight_layout()









