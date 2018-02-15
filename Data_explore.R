# This script explore economic and financial data and does exploratory modeling 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

 



#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(tidyverse)
library(readxl)
library(magrittr)
library(stringr)



# packages for time series analysis
library(forecast) # Arima
library(TSA)      # arimax
library(tseries)  #

library(zoo)
library(xts)

library(timetk)
library(tidyquant)

library(lubridate)
# check tidyquant, timetk, sweep, tibbletime

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"



#**********************************************************************
#                     Loading Data                                 ####
#**********************************************************************

# Loading saved data 
load(paste0(dir_data_out, "dataAll.RData"))


#**********************************************************************
#                 Selecting data for Wilkie-like models            ####
#**********************************************************************

# Data needed:
# 1. Inflation:
#   - SBBI inflation index
#   - FRED CPI-U, seasonally adjusted
#   - FRED CPI_U, not seasonally adjusted
# 2. Equity total return index (large cap)
#   - SBBI total return index
#   - SP500TR index 
# 3. Equity price index
#   - SBBI capital appreciation index
#   - SP500 index
# 4. Long-term bond yield index
#   - SBBI long-term corb bond yield
#   - SBBI long-term gov bond yield
#   - Moody's long-term bond yield
# 5. Long-term bond total return
#   - SBBI long-term corb bond total return index
#   - SBBI long-term gov bond  total return index
# 6. GDP
#   - FRED quarterly GDP 
#   - Stock-Watson monthly GDP
#   - MA monthly GDP

vars_wilkie <- c("Inflation_Index",
					       "CPIU_SA_FRED",
					       "CPIU_NA_FRED",
					       
					       "LCap_TRI",
					       "LCap_CAI",
					       
					       "CBond_TRI",
					       "LTGBond_TRI",
					       
					       "CBond_Yield_AAA",
					       
					       "GDP_FRED"
)

df_dataWilkie <- df_dataAll %>% select(year, month, yearMon, one_of(vars_wilkie))


#**********************************************************************
#      Review time series classes and functions in R               ####
#**********************************************************************

# 


# Time series classes
#		ts
#		zoo
#		xts
#   timeSeries

# Model

# ARIMA model: 
#   arima: from stats
#   Arima: from forecast
#   arimax: from TSA, ARIMA with transfer function
# 




# Structural change : R is particularly strong when dealing with structural changes and changepoints in parametric models, see strucchange and segmented.

# Linear regression models : A convenience interface to lm() for estimating OLS and 2SLS models based on time series data is dynlm. Linear regression models with AR error terms via GLS is possible using gls() from nlme.


df <- data.frame(value = 1:5, var2 = 2:6, yearMon = as.yearmon("2017-01") + 0:4/12)
df

as.xts(df, order.by = df$yearMon)
tk_xts(df, date_var = yearMon)
tk_xts(df) %>% as.data.frame() %>% mutate(yearMon = row.names(.))

x <- read.zoo(df, index.column = "yearMon") %>% as.xts # column names are lost
class(x)

x[month(index(x)) %in% c(2:3) ]
x[index(x) > "2017-03" ] # cannot omit "-"
x["201703/05"]



