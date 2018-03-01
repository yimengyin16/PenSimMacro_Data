# Doodling for learning packages and random things. 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************
# A discussion of ARIMA transfer function models:
# https://robjhyndman.com/hyndsight/arimax/


## Issues:
  # Simulate ARIMA process with initial values and regressors


## Results


# GDP

# ARIMA modeling: 
 # Annual GDP growth rates tend to be random walk
 # Quarterly GDP growth 



# Inflation


# Dividend Yield 


# Dividend Index


# Long term Corp Bond rate




#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(tidyverse)
library(readxl)
library(magrittr)
library(stringr)
library(forcats)
library(grid)
library(gridExtra)
library(scales)

# packages for time series modeling
library(astsa)    # companion package
library(TSA)      # companion package;  arimax: flexible transfer function model
library(tseries)  #
library(forecast) # Arima

# packages for 
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
#   - x SBBI long-term corb bond yield
#   - x SBBI long-term gov bond yield
#   - Moody's long-term bond yield
# 5. Long-term bond total return
#   - SBBI long-term corb bond total return index
#   - SBBI long-term gov bond  total return index
# 6. GDP
#   - FRED quarterly GDP 
#   - x Stock-Watson monthly GDP
#   - x MA monthly GDP

vars_wilkie <- c("Inflation_Index", # SBBI price index, based on CPI-U, not seasonally adjusted
					       "CPIU_SA_FRED",    # CPI-U, seasonally adjusted
					       "CPIU_NA_FRED",    # CPI-U, not seasonally adjusted
					       
					       "LCap_TRI", # SBBI Large Cap total return
					       "LCap_CAI", # SBBI Large Cap capital appreciation
					       
					       "CBond_TRI",   # SBBI corporate bond total return index
					       "LTGBond_TRI", # SBBI government bond total return index
					       
					       "CBond_Yield_AAA", # Moody's AAA long-term bond yield
					       
					       "GDP_FRED"  # GDP
)

df_dataWilkie <- df_dataAll %>% select(year, month, yearMon, one_of(vars_wilkie))


#**********************************************************************
#      Review time series classes and functions in R               ####
#**********************************************************************

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

# Nonlinear ts
# tsDyn https://www.rdocumentation.org/packages/tsDyn/versions/0.8-1; includes afunction for VAR simulation
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


# Transfer function models


x <- df_dataAll_q %>% 
	select(yearMon,
		     GDP = GDP_FRED,
				 infl = CPIU_SA_FRED) %>% 
	mutate(dl_GDP  =  log(GDP/lag(GDP)),
				 dl_infl = log(infl/lag(infl)),
				 d2l_infl = dl_infl - lag(dl_infl)
				 ) %>% 
	tk_xts(date_var = yearMon)

x["1948/2015"]

plot(x["1950/2007", "d2l_infl"])
acf2(x["1983/2015", "d2l_infl"])
sarima(x["1950/2007", "d2l_infl"], 5, 0, 2)
auto.arima(x["1950/2015", "d2l_infl"], seasonal = FALSE)

# regARIMA model with fixed parameter for regressors. 
Arima(x["1983/2015", "dl_GDP"], order = c(1, 0, 1), xreg = x["1983/2015", "d2l_infl"], 
			fixed = c(NA, NA, NA, 1)) 




