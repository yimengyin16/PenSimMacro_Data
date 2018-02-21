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


#**********************************************************************
#                          Wilkie Inflation                        ####
#**********************************************************************
## Variable "Inflation_Index"
df_inflation_q <- 
  df_dataAll_q %>% 
	select(year, month, yearMon, Inflation_Index) %>% 
	mutate(dl_inflation = log(Inflation_Index / lag(Inflation_Index)),
				 l_inflation  = log(Inflation_Index)) %>% 
  filter(!is.na(dl_inflation)) 
df_inflation_q 
range(df_inflation$yearMon)  # 1926Q1 - 2015Q4


df_inflation_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, Inflation_Index) %>% 
	mutate(dl_inflation = log(Inflation_Index / lag(Inflation_Index))) %>% 
	filter(!is.na(dl_inflation)) 
df_inflation_y 
range(df_inflation_y$year) #1927 - 2015

## converting to xts format
df_inflation_y %<>% tk_xts(date_var = yearMon)	
df_inflation_y


## Modeling with AR(1)
# mod_inflation_q <- Arima(df_inflation_q$dl_inflation, order = c(1, 0, 0))

Arima(df_inflation_y["1927/1989","dl_inflation" ], order = c(1, 0, 0))
Arima(df_inflation_y["1927/2015","dl_inflation" ], order = c(1, 0, 0))

mod_inflation_y <- Arima(df_inflation_y["1927/2015", "dl_inflation" ], order = c(1, 0, 0))

df_inflation_y <- cbind(df_inflation_y, infl_residual = xts(mod_inflation_y$residuals, index(df_inflation_y)))


## diagnostic of AR(1)
df_inflation_y[,"infl_residual"] %>% acf
df_inflation_y[,"infl_residual"] %>% pacf
 # AR(1) seems not sufficient for yearly inflation 


#**********************************************************************
#                      Wilkie Dividend Yield             ####
#**********************************************************************

df_dividend_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, LCap_TRI, LCap_CAI) %>% 
	mutate(LCap_DivI   = (LCap_TRI/lag(LCap_TRI)) * lag(LCap_CAI) - LCap_CAI,
				 LCap_DivY   = LCap_DivI / LCap_CAI,
		     l_LCap_DivY = log(LCap_DivY),
				 dl_LCap_DivI = log(LCap_DivI/lag(LCap_DivI))) %>% 
	filter(!is.na(l_LCap_DivY))

df_dividend_y %<>% tk_xts(date_var = yearMon)	
range(df_dividend_y$year)

# Model AR(1)
mod_divY_y <- Arima(df_dividend_y["1927/2014", "l_LCap_DivY" ], order = c(1, 0, 0))
mod_divY_y
mod_divY_y$sigma2^0.5

Arima(df_dividend_y["1927/1989", "l_LCap_DivY" ], order = c(1, 0, 0))

# Model AR(1)+transfer function of inflation




#**********************************************************************
#                      Wilkie Dividend Index             ####
#**********************************************************************

dd <- 0.38
wd <- 1

get_ewa <- function(x, d){
  # compute exponentially weighted average
  # x: time series in xts 
	# d: weight of current period
  
	#x <- df_inflation_y[, "dl_inflation"]
	#d <- 0.38
  n <- length(x)  
  ewa <- numeric(n)
  
  ewa[1] <- x[1]
  for(i in 2:n) ewa[i] <- (1 - d) * ewa[i - 1] + d * x[i]
  
  ewa <- xts(ewa, index(x))
}
df_dividend_y <- cbind(df_dividend_y, dl_inflation_ewa =  get_ewa(df_inflation_y[, "dl_inflation"], dd))

mod_divI_y <- Arima(df_dividend_y[, "dl_LCap_DivI"] - df_dividend_y[, "dl_inflation_ewa"], order = c(0, 0, 1))
mod_divI_y$sigma2^0.5

Arima(df_dividend_y["1928/1989", "dl_LCap_DivI"] - df_dividend_y["1928/1989", "dl_inflation_ewa"], order = c(0, 0, 1))


#**********************************************************************
#                   Wilkie Long term interest rate                 ####
#**********************************************************************

df_interest_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, Inflation_Index, CBond_Yield_AAA) %>% 
	mutate(CBond_Yield_AAA = CBond_Yield_AAA/100,
				 dl_inflation = log(Inflation_Index / lag(Inflation_Index)),
				 CBond_Yield_real = CBond_Yield_AAA - dl_inflation,
				 l_CBond_Yield_real = log(CBond_Yield_AAA)
				 ) 

df_interest_y %<>% tk_xts(date_var = yearMon)	
range(df_interest_y$year)


dc <- 0.058
wc <- 1

mod_interest_real <- Arima(df_interest_y["1926/2015", "l_CBond_Yield_real"], order = c(1, 0, 0))
mod_interest_real
mod_interest_real$coef[2] %>% exp
mod_interest_real$sigma2^0.5


inflation_ewa_interest <- get_ewa(df_interest_y["1927/2015", "dl_inflation"], dc)
inflation_ewa_interest 


















