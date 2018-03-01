# This script explore economic and financial data and does exploratory modeling 


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




#**********************************************************************
#                          GDP growth                        ####
#**********************************************************************

# Quarterly GDP
df_GDP_q <- 
	df_dataAll_q %>% 
	select(yearMon,
				 GDP = GDP_FRED) %>% 
	mutate(dl_GDP  =  log(GDP/lag(GDP)),
				 l_GDP   = log(GDP)) %>% 
	tk_xts(date_var = yearMon)

# Annual GDP
df_GDP_y <- 
	df_dataAll_y %>% 
	select(yearMon,
				 GDP = GDP_FRED) %>% 
	mutate(l_GDP   = log(GDP),
		     dl_GDP  =  log(GDP/lag(GDP))) %>% 
	tk_xts(date_var = yearMon)

df_GDP_q["1945/", "l_GDP"] %>% plot
df_GDP_q["1945/", "dl_GDP"] %>% plot

df_GDP_y["1945/", "l_GDP"] %>% plot
df_GDP_y["1945/", "dl_GDP"] %>% plot

df_GDP_q["1945/", "dl_GDP"] %>% mean(na.rm = TRUE)
df_GDP_y["1945/", "dl_GDP"] %>% mean(na.rm = TRUE)


## ACF and PACF 
acf2(df_GDP_q["1945/2015", "dl_GDP"]) # Looks like AR1
acf2(df_GDP_y["1945/2015", "dl_GDP"]) # Looks like random walk

## Auto ARIMA modeling

# Quarterly data
sample_period <- "1950/"
auto.arima(df_GDP_q[sample_period, "l_GDP"], max.d = 1, seasonal = FALSE) # ARIMA(2, 1, 0) with drift
sarima(df_GDP_q[sample_period , "dl_GDP"], 1, 0, 0)
 # AR1 (or AR2) looks fine for quarterly GDP growth


# Annual data
sample_period <- "1950/"
auto.arima(df_GDP_y[sample_period, "l_GDP"], max.d = 1, seasonal = FALSE) # ARIMA(0, 1, 0) with drift
sarima(df_GDP_y[sample_period , "dl_GDP"], 0, 0, 0)
  # Annual data looks random walk with a mean around 3.1%



#**********************************************************************
#                          Inflation                               ####
#**********************************************************************
## Variable "Inflation_Index" 
df_inflation_q <- 
  df_dataAll_q %>% 
	select(year, month, yearMon, infl_index = CPIU_SA_FRED) %>% 
	mutate(dl_inflation = log(infl_index / lag(infl_index)),
				 l_inflation  = log(infl_index)) %>% 
  filter(!is.na(dl_inflation)) %>% 
  tk_xts(date_var = yearMon)	
range(df_inflation_q %>% index)  # 1926Q1 - 2015Q4 (SBBI data range, FRED data starts in 1948)


df_inflation_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, infl_index = CPIU_SA_FRED) %>% 
	mutate(dl_inflation = log(infl_index / lag(infl_index)),
				 l_inflation  = log(infl_index)) %>% 
	filter(!is.na(dl_inflation)) %>% 
	tk_xts(date_var = yearMon)
range(df_inflation_y %>% index) #1927 - 2015


# Plotting series

sample_period <- "1950/"
df_inflation_q[sample_period, "l_inflation"]  %>% plot
df_inflation_q[sample_period, "dl_inflation"] %>% plot  # potential issue: inflation rate does not look stationary
df_inflation_q[sample_period, "dl_inflation"] %>% diff %>% plot  # differenced inflation rate has stable mean, but time varying variance

df_inflation_y[sample_period, "l_inflation"]  %>% plot
df_inflation_y[sample_period, "dl_inflation"] %>% plot
df_inflation_y[sample_period, "dl_inflation"] %>% diff %>% plot # annual data look more like stationary

df_inflation_q[sample_period, "dl_inflation"] %>% mean(na.rm = TRUE)
df_inflation_y[sample_period, "dl_inflation"] %>% mean(na.rm = TRUE)
df_inflation_y[sample_period, "dl_inflation"] %>% diff %>% mean(na.rm = TRUE)


## ACF and PACF 
acf2(df_inflation_q[sample_period, "dl_inflation"]) # Look non-stationary
acf2(df_inflation_y[sample_period, "dl_inflation"]) # Look AR(1)

acf2(df_inflation_q[sample_period, "dl_inflation"] %>% diff) # AR?
acf2(df_inflation_y[sample_period, "dl_inflation"] %>% diff) # AR?

df_inflation_q

## Auto ARIMA modeling

# Quarterly data
sample_period <- "1950/"
# Auto ARIMA
auto.arima(df_inflation_q[sample_period, "l_inflation"], max.d = 1, seasonal = FALSE) # ARIMA(5, 1, 1) with drift
auto.arima(df_inflation_q[sample_period, "l_inflation"], max.d = 2, seasonal = FALSE) # estimation issue with second order difference
# Try manually
 # assume inflation rate is stationary 
 sarima(df_inflation_q[sample_period , "dl_inflation"], 3, 0, 0) # AR(3), or even larger AR orders, with non-zero mean
 # assume differenced inflation rate is stationary (changes in quarterly inflation rate)
 sarima(df_inflation_q[sample_period , "dl_inflation"], 3, 1, 1) # AR(3), or even larger AR orders, with zero mean
 

# Annual data
sample_period <- "1950/"
# Auto ARIMA: log inflation index
auto.arima(df_inflation_y[sample_period, "l_inflation"], max.d = 1, seasonal = FALSE) # ARIMA(1, 1, 0) with drift
auto.arima(df_inflation_y[sample_period, "l_inflation"], max.d = 2, seasonal = FALSE) # ARIMA(0, 2, 0) annual changes in infl rate looks like random walk

# Try manually: inflation rate
sarima(df_inflation_y[sample_period , "dl_inflation"], 1, 0, 0) # AR(1)
sarima(df_inflation_y[sample_period , "dl_inflation"], 0, 1, 0) # random walk in changes of infl rate, worse than AR(1)
# AR(1) looks fine for annual inflation rate, which is consistent with the Wikie approach 



## Outlier at 2008Q4
df_inflation_q1 <- df_inflation_q
df_inflation_q1["200812", "l_inflation"] <- 
	(as.numeric(df_inflation_q["200809","l_inflation"]) + as.numeric(df_inflation_q["200903","l_inflation"]))/2
df_inflation_q1["200812", "dl_inflation"] <- as.numeric(df_inflation_q1["200812", "l_inflation"]) - as.numeric(df_inflation_q1["200809", "l_inflation"])

sample_period <- "1950/"
auto.arima(df_inflation_q1[sample_period, "l_inflation"], max.d = 2, seasonal = FALSE) # ARIMA(5, 1, 4) with drift
sarima(df_inflation_q1[sample_period , "dl_inflation"], 4, 0, 0)




## Wilkie Model: Inflation rate modeled as AR(1)
# mod_inflation_q <- Arima(df_inflation_q$dl_inflation, order = c(1, 0, 0))

Arima(df_inflation_y["1927/1989","dl_inflation" ], order = c(1, 0, 0))
Arima(df_inflation_y["1927/2015","dl_inflation" ], order = c(1, 0, 0))
Arima(df_inflation_y["1950/2015","dl_inflation" ], order = c(1, 0, 0))

mod_inflation_y <- Arima(df_inflation_y["1927/2015", "dl_inflation" ], order = c(1, 0, 0))
df_inflation_y  <- cbind(df_inflation_y, infl_residual = xts(mod_inflation_y$residuals, index(df_inflation_y)))


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
				 dl_LCap_DivY = l_LCap_DivY - lag(l_LCap_DivY),
				 l_LCap_DivI  = log(LCap_DivI),
				 dl_LCap_DivI = l_LCap_DivI - lag(l_LCap_DivI)) %>% 
	filter(!is.na(l_LCap_DivY)) %>% 
  tk_xts(date_var = yearMon)	
range(df_dividend_y %>% index)

df_dividend_y <- cbind(df_inflation_y, df_dividend_y)

df_dividend_q <- 
	df_dataAll_q %>% 
	select(year, month, yearMon, LCap_TRI, LCap_CAI) %>% 
	mutate(LCap_DivI   = (LCap_TRI/lag(LCap_TRI)) * lag(LCap_CAI) - LCap_CAI,
				 LCap_DivY   = LCap_DivI / LCap_CAI,
				 l_LCap_DivY = log(LCap_DivY),
				 dl_LCap_DivY = l_LCap_DivY - lag(l_LCap_DivY),
				 l_LCap_DivI  = log(LCap_DivI),
				 dl_LCap_DivI = l_LCap_DivI - lag(l_LCap_DivI)) %>% 
	filter(!is.na(l_LCap_DivY)) %>% 
	tk_xts(date_var = yearMon)	
range(df_dividend_y %>% index)

df_dividend_q <- cbind(df_inflation_q, df_dividend_q)



## Plotting series
sample_period <- "1927/"
df_dividend_q[sample_period, "l_LCap_DivY"]  %>% plot
df_dividend_q[sample_period, "dl_LCap_DivY"] %>% plot  

df_dividend_y[sample_period, "l_LCap_DivY"]  %>% plot
df_dividend_y[sample_period, "dl_LCap_DivY"] %>% plot  

df_dividend_q[sample_period, "dl_LCap_DivY"] %>% mean(na.rm = TRUE)
df_dividend_y[sample_period, "dl_LCap_DivY"] %>% mean(na.rm = TRUE)


## ACF and PACF 
acf2(df_dividend_q[sample_period, "dl_LCap_DivY"]) # looks like AR1 or AR3
acf2(df_dividend_y[sample_period, "dl_LCap_DivY"]) # looks like random walk 


## Auto ARIMA modeling

# Quarterly data
sample_period <- "1950/"
# Auto ARIMA
auto.arima(df_dividend_q[sample_period, "l_LCap_DivY"], max.d = 0, seasonal = FALSE) # AR(2) is selected if assuming Div Yield is stationary
auto.arima(df_dividend_q[sample_period, "l_LCap_DivY"], max.d = 1, seasonal = FALSE) # ARIMA(5, 1, 2)

# Try manually
 # assume log dividend yield is stationary 
 sarima(df_dividend_q[sample_period , "l_LCap_DivY"], 2, 0, 2) # No satisfactory spec
 # assume differenced dividend yield is stationary (change in dividend yield)
 sarima(df_dividend_q[sample_period , "l_LCap_DivY"], 5, 1, 2) # ARIMA(5, 1, 2) (selected by auto arima) is not satisfactory 


# Annual data
sample_period <- "1950/"
# Auto ARIMA: log inflation index
auto.arima(df_dividend_y[sample_period, "l_LCap_DivY"], max.d = 1, seasonal = FALSE) # Looks random walk

# Try manually: inflation rate
sarima(df_dividend_y[sample_period , "l_LCap_DivY"], 1, 0, 0) # Level:       AR(1) or AR(2) 
sarima(df_dividend_y[sample_period , "l_LCap_DivY"], 1, 1, 0) # differenced: AR(1) par is not significant
sarima(df_dividend_y[sample_period , "l_LCap_DivY"], 0, 1, 0) # differentced:Looks random walk


 # Dividend yield does not look stationary 
 # No satisfactory model spec found for quarterly dividend yield (for neither level nor difference)
 # Annual level dividend yield: AR(1) or AR(2), but the problem is that the level data do not look stationary 
 # Annual differenced dividend yield: look like random walk.


## Wilkie model: 
# Dividend yield level is modeled as AR(1)

Arima(df_dividend_y["1927/2014", "l_LCap_DivY" ], xreg = df_dividend_y["1927/2014", "dl_inflation"], order = c(1, 0, 0))
# As in HSZ2016, parameter for inflation rate is not significant. (while we have very different point estiamte)

mod_divY_y <- Arima(df_dividend_y["1927/2014", "l_LCap_DivY" ], order = c(1, 0, 0))
mod_divY_y
mod_divY_y$sigma2^0.5
Arima(df_dividend_y["1927/1989", "l_LCap_DivY" ], order = c(1, 0, 0))

# Model AR(1)+transfer function of inflation





#**********************************************************************
#                      Wilkie Dividend Index             ####
#**********************************************************************

## Plotting series
sample_period <- "1927/"
df_dividend_q[sample_period, "l_LCap_DivI"]  %>% plot
df_dividend_q[sample_period, "dl_LCap_DivI"] %>% plot  
# Great volatility before 1955. Data after 1955 look stationary

df_dividend_y[sample_period, "LCap_DivI"]  %>% plot
df_dividend_y[sample_period, "dl_LCap_DivI"] %>% plot  

df_dividend_q[sample_period, "dl_LCap_DivI"] %>% mean(na.rm = TRUE)
df_dividend_y[sample_period, "dl_LCap_DivI"] %>% mean(na.rm = TRUE)


## ACF and PACF 
acf2(df_dividend_q[sample_period, "dl_LCap_DivI"]) # looks like MA2
acf2(df_dividend_y[sample_period, "dl_LCap_DivI"]) # looks like AR1 or MA1


## Auto ARIMA modeling

# Quarterly data
sample_period <- "1950/"
# Auto ARIMA
auto.arima(df_dividend_q[sample_period, "l_LCap_DivI"], max.d = 1, seasonal = FALSE) # ARIMA(3, 1, 3) with drift
# model spec selected by auto.arima is sensitive to sample period

# Try manually
# assume log differenced dividend yield is stationary 
sarima(df_dividend_q[sample_period , "dl_LCap_DivI"], 4, 0, 3) # 
# auto-selected model spec cannot pass Ljung-Box tests


# Annual data
sample_period <- "1960/"
# Auto ARIMA: log inflation index
auto.arima(df_dividend_y[sample_period, "l_LCap_DivI"], max.d = 1, seasonal = FALSE) # MA1 looks fine

# Try manually: inflation rate
sarima(df_dividend_y[sample_period , "dl_LCap_DivI"], 0, 0, 1) # MA1, but bad performance in normaltiy and L-B test. 



## Wilkie model: 

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

# add exponentially weighted inflation
df_dividend_y <- cbind(df_dividend_y, dl_inflation_ewa =  get_ewa(df_inflation_y[, "dl_inflation"], dd))
df_dividend_y[sample_period, "dl_inflation_ewa"] %>% plot

# reduced Wilkie specification: MA(1) + exponentially weighted inflation
sample_period <- "1950/2014"
mod_divI_y <- Arima(df_dividend_y[sample_period, "dl_LCap_DivI"], xreg = df_dividend_y[sample_period, "dl_inflation_ewa"], order = c(0, 0, 1),
										fixed = c(NA, NA, wd))
mod_divI_y
mod_divI_y$sigma2^0.5

# Pure MA1
Arima(df_dividend_y[sample_period, "dl_LCap_DivI"], order = c(0, 0, 1), 
			fixed = c(NA, NA))



## Issues:
 # Model parameter estimates are sensitive sample period. Including data after 2010 greatly increases the estiamte of intercept
 # Parameter estiamtes are not consistent with HSZ2016 paper. (p6). We have smaller MA1 parameter and greater intercept parameter


## Further examine the diffrence between dividend growth and inflation rate  
dfx <- df_dividend_y[sample_period, "dl_LCap_DivI"] - df_dividend_y[sample_period, "dl_inflation_ewa"]
dfx %>% plot
dfx %>% acf2
auto.arima(dfx) # MA(1) model selected


#**********************************************************************
#                   Long term corporate bond yield                 ####
#**********************************************************************

df_interest_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, Inflation_Index = CPIU_SA_FRED, CBond_Yield_AAA) %>% 
	mutate(CBond_Yield_AAA = CBond_Yield_AAA/100,
				 dl_inflation = log(Inflation_Index / lag(Inflation_Index)),
				 CBond_Yield_real = CBond_Yield_AAA - dl_inflation,
				 l_CBond_Yield_real = log(CBond_Yield_AAA),
				 dl_CBond_Yield_real = l_CBond_Yield_real - lag(l_CBond_Yield_real)
				 ) %>% tk_xts(date_var = yearMon)	
range(df_interest_y %>% index)


df_interest_q <- 
	df_dataAll_q %>% 
	select(year, month, yearMon, Inflation_Index = CPIU_SA_FRED, CBond_Yield_AAA) %>% 
	mutate(CBond_Yield_AAA = CBond_Yield_AAA/100,
				 dl_inflation = log(Inflation_Index / lag(Inflation_Index)),
				 CBond_Yield_real = CBond_Yield_AAA - dl_inflation,
				 l_CBond_Yield_real = log(CBond_Yield_AAA),
				 dl_CBond_Yield_real = l_CBond_Yield_real - lag(l_CBond_Yield_real)
	) %>% tk_xts(date_var = yearMon)	
range(df_interest_y %>% index)


## Plotting series
sample_period <- "1927/"
df_interest_q[sample_period, "CBond_Yield_AAA"] %>% plot  
df_interest_q[sample_period, "l_CBond_Yield_real"] %>% plot
df_interest_q[sample_period, "dl_CBond_Yield_real"] %>% plot

df_interest_y[sample_period, "CBond_Yield_AAA"] %>% plot  
df_interest_y[sample_period, "l_CBond_Yield_real"] %>% plot
df_interest_y[sample_period, "dl_CBond_Yield_real"] %>% plot


df_interest_q[sample_period, "CBond_Yield_real"] %>% mean(na.rm = TRUE)
df_interest_y[sample_period, "CBond_Yield_real"] %>% mean(na.rm = TRUE)


## ACF and PACF 
acf2(df_interest_q[sample_period, "dl_CBond_Yield_real"]) # look like random walk 
acf2(df_interest_y[sample_period, "dl_CBond_Yield_real"]) # look like random walk


## ARIMA modeling

# Quarterly data
sample_period <- "1950/"
# Auto ARIMA
auto.arima(df_interest_q["1927/",  "l_CBond_Yield_real"], max.d = 1, seasonal = FALSE)  # ARIMA(2, 1, 2) starting from 1927
auto.arima(df_interest_q["1950/",  "l_CBond_Yield_real"], max.d = 1, seasonal = FALSE) # ARIMA(0, 1, 0) starting from 1950


# Try manually
# assume log differenced interest yield is stationary 
sarima(df_interest_q["1927/" , "dl_CBond_Yield_real"], 2, 0, 2) # ok
sarima(df_interest_q["1927/" , "dl_CBond_Yield_real"], 1, 0, 1) # 

sarima(df_interest_q["1950/" , "dl_CBond_Yield_real"], 0, 0, 0) # ok 
sarima(df_interest_q["1950/" , "dl_CBond_Yield_real"], 2, 0, 2) # ok
sarima(df_interest_q["1950/" , "dl_CBond_Yield_real"], 1, 0, 0) # not good in L-B test
sarima(df_interest_q["1950/" , "dl_CBond_Yield_real"], 3, 0, 0) # OK, good AIC, but not BIC


# annual data: 
sample_period <- "1927/"
# Auto ARIMA: log inflation index
auto.arima(df_interest_y[sample_period, "l_CBond_Yield_real"], max.d = 1, seasonal = FALSE) # random walk regardless of starting point

# Try manually: inflation rate
sarima(df_interest_y[sample_period , "dl_CBond_Yield_real"], 1, 0, 0) # Prefered by AIC, but small AR1 parameter and big p value(0.11)
sarima(df_interest_y[sample_period , "dl_CBond_Yield_real"], 0, 0, 0) # Prefered by BIC


## Wilkie model for real part: log yield modeled as AR(1) (reduced model)

dc <- 0.058
wc <- 1

mod_interest_real <- Arima(df_interest_y["1926/2015", "l_CBond_Yield_real"], order = c(1, 0, 0))
mod_interest_real
mod_interest_real$coef[2] %>% exp
mod_interest_real$sigma2^0.5 # much lower than the estimates in HSZ2016

# Issue



## Wilkie model inflation part
inflation_ewa_interest <- get_ewa(df_interest_y["1927/2015", "dl_inflation"], dc)
inflation_ewa_interest 

# set c_min, so that
#  if the real component of long-term bond yield is smaller than c_min, then
#  forces the real component to be c_min, and the inflationary component to be 
#  the total rate minus c_min, where the total rate is the sum of unadjusted real
#  and inflationary components 
#  This ensures that simulated the real part is always positive and can take log. 




#**********************************************************************
#                        Try simulations                           ####
#**********************************************************************

## Inflation

# Models:
 #1a. AR(1) of inflation rate on 1951-1989
 #1b. AR(1) of inflation rate on 1951-2015
 #2a. ARIMA(0, 1, 0) of inflation rate on 1951-1989, mean forced to 0
 #2b. ARIMA(0, 1, 0) of inflation rate on 1951-2015, mean forced to 0
 #3. HSZ2016 estimate on 1951-1989


mdl_infl_y1a <- Arima(df_inflation_y["1951/1989" , "dl_inflation"],  c(1, 0, 0)) # AR(1)
mdl_infl_y1b <- Arima(df_inflation_y["1951/2015" , "dl_inflation"],  c(1, 0, 0)) # AR(1)
mdl_infl_y2a <- Arima(df_inflation_y["1951/1989" , "dl_inflation"],  c(0, 1, 0)) # random walk in changes of infl rate 
mdl_infl_y2b <- Arima(df_inflation_y["1951/2015" , "dl_inflation"],  c(0, 1, 0)) # random walk in changes of infl rate

mdl_infl_y1a
mdl_infl_y1b
mdl_infl_y2a
mdl_infl_y2b


 # HSZ2016 estimate 
mdl_infl_y3 <- mod_infl_y1a
mdl_infl_y3$coef   <- c(ar1 = 0.8067, intercept = 0.0396)
mdl_infl_y3$sigma2 <- 0.0189^2 # 0.00035721

# Check simulated series
Arima(simulate(mdl_infl_y3, 100), order = c(1, 0, 0))



# Simulation
nsim <- 2000
nyear_sim <- 100

set.seed(1234); sim_infl_mdl_infl_y1a <- replicate(nsim,simulate(mdl_infl_y1a, 100, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y1b <- replicate(nsim,simulate(mdl_infl_y1b, 100, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y2a <- replicate(nsim,simulate(mdl_infl_y2a, 100, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y2b <- replicate(nsim,simulate(mdl_infl_y2b, 100, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y3  <- replicate(nsim,simulate(mdl_infl_y3,  100, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y3" , year = seq_len(nyear_sim))

x <- sim_infl_mdl_infl_y3 %>% 
	gather(sim, value, -year, -mdl)
x %>% head

x %<>% group_by(year) %>% 
	summarize(
		        q10 = quantile(value, 0.10),
						q25 = quantile(value, 0.25),
						q50 = quantile(value, 0.50),
						q75 = quantile(value, 0.75),
						q90 = quantile(value, 0.90),
						mdl = unique(mdl))
x %>% head()

x %>% gather(type, value, -year, - mdl) %>% 
	mutate(type = as.factor(type) %>% fct_rev) %>% 
	ggplot(aes(x = year, y = value, color = type)) + theme_bw() + 
	geom_line() +
	geom_point() +
	coord_cartesian(ylim = c(0, 0.1)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.01))


sim_infl_mod_infl_y1a %>% head
sim_infl_mod_infl_y3  %>% head










