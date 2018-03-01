# This script explore economic and financial data and does exploratory modeling 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************
# A discussion of ARIMA transfer function models:
# https://robjhyndman.com/hyndsight/arimax/


## Issues:
  # Simulate ARIMA process with initial values and regressors





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

# df_dataWilkie <- df_dataAll %>% select(year, month, yearMon, one_of(vars_wilkie))



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
df_inflation_y  <- cbind(df_inflation_y["1927/2015",], infl_residual = xts(mod_inflation_y$residuals, index(df_inflation_y["1927/2015",])))


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
 # Model parameter estimates are sensitive to sample period. Including data after 2010 greatly increases the estiamte of intercept
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
inflation_ewa_interest <- get_ewa(df_interest_y["1951/2015", "dl_inflation"], dc)
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
 #1a. AR(1) of inflation rate on 1951-1984
 #1b. AR(1) of inflation rate on 1951-2015
 #2a. ARIMA(0, 1, 0) of inflation rate on 1951-1984
 #2b. ARIMA(0, 1, 0) of inflation rate on 1951-2015
 #3. HSZ2016 (Hardy, Saunders, Zhang, 2016) estimates on 1951-1984


mdl_infl_y1a <- Arima(df_inflation_y["1951/1984" , "dl_inflation"],  c(1, 0, 0)) # AR(1)
mdl_infl_y1b <- Arima(df_inflation_y["1951/2015" , "dl_inflation"],  c(1, 0, 0)) # AR(1)
mdl_infl_y2a <- Arima(df_inflation_y["1951/1984" , "dl_inflation"],  c(0, 1, 0)) # random walk in changes of infl rate 
mdl_infl_y2b <- Arima(df_inflation_y["1951/2015" , "dl_inflation"],  c(0, 1, 0)) # random walk in changes of infl rate

mdl_infl_y1a
mdl_infl_y1b
mdl_infl_y2a
mdl_infl_y2b


 # HSZ2016 estimate 
mdl_infl_y3 <- mdl_infl_y1a
mdl_infl_y3$coef   <- c(ar1 = 0.8067, intercept = 0.0396)
mdl_infl_y3$sigma2 <- 0.0189^2 # 0.00035721

# Check simulated series
Arima(simulate(mdl_infl_y3, 100), order = c(1, 0, 0))
# OK

# Simulation
nsim <- 5000
nyear_sim <- 100

set.seed(1234); sim_infl_mdl_infl_y1a <- replicate(nsim,simulate(mdl_infl_y1a, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y1b <- replicate(nsim,simulate(mdl_infl_y1b, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y2a <- replicate(nsim,simulate(mdl_infl_y2a, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y2b <- replicate(nsim,simulate(mdl_infl_y2b, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_mdl_infl_y3  <- replicate(nsim,simulate(mdl_infl_y3,  nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y3" , year = seq_len(nyear_sim))

fn_plot <- function(data){
	
	df <- data %>% 
		gather(sim, value, -year, -mdl) %>% 
		group_by(year) %>% 
		summarize(
			q10 = quantile(value, 0.10),
			q25 = quantile(value, 0.25),
			q50 = quantile(value, 0.50),
			q75 = quantile(value, 0.75),
			q90 = quantile(value, 0.90),
			mdl = unique(mdl))
	# df %>% head()
	
	fig <- 
	df %>% gather(percentile, value, -year, - mdl) %>% 
		mutate(percentile = as.factor(percentile) %>% fct_rev) %>% 
		ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
		geom_line() +
		geom_point() +
		coord_cartesian(ylim = c(0, 0.1)) + 
		scale_y_continuous(breaks = seq(0, 1, 0.01))
}

fig_infl_y1a <- fn_plot(sim_infl_mdl_infl_y1a)
fig_infl_y1b <- fn_plot(sim_infl_mdl_infl_y1b)
fig_infl_y3  <- fn_plot(sim_infl_mdl_infl_y3)

fig_infl_y2a <- fn_plot(sim_infl_mdl_infl_y2a) # divergent, should not be used for simulation
fig_infl_y2b <- fn_plot(sim_infl_mdl_infl_y2b) # divergent, should not be used for simulation

fig_infl_y1a
fig_infl_y1b
fig_infl_y3

sim_infl <- bind_rows(sim_infl_mdl_infl_y1a,
											sim_infl_mdl_infl_y1b,
											sim_infl_mdl_infl_y3 )

df <- sim_infl %>% 
	gather(sim, value, -year, -mdl) %>% 
	group_by(year, mdl) %>% 
	summarize(
		q10 = quantile(value, 0.10),
		q25 = quantile(value, 0.25),
		q50 = quantile(value, 0.50),
		q75 = quantile(value, 0.75),
		q90 = quantile(value, 0.90))
# df %>% head()

fig_infl <- 
	df %>% gather(percentile, value, -year, -mdl) %>% 
	mutate(percentile = as.factor(percentile) %>% fct_rev,
				 mdl = factor(mdl, labels = c("AR(1) 1951-1984", "AR(1) 1951-2015", "AR(1) HSZ2016 1951-1984"))) %>% 
	ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
	facet_grid(. ~ mdl) +
	geom_line() +
	geom_point() +
	coord_cartesian(ylim = c(0, 0.1)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.01))

fig_infl

# Observations:
#   Distribution, especially the mean, can be sensitive to sample period
#   
#   
#                         mean      AR(1)     sd
#   AR(1)   1951-1984     0.046     0.77    0.022
#   HSZ2016 1951-2084     0.039     0.807   0.019
#   AR(1)   1951-2015     0.036     0.74    0.019

#  

mdl_infl_y1a$sigma2
mdl_infl_y1b$sigma2
mdl_infl_y3$sigma2

mdl_infl_y1a$sigma2^0.5
mdl_infl_y1b$sigma2^0.5
mdl_infl_y3$sigma2^0.5









