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
library(broom)
library(readxl)
library(magrittr)
library(stringr)
library(forcats)
library(grid)
library(gridExtra)
library(scales)
library(knitr)

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

# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

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
				 year, month,
				 GDP = GDP_FRED) %>% 
	mutate(dl_GDP  =  log(GDP/lag(GDP)),
				 l_GDP   = log(GDP)) %>% 
	tk_xts(date_var = yearMon)



# Annual GDP
df_GDP_y <- 
	df_dataAll_y %>% 
	select(yearMon,
				 year, month,
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
	select(year, month, yearMon, infl_index = CPIU_SA_FRED, infl_index_NA = Inflation_Index) %>% 
	mutate(dl_inflation = log(infl_index / lag(infl_index)),
				 l_inflation  = log(infl_index),
				 dl_inflation_NA = log(infl_index_NA / lag(infl_index_NA)),
				 l_inflation_NA  = log(infl_index_NA)) %>% 
  filter(!is.na(dl_inflation_NA)) %>% 
  tk_xts(date_var = yearMon)	
range(df_inflation_q %>% index)  # 1926Q1 - 2015Q4 (SBBI data range, FRED data starts in 1948)


df_inflation_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, infl_index = CPIU_SA_FRED, infl_index_NA = Inflation_Index) %>% 
	mutate(dl_inflation = log(infl_index / lag(infl_index)),
				 l_inflation  = log(infl_index),
				 dl_inflation_NA = log(infl_index_NA / lag(infl_index_NA)),
				 l_inflation_NA  = log(infl_index_NA)) %>% 
	filter(!is.na(dl_inflation_NA)) %>% 
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
df_dividend_y <- cbind(df_dividend_y, dl_inflation_NA_ewa =  get_ewa(df_inflation_y[, "dl_inflation_NA"], dd))
df_dividend_y[sample_period, "dl_inflation_NA_ewa"] %>% plot

# reduced Wilkie specification: MA(1) + exponentially weighted inflation

sample_period <- "1926/1989"
mod_divI_y <- Arima(df_dividend_y[sample_period, "dl_LCap_DivI"], xreg = df_dividend_y[sample_period, "dl_inflation_NA_ewa"], order = c(0, 0, 1),
										fixed = c(NA, NA, wd))
mod_divI_y
mod_divI_y$sigma2^0.5

sample_period <- "1926/2014"
mod_divI_y <- Arima(df_dividend_y[sample_period, "dl_LCap_DivI"], xreg = df_dividend_y[sample_period, "dl_inflation_NA_ewa"], order = c(0, 0, 1),
										fixed = c(NA, NA, wd))
mod_divI_y
mod_divI_y$sigma2^0.5


# Pure MA1
Arima(df_dividend_y[sample_period, "dl_LCap_DivI"], order = c(0, 0, 1), 
			fixed = c(NA, NA))

## Issues:
 # Model parameter estimates are sensitive to sample period. Including data after 2010 significantly increases the estiamte of intercept

## Further examine the diffrence between dividend growth and inflation rate  
dfx <- df_dividend_y[sample_period, "dl_LCap_DivI"] - df_dividend_y[sample_period, "dl_inflation_ewa"]
dfx %>% plot
dfx %>% acf2
auto.arima(dfx) # MA(1) model selected




#**********************************************************************
#                   Long term corporate bond yield                 ####
#**********************************************************************

dc    <- 0.058
wc    <- 1
c_min <-  0.005


df_cbond_y <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, 
				 infl_index    = CPIU_SA_FRED,
				 infl_index_NA = Inflation_Index,
				 CBond_Yield_AAA) %>% 
	mutate(CBond_Yield_AAA = CBond_Yield_AAA/100,
				 dl_inflation    = log(infl_index    / lag(infl_index)),
				 dl_inflation_NA = log(infl_index_NA / lag(infl_index_NA))
				 #CBond_Yield_real = CBond_Yield_AAA - dl_inflation,
				 #l_CBond_Yield_real = log(CBond_Yield_AAA),
				 #dl_CBond_Yield_real = l_CBond_Yield_real - lag(l_CBond_Yield_real)
				 ) %>% tk_xts(date_var = yearMon)	
range(df_cbond_y %>% index)
Index_cbond_y <- index(df_cbond_y)

df_cbond_q <- 
	df_dataAll_q %>% 
	select(year, month, yearMon, 
				 infl_index    = CPIU_SA_FRED,
				 infl_index_NA = Inflation_Index, 
				 CBond_Yield_AAA) %>% 
	mutate(CBond_Yield_AAA = CBond_Yield_AAA/100,
				 dl_inflation    = log(infl_index    / lag(infl_index)),
				 dl_inflation_NA = log(infl_index_NA / lag(infl_index_NA))
				 #CBond_Yield_real = CBond_Yield_AAA - dl_inflation,
				 #l_CBond_Yield_real = log(CBond_Yield_AAA),
				 #dl_CBond_Yield_real = l_CBond_Yield_real - lag(l_CBond_Yield_real)
	) %>% tk_xts(date_var = yearMon)	
range(df_cbond_y %>% index)
Index_cbond_q <- index(df_cbond_q)


# Inflationary and real parts of long-term corp bond rate. 

# Notes:

# set c_min, so that
  #  if the real component of long-term bond yield is smaller than c_min, then
  #  forces the real component to be c_min, and the inflationary component to be 
  #  the total rate minus c_min, where the total rate is the sum of unadjusted real
  #  and inflationary components 
  #  This ensures that simulated the real part is always positive and can take log. 

decomp_cbondY <- function(infl, yield_tot, dc, c_min){
	## compute inflationary and real components of corp bond yield based on p9 HSZ2016
	# infl:      inflation in xts 
	# yield_tot: weight of current period
	# dc: weight
	# c_min: min of real component of yield
	
	# infl      <- df_cbond_y[, "dl_inflation"]
	# yield_tot <- df_cbond_y[,  "CBond_Yield_AAA"]
	# dc <- 0.058 
	# c_min <- 0.005
	
	Index <- index(infl)
	non_NA <- (!is.na(infl) & !is.na(yield_tot)) %>% as.vector
	
	infl      <- infl[non_NA, ]
	yield_tot <- yield_tot[non_NA, ]
	
	n <- length(infl)  
	yield_infl <- numeric(n)
	yield_real <- numeric(n)
	
	yield_infl[1] <- min(infl[1], yield_tot[1] - c_min)
	for(i in 2:n) yield_infl[i] <- min((1 - dc) * yield_infl[i - 1] + dc * infl[i], yield_tot[i] - c_min)
	yield_infl <- xts(yield_infl, index(yield_tot))
	yield_real <- yield_tot - yield_infl
	colnames(yield_real) <- "yield_real"
	
	yield_decomp <- merge(merge(yield_real, yield_infl), Index)
  yield_decomp	
}

cbondY_comp_y    <- decomp_cbondY(df_cbond_y[, "dl_inflation"],    df_cbond_y[, "CBond_Yield_AAA"], dc, c_min)
cbondY_comp_y_NA <- decomp_cbondY(df_cbond_y[, "dl_inflation_NA"], df_cbond_y[, "CBond_Yield_AAA"], dc, c_min)
colnames(cbondY_comp_y_NA) <- paste(colnames(cbondY_comp_y_NA), "NA", sep = "_")

cbondY_comp_q    <- decomp_cbondY(df_cbond_q[, "dl_inflation"],    df_cbond_q[, "CBond_Yield_AAA"], dc, c_min)
cbondY_comp_q_NA <- decomp_cbondY(df_cbond_q[, "dl_inflation_NA"], df_cbond_q[, "CBond_Yield_AAA"], dc, c_min)
colnames(cbondY_comp_q_NA) <- paste(colnames(cbondY_comp_q_NA), "NA", sep = "_")

df_cbond_y <- merge(df_cbond_y, cbondY_comp_y, cbondY_comp_y_NA)
df_cbond_q <- merge(df_cbond_q, cbondY_comp_q, cbondY_comp_q_NA)

cbondY_comp_q_NA %>% plot
cbondY_comp_y_NA %>% plot
df_cbond_y[, c("dl_inflation_NA", "CBond_Yield_AAA")] %>% plot


df_cbond_y %<>% 
	as.data.frame() %>% 
  mutate(l_yield_real = log(yield_real),
  			 l_yield_real_NA = log(yield_real_NA),
  			 dl_yield_real    = l_yield_real - lag(l_yield_real),
  			 dl_yield_real_NA = l_yield_real_NA - lag(l_yield_real_NA)) %>% 
  tk_xts(order.by = Index_cbond_y)

df_cbond_q %<>% 
	as.data.frame() %>% 
	mutate(l_yield_real = log(yield_real),
				 l_yield_real_NA = log(yield_real_NA),
				 dl_yield_real    = l_yield_real - lag(l_yield_real),
				 dl_yield_real_NA = l_yield_real_NA - lag(l_yield_real_NA)) %>% 
	tk_xts(order.by = Index_cbond_q)

df_cbond_y %>% head


## Plotting series
sample_period <- "1927/"
df_cbond_q[sample_period, "CBond_Yield_AAA"] %>% plot  
df_cbond_q[sample_period, "yield_real_NA"] %>% plot
df_cbond_q[sample_period, "dl_yield_real_NA"] %>% plot

df_cbond_y[sample_period, "CBond_Yield_AAA"]  %>% plot  
df_cbond_y[sample_period, "yield_real_NA"] %>% plot
df_cbond_y[sample_period, "yield_infl_NA"] %>% plot
df_cbond_y[sample_period, "dl_inflation_NA"] %>% plot
df_cbond_y[sample_period, "dl_yield_real_NA"] %>% plot

df_cbond_q[sample_period, "yield_real"] %>% mean(na.rm = TRUE)
df_cbond_y[sample_period, "yield_real"] %>% mean(na.rm = TRUE)



## ACF and PACF 
acf2(df_cbond_q[sample_period, "dl_yield_real"]) # 
acf2(df_cbond_y[sample_period, "dl_yield_real"]) #


## ARIMA modeling

# Quarterly data
sample_period <- "1950/"
# Auto ARIMA
auto.arima(df_cbond_q["1927/2014",  "l_yield_real_NA"], max.d = 1, seasonal = FALSE)  # ARIMA(0, 1, 0) starting from 1927 ?
auto.arima(df_cbond_q["1950/2014",  "l_yield_real_NA"], max.d = 1, seasonal = FALSE)  # ARIMA(0, 1, 0) starting from 1950 ?


# Try manually
# assume log differenced interest yield is stationary 
sarima(df_cbond_q["1927/" , "l_yield_real_NA"], 0, 1, 0) # Not so good

sarima(df_cbond_q["1950/" , "dl_yield_real_NA"], 1, 0, 0) # higher orders are needed for quarterly data
sarima(df_cbond_q["1950/" , "dl_yield_real_NA"], 4, 0, 0) # 

# residuals do not look normally distributed


# annual data: 
sample_period <- "1927/"
# Auto ARIMA: log inflation index
auto.arima(df_cbond_y["1927/2014", "l_yield_real_NA"], max.d = 1, seasonal = FALSE) # ARIMA(1, 0, 2)
auto.arima(df_cbond_y["1951/2014", "l_yield_real_NA"], max.d = 1, seasonal = FALSE) # ARIMA(1, 1,  0)
auto.arima(df_cbond_y["1951/2014", "l_yield_real_NA"], max.d = 0, seasonal = FALSE) # ARIMA(2, 0,  0)


# Try manually: inflation rate
sarima(df_cbond_y["1927/2014" , "l_yield_real_NA"], 1, 0, 1) #
sarima(df_cbond_y["1951/2014" , "l_yield_real_NA"], 1, 0, 1) # 


## Wilkie model for real part: log yield modeled as AR(1) (reduced model)

mod_cbond_real <- Arima(df_cbond_y["1927/1989", "l_yield_real_NA"], order = c(1, 0, 0))
mod_cbond_real
mod_cbond_real$coef[2] %>% exp; mod_cbond_real$sigma2^0.5 


mod_cbond_real <- Arima(df_cbond_y["1926/2014", "l_yield_real_NA"], order = c(1, 0, 0))
mod_cbond_real
mod_cbond_real$coef[2] %>% exp; mod_cbond_real$sigma2^0.5 

mod_cbond_real <- Arima(df_cbond_y["1951/2014", "l_yield_real_NA"], order = c(1, 0, 0))
mod_cbond_real
mod_cbond_real$coef[2] %>% exp; mod_cbond_real$sigma2^0.5 

mod_cbond_real <- Arima(df_cbond_y["1991/2014", "l_yield_real_NA"], order = c(1, 0, 0))
mod_cbond_real
mod_cbond_real$coef[2] %>% exp; mod_cbond_real$sigma2^0.5 # much lower than the estimates in HSZ2016



#**********************************************************************
#                     Try Simulatio: Inflation                   ####
#**********************************************************************


# Models:
 #1a. AR(1) of inflation rate on 1951-2014
 #1b. AR(1) of inflation rate on 1991-2014
 #2a. HSZ2016 (Hardy, Saunders, Zhang, 2016) estimates on 1951-2014
 #2b. HSZ2016 (Hardy, Saunders, Zhang, 2016) estimates on 1991-2014

mdl_infl_y1a <- Arima(df_inflation_y["1951/2014" , "dl_inflation"],  c(1, 0, 0)) # AR(1)
mdl_infl_y1b <- Arima(df_inflation_y["1991/2014" , "dl_inflation"],  c(1, 0, 0)) # AR(1)

mdl_infl_y1a
mdl_infl_y1b


 # HSZ2016 estimate on 1951-2014
mdl_infl_y2a <- mdl_infl_y1a
mdl_infl_y2a$coef   <- c(ar1 = 0.7575, intercept = 0.0338)
mdl_infl_y2a$sigma2 <- 0.0171^2 # 0.00029241

# HSZ2016 estimate on 1991-2014
mdl_infl_y2b <- mdl_infl_y1a
mdl_infl_y2b$coef   <- c(ar1 = 0, intercept = 0.0244 )
mdl_infl_y2b$sigma2 <- 0.0111^2 # 0.00035721


# Compare estimates
extract_est <- function(fit, mdl){
	#names_col <- c(names(mdl_infl_y2b$coef), "sigma")
	est <- c(fit$coef, fit$sigma2^0.5) %>% matrix( nrow = 1) %>% as.data.frame
	names(est) <- c(names(fit$coef), "sigma")
	est$mdl <- mdl 
	est %<>% select(mdl, everything()) 
	est
}

bind_rows(
	extract_est(mdl_infl_y1a, "y1a"),
	extract_est(mdl_infl_y1b, "y1b"), # AR1 estimate is -0.1011, forced to 0 in simulation
	extract_est(mdl_infl_y2a, "y2a"),
	extract_est(mdl_infl_y2b, "y2b")
) %>% kable(digits = 4)



# Check simulated series
Arima(simulate(mdl_infl_y2a, 100), order = c(1, 0, 0))
Arima(simulate(mdl_infl_y2b, 100), order = c(0, 0, 0))
# OK

# Simulation
nsim <- 2000
nyear_sim <- 100

set.seed(1234); sim_infl_y1a <- replicate(nsim,simulate(mdl_infl_y1a, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_y1b <- replicate(nsim,simulate(mdl_infl_y1b, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_y2a <- replicate(nsim,simulate(mdl_infl_y2a, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_infl_y2b <- replicate(nsim,simulate(mdl_infl_y2b, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))

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
		coord_cartesian(ylim = c(0, 0.08)) + 
		scale_y_continuous(breaks = seq(0, 1, 0.01))
}

fig_infl_y1a <- fn_plot(sim_infl_y1a)
fig_infl_y1b <- fn_plot(sim_infl_y1b)
fig_infl_y2a <- fn_plot(sim_infl_y2a)
fig_infl_y2b <- fn_plot(sim_infl_y2b)

fig_infl_y1a
fig_infl_y1b
fig_infl_y2a
fig_infl_y2b

sim_infl <- bind_rows(sim_infl_mdl_infl_y1a,
											sim_infl_mdl_infl_y1b,
											sim_infl_mdl_infl_y2a,
											sim_infl_mdl_infl_y2b)

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
				 mdl = factor(mdl, labels = c("AR(1) 1951-2014", "AR(1) 1991-2014", 
				 														  "AR(1) HSZ 1951-2014", "AR(1) HSZ 1991-2014"))) %>% 
	ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
	facet_grid(. ~ mdl) +
	geom_line() +
	geom_point() +
	coord_cartesian(ylim = c(0, 0.08)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.01))

fig_infl

# Inflation sims based on more recent sample period (1991-2014) 
# have much lower mean and variance

# There are differences between our estimate and HSZ estimate, but the 
# the magnitude is not large. 



#**********************************************************************
#                   Try Simulation: Dividend Yield                    ####
#**********************************************************************

# Models
 # 1a. AR(1) of log div yield on 1951-1984, with inflation as regressor
 # 1b. AR(1) of log div yield on 1991-2014, with inflation as regressor
 # 2a. HSZ2016 par values: AR(1) of log div yield  on 1951-2014, with inflation as regressor
 # 2b. HSZ2016 par values: AR(1) of log div yield  on 1991-2014, with inflation as regressor


df_dividend_y %>% head

mdl_divY_y1a <- Arima(df_dividend_y["1951/2014", "l_LCap_DivY"], xreg = df_dividend_y["1951/2014", "dl_inflation"], order = c(1, 0, 0))
mdl_divY_y1a

mdl_divY_y1b <- Arima(df_dividend_y["1991/2014", "l_LCap_DivY"], xreg = df_dividend_y["1991/2014", "dl_inflation"], order = c(1, 0, 0))
mdl_divY_y1b

mdl_divY_y2a <- mdl_divY_y1a
mdl_divY_y2a$coef <- c(ar1 = 0.9582, intercept = log(0.0331), dl_inflation = 0.0504)
mdl_divY_y2a$sigma2 <- 0.131^2

mdl_divY_y2b <- mdl_divY_y1a
mdl_divY_y2b$coef <- c(ar1 = 0.9112, intercept = log(0.0252), dl_inflation = -4.5762 )
mdl_divY_y2b$sigma2 <- 0.1159^2


extract_est <- function(fit, mdl){
	#names_col <- c(names(mdl_infl_y2b$coef), "sigma")
	est <- c(fit$coef, fit$sigma2^0.5) %>% matrix( nrow = 1) %>% as.data.frame
	names(est) <- c(names(fit$coef), "sigma")
	est$mdl <- mdl 
	est %<>% select(mdl, everything()) 
	est
}

bind_rows(
	extract_est(mdl_divY_y1a, "y1a"),
	extract_est(mdl_divY_y1b, "y1b"), # AR1 estimate is -0.1011, forced to 0 in simulation
	extract_est(mdl_divY_y2a, "y2a"),
	extract_est(mdl_divY_y2b, "y2b")
) %>% kable(digits = 4)


# Notes
# 1. log dividend yield does not look stationary over time. 
# 2. Our estimates of AR term, intercept, and sd are not very different from HSZ
# 3. Estimation on inflation does not match HSZ well
#     -  0.62 vs 0.05 for 1951-2014
#     -  -3.4 vs -4.5 for 1991-2014 
# 4. inflation parameter for 1991-2014 is negative, which contradicts theoretical relationship between div yield and infl
# 5. The figure shows that inflation and dividend yield may have a long-term positive relationship, which we may want
#    to capture in the model. Perhaps transfer model is not a appropriate way to model inflation and div yield jointly?

# For simulation:
  # Because of the unreasonable sign of estimated parameter on inflation, 
  # we should just use AR(1) for simulating dividend yield

# Simulation
nsim <- 2000
nyear_sim <- 100


mdl_divY_y1a_sim <- mdl_divY_y1a
mdl_divY_y1b_sim <- mdl_divY_y1b
mdl_divY_y2a_sim <- mdl_divY_y2a

mdl_divY_y1a_sim$coef["dl_inflation"] <- 0
mdl_divY_y1b_sim$coef["dl_inflation"] <- 0
mdl_divY_y2a_sim$coef["dl_inflation"] <- 0


set.seed(1234); sim_divY_y1a <- sapply(1:2000, function(x) {simulate(mdl_divY_y1a_sim, nyear_sim, xreg = sim_infl_y1a[, x], future = TRUE) %>% exp %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_divY_y1b <- sapply(1:2000, function(x) {simulate(mdl_divY_y1b_sim, nyear_sim, xreg = sim_infl_y1b[, x], future = TRUE) %>% exp %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))

set.seed(1234); sim_divY_y2a <- sapply(1:2000, function(x) {simulate(mdl_divY_y2a_sim, nyear_sim, xreg = sim_infl_y2a[, x], future = TRUE) %>% exp %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_divY_y2b <- sapply(1:2000, function(x) {simulate(mdl_divY_y2b, nyear_sim, xreg = sim_infl_y2b[, x], future = TRUE) %>% exp %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))


sim_divY <- bind_rows(sim_divY_y1a,
											sim_divY_y1b,
											sim_divY_y2a,
											sim_divY_y2b)

df <- sim_divY %>% 
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
				 mdl = factor(mdl, labels = c("AR(1) 1951-2014",     "AR(1) 1991-2014", 
				 														  "AR(1) HSZ 1951-2014", "AR(1) HSZ 1991-2014"))) %>% 
	ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
	facet_grid(. ~ mdl) +
	geom_line() +
	geom_point() +
	coord_cartesian(ylim = c(0, 0.08)) + 
	scale_y_continuous(breaks = seq(0, 1, 0.01))

fig_infl


# set.seed(1234); sim_infl_y1a <- replicate(nsim,simulate(mdl_infl_y1a, nyear_sim, future = TRUE)) %>% as.tibble() %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))



#**********************************************************************
#                   Try Simulation: Dividend growth                ####
#**********************************************************************

# Models
 # 1a. MA(1) of dividend growth on 1951-1984, with    div yield shock as regressor 
 # 1b. MA(1) of dividend growth on 1991-2014, without div yield shock as regressor
 # 2a. HSZ2016 par values for 1a
 # 2b. HSZ2016 par values for 1b

mdl_divY_y1a$residuals
mdl_divY_y1b$residuals


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

# regressors 
infl_ewa_1a <- df_dividend_y["1951/2014", "dl_inflation_NA_ewa"]
divY_res_1a <- xts(mdl_divY_y1a$residuals, index(infl_ewa)) %>% lag; colnames(divY_res_1a) <- "divY_res"
xreg_1a <- cbind(infl_ewa_1a, divY_res_1a)[-1, ]

infl_ewa_1b <- df_dividend_y["1991/2014", "dl_inflation_NA_ewa"]
divY_res_1b <- xts(mdl_divY_y1b$residuals, index(infl_ewa_1b)) %>% lag; colnames(divY_res_1b) <- "divY_res"
xreg_1b <- cbind(infl_ewa_1b, divY_res_1b)[-1, ]


# Wilkie specification: MA(1) + regressors 

mdl_divI_y1a <- Arima(df_dividend_y["1952/2014", "dl_LCap_DivI"], xreg = xreg_1a, order = c(0, 0, 1),
										fixed = c(NA, NA, wd, NA))
mdl_divI_y1a; mdl_divI_y1a$sigma2^0.5

mdl_divI_y1b <- Arima(df_dividend_y["1992/2014", "dl_LCap_DivI"], xreg = xreg_1a["1992/2014"], order = c(0, 0, 1),
											fixed = c(NA, NA, wd, NA))
mdl_divI_y1b; mdl_divI_y1b$sigma2^0.5



mdl_divI_y2a <- mdl_divI_y1a
mdl_divI_y2a$coef <- c(ma1 = -.3468, intercept = 0.0202 , dl_inflation_NA_ewa = 1,  divY_res = 0.2138 )
mdl_divI_y2a$sigma2 <- 0.1146^2

mdl_divI_y2b <- mdl_divI_y1b
mdl_divI_y2b$coef <- c(ma1 = 0.1215, intercept = 0.0306 , dl_inflation_NA_ewa = 1,  divY_res = 0.1567)
mdl_divI_y2b$sigma2 <- 0.1097^2

extract_est <- function(fit, mdl){
	#names_col <- c(names(mdl_infl_y2b$coef), "sigma")
	est <- c(fit$coef, fit$sigma2^0.5) %>% matrix( nrow = 1) %>% as.data.frame
	names(est) <- c(names(fit$coef), "sigma")
	est$mdl <- mdl 
	est %<>% select(mdl, everything()) 
	est
}

bind_rows(
	extract_est(mdl_divI_y1a, "y1a"),
	extract_est(mdl_divI_y2a, "y2a"),
	extract_est(mdl_divI_y1b, "y1b"), 
	extract_est(mdl_divI_y2b, "y2b")
) %>% kable(digits = 4)

# Notes
  # In HSZ2016, all coefficients except for variance are set to 0 for simulation based on 1991-2014
  # Our estimates on 1991-2014 are very different from HSZ2016. But all are insignificant as in HSZ2016 
  # Dividend growth is much more volatile than other variables. 



# Simulation
nsim <- 2000
nyear_sim <- 100

mdl_divI_y1a_sim <- mdl_divI_y1a
mdl_divI_y1b_sim <- mdl_divI_y1b
mdl_divI_y2a_sim <- mdl_divI_y2a
mdl_divI_y2b_sim <- mdl_divI_y2b

#mdl_divI_y1a_sim$coef["divY_res"] <- 0
#mdl_divI_y1b_sim$coef["divY_res"] <- 0
#mdl_divI_y2a_sim$coef["divY_res"] <- 0

set.seed(1234); sim_divI_y1a <- sapply(seq_len(nsim), function(x) {simulate(mdl_divI_y1a_sim, nyear_sim, xreg = cbind(sim_infl_y1a[,x], sim_divY_y1a[,x]), future = TRUE) %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_divI_y1b <- sapply(seq_len(nsim), function(x) {simulate(mdl_divI_y1b_sim, nyear_sim, xreg = cbind(sim_infl_y1b[,x], sim_divY_y1b[,x]), future = TRUE) %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))
set.seed(1234); sim_divI_y2a <- sapply(seq_len(nsim), function(x) {simulate(mdl_divI_y2a_sim, nyear_sim, xreg = cbind(sim_infl_y2a[,x], sim_divY_y2a[,x]), future = TRUE) %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_divI_y2b <- sapply(seq_len(nsim), function(x) {simulate(mdl_divI_y2b_sim, nyear_sim, xreg = cbind(sim_infl_y2b[,x], sim_divY_y2b[,x]), future = TRUE) %>% as.numeric}) %>% as.tibble %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))


sim_divI <- bind_rows(sim_divI_y1a,
											sim_divI_y1b,
											sim_divI_y2a,
											sim_divI_y2b)

df <- sim_divI %>% 
	gather(sim, value, -year, -mdl) %>% 
	group_by(year, mdl) %>% 
	summarize(
		q10 = quantile(value, 0.10),
		q25 = quantile(value, 0.25),
		q50 = quantile(value, 0.50),
		q75 = quantile(value, 0.75),
		q90 = quantile(value, 0.90))
# df %>% head()

fig_divI <- 
	df %>% gather(percentile, value, -year, -mdl) %>% 
	mutate(percentile = as.factor(percentile) %>% fct_rev,
				 mdl = factor(mdl, labels = c("MA(1) 1951-2014",     "MA(1) 1991-2014", 
				 														  "MA(1) HSZ 1951-2014", "MA(1) HSZ 1991-2014"))) %>% 
	ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
	facet_grid(. ~ mdl) +
	geom_line() +
	geom_point() +
	#coord_cartesian(ylim = c(0, 0.08)) + 
	scale_y_continuous(breaks = seq(0-.5, 0.5, 0.05))

fig_divI


#**********************************************************************
#                   Try Simulation: stock returns                  ####
#**********************************************************************

# Construct simulations of stock returns from dividend yield and dividend growth

#total return(t) = (price(t) + div(t)) / price(t-1)
#                = (D(t)/Y(t) + D(t)) / (D(t - 1) / Y(t-1))

sim_div_Igrowth <- sim_divI_y1b
sim_div_yield   <- sim_divY_y1b


# recover dividend index from dividend growth (assuming dividend at t = 0 is 1)
sim_div <- sim_div_Igrowth[, seq_len(nsim)] %>% as.tibble %>% 
		mutate_all(funs(cumprod(1+.))) %>% as.matrix

# recover stock price and lagged price from dividend index and yield
sim_eqtprice     <- sim_div / as.matrix(sim_div_yield[, seq_len(nsim)])
sim_eqtprice_lag <- sim_eqtprice %>% as.tibble() %>% mutate_all(funs(lag)) %>% as.matrix

# compute stock return from price and dividend index
sim_eqtreturn    <- (sim_eqtprice + sim_div) / sim_eqtprice_lag - 1

sim_eqtreturn %>% as.tibble %>% head

sim_eqtprice_lag  %>% as.tibble %>% head
sim_eqtprice      %>% as.tibble %>% head
sim_div           %>% as.tibble %>% head

# Examine stock returns
sim_eqtreturn[-1, ] %>% mean
sim_eqtreturn[-1, ] %>% sd


#**********************************************************************
#                  Try simulation Simulation: Corp bond yield                   ####
#**********************************************************************

# Models to estimate
# 1a. AR(1) of real cbond yield on 1951-1984, with div yield shock as regressor (regressor may be excluded in simulation)
# 1b. AR(1) of real cbond yield on 1991-2014, with div yield shock as regressor (regressor may be excluded in simulation)
# 2a. HSZ2016 par values for 1a
# 2b. HSZ2016 par values for 1b

# Regressors 
divY_res_1a <- xts(mdl_divY_y1a$residuals, index(infl_ewa)); colnames(divY_res_1a) <- "divY_res"
xreg_1a     <- divY_res_1a

divY_res_1b <- xts(mdl_divY_y1b$residuals, index(infl_ewa_1b)); colnames(divY_res_1b) <- "divY_res"
xreg_1b     <- divY_res_1b


# Wilkie model

mdl_cbond_real_y1a <- Arima(df_cbond_y["1951/2014", "l_yield_real"], xreg = xreg_1a,  order = c(1, 0, 0))
mdl_cbond_real_y1a
mdl_cbond_real_y1a$coef[2] %>% exp; mdl_cbond_real_y1a$sigma2^0.5 

mdl_cbond_real_y1b <- Arima(df_cbond_y["1991/2014", "l_yield_real"], xreg = xreg_1b,  order = c(1, 0, 0))
mdl_cbond_real_y1b
mdl_cbond_real_y1b$coef[2] %>% exp; mdl_cbond_real_y1b$sigma2^0.5 


mdl_cbond_real_y2a <- mdl_cbond_real_y1a
mdl_cbond_real_y2a$coef <- c(ar1 = 0.9109, intercept = log(0.0213) , divY_res = -0.1958)
mdl_cbond_real_y2a$sigma2 <- 0.3157^2

mdl_cbond_real_y2b <- mdl_cbond_real_y1b
mdl_cbond_real_y2b$coef <- c(ar1 = 0.8020, intercept = log(0.0338), divY_res = -0.2785)
mdl_cbond_real_y2b$sigma2 <- 0.2538^2

extract_est <- function(fit, mdl){
	#names_col <- c(names(mdl_infl_y2b$coef), "sigma")
	est <- c(fit$coef, fit$sigma2^0.5) %>% matrix( nrow = 1) %>% as.data.frame
	names(est) <- c(names(fit$coef), "sigma")
	est$mdl <- mdl 
	est %<>% select(mdl, everything()) 
	est
}

bind_rows(
	extract_est(mdl_cbond_real_y1a, "y1a"),
	extract_est(mdl_cbond_real_y2a, "y2a"),
	extract_est(mdl_cbond_real_y1b, "y1b"), 
	extract_est(mdl_cbond_real_y2b, "y2b")
) %>% kable(digits = 4)

# Notes
 # For 1951-2014, our estimation is close to HSZ except for div yield residuals (but it is insignificant).
 # For 1991-2014, the difference is relatively large. 
 # For simulation: suggest using model without div yield residuals as regressor



# Simulation
nsim <- 2000
nyear_sim <- 100

mdl_cbond_real_y1a_sim <- mdl_cbond_real_y1a
mdl_cbond_real_y1b_sim <- mdl_cbond_real_y1b
mdl_cbond_real_y2a_sim <- mdl_cbond_real_y2a
mdl_cbond_real_y2b_sim <- mdl_cbond_real_y2b

mdl_cbond_real_y1a_sim$coef["divY_res"] <- 0
mdl_cbond_real_y1b_sim$coef["divY_res"] <- 0
mdl_cbond_real_y2a_sim$coef["divY_res"] <- 0
mdl_cbond_real_y2b_sim$coef["divY_res"] <- 0

set.seed(1234); sim_cbond_real_y1a <- sapply(seq_len(nsim), function(x) {simulate(mdl_cbond_real_y1a_sim, nyear_sim, xreg = sim_divY_y1a[,x], future = TRUE) %>% as.numeric}) %>% exp %>%  as.tibble %>% mutate(mdl = "y1a", year = seq_len(nyear_sim))
set.seed(1234); sim_cbond_real_y1b <- sapply(seq_len(nsim), function(x) {simulate(mdl_cbond_real_y1b_sim, nyear_sim, xreg = sim_divY_y1b[,x], future = TRUE) %>% as.numeric}) %>% exp %>%  as.tibble %>% mutate(mdl = "y1b", year = seq_len(nyear_sim))
set.seed(1234); sim_cbond_real_y2a <- sapply(seq_len(nsim), function(x) {simulate(mdl_cbond_real_y2a_sim, nyear_sim, xreg = sim_divY_y2a[,x], future = TRUE) %>% as.numeric}) %>% exp %>%  as.tibble %>% mutate(mdl = "y2a", year = seq_len(nyear_sim))
set.seed(1234); sim_cbond_real_y2b <- sapply(seq_len(nsim), function(x) {simulate(mdl_cbond_real_y2b_sim, nyear_sim, xreg = sim_divY_y2b[,x], future = TRUE) %>% as.numeric}) %>% exp %>%  as.tibble %>% mutate(mdl = "y2b", year = seq_len(nyear_sim))


sim_cbond_real <- bind_rows(sim_cbond_real_y1a,
											      sim_cbond_real_y1b,
											      sim_cbond_real_y2a,
											      sim_cbond_real_y2b)

df <- sim_cbond_real %>% 
	gather(sim, value, -year, -mdl) %>% 
	group_by(year, mdl) %>% 
	summarize(
		q10 = quantile(value, 0.10),
		q25 = quantile(value, 0.25),
		q50 = quantile(value, 0.50),
		q75 = quantile(value, 0.75),
		q90 = quantile(value, 0.90))
# df %>% head()

fig_cbond_real <- 
	df %>% gather(percentile, value, -year, -mdl) %>% 
	mutate(percentile = as.factor(percentile) %>% fct_rev
				 # mdl = factor(mdl, labels = c("MA(1) 1951-2014",     "MA(1) 1991-2014", 
				 #														  "MA(1) HSZ 1951-2014", "MA(1) HSZ 1991-2014")
         ) %>% 
	ggplot(aes(x = year, y = value, color = percentile)) + theme_bw() + 
	facet_grid(. ~ mdl) +
	geom_line() +
	geom_point() +
	#coord_cartesian(ylim = c(0, 0.08)) + 
	scale_y_continuous(breaks = seq(-0.5, 0.5, 0.01))


fig_cbond_real






#**********************************************************************
#            Plotting and validating data         ####
#**********************************************************************

# Produce plots similar to fig 2 and fig 3 in HSZ2016 p10, and compare plots

df <- left_join(df_dividend_y %>% as.data.frame %>% select(year, dl_inflation, dl_inflation_NA, LCap_DivY, dl_LCap_DivI),
								df_cbond_y    %>% as.data.frame %>% select(year, CBond_Yield_AAA, yield_real_NA)) %>% 
			left_join(df_GDP_y      %>% as.data.frame %>% select(year, dl_GDP))

# fig2
df %>% 
	select(year, dl_inflation_NA, LCap_DivY, CBond_Yield_AAA, dl_GDP) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05))+
	scale_x_continuous(breaks = seq(1930, 2015, 10))

	
# fig3
df %>% 
	select(year, dl_inflation_NA, LCap_DivY, dl_LCap_DivI) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.8, 0.6, 0.1)) +
	scale_x_continuous(breaks = seq(1930, 2015, 10))

cor(
df_inflation_y["1950/2015", "dl_inflation"],
df_GDP_y["1950/2015", "dl_GDP"])



df %>% 
	select(year, dl_inflation_NA, LCap_DivY) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05))+
	scale_x_continuous(breaks = seq(1930, 2015, 10))




