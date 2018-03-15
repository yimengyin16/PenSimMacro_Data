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
library(MSwM)
#library(MSBVAR)

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

# NBER recession periods, post-WWII
recessionPeriods <- 
  matrix(c(
  	1953+2/4, 1954+2/4,
  	1957+3/4, 1958+2/4,
  	1960+2/4, 1961+1/4,
  	1969+4/4, 1970+4/4,
  	1973+4/4, 1975+1/4,
  	1980+1/4, 1980+3/4,
  	1981+3/4, 1982+4/4,
  	1990+3/4, 1991+1/4,
  	2001+1/4, 2001+4/4,
  	2007+4/4, 2009+2/4
  ) , ncol = 2, byrow = T) %>% 
  	as.data.frame() %>% 
	rename(peak =   V1,
				 trough = V2) %>% 
	mutate(peak = peak - 1/4,
				 trough = trough - 1/4)

recessionPeriods





#**********************************************************************
#                     Loading Data                                 ####
#**********************************************************************
# Loading saved data 
load(paste0(dir_data_out, "dataAll.RData"))



#**********************************************************************
#               Plotting stock returns and volatility    ####
#**********************************************************************

# Stock return and volatility with different data frequencies

# Stock returns:
  # log total return (price appreciation + dividend)
  # Equity premium calculated with different measures of risk free rate
# Frequencies:
  # 1. Monthly
  # 2. Quarterly
  # 3. Yearly

get_logReturn <- function(x){
	if(any(x <= 0, na.rm = TRUE)) stop("Nagative value(s)")
	log(x/lag(x))
}

Vars <- c("year", "month", "yearMon", 
					"TBill3m_FRED", 
					"Tbond10y_FRED",
					"LCapStock_TRI",
					"LCapStock_CAI",
					"CBond_TRI",
					"LTGBond_TRI",
					"Inflation_Index",
					"GDP_FRED")


fn <- function(df, year_range, rolling_width, freq){
	df_stock_m <- 
		df %>% 
		select(one_of(Vars)) %>% 
		filter(year %in% year_range) %>% 
		mutate(return_tot = (1 + get_logReturn(LCapStock_TRI))^freq - 1,
					 dl_gdp     = (1 + get_logReturn(GDP_FRED))^freq - 1,
					 dl_cbond    = (1 + get_logReturn(CBond_TRI))^freq - 1,
					 dl_gbond    = (1 + get_logReturn(LTGBond_TRI))^freq - 1,
					 
					 return_tot_o = get_logReturn(LCapStock_TRI),
					 dl_gdp_o     = get_logReturn(GDP_FRED),
					 dl_cbond_o   = get_logReturn(CBond_TRI),
					 dl_gbond_o   = get_logReturn(LTGBond_TRI),
					 
					 TBill3m_FRED =  TBill3m_FRED/100,
					 Tbond10y_FRED = Tbond10y_FRED/100,
					 ERP_3m     = return_tot - TBill3m_FRED,
					 ERP_10y    = return_tot - Tbond10y_FRED) %>% 
		mutate(sd_return    = rollapply(return_tot, rolling_width, sd,   align = "right", fill = NA),
					 mean_return  = rollapply(return_tot, rolling_width, mean, align = "right", fill = NA),
					 sd_ERP_3m    = rollapply(ERP_3m, rolling_width, sd,    align = "right", fill = NA),
					 mean_ERP_3m  = rollapply(ERP_3m, rolling_width, mean,  align = "right", fill = NA),
					 sd_ERP_10y   = rollapply(ERP_10y, rolling_width, sd,   align = "right", fill = NA),
					 mean_ERP_10y = rollapply(ERP_10y, rolling_width, mean, align = "right", fill = NA),
					 sd_cbond     = rollapply(dl_cbond, rolling_width, sd,   align = "right", fill = NA),
					 mean_cbond   = rollapply(dl_cbond, rolling_width, mean, align = "right", fill = NA),
					 sd_gbond     = rollapply(dl_gbond, rolling_width, sd,   align = "right", fill = NA),
					 mean_gbond   = rollapply(dl_gbond, rolling_width, mean, align = "right", fill = NA),
					 mean_gdp     = rollapply(dl_gdp, rolling_width, mean,  align = "right", fill = NA)
		)
}

df_stock_m <- fn(df_dataAll,   1953:2015, 12, 12)
df_stock_q <- fn(df_dataAll_q, 1953:2015, 12, 4)
df_stock_y <- fn(df_dataAll_y, 1953:2015, 5,  1)

# total return and ERPs
# monthly
df_stock_m %>% 
	select(yearMon, return_tot, ERP_3m) %>% 
	gather(var,value, -yearMon) %>% 
	ggplot(aes(x = yearMon , y = value, color = var)) + theme_bw()+
	geom_line()

# quarterly 
df_stock_q %>% 
	select(yearMon, return_tot, ERP_3m) %>% 
	gather(var,value, -yearMon) %>% 
	ggplot(aes(x = yearMon , y = value, color = var)) + theme_bw()+
	geom_line()

# annual
df_stock_y %>% 
	select(yearMon, return_tot, ERP_3m) %>% 
	gather(var,value, -yearMon) %>% 
	ggplot(aes(x = yearMon , y = value, color = var)) + theme_bw()+
	geom_line()



# total return and volatility over time
fn_fig <- function(df){
	fig <- 
	df %>% 
		select(yearMon, 
					 sd_return,  mean_return,
					 sd_ERP_10y, mean_ERP_10y) %>% 
		gather(var, value, -yearMon) %>% 
		mutate(type = case_when(
			str_detect(var, "return") ~ "return",
			str_detect(var, "ERP_3m") ~ "ERP_3m",
			str_detect(var, "ERP_10y") ~ "ERP_10y",
			TRUE ~ ""
		),
		Stat = case_when(
			str_detect(var, "sd") ~ "sd",
			str_detect(var, "mean") ~ "mean")) %>% 
		ggplot() + theme_bw()+ 
		facet_grid(type~.)+
		geom_line(aes(x = yearMon , y = value, color = Stat)) +
		geom_rect(data = recessionPeriods, 
							aes(xmin = peak, xmax = trough, 
									ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
		scale_x_continuous(breaks = seq(1950, 2020, 5))
	  fig
}

df_stock_m %>% fn_fig  # sd: 12 months rolling
df_stock_q %>% fn_fig  # sd: 12 quarters rolling
df_stock_y %>% fn_fig  # sd: 5 years rolling


# GDP, stock return and volatiltiy
{df_stock_q %>% 
	select(yearMon,
				 sd_ERP_10y, 
				 mean_ERP_10y,
				 dl_gdp) %>% 
	gather(var, value, -yearMon) %>% 
	ggplot() + theme_bw()+ 
	#facet_grid(type~.) +
	geom_line(aes(x = yearMon , y = value, color = var)) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1))+ 
  coord_cartesian(ylim = c(-0.2, 0.6))}


# bond return and volatility
df_stock_q %>% 
	select(yearMon,
				 mean_cbond,
				 sd_cbond) %>% 
	gather(var, value, -yearMon) %>% 
	ggplot() + theme_bw() + 
	geom_line(aes(x = yearMon , y = value, color = var)) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1)) + 
	coord_cartesian(ylim = c(-0.2, 0.6))

# bond return and ERP 
df_stock_q %>% 
	select(yearMon,
				 mean_cbond,
				 mean_ERP_10y) %>% 
	gather(var, value, -yearMon) %>% 
	ggplot() + theme_bw() + 
	geom_line(aes(x = yearMon , y = value, color = var)) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1)) + 
	coord_cartesian(ylim = c(-0.2, 0.6))

# bond and ERP volatility 
df_stock_q %>% 
	select(yearMon,
				 mean_cbond,
				 mean_ERP_10y) %>% 
	gather(var, value, -yearMon) %>% 
	ggplot() + theme_bw() + 
	geom_line(aes(x = yearMon , y = value, color = var)) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1)) + 
	coord_cartesian(ylim = c(-0.2, 0.6))

# correlation over time, 



#**********************************************************************
#                 Regime switching model ####
#**********************************************************************
## Replicate results in Hardy2001
 # Not clear about the sample period in Hardy2001, use total return in 1954-2000
mod_hardy <- msmFit(return_tot_o ~ 1, data = df_stock_m %>% filter(year %in% 1953:2000), k = 2, sw = c(T, T))
summary(mod_hardy)

mod_hardy@transMat
mod_hardy@Coef
mod_hardy@seCoef
mod_hardy@std

#         Hardy      model  
# mean1   0.0126     0.0126
# mean2   -0.0185    -0.025   # insignificant
# sd1     0.035      0.036
# sd2     0.0748     0.078
# p12     0.0398     0.0287
# p21     0.3798     0.395

# Notes:
 # Estimates are generally consistent. 
 # The only large difference is in p12 (prob of trans from low-vol to high-vol) 


## Monthly, quarterly, and yearly models, total return 
mod_m <- msmFit(return_tot_o ~ 1, data = df_stock_m %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_m)
plotProb(mod_m)

mod_q <- msmFit(return_tot_o ~ 1, data = df_stock_q %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_q)
plotProb(mod_q)
mod_q

mod_y <- msmFit(return_tot ~ 1, data = df_stock_y %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_y)
plotProb(mod_y)

# Notes
# Mean and sd estimates generally make sense.
# Transition probabilities differ across models with different frequencies; 
# need to further verify the implied distribution of regimes are consistent.
# Low-return, high volatility regimes captured:
# monthly:  
# quarterly: 
# annual:    
# Need to further examine how these high-vol regimes are aligned with NBER ressessions and model-implied GDP regimes. 


## Models of ERP, annual data
 # Notes:
 #  gbond rates are annual, need to appropriately convert to quarterly and monthly rates. 

mod_ERP3m_y <- msmFit(ERP_3m ~ 1, data = df_stock_y %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_ERP3m_y)
plotProb(mod_ERP3m_y)

mod_ERP10y_y <- msmFit(ERP_10y ~ 1, data = df_stock_y %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_ERP10y_y)
plotProb(mod_ERP10y_y)

# Notes
 # Generally consistent with total return models
 # have shorter high vol regimes (higher prob of getting of of high-vol regimes), which is more consistent with NBER recessions
 # but also creates more high volatility regimes that are non aligned with recessions.  


## High volatility regimes and NBER recession periods

#Quarterly
index_q <- (df_stock_q %>% filter(year %in% 1954:2015))[, "yearMon"]
regime_q <- 
	mod_q@Fit@smoProb[-1,] %>% as.data.frame %>% 
	rename(prob_lVol = V2,
				 prob_hVol = V1)
regime_q <- bind_cols(yearMon = index_q, regime_q)

{left_join(df_stock_q, regime_q) %>% 
	select(yearMon,
				 prob_hVol) %>% 
	#gather(var, value, -yearMon) %>% 
	ggplot() + theme_bw()+ 
	#facet_grid(type~.) +
	geom_line(aes(x = yearMon , y = prob_hVol)) +
	geom_hline(yintercept = 0.5) + 
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1))+ 
	coord_cartesian(ylim = c(0, 1))}

# Annual
index_y <- (df_stock_y %>% filter(year %in% 1954:2015))[, "yearMon"]
regime_y <- 
	mod_ERP10y_y@Fit@smoProb[-1, ] %>% as.data.frame %>% 
	rename(prob_lVol = V2,
				 prob_hVol = V1)
regime_y <- bind_cols(yearMon = index_y, regime_y)

{left_join(df_stock_y, regime_y) %>% 
		select(yearMon,
					 prob_hVol) %>% 
		#gather(var, value, -yearMon) %>% 
		ggplot() + theme_bw()+ 
		#facet_grid(type~.) +
		geom_line(aes(x = yearMon , y = prob_hVol)) +
		geom_hline(yintercept = 0.5) + 
		geom_rect(data = recessionPeriods, 
							aes(xmin = peak, xmax = trough, 
									ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
		scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
		scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1))+ 
		coord_cartesian(ylim = c(0, 1))}

# Monthly
index_m <- (df_stock_m %>% filter(year %in% 1954:2015))[, "yearMon"]
regime_m <- 
	mod_m@Fit@smoProb[-1, ] %>% as.data.frame %>% 
	rename(prob_lVol = V2,
				 prob_hVol = V1)
regime_m <- bind_cols(yearMon = index_m, regime_m)

{left_join(df_stock_m, regime_m) %>% 
		select(yearMon,
					 prob_hVol) %>% 
		#gather(var, value, -yearMon) %>% 
		ggplot() + theme_bw()+ 
		#facet_grid(type~.) +
		geom_line(aes(x = yearMon , y = prob_hVol)) +
		geom_hline(yintercept = 0.5) + 
		geom_rect(data = recessionPeriods, 
							aes(xmin = peak, xmax = trough, 
									ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
		scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
		scale_y_continuous(breaks = seq(-0.5, 1.5, 0.1))+ 
		coord_cartesian(ylim = c(0, 1))}

# monthly model has 13 high volatility regimes
#   captures 7 of 9 NBER recessions, misses 2; 6 not aligned 
#   does not capture the 1981 recession well

# Quarterly model has 13 high volatility regimes
#   captures 7 of 9 NBER recessions, misses 2; 6 not aligned (all very short)

# Annual model has 7 high volatility regimes
#   captures 5 of 9 NBER recessions, misses 4; 2 not aligned

# Montly and quarterly models capture all major recessions, but have
# more high-vol regimes that are not aligned with official recessions periods.



# Regimes in GDP growth

sarima(df_stock_q$dl_gdp_o[-1], 1, 0, 0) # AR(1)
sarima(df_stock_y$dl_gdp_o[-1], 1, 0, 0) # random walk


mod_gdp_q <- msmFit(dl_gdp_o ~ 1, k = 2, p = 4, data = df_stock_q %>% filter(year %in% 1954:1990),  sw = c(T, F, F,F, F,F))
summary(mod_gdp_q)
plotProb(mod_gdp_q)

mod_gdp_y <- msmFit(dl_gdp_o ~ 1, k = 2, p = 0, data = df_stock_y %>% filter(year %in% 1954:2015),  sw = c(T, T))
summary(mod_gdp_y)
plotProb(mod_gdp_y)


df_stock_q$dl_gdp_o[-1] %>% plot(type = "l")

##! MSwM package cannot replicate hamilton1989 because of the treatment of intercept  

# Try MSBVAR package

data(HamiltonGDP)
HamiltonGDP %>% plot
set.seed(1)
m2 <- msvar(HamiltonGDP, p=1, h=2, niterblkopt=20)
fp.rec <- ts(m2$fp[,1], start=tsp(HamiltonGDP)[1], freq=tsp(HamiltonGDP)[3])
plot(fp.rec)

m2$fp
HamiltonGDP


set.seed(214)
m2 <- msbvar(HamiltonGDP, p=1, h=2,
						 lambda0=0.8, lambda1=0.15, lambda3=1, lambda4=0.25,
						 lambda5=1, mu5=0, mu6=0, qm=12,
						 alpha.prior=c(100, 30)*diag(2) +
						 	matrix(12, 2, 2), prior=0, max.iter=30,
						 initialize.opt=NULL)
# Now plot the filtered probabilities of a recession
# Compare to Kim and Nelson (1999: 79, 220)
fp.rec <- ts(m2$fp[,2], start=tsp(HamiltonGDP)[1],
						 freq=tsp(HamiltonGDP)[3])
plot(fp.rec)
m2$hreg


# Regimes in total fixed income returns, 


# How correlation between stock and bond change over time-














