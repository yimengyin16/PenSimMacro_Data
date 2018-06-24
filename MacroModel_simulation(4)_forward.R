# Simulating stock returns and bond returns based on GDP regimes


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************
# A discussion of ARIMA transfer function models:
# https://robjhyndman.com/hyndsight/arimax/


# Note
# Multivariate Normal
# https://www.r-bloggers.com/simulating-from-the-bivariate-normal-distribution-in-r/




## Issues:
  # Simulate ARIMA process with initial values and regressors

#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(markovchain) # Markov chain object
library(MASS)        # multivariate normal generator, must be loaded before tidyverse, otherwise 'select' will be masked


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
library(TTR)

#library(MSBVAR)


# packages for ts
library(zoo)
library(xts)


library(timetk)
library(tidyquant)

library(lubridate)
library(feather)

library(psych) # describe
library(xlsx)
library(qqplotr)

# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"
dir_outputs  <- "outputs_report/"
# dir_outputs <- "techReport_out"

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

get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1

RIG.theme <- function() {
	theme(
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(),
		panel.grid.minor.y = element_blank(),
		panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
		plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		plot.caption = element_text(hjust = 0, size = 9)
	)
}

RIG.themeLite <- function() {
	theme(
		plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		plot.caption = element_text(hjust = 0, size = 9)
	)
}



#**********************************************************************
#                     Loading Data                                 ####
#**********************************************************************
# Loading saved data 
load(paste0(dir_data_out, "dataAll.RData"))



#**********************************************************************
#               Data  ####
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


# save data in feather format for python use
write_feather(df_stock_q, "data_out/df_stock_q.feather" )
df_stock_q$dl_gdp_o



#***********************************************************************************
#                    0. Overview the simulation procedure          ####
#***********************************************************************************

#' Overview of the simplified modeling approach:
#' 
#' 1. GDP (quarterly, core variable):
#'    - Markov-switching model (AR or RW-drift)
#'    - Transition matrix that determines regime-switching behavior
#'    - Regime-dependent mean/std/parameters
#'         - mean/std based on NBER definition
#'         - mean/std based on stock MS model
#'         - mean/std based on GDP   MS model
#'    - Simulated regimes and paths of GDP growth
#' 2. Total stock returns (for now, we may want to model ERP later in order to incorporate inflation)
#'    - Markov-switching model: RW-drift
#'    - Return regimes are highly aligned with GDP regimes
#'    - For simulation: different mean return and std in GDP different GDP regimes
#' 3. Total cbond returns 
#'    - Show regime-switching behavior, but very different from those for GDP and stock returns.
#'       - high return and high volatility regime 
#'       - low return and low volatility regime
#'       - May not be pure random walk within regimes, may need to model MSAR process. 
#'    - Correlation with stock changes wildly over time, with no obivous pattern. 
#'    - Simulation stragegy:
#'       - 1. modeled and simulated as a seperate MS-AR/DR process, calibrate 
#'            correlation with stock to historical value/assumed value by setting
#'            the correlation between error terms of stock model and bond model.
#'       - 2. 
#' 4. Return of Portfolio
#'    - Construct portfolio returns   
#


#' Summary of quaterly stock mean return and std 
#' 
#' 1. Based on NBER recession periods 1953 - 2015
#'            mean       std
#' expansion  0.032     0.0686
#' recession -0.014     0.119  
#' 
#' 
#' 2. Based on MS model of stock return 1954-2015
#'            mean       std
#' expansion:  0.0419  0.0547
#' recession: -0.0192  0.1126
#' 
#' 
#' 3. Based on MS RW-drift model of GDP (regimes imported from python program)
#' 
#' - estimate seed 123 (?a local optimum, but consistent with hamilton1989)
#'              mean       std
#' expansion:  0.032   0.0685
#' recession:  0.0049  0.1044
#' 
#' - estimate seed 127 (?global optimum, matches historical length of recessions best)
#'             mean       std
#' expansion: 0.028    0.0723
#' recession: 0.0027   0.122
#' 



#' Comparing NBER and model recession/expansion period length  
#' 
#' US recessions NBER:
#' 	recession       expansion
#' Starting 1953:      3.7Q            20.2Q
#' Starting 1980       3.7Q            23.7Q
#' 
#' 
#' Model recessions
#'                   recession       expansion
#' MSAR4               1.6             19.9 
#' MSAR2               1.7             20.9      
#' MSAR1               1.8             20.4
#' RW-drift            4.4             11.4
#' RW-drift            3.1             20.7 
#' Hamilton1989        4.1             16.2


#' Summary  
#' 
#' - Simulated quarterly returns
#'    - Simulated mean and annual returns are reasonable and generally consistent with historical data
#'    - Skewness and kurtosis are still much smaller than historical  
#'    - Compare to normal: 
#'    
#'    
#' - Simulated annual returns
#'    - Simulated mean annual return and standard deviation looks reasonable
#'    - Kurtosis of historical kurtosis is surprisingly low 
#'    - compare to normal"
#'    
#' - Note: 
#'    - qarterly returns non-normal and fat tail
#'    - It seems that annual returns are normal?


#'Type of returns
#'  Frequency:
#'   1. quarterly
#'   2. annual
#'  How returns are generated:
#'   1. historical data
#'   2. simulations using gdp regimes
#'     2.1 mean/std from historical regimes
#'     2.2 mean/std from gdp model regimes
#'     2.3 mean/std from stock model regimes
#'     2.4 other scenarios
#'   3.Normal distribution
#'   
#'Measures and plots
#' mean
#' std
#' skewness
#' kurtosis
#' 1%, 5% percentiles
#' 
#' histogram
#' kernal smoothed curve
#' qqplot
#'   




#***********************************************************************************
#                 1. Global parameters for simulation                    ####
#***********************************************************************************

nyear <- 30
nsim  <- 2000


#***********************************************************************************
#             2. Defining the 2-state Markov chain for GDP             ####
#***********************************************************************************

# GDP regimes:
gdpStates <- c('0',  # 0: Expansion
							 '1')  # 1: recession


# Transition probabilities of GDP 
p01 <- 0.048 # Expansion to recession
p10 <- 0.32  # Recession to expansion 
p00 <- 1 - p01
p11 <- 1 - p10

gdpMatrix <- matrix(c(p00, p01,
											p10, p11), 
										 byrow = TRUE, 
										 nrow = 2) 
gdpMatrix


# Defining markovchain object
mc_gdp <- new("markovchain", 
							states = gdpStates,
							transitionMatrix = gdpMatrix,
							name = 'gdp'
							)

mc_gdp %>% plot
summary(mc_gdp)

# Try simulation with markov chain
rmarkovchain(n = 4*30, object = mc_gdp, t0 = '0')
replicate(10, rmarkovchain(n = 4*30, object = mc_gdp, t0 = '1')) 




#***********************************************************************************
#             3. Paramters of gdp growth, stock and bond returns         ####
#***********************************************************************************

# # Stock: expansion
# s.mean_0 <- 0.032
# s.std_0  <- 0.069 
# 
# # Stock: recession
# s.mean_1 <- -0.014
# s.std_1  <- 0.119
# 
# # Bond
# b.mean  <- 0.016 #0.035/4
# b.std   <- 0.051 #0.04/2
# 
# # Correlation between error terms(deviation from mean) of stock and bond returns
# # (Calibration needed)
# # Goal is to find a rho that can make the correlation between the simulated
# # annual stock returns and annual bond returns (both calculated from quarterly returns)
# # match the target value
# rho <- 0.15 
# 
# # GDP expansion 
# gdp.mean_0 <- 0.0095
# gdp.std_0  <- 5.463e-05^0.5 # 0.007391211
# 
# # GDP recession
# gdp.mean_1 <- -0.0055
# gdp.std_1  <- 5.463e-05^0.5 # 0.007391211



simInputs_historical <- 
	list(
		nyear = 30,
		nsim  = 2000,
		
		# Markov Chain object for GDP
		mc_gdp = mc_gdp,
		
		# Stock: expansion
		s.mean_0 = 0.032,
		s.std_0  = 0.069 ,
		
		# Stock: recession
		s.mean_1 = -0.014,
		s.std_1  = 0.119,
		
		# Bond
		b.mean  = 0.016, #0.035/4
		b.std   = 0.051, #0.04/2
		
		# Correlation between error terms(deviation from mean) of stock and bond returns
		# (Calibration needed)
		# Goal is to find a rho that can make the correlation between the simulated
		# annual stock returns and annual bond returns (both calculated from quarterly returns)
		# match the target value
		rho = 0.15, 
		
		# GDP expansion 
		gdp.mean_0 = 0.0095,
		gdp.std_0  = 5.463e-05^0.5, # 0.007391211
		
		# GDP recession
		gdp.mean_1 = -0.0055,
		gdp.std_1  = 5.463e-05^0.5 # 0.007391211
		
	)
	

simInputs_forward <- 
	list(
		nyear = 30,
		nsim  = 2000,
		
		# Markov Chain object for GDP
		mc_gdp = mc_gdp,
		
		# Stock: expansion
		s.mean_0 = 0.032 - 0.00656,  #  0.02544
		s.std_0  = 0.069 ,
		
		# Stock: recession
		s.mean_1 = -0.014 - 0.00656, # -0.0205
		s.std_1  = 0.119,
		
		# Bond
		b.mean  = 0.009086, # 
		b.std   = 0.02, #  0.04/2
		
		# Correlation between error terms(deviation from mean) of stock and bond returns
		# (Calibration needed)
		# Goal is to find a rho that can make the correlation between the simulated
		# annual stock returns and annual bond returns (both calculated from quarterly returns)
		# match the target value
		rho = 0.15, 
		
		# GDP expansion 
		gdp.mean_0 = 0.0095 - 0.002786, # 0.006714
		gdp.std_0  = 5.463e-05^0.5,     # 0.007391211
		
		# GDP recession
		gdp.mean_1 = -0.0055 - 0.002786, # -0.008286
		gdp.std_1  = 5.463e-05^0.5       # 0.007391211
	)

# GDP 
  # Target: 1.019242^0.25 - 1 = 0.004776
  # simulated based on historical data:0.007562
  # adjustment: 0.007562 - 0.004776 = 0.002786

# Stock:
  # Target:  1.08145^0.25 - 1 =  0.01976854
  # current: 1.1095593^0.25 - 1 = 0.02633
  # adjustment: 0.00656146


#***********************************************************************************
#     5. Simulating quarterly GDP growth and asset returns with regime-switching   ####
#***********************************************************************************

# simInputs <- simInputs_historical
# sim_name  <- "sim_results_historical"

simInputs <- simInputs_forward
sim_name  <- "MacroModel_sim_results_forward"




set.seed(11)
{
	
# Generating GDP regimes	
sim_gdp_regimes <- replicate(simInputs$nsim, rmarkovchain(n = simInputs$nyear * 4, object = simInputs$mc_gdp, t0 = '0') %>% as.numeric())

	
# Generating of stock and bond returns (approach 1)
{
 # Mean vector and covariance matrix 
 # (Assuming the correlation between bond and stock returns does not change over regimes)
# mu_0 <- c(s.mean_0, b.mean)
# mu_1 <- c(s.mean_1, b.mean)
# 
# sigma_0 <- matrix(c(s.std_0^2,            s.std_0*b.std*rho,
# 							 		  s.std_0*b.std*rho,    b.std^2), 
# 								    2)
# sigma_1 <- matrix(c(s.std_1^2,            s.std_1*b.std*rho,
# 										s.std_1*b.std*rho,    b.std^2), 
# 									  2)
# mu;sigma_0;sigma_1
# 
# sim_errorTerms_0 <- replicate(nsim, mvrnorm(nyear, mu = mu_0, Sigma = sigma_0)) 
# sim_errorTerms_1 <- replicate(nsim, mvrnorm(nyear, mu = mu_1, Sigma = sigma_1)) 
# 
# sim_stockreturn_0 <- sim_errorTerms_0[,1,]
# sim_stockreturn_1 <- sim_errorTerms_1[,1,]
# 
# sim_bondreturn_0 <- sim_errorTerms_0[,2,]
# sim_bondreturn_1 <- sim_errorTerms_1[,2,]
}
	
# Generating of stock and bond returns (approach 2, based on standard multivariate normal distribution)

sim_errorTerms_stdNormal <- replicate(simInputs$nsim, mvrnorm(simInputs$nyear*4, mu = c(0, 0), Sigma = matrix(c(1, simInputs$rho, simInputs$rho, 1), 2))) 

sim_stockreturn_0 <- sim_errorTerms_stdNormal[,1,]*simInputs$s.std_0 + simInputs$s.mean_0
sim_stockreturn_1 <- sim_errorTerms_stdNormal[,1,]*simInputs$s.std_1 + simInputs$s.mean_1

sim_bondreturn <- sim_errorTerms_stdNormal[,2,]*simInputs$b.std + simInputs$b.mean

# corr.test(cbind(as.vector(sim_bondreturn), as.vector(sim_stockreturn_1))) %>% print(short = F)
# sim_stockreturn_0 <- replicate(nsim, rnorm(nyear*4, s.mean_0, s.std_0))
# sim_stockreturn_1 <- replicate(nsim, rnorm(nyear*4, s.mean_1, s.std_1))

#  Simulating GDP growth
sim_gdp_0 <- replicate(simInputs$nsim, rnorm(simInputs$nyear*4, simInputs$gdp.mean_0, simInputs$gdp.std_0))
sim_gdp_1 <- replicate(simInputs$nsim, rnorm(simInputs$nyear*4, simInputs$gdp.mean_1, simInputs$gdp.std_1))

# Compute stock returns with regime-switching
sim_stockreturn <- (sim_gdp_regimes == 0) * sim_stockreturn_0 + (sim_gdp_regimes == 1) * sim_stockreturn_1
sim_gdp_growth  <- (sim_gdp_regimes == 0) * sim_gdp_0 +         (sim_gdp_regimes == 1) * sim_gdp_1
}


# check correlation between regime-switching stock returns and single-regime bond returns
cor(cbind(as.vector(sim_bondreturn), as.vector(sim_stockreturn))) 


# sim_stockreturn
# sim_bondreturn
# sim_gdp_growth
# 
# sim_gdp_regimes
# 
# sim_stockreturn_0 
# sim_stockreturn_1 
# 
# sim_gdp_0 %>% mean 
# sim_gdp_1 %>% mean



#' TO what extent the simulation approach is capable of generating economic and investment return
#' scenarios with statistical characteristics that are similar to historical data.

#sim_gdp_regimes %>% dim()


#***********************************************************************************
#           6. Converting results to annual data and save                         ####
#***********************************************************************************

## Convert quarterly returns / growth into annual values (quarterly returns compounded within a year)

df_sim_stockreturn_q <- 
	as.data.frame(sim_stockreturn) %>% 
	mutate(year = rep(1:nyear, each = 4))
df_sim_stockreturn_y <-  
	df_sim_stockreturn_q %>% 
	gather(sim, return, -year) %>% 
	group_by(sim, year) %>% 
	summarise(return_y = prod(1+return) - 1 )


df_sim_bondreturn_q <- 
	as.data.frame(sim_bondreturn) %>% 
	mutate(year = rep(1:nyear, each = 4))
df_sim_bondreturn_y <-  
	df_sim_bondreturn_q %>% 
	gather(sim, return, -year) %>% 
	group_by(sim, year) %>% 
	summarise(return_y = prod(1+return) - 1 )

df_sim_gdp_q <- 
	as.data.frame(sim_gdp_growth) %>% 
	mutate(year = rep(1:nyear, each = 4))
df_sim_gdp_y <-  
	df_sim_gdp_q %>% 
	gather(sim, return, -year) %>% 
	group_by(sim, year) %>% 
	summarise(return_y = prod(1+return) - 1 )

# regimes
df_sim_gdp_regimes_q <- 
	as.data.frame(sim_gdp_regimes) %>% 
	mutate(year = rep(1:nyear, each = 4))


df_sim_gdp_regimes_y <- 
	as.data.frame(df_sim_gdp_regimes_q) %>% 
	gather(sim, regime, -year) %>% 
	group_by(sim, year) %>% 
	summarise(recession_nqtr = sum(regime))


sim_results <- list(
	df_sim_stockreturn_q = df_sim_stockreturn_q,
	df_sim_bondreturn_q  = df_sim_bondreturn_q,
	df_sim_gdp_q         = df_sim_gdp_q,
	df_sim_gdp_regimes_q = df_sim_gdp_regimes_q,
	
	
	df_sim_stockreturn_y= df_sim_stockreturn_y,
	df_sim_bondreturn_y = df_sim_bondreturn_y,
	df_sim_gdp_y        = df_sim_gdp_y,
	df_sim_gdp_regimes_y= df_sim_gdp_regimes_y
)


assign(sim_name, sim_results)


# save(df_sim_stockreturn_q,
# 	   df_sim_bondreturn_q,
# 	   df_sim_gdp_q,
# 	   df_sim_gdp_regimes_q,
# 	   
# 	   
# 	   df_sim_stockreturn_y,
# 	   df_sim_bondreturn_y,
# 	   df_sim_gdp_y,
# 	   df_sim_gdp_regimes_y,
save(sim_results,
		 file = paste0(dir_data_out, sim_name, ".RData"))

sim_name








