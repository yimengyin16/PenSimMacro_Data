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
#                    Overview the simulation procedure          ####
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
#                 Global parameters for simulation                    ####
#***********************************************************************************

nyear <- 30
nsim  <- 2000


#***********************************************************************************
#             Defining the 2-state Markov chain for GDP             ####
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
#             Paramters of gdp growth, stock and bond returns         ####
#***********************************************************************************

# Stock: expansion
s.mean_0 <- 0.032
s.std_0  <- 0.068 

# Stock: recession
s.mean_1 <- -0.014
s.std_1  <- 0.119

# Bond
b.mean  <- 0.035/4
b.std   <- 0.04/2

# Correlation between error terms(deviation from mean) of stock and bond returns
# (Calibration needed)
# Goal is to find a rho that can make the correlation between the simulated
# annual stock returns and annual bond returns (both calculated from quarterly returns)
# match the target value
rho <- 0.15 

# GDP expansion 
gdp.mean_0 <- 0.0095
gdp.std_0  <- 5.463e-05^0.5 # 0.007391211

# GDP recession
gdp.mean_1 <- -0.0055
gdp.std_1  <- 5.463e-05^0.5 # 0.007391211


#***********************************************************************************
#     Simulating quarterly GDP growth and stock returns with regime-switching   ####
#***********************************************************************************


set.seed(11)
{
	
# Generating GDP regimes	
sim_gdp_regimes <- replicate(nsim, rmarkovchain(n = nyear * 4, object = mc_gdp, t0 = '0') %>% as.numeric())

	
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

sim_errorTerms_stdNormal <- replicate(nsim, mvrnorm(nyear*4, mu = c(0, 0), Sigma = matrix(c(1, rho, rho, 1), 2))) 

sim_stockreturn_0 <- sim_errorTerms_stdNormal[,1,]*s.std_0 + s.mean_0
sim_stockreturn_1 <- sim_errorTerms_stdNormal[,1,]*s.std_1 + s.mean_1

sim_bondreturn <- sim_errorTerms_stdNormal[,2,]*b.std + b.mean

# corr.test(cbind(as.vector(sim_bondreturn), as.vector(sim_stockreturn_1))) %>% print(short = F)
# sim_stockreturn_0 <- replicate(nsim, rnorm(nyear*4, s.mean_0, s.std_0))
# sim_stockreturn_1 <- replicate(nsim, rnorm(nyear*4, s.mean_1, s.std_1))

#  Simulating GDP growth
sim_gdp_0 <- replicate(nsim, rnorm(nyear*4, gdp.mean_0, gdp.std_0))
sim_gdp_1 <- replicate(nsim, rnorm(nyear*4, gdp.mean_1, gdp.std_1))

# Compute stock returns with regime-switching
sim_stockreturn <- (sim_gdp_regimes == 0) * sim_stockreturn_0 + (sim_gdp_regimes == 1) * sim_stockreturn_1
sim_gdp_growth  <- (sim_gdp_regimes == 0) * sim_gdp_0 +         (sim_gdp_regimes == 1) * sim_gdp_1
}

# check correlation between regime-switching stock returns and single-regime bond returns
cor(cbind(as.vector(sim_bondreturn), as.vector(sim_stockreturn))) 


sim_stockreturn
sim_bondreturn
sim_gdp_growth

sim_gdp_regimes

sim_stockreturn_0 
sim_stockreturn_1 

sim_gdp_0 %>% mean 
sim_gdp_1 %>% mean




#***********************************************************************************
#           Examine recession regime: number and length in 30-year period        ####
#***********************************************************************************

# calculate length of regimes in each simulation
get_regimeLength <- function(x, rgm = 1){

#x <- sim_gdp_regimes[,2]
y <- numeric(length(x))
y[1] <- 1

for(i in 2:length(x)){
	if(x[i] != x[i-1]) y[i] <- y[i-1] + 1 else y[i] <- y[i-1] 
}

df <- data.frame(idx = y, regime = x)
df %<>% filter(regime == rgm) %>% 
	group_by(idx) %>% 
	summarise(regime_len = n())
df

r <- df$regime_len
r
}

summary_gdpRegime_0 <- sapply(as.data.frame(sim_gdp_regimes), get_regimeLength, rgm = 0)
summary_gdpRegime_1 <- sapply(as.data.frame(sim_gdp_regimes), get_regimeLength, rgm = 1)

# Number of recessions 
sapply(summary_gdpRegime_1, length) %>% mean  # ~5 recessions in 30 years (many are very short)

# Expected length of regimes
sapply(summary_gdpRegime_1, mean) %>% mean(., na.rm = TRUE)  # average length is ~3.1 quarters. 
sapply(summary_gdpRegime_0, mean) %>% mean(., na.rm = TRUE)  # average length is ~20  quarters. 
 # all as expected

# distribution of recession length (all sims together)
x <- data.frame(n.rec = unlist(summary_gdpRegime_1))
x %>% count(n.rec)
nrow(x)	
unlist(summary_gdpRegime_1) %>% hist()
# about 1/3 "recessions" only last for 1 quarter

# distribution of average recession length across sims
sapply(summary_gdpRegime_1, mean) %>% hist()

# distribution of number of recessions across sims
sapply(summary_gdpRegime_1, length) %>% hist()



#***********************************************************************************
#           Examine simulation results of stock returns                         ####
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


## Check annual returns and growth

# annual stock returns, all sims
df_sim_stockreturn_y$return_y	%>% mean # 10.8% mean annual return (quarterly return compounded)
df_sim_stockreturn_y$return_y	%>% sd   # 17.2% std
df_sim_stockreturn_y$return_y	%>% describe  # ~0.57 kurtosis, higher than historical returns
df_sim_stockreturn_y$return_y %>% hist(seq(-0.65, 1.17, 0.02))


# annual bond returns, all sims
df_sim_bondreturn_y$return_y	%>% mean # ~3.5% mean annual return (quarterly return compounded)
df_sim_bondreturn_y$return_y	%>% sd   # ~4.1% std
df_sim_bondreturn_y$return_y	%>% describe  # very small skewness and kurtosis
df_sim_bondreturn_y$return_y %>% hist(seq(-0.25, 0.25, 0.01))


# annual gdp returns, all sims
df_sim_gdp_y$return_y	%>% mean # ~3.05% mean annual return (quarterly return compounded)
df_sim_gdp_y$return_y	%>% sd   # ~2.1% std (larger than std of shocks due to regime-switching)
df_sim_gdp_y$return_y	%>% describe  # heavily skewed, 0.8 kurtosis
df_sim_gdp_y$return_y %>% hist


cor(df_sim_stockreturn_y$return_y,df_sim_bondreturn_y$return_y)




## Compared with historical distribution 

## Stock
# qq-plot 
qqnorm(rnorm(10000))
qqline(rnorm(10000))

qqnorm(df_sim_stockreturn_y$return_y[1:10000])
qqline(df_sim_stockreturn_y$return_y[1:10000])

df_stock_y$return_tot_o[-1] %>% qqnorm
df_stock_y$return_tot_o[-1] %>% qqline

# descriptive statistics
rnorm(10000) %>% describe
df_sim_stockreturn_y$return_y[1:10000] %>% describe
df_stock_y$return_tot_o[-1] %>% describe


df_stock_q$return_tot_o[-1] %>% qqnorm
df_stock_q$return_tot_o[-1] %>% qqline

# probability of -30% return or worse
pnorm(-0.3, 0.108, 0.172) # 0.8%
pnorm(-0.3, 0.067, 0.164)

(df_sim_stockreturn_y$return_y[1:10000]<= -0.3) %>% sum


df_sim_stockreturn_y$return_y[1:10000]


## bond

df_sim_bondreturn_y$return_y[1:10000] %>% qqnorm
df_sim_bondreturn_y$return_y[1:10000] %>% qqline

df_stock_y$dl_cbond[-1] %>% qqnorm
df_stock_y$dl_cbond[-1] %>% qqline


## gdp growth

df_sim_gdp_y$return_y[1:10000] %>% qqnorm
df_sim_gdp_y$return_y[1:10000] %>% qqline

df_stock_y$dl_gdp[-1] %>% qqnorm
df_stock_y$dl_gdp[-1] %>% qqline



df_stock_y









# simulated quarterly stock returns

sim_stockreturn %>% mean
sim_stockreturn %>% sd
sim_stockreturn %>% as.vector() %>% describe


sim_gdp_growth %>% mean
sim_gdp_growth %>% sd





df_sim_stockreturn_y$return_y	     %>% hist(27)
sim_stockreturn	%>% as.vector()    %>% hist(27)
rnorm(20000,0.02607146, 0.0790173) %>% hist(27)


rnorm(20000,0.02607146, 0.0790173) %>% quantile(0.10)
sim_stockreturn %>% quantile(0.10)


# historical data (quarterly)

#stock quarterly
df_stock_q$return_tot_o[-1] %>% mean #2.55%
df_stock_q$return_tot_o[-1] %>% sd   #7.9%
df_stock_q$return_tot_o[-1] %>% describe # skew -0.92, kurtosis 1.72
rnorm(2000,0.0255,0.0792)   %>% describe

# stock annual
df_stock_y$return_tot_o[-1] %>% mean  # 10.3%
df_stock_y$return_tot_o[-1] %>% sd    # 15.7% 
df_stock_y$return_tot_o[-1] %>% describe() # skew -0.38, kurtosis 0.03
rnorm(20000,0.0255,0.0792)   %>% describe   # skew 0.03,  kur

df_stock_y$return_tot_o[-1] %>% plot(type = 'l')

df_stock_y$return_tot_o[-1] %>% hist(27)
df_stock_q$return_tot_o[-1] %>% hist(27)

df_stock_q$return_tot_o[-1] %>% qqnorm
df_stock_q$return_tot_o[-1] %>% qqline

df_stock_y$return_tot_o[-1] %>% qqnorm
df_stock_y$return_tot_o[-1] %>% qqline




df_stock_q$return_tot_o[-1] %>% quantile(0.10)



#cbond
df_stock_q$dl_cbond_o[-1] %>% mean
df_stock_q$dl_cbond_o[-1] %>% sd

#gbond
df_stock_q$dl_gbond_o[-1] %>% mean
df_stock_q$dl_gbond_o[-1] %>% sd

shapiro.test(df_stock_q$return_tot_o)



























