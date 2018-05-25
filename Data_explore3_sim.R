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


RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
								 RIG.orange,
								 RIG.purple,
								 RIG.green ,
								 RIG.blue,
								 RIG.yellow.dark)


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


# save data in feather format for python use
write_feather(df_stock_q, "data_out/df_stock_q.feather" )
df_stock_q$dl_gdp_o



#***********************************************************************************
#   Examine simplified approach to modeling GDP and returns jointly             ####
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
#' recession       expansion
#' MSAR4               1.6             19.9 
#' MSAR2               1.7             20.9      
#' MSAR1               1.8             20.4
#' RW-drift            4.4             11.4
#' RW-drift            3.1             20.7 
#' Hamilton1989        4.1             16.2


#***********************************************************************************
#   Implementing a simple 2-state Markov chain             ####
#***********************************************************************************

# Defining transition probabilities
p12 <- 0.048
p21 <- 0.32

p11 <- 1 - p12
p22 <- 1 - p21


# Use package markovchain
library(markovchain)
gdpStates <- c('0', '1')
byRow     <- TRUE
gdpMatrix <- matrix(c(p11, p12,
											p21, p22), 
										byrow = byRow, 
										nrow = 2) 
gdpMatrix

mc_gdp <- new("markovchain", 
							states = gdpStates,
							transitionMatrix = gdpMatrix,
							name = 'gdp'
							)
mc_gdp %>% plot

summary(mc_gdp)

# simulating with markov chain
rmarkovchain(n = 4*30, object = mc_gdp, t0 = '0')

replicate(10, rmarkovchain(n = 4*30, object = mc_gdp, t0 = '1')) 


#***********************************************************************************
#             Simulating quarterly stock returns with regime-switching          ####
#***********************************************************************************

s.mean_0 <- 0.03
s.std_0  <- 0.07
s.mean_1 <- 0
s.std_1  <- 0.12

gdp.mean_0 <- 0.0095
gdp.std_0  <- 5.463e-05^0.5 # 0.007391211
gdp.mean_1 <- -0.0055
gdp.std_1  <- 5.463e-05^0.5 # 0.007391211


nyear <- 30
nsim  <- 2000

set.seed(11)
{
sim_gdp_regimes <- replicate(nsim, rmarkovchain(n = nyear * 4, object = mc_gdp, t0 = '0') %>% as.numeric())

sim_stockreturn_0 <- replicate(nsim, rnorm(nyear*4, s.mean_0, s.std_0))
sim_stockreturn_1 <- replicate(nsim, rnorm(nyear*4, s.mean_1, s.std_1))

sim_gdp_0 <- replicate(nsim, rnorm(nyear*4, gdp.mean_0, gdp.std_0))
sim_gdp_1 <- replicate(nsim, rnorm(nyear*4, gdp.mean_1, gdp.std_1))

}

sim_gdp_regimes

sim_stockreturn_0 
sim_stockreturn_1 

sim_gdp_0 %>% mean 
sim_gdp_1 %>% mean


sim_stockreturn <- (sim_gdp_regimes == 0) * sim_stockreturn_0 + (sim_gdp_regimes == 1) * sim_stockreturn_1
sim_gdp_growth  <- (sim_gdp_regimes == 0) * sim_gdp_0 +         (sim_gdp_regimes == 1) * sim_gdp_1

sim_stockreturn
sim_gdp_growth


#***********************************************************************************
#           Examine recession regime:number and length in 30-year period        ####
#***********************************************************************************


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

get_regimeLength(sim_gdp_regimes[,3], 1) 


summary_gdpRegime_0 <- sapply(as.data.frame(sim_gdp_regimes), get_regimeLength, rgm = 0)
summary_gdpRegime_1 <- sapply(as.data.frame(sim_gdp_regimes), get_regimeLength, rgm = 1)


# number of recessions and expected length of recession
sapply(summary_gdpRegime_1, length) %>% mean  # 5.35 recessions in 30 years (many are very short)
sapply(summary_gdpRegime_1, mean)    # average length is 3.1 quarters. 

# distribution of recession length (all sims together)
x <- data.frame(n.rec = unlist(summary_gdpRegime_1))
x %>% count(n.rec)
nrow(x)	
unlist(summary_gdpRegime_1) %>% hist()

# distribution of average recession length across sims
sapply(summary_gdpRegime_1, mean) %>% hist()

# distribution of number of recessions across sims
sapply(summary_gdpRegime_1, length) %>% hist()



#***********************************************************************************
#           Examine simulation results of stock returns                         ####
#***********************************************************************************

# simulated quarterly stock returns

sim_stockreturn %>% mean
sim_stockreturn %>% sd
sim_stockreturn %>% as.vector() %>% describe


sim_gdp_growth %>% mean
sim_gdp_growth %>% sd


# Convert quarterly returns / growth into annual values

df_sim_stockreturn <- as.data.frame(sim_stockreturn) %>% 
	mutate(year = rep(1:nyear, each = 4))

df_sim_stockreturn_y <-  
	df_sim_stockreturn %>% 
	gather(sim, return, -year) %>% 
	group_by(sim, year) %>% 
	summarise(return_y = prod(1+return) - 1 )

df_sim_stockreturn_y$return_y	%>% mean # 10.8% mean annual return (quarterly return compounded)
df_sim_stockreturn_y$return_y	%>% sd   # 17.2% std
df_sim_stockreturn_y$return_y	%>% describe


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





#***********************************************************************************
#           Generating variables with multivariate normal distribution                         ####
#***********************************************************************************
library(MASS)

# Note
# https://www.r-bloggers.com/simulating-from-the-bivariate-normal-distribution-in-r/
#

mean_stock <- 0.10
mean_bond  <- 0.035

std_stock <- 0.17
std_bond  <- 0.04

rho <- 0.17

mu    <- c(mean_stock, mean_bond)
sigma <- matrix(c(std_stock^2,            std_stock*std_bond*rho,
									std_stock*std_bond*rho, std_bond^2           ), 2)

mu
sigma

nyear = 30
nsim = 1000

mvrnorm(nyear, mu = mu, Sigma = sigma) # from MASS package

set.seed(10)
sim_errorTerms <- replicate(nsim, mvrnorm(nyear, mu = mu, Sigma = sigma)) 

sim_error_stock <- sim_errorTerms[, 1, ]
sim_error_bond  <- sim_errorTerms[, 2, ]

sim_error_stock %>% dim
sim_error_bond  %>% dim








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
#'    
#'    
#'    


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




















