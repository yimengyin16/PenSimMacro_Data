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
#library(MASS)        # multivariate normal generator, must be loaded before tidyverse, otherwise 'select' will be masked


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
load(paste0(dir_data_out, "MacroModel_sim_results_forward.RData"))

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



#***********************************************************************************
#          1. Examine recession regime: number and length in 30-year period        ####
#***********************************************************************************

nyear <- 30

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

summary_gdpRegime_0 <- sapply(sim_results$df_sim_gdp_regimes_q %>% select(-year), get_regimeLength, rgm = 0)
summary_gdpRegime_1 <- sapply(sim_results$df_sim_gdp_regimes_q %>% select(-year), get_regimeLength, rgm = 1)

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
#           2. Examine simulation results                        ####
#***********************************************************************************

# ## Check annual returns and growth
# 
# annual stock returns, all sims
sim_results$df_sim_stockreturn_y$return_y	%>% mean # 10.8% mean annual return (quarterly return compounded)
sim_results$df_sim_stockreturn_y$return_y	%>% sd   # 17.2% std
sim_results$df_sim_stockreturn_y$return_y	%>% describe  # ~0.57 kurtosis, higher than historical returns
sim_results$df_sim_stockreturn_y$return_y %>% hist()

sim_results$df_sim_stockreturn_y %>% 
	group_by(sim) %>% 
	summarise(geoReturn = get_geoReturn(return_y)) %>% 
	summarise(geoReturn_mean = mean(geoReturn)) # 6.8%
sim_results$df_sim_stockreturn_y$return_y	%>% sd   # 17.2% std

# annual bond returns, all sims
sim_results$df_sim_bondreturn_y$return_y	%>% mean # ~3.5% mean annual return (quarterly return compounded)
sim_results$df_sim_bondreturn_y$return_y	%>% sd   # ~4.1% std
sim_results$df_sim_bondreturn_y$return_y	%>% describe  # very small skewness and kurtosis
sim_results$df_sim_bondreturn_y$return_y %>%  hist(seq(-0.25, 0.25, 0.01))


sim_results$df_sim_bondreturn_y %>% 
	group_by(sim) %>% 
	summarise(geoReturn = get_geoReturn(return_y)) %>% 
	summarise(geoReturn_mean = mean(geoReturn)) # 3.6%
sim_results$df_sim_bondreturn_y$return_y	%>% sd   # 4.1% std



# annual gdp returns, all sims
sim_results$df_sim_gdp_y$return_y	%>% mean # ~3.05% mean annual return (quarterly return compounded)
sim_results$df_sim_gdp_y$return_y	%>% sd   # ~2.2% std (larger than std of shocks due to regime-switching)
sim_results$df_sim_gdp_y$return_y	%>% describe  # heavily skewed, 0.8 kurtosis
sim_results$df_sim_gdp_y$return_y %>% hist
# 

sim_results$df_sim_gdp_y %>% 
	group_by(sim) %>% 
	summarise(geoReturn = get_geoReturn(return_y)) %>% 
	summarise(geoReturn_mean = mean(geoReturn)) # 1.9%
sim_results$df_sim_gdp_y$return_y	%>% sd   # 2.2% std


cor(sim_results$df_sim_stockreturn_y$return_y,  sim_results$df_sim_bondreturn_y$return_y)




## Compared with historical distribution

## qqplot for GDP growth
library(qqplotr)

nsim_plot <- 50
nyear <- 30

df_qqplot_sim <-
bind_rows(
	 df_stock_y %>% select(value = dl_gdp_o)    %>% mutate(type = "Historical", var = 'GDP growth' ),
	(sim_results$df_sim_gdp_y %>% select(value = return_y) %>% mutate(type = 'Simulated',  var = 'GDP growth'))[1:(nsim_plot*nyear),],  # plot based on 50 simulations

	df_stock_y %>% select(value = return_tot_o ) %>% mutate(type = "Historical", var = 'Stock return'),
	(sim_results$df_sim_stockreturn_y %>% select(value = return_y) %>% mutate(type = 'Simulated',   var = 'Stock return'))[1:(nsim_plot*nyear),],  # plot based on 50 simulations

	df_stock_y           %>% select(value = dl_gbond_o) %>% mutate(type = "Historical", var = 'Bond return'),
	(sim_results$df_sim_bondreturn_y %>% select(value = return_y)   %>% mutate(type = 'Simulated',  var = 'Bond return'))[1:(nsim_plot*nyear),]  # plot based on 50 simulations
	)
df_qqplot_sim %>% head


fig_qqplot_simGDP <-
	df_qqplot_sim %>% filter(var == 'GDP growth') %>%
	ggplot(aes(sample = value)) + facet_wrap(var~type, scales = 'fixed') + theme_bw() + RIG.themeLite()+
	stat_qq_point(size = 1) +
	stat_qq_line() +
	#stat_qq_band(alpha = 0.5, con = 0.95, bandType = "boot") +
	labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
			 title = 'Comparing Q-Q plots of historical annual GDP growth and simulated annual GDP growth')
fig_qqplot_simGDP


fig_qqplot_simStock <-
	df_qqplot_sim %>% filter(var == 'Stock return') %>%
	ggplot(aes(sample = value)) + facet_wrap(var~type, scales = 'fixed') + theme_bw() + RIG.themeLite()+
	stat_qq_point(size = 1) +
	stat_qq_line() +
	#stat_qq_band(alpha = 0.5, con = 0.95, bandType = "boot") +
	labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
			 title = 'Comparing Q-Q plots of historical annual stock return and simulated annual stock return')
fig_qqplot_simStock


fig_qqplot_simBond <-
	df_qqplot_sim %>% filter(var == 'Bond return') %>%
	ggplot(aes(sample = value)) + facet_wrap(var~type, scales = 'fixed') + theme_bw() + RIG.themeLite()+
	stat_qq_point(size = 1) +
	stat_qq_line() +
	#stat_qq_band(alpha = 0.5, con = 0.95, bandType = "boot") +
	labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
			 title = 'Comparing Q-Q plots of historical annual bond return and simulated annual bond return')
fig_qqplot_simBond



ggsave(paste0(dir_outputs, "fig_MacroModel_qqplot_simGDP.png"),   fig_qqplot_simGDP,   width = 10*0.95, height = 5.5*0.95)
ggsave(paste0(dir_outputs, "fig_MacroModel_qqplot_simStock.png"), fig_qqplot_simStock, width = 10*0.95, height = 5.5*0.95)
ggsave(paste0(dir_outputs, "fig_MacroModel_qqplot_simBond.png"),  fig_qqplot_simBond,  width = 10*0.95, height = 5.5*0.95)


df_sim_descriptive <-
df_qqplot_sim %>%
	filter(!is.na(value)) %>%
	group_by(var, type) %>%
	select(-sim) %>%
	do(describe(.$value))

df_sim_descriptive

write_csv(df_sim_descriptive, paste0(dir_outputs, 'tab_MacroModel_descriptive_stats_sim.csv'))




# 
# (df_sim_gdp_y %>% select(GDP_growth = return_y) %>% mutate(type = 'Simulated'))[1:900,]$GDP_growth %>% describe
# 
# 
# 
# ## Quantiles of GDP simulation
# 
# sim_results$df_sim_gdp_y %>%
# 	group_by(sim) %>%
# 	summarise(avg = mean(return_y),
# 						std  = sd(return_y)) %>%
# 	summarise(avg_q25 = quantile(avg, 0.25),
# 						avg_q50 = quantile(avg, 0.50),
# 						avg_q75 = quantile(avg, 0.75),
# 
# 						std_q25 = quantile(std, 0.25),
# 						std_q50 = quantile(std, 0.50),
# 						std_q75 = quantile(std, 0.75)
# 						)
# 
# # number of recessions
# 
# # Number of recessions
# sapply(summary_gdpRegime_1, length) %>% quantile(0.25)   # ~5 recessions in 30 years (many are very short)
# sapply(summary_gdpRegime_1, length) %>% quantile(0.50)   # ~5 recessions in 30 years (many are very short)
# sapply(summary_gdpRegime_1, length) %>% quantile(0.75)   # ~5 recessions in 30 years (many are very short)
# 
# # Expected length of regimes
# sapply(summary_gdpRegime_1, mean) %>% quantile(0.25, na.rm = TRUE)   # average length is ~3.1 quarters.
# sapply(summary_gdpRegime_1, mean) %>% quantile(0.50, na.rm = TRUE)   # average length is ~3.1 quarters.
# sapply(summary_gdpRegime_1, mean) %>% quantile(0.75, na.rm = TRUE)   # average length is ~3.1 quarters.
# 
# sapply(summary_gdpRegime_0, mean) %>% quantile(0.25, na.rm = TRUE)   # average length is ~3.1 quarters.
# sapply(summary_gdpRegime_0, mean) %>% quantile(0.50, na.rm = TRUE)   # average length is ~3.1 quarters.
# sapply(summary_gdpRegime_0, mean) %>% quantile(0.75, na.rm = TRUE)   # average length is ~3.1 quarters.
# 
# 
# 
# sapply(summary_gdpRegime_1, mean) %>% head
# 
# summary_gdpRegime_1 %>% head
# 
# sapply(summary_gdpRegime_0, mean) %>% mean(., na.rm = TRUE)  # average length is ~20  quarters.
# # all as expected
# 
# 
# 
# 
# ## Stock
# # qq-plot
# qqnorm(rnorm(10000))
# qqline(rnorm(10000))
# 
# qqnorm(sim_results$df_sim_stockreturn_y$return_y[1:10000])
# qqline(sim_results$df_sim_stockreturn_y$return_y[1:10000])
# 
# df_stock_y$return_tot_o[-1] %>% qqnorm
# df_stock_y$return_tot_o[-1] %>% qqline
# 
# # descriptive statistics
# rnorm(10000) %>% describe
# sim_results$df_sim_stockreturn_y$return_y[1:10000] %>% describe
# df_stock_y$return_tot_o[-1] %>% describe
# 
# 
# df_stock_q$return_tot_o[-1] %>% qqnorm
# df_stock_q$return_tot_o[-1] %>% qqline
# 
# # probability of -30% return or worse
# pnorm(-0.3, 0.108, 0.172) # 0.8%
# pnorm(-0.3, 0.067, 0.164)
# 
# 
# (sim_results$df_sim_stockreturn_y$return_y[1:10000]<= -0.3) %>% sum
# 
# 
# sim_results$df_sim_stockreturn_y$return_y[1:10000]
# 
# 
# ## bond
# 
# sim_results$df_sim_bondreturn_y$return_y[1:10000] %>% qqnorm
# sim_results$df_sim_bondreturn_y$return_y[1:10000] %>% qqline
# 
# df_stock_y$dl_cbond[-1] %>% qqnorm
# df_stock_y$dl_cbond[-1] %>% qqline
# 
# 
# ## gdp growth
# 
# sim_results$df_sim_gdp_y$return_y[1:10000] %>% qqnorm
# sim_results$df_sim_gdp_y$return_y[1:10000] %>% qqline
# 
# df_stock_y$dl_gdp[-1] %>% qqnorm
# df_stock_y$dl_gdp[-1] %>% qqline
# 
# 
# 
# df_stock_y
# 
# 
# 
# 
# 
# 
# 
# sim_results$df_sim_stockreturn_y$return_y	     %>% hist(27)
# sim_results$sim_stockreturn	%>% as.vector()    %>% hist(27)
# rnorm(20000,0.02607146, 0.0790173) %>% hist(27)
# 
# 
# rnorm(20000,0.02607146, 0.0790173) %>% quantile(0.10)
# sim_stockreturn %>% quantile(0.10)
# 
# 
# # historical data (quarterly)
# 
# #stock quarterly
# df_stock_q$return_tot_o[-1] %>% mean #2.55%
# df_stock_q$return_tot_o[-1] %>% sd   #7.9%
# df_stock_q$return_tot_o[-1] %>% describe # skew -0.92, kurtosis 1.72
# rnorm(2000,0.0255,0.0792)   %>% describe
# 
# # stock annual
# df_stock_y$return_tot_o[-1] %>% mean  # 10.3%
# df_stock_y$return_tot_o[-1] %>% sd    # 15.7%
# df_stock_y$return_tot_o[-1] %>% describe() # skew -0.38, kurtosis 0.03
# rnorm(20000,0.0255,0.0792)   %>% describe   # skew 0.03,  kur
# 
# df_stock_y$return_tot_o[-1] %>% plot(type = 'l')
# 
# df_stock_y$return_tot_o[-1] %>% hist(27)
# df_stock_q$return_tot_o[-1] %>% hist(27)
# 
# df_stock_q$return_tot_o[-1] %>% qqnorm
# df_stock_q$return_tot_o[-1] %>% qqline
# 
# df_stock_y$return_tot_o[-1] %>% qqnorm
# df_stock_y$return_tot_o[-1] %>% qqline
# 
# 
# 
# 
# df_stock_q$return_tot_o[-1] %>% quantile(0.10)
# 
# 
# 
# #cbond
# df_stock_q$dl_cbond_o[-1] %>% mean
# df_stock_q$dl_cbond_o[-1] %>% sd
# 
# #gbond
# df_stock_q$dl_gbond_o[-1] %>% mean
# df_stock_q$dl_gbond_o[-1] %>% sd
# 
# shapiro.test(df_stock_q$return_tot_o)
# 
# 



#***********************************************************************************
#                             Saving results                                ####
#***********************************************************************************

# save(
# df_sim_stockreturn_q,
# df_sim_bondreturn_q,
# df_sim_gdp_q,
# df_sim_gdp_regimes_q,
# 
# 
# df_sim_stockreturn_y,
# df_sim_bondreturn_y,
# df_sim_gdp_y,
# df_sim_gdp_regimes_y,
# file = "policyBrief/simulation_MS1.RData")
# 
# 
# load("techReport_out/simulation_MS1.RData")












