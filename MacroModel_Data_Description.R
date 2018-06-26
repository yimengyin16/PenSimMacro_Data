# Descriptive analysis of GDP and asset returns data for the technical report


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

#'Notes on modeling:
#'
#' A discussion of ARIMA transfer function models:
#'  https://robjhyndman.com/hyndsight/arimax/


#' Notes on fixed income
#' 
#' An argument for low real bond returns in the near future. 
#'  https://pensionpartners.com/what-real-returns-should-bond-investors-expect/
#'
#' Treasury coupon rate and yield 
#'  https://www.quora.com/How-does-the-U-S-Treasury-decide-what-coupon-rate-to-offer-on-Treasury-notes
#'  
#' One way of caculating bond returns from yield
#'  http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html (in the linked .xlsx file)
#' 

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

# guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
# 	RIG.theme()+
# 	theme(legend.position="none",
# 				title =element_text(size=9)) 



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
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"
dir_outputs  <- 'outputs_report/'
#dir_techReport <- 'techReport_out/'


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
	mutate(peak = peak - 1 / 4,
				 trough = trough - 1 / 4)

recessionPeriods


#**********************************************************************
#                     Loading Data                                 ####
#**********************************************************************
# Loading saved data
load(paste0(dir_data_out, "dataAll.RData"))



#**********************************************************************
#               Create additional veriables                        ####
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

df_stock_m <- fn(df_dataAll,   1953:2015, 12, 12) # 1 year rolling
df_stock_q <- fn(df_dataAll_q, 1953:2015, 12, 4)  # 3 year rolling
df_stock_y <- fn(df_dataAll_y, 1953:2015, 5,  1)  # 5 year rolling


# # save data in feather format for python use
# write_feather(df_stock_q, "data_out/df_stock_q.feather" )
# df_stock_q$dl_gdp_o



#**********************************************************************
#               Data Description    ####
#**********************************************************************

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

fig_title <- 'Historical stock return, volatility, and GDP growth rates'
fig_stock.gdp <- 
df_stock_q %>% 
	select(yearMon,
				 sd_return, 
				 mean_return,
				 dl_gdp) %>% 
	gather(var, value, -yearMon) %>%
	mutate(var = factor(var, levels = c('mean_return', 'sd_return', 'dl_gdp'),
											     labels = c('Stock return: \n12-quarter moving average',
											     					  'Stock return: \n12-quarter standard deviation',
											     					  'GDP growth'))) %>% 
	ggplot() + theme_bw()+ RIG.themeLite()+
	#facet_grid(type~.) +
	geom_line(aes(x = yearMon , y = value, color = var), size = 0.5) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.05))+ 
	scale_color_manual(values = c(RIG.blue, RIG.red, RIG.green),  name = NULL) + 
  coord_cartesian(ylim = c(-0.2, 0.6)) + 
	guides(color = guide_legend(keywidth = 5, keyheight = 1))+
	labs(title = fig_title,
		   x = NULL,
			 y = 'Return / Growth rate') + 
	theme(legend.position = "bottom",
				legend.title = element_blank())

# Note:
 # Stock returns are annualized quarterly return; 
 # GDP growth rates are annualized quarterly growth rates
 # shaded area


# bond return and volatility
fig_title <- 'Historical long-term Treasury bond return, volatility, and GDP growth rates'
fig_gbond.gdp <- 
df_stock_q %>% 
	select(yearMon,
				 mean_gbond,
				 sd_gbond,
				 dl_gdp) %>% 
	gather(var, value, -yearMon) %>%
	mutate(var = factor(var, 
											levels = c('mean_gbond', 'sd_gbond', 'dl_gdp'),
											labels = c("Long-term Teasury bond return: \n12-quarter moving average",
																 "Long-term Teasury bond return: \n12-quarter standard deviation",
																 'GDP growth'))) %>% 
	ggplot() + theme_bw() + RIG.themeLite() + 
	geom_line(aes(x = yearMon , y = value, color = var), size = 0.5) +
	geom_rect(data = recessionPeriods, 
						aes(xmin = peak, xmax = trough, 
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-0.5, 1.5, 0.05)) + 
	scale_color_manual(values = c(RIG.blue, RIG.red, RIG.green),  name = NULL) + 
	coord_cartesian(ylim = c(-0.2, 0.6)) + 
  guides(color = guide_legend(keywidth = 5, keyheight = 1))+
	labs(title = fig_title,
			 x = NULL,
			 y = 'Return / Growth rate') + 
	theme(legend.position = "bottom",
				legend.title = element_blank())

# Teasury bond returns are annualized quarterly return; 
# GDP growth rates are annualized quarterly growth rates
# shaded area




## How correlation between stock and bond change over time
rollcorr_stock.gbond <- runCor(df_stock_q$return_tot_o, df_stock_q$dl_gbond_o, 12)
rollcorr_stock.cbond <- runCor(df_stock_q$return_tot_o, df_stock_q$dl_cbond_o, 12)
df_stock_q$rollcorr_stock.gbond <- rollcorr_stock.gbond
df_stock_q$rollcorr_stock.cbond <- rollcorr_stock.cbond

fig_title <- '12-quarter rolling window correlation coefficient of stock and Treasury bond returns'
fig_corr_stock.gbond <- 
df_stock_q %>% 
		select(yearMon,
					 rollcorr_stock.gbond) %>% 
		gather(var, value, -yearMon) %>% 
		ggplot() + theme_bw()+ RIG.themeLite() + 
		#facet_grid(type~.) +
		geom_line(aes(x = yearMon , y = value, color = var), size = 1) +
		geom_rect(data = recessionPeriods, 
							aes(xmin = peak, xmax = trough, 
									ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
		scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
		scale_y_continuous(breaks = seq(-1, 1, 0.1) )+ 
  	scale_color_manual(values = c(RIG.red),  name = NULL) + 
		coord_cartesian(ylim = c(-1, 1)) +
	theme(legend.position = "none",
				legend.title = element_blank()) + 
	labs(title = fig_title,
			x = NULL,
			y = 'Correlation coefficient') 

fig_corr_stock.gbond


#' Correlations between total nominal returns are high unstable overtime
#'   - correlation used to be very high before 2000, with a couple of sharp temporary decline
#'   - correlation is generally negative after 2000, with strong swings.  
#' No obvious pattern in relation to business cycles
#' The only observed sharp rise in correlation is in the middle of the Great Recession. 


## qqplot for stock return bond return and GDP growth
library(qqplotr)

df <- 
bind_rows(
	df_stock_q %>% 
		select(dl_gdp_o, return_tot_o, dl_gbond_o) %>% 
		gather(var, value) %>% 
		mutate(var = factor(var, levels = c('dl_gdp_o', 'return_tot_o', 'dl_gbond_o'),
												     labels = c('GDP growth', 'Stock return', 'Long-term Treasury bond return'))) %>% 
		mutate(freq = "Quarterly"),
	
	df_stock_y %>% 
		select(dl_gdp_o, return_tot_o, dl_gbond_o) %>% 
		gather(var, value) %>% 
		mutate(var = factor(var, levels = c('dl_gdp_o', 'return_tot_o', 'dl_gbond_o'),
												labels = c('GDP growth', 'Stock return', 'Long-term Treasury bond return'))) %>% 
		mutate(freq = "Annual")
)

fig_qqplot <- 
df %>% 
	ggplot(aes(sample = value)) + facet_wrap(freq~var, scales = 'free') + theme_bw() + RIG.themeLite()+
	stat_qq_point(size = 1) +
	stat_qq_line() +
	#stat_qq_band(alpha = 0.5, con = 0.95, bandType = "boot") + 
	labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
			 title = 'Q-Q plots for annual and quarterly GDP growth, \nstock return and long-term Treasury bond return') 
fig_qqplot	



ggsave(paste0(dir_outputs, "fig_data_stock.gdp.png"), fig_stock.gdp, width = 10*0.95, height = 5*0.95)
ggsave(paste0(dir_outputs, "fig_data_gbond.gdp.png"), fig_gbond.gdp, width = 10*0.95, height = 5*0.95)
ggsave(paste0(dir_outputs, "fig_data_corr_stock.gbond.png"), fig_corr_stock.gbond, width = 10*0.95, height = 5*0.95)
ggsave(paste0(dir_outputs, "fig_data_qqplot.png"), fig_qqplot, width = 10, height = 7)


df_stock_q$dl_gdp_o %>% hist(20)
df_stock_y$dl_gdp_o %>% hist(15)



## Descriptive statistics

df_stats <- 
bind_rows(
df_stock_q %>% 
	select(dl_gdp = dl_gdp_o, return_tot = return_tot_o, dl_gbond = dl_gbond_o, dl_cbond = dl_cbond_o) %>% 
	gather(var, value) %>% 
	mutate(var = factor(var, levels = c('dl_gdp', 'return_tot', 'dl_gbond', 'dl_cbond'),
											labels = c('GDP growth', 'Stock return', 'Long-term Treasury bond return', 'Long-term coorporate bond'))) %>% 
	group_by(var) %>% 
	do(describe(.$value, na.rm = TRUE)) %>% 
	mutate(freq = "Quarterly"),

 df_stock_y %>% 
	select(dl_gdp, return_tot, dl_gbond, dl_cbond) %>% 
	gather(var, value) %>% 
	mutate(var = factor(var, levels = c('dl_gdp', 'return_tot', 'dl_gbond', 'dl_cbond'),
											labels = c('GDP growth', 'Stock return', 'Long-term Treasury bond return', 'Long-term coorporate bond'))) %>% 
	group_by(var) %>% 
	do(describe(.$value, na.rm = TRUE)) %>% 
 	mutate(freq = "Annual")
)

write_excel_csv(df_stats, paste0(dir_outputs, 'tab_data_descriptive_stats.csv') )

# paste0(dir_outputs, 'tab_data_descriptive_stats.csv')




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
mod_m <- msmFit(return_tot_o ~ 1, data = df_stock_m %>% filter(year %in% 1953:2015), k = 2, sw = c(T, T))
summary(mod_m)
plotProb(mod_m)

mod_q <- msmFit(return_tot_o ~ 1, data = df_stock_q %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_q)
plotProb(mod_q)
mod_q

mod_y <- msmFit(return_tot ~ 1, data = df_stock_y %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_y)
plotProb(mod_y)



mod_gdp_q <- msmFit(dl_gdp ~ 1, data = df_stock_q %>% filter(year %in% 1954:2015), k = 2, sw = c(T, F))
summary(mod_gdp_q)
plotProb(mod_gdp_q)
mod_gdp_q



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
	mod_y@Fit@smoProb[-1, ] %>% as.data.frame %>% 
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



#*******************************************************
### Examine fixed income returns ####
#*******************************************************

## Descriptive analysis

# annual stock and bond
bind_rows((df_stock_y$dl_cbond_o[-1]*100)   %>% describe(),
					(df_stock_y$return_tot_o[-1]*100) %>% describe()) %>% 
	mutate(varname = c('cbond_y', 'stock_y'))

(df_stock_y$dl_cbond_o[-1]*100)   %>% hist(10)  # skewed toward right (fat tail in high returns)
(df_stock_y$return_tot_o[-1]*100) %>% hist(10)  # skewed toward left  (fat tail in low  returns)

 
cor(df_stock_y[-1, ] %>% select(dl_cbond_o, return_tot_o))

df_stock_y %>% 
	select(yearMon, mean_return, mean_cbond, mean_gbond) %>% 
	gather(var,value, -yearMon) %>% 
	ggplot(aes(x = yearMon , y = value, color = var)) + theme_bw()+
	geom_line()

# x <- 
# df_stock_y %>% 
# 	select(yearMon, return_tot_o, dl_cbond_o, dl_gbond_o) 
# x



# Quarterly stock and bond
bind_rows((df_stock_q$dl_cbond_o[-1]*100)   %>% describe(),
					(df_stock_q$return_tot_o[-1]*100) %>% describe()) %>% 
	mutate(varname = c('cbond_q', 'stock_q'))

(df_stock_q$dl_cbond_o[-1]*100)   %>% hist(20)  # skewed toward right (fat tail in high returns)
(df_stock_q$return_tot_o[-1]*100) %>% hist(20)  # skewed toward left  (fat tail in low  returns)

cor(df_stock_q[-1, ] %>% select(dl_cbond_o, return_tot_o))

df_stock_q %>% 
	select(yearMon, mean_return, mean_cbond, mean_gbond) %>% 
	gather(var,value, -yearMon) %>% 
	ggplot(aes(x = yearMon , y = value, color = var)) + theme_bw()+
	geom_line()


#' Notes:
#' Quarterly returns of stock and bonds show stronger skewness and fat-tailness than annual returns
#' Correlation between stock and bond returns is stronger in annual data


## How correlation between stock and bond change over time
rollcorr_stock.cbond <- runCor(df_stock_q$return_tot_o, df_stock_q$dl_cbond_o, 12)
df_stock_q$rollcorr_stock.cbond <- rollcorr_stock.cbond

{df_stock_q %>% 
		select(yearMon,
					 rollcorr_stock.cbond) %>% 
		gather(var, value, -yearMon) %>% 
		ggplot() + theme_bw()+ 
		#facet_grid(type~.) +
		geom_line(aes(x = yearMon , y = value, color = var)) +
		geom_rect(data = recessionPeriods, 
							aes(xmin = peak, xmax = trough, 
									ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
		scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
		scale_y_continuous(breaks = seq(-1, 1, 0.1))+ 
		coord_cartesian(ylim = c(-1, 1))}

#' Correlations between total nominal returns are high unstable overtime
#'   - correlation used to be very high before 2000, with a couple of sharp temporary decline
#'   - correlation is generally negative after 2000, with strong swings.  
#' No obvious pattern in relation to business cycles
#' The only observed sharp rise in correlation is in the middle of the Great Recession. 


## Modeling total bond returns with ARMA model

# quarterly

 #cbond
auto.arima(df_stock_q$dl_cbond_o[-1]) # ARMA(3, 2) selected
sarima(df_stock_q$dl_cbond_o[-1], 3, 0, 2) # Best ARMA model
sarima(df_stock_q$dl_cbond_o[-1], 2, 0, 0) # AR2 model works ok 
sarima(df_stock_q$dl_cbond_o[-1], 0, 0, 0) # RW-drift works fine too


# Annual 

 #cbond
auto.arima(df_stock_y$dl_cbond_o[-1])      # random walk is selected
sarima(df_stock_y$dl_cbond_o[-1], 1, 0, 0) # Best ARMA model looks fine
sarima(df_stock_y$dl_cbond_o[-1], 0, 0, 0) # RW-drift model works better

 #gbond
auto.arima(df_stock_y$dl_gbond_o[-1])      # ARMA(1,1) is selected
sarima(df_stock_y$dl_gbond_o[-1], 1, 0, 1) # Best auto arima model
sarima(df_stock_y$dl_gbond_o[-1], 0, 0, 0) # But RW-drift has better AIC, BIC

Arima(df_stock_y$dl_gbond_o[-1], c(1, 0, 1))
Arima(df_stock_y$dl_gbond_o[-1], c(0, 0, 0)) # random walk 

df_stock_y %>% filter(year >= 1970) %>% .$dl_cbond_o %>% mean

# Nnotes:
# It seems ok to model total long-term cbond or gbond returns based on 
# 



# MSDR model for bond returns
mod_bond_q <- msmFit(dl_cbond_o ~ 1, data = df_stock_q %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_bond_q)
plotProb(mod_bond_q)
mod_bond_q

index_q <- (df_stock_q %>% filter(year %in% 1954:2015))[, "yearMon"]
regime_q <- 
	mod_bond_q@Fit@smoProb[-1,] %>% as.data.frame %>% 
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

# Regimes of total bond returns does not align well with recessions
  # High return high volatility regime: 1980s and Great Recession
  # Low return low volatility regime
  # Bond returns may have tendency to go into high-vol-high-return regime around severe recessions






#***********************************************************************************
#   Examine simplified approach to modeling GDP and returns jointly             ####
#***********************************************************************************

#' Overview of the simplified modeling approach:
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
#'

#*******************************************************************************
##   Check stock returns in different economic (GDP regimes)
#*******************************************************************************

# Mean and variance of stock returns in NBER recessions an expansions

# Import NBER recession periods from a python module 
usrec       <- feather("data_out/usrec.feather") %>% as.tibble()
usrec_index <- feather("data_out/usrec_index.feather") %>% as.tibble

usrec_index %<>% 
	rename(dt = '0') %>% 
	transmute(yearMon = as.yearmon(dt)) 

usrec_index_appd <- data.frame(yearMon = seq(as.Date("2013/5/1"), as.Date("2015/12/1"), by="mon") %>% as.yearmon)
usrec_appd <- data.frame(USREC = numeric(nrow(usrec_index_appd)))

usrec_index <- bind_rows(usrec_index, usrec_index_appd) %>% 
	mutate(yearMon = as.yearmon(yearMon))
usrec <- bind_rows(usrec, usrec_appd)


usrec_m <- bind_cols(usrec, usrec_index) %>% 
	mutate(year = year(yearMon),
				 month = month(yearMon))
usrec_q <- 
	usrec_m %>%
	filter(month %in% c(3, 6, 9, 12))

usrec_q %>% tail
usrec_m %>% tail

df_stock_m <- fn(df_dataAll,   1953:2015, 12, 12)
df_stock_q <- fn(df_dataAll_q, 1953:2015, 12, 4)

df_stock_m %<>% left_join(usrec_m) 
df_stock_q %<>% left_join(usrec_q) 

df_stock_q %>% 
	select(yearMon, return_tot_o, USREC) %>% 
  filter(!is.na(USREC) & !is.na(return_tot_o)) %>% 
	group_by(USREC) %>% 
	summarise(avg = mean(return_tot_o),
						std = sd(return_tot_o))


# Mean and variance of stock returns based on MS model of stock return
mod_q <- msmFit(return_tot_o ~ 1, data = df_stock_q %>% filter(year %in% 1954:2015), k = 2, sw = c(T, T))
summary(mod_q)
plotProb(mod_q)


# Mean and variance of stock returns based on GDP regimes
gdp_regimes_q <- feather("data_out/regimes_gdp.feather") %>% as.tibble()

gdp_regimes_q %<>% 
	mutate(yearQtr = as.yearqtr(dt),
				 year  = year(yearQtr),
				 qtr   = quarter(yearQtr),
				 rec2_filtered = p2_filtered >=0.5,
				 rec2_smoothed = p2_smoothed >=0.5,
				 rec3_filtered = p3_filtered >=0.5,
				 rec3_smoothed = p3_smoothed >=0.5)

df_stock_q <- fn(df_dataAll_q, 1953:2015, 12, 4)

df <- 
df_stock_q %>% 
	mutate(yearQtr = as.yearqtr(yearMon)) %>%
	left_join(gdp_regimes_q, by = 'yearQtr') %>% 
	select(yearQtr, return_tot_o, starts_with("rec")) %>% 
	filter(!is.na(return_tot_o))

f <- function(df, v){
	v <- enquo(v)
	df %>% 
		group_by(!!v) %>% 
		summarise(avg = mean(return_tot_o),
							std = sd(return_tot_o))
}


f(df, rec2_smoothed)
f(df, rec3_smoothed)




# Summary of quaterly stock mean return and std 

# Based on NBER recession periods 1953 - 2015
#            mean       std
# expansion  0.032     0.0686
# recession -0.014     0.119  

# Based on MS model of stock return 1954-2015
#            mean       std
# expansion:  0.0419  0.0547
# recession: -0.0192  0.1126


# Based on MS RW-drift model of GDP (regimes imported from python program)

# estimate seed 123 (?a local optimum, but consistent with hamilton1989)
#            mean       std
# expansion:  0.032   0.0685
# recession:  0.0049  0.1044

# estimate seed 127 (?global optimum, matches historical length of recessions best)
#            mean       std
# expansion: 0.028    0.0723
# recession: 0.0027   0.122
















