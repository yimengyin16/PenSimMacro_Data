# This script is for modeling tax revenue of stylized governments


#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(tidyverse)
library(broom)
library(readxl)
library(magrittr)
library(ggrepel)
library(stringr)
library(forcats)
library(grid)
library(gridExtra)
library(scales)
library(knitr)

library(xlsx)

# packages for econometric and time series modeling
library(plm)
library(astsa)    # companion package
library(TSA)      # companion package;  arimax: flexible transfer function model
library(tseries)  #
library(forecast) # Arima
library(MSwM)
library(TTR)
library(dynlm)
library(broom)

#library(MSBVAR)

# packages for ts
library(zoo)
library(xts)


library(timetk)
library(tidyquant)

library(lubridate)
library(feather)

library(psych) # describe

options(tibble.print_max = 60, tibble.print_min = 60)


# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

#**********************************************************************
#                     Global settings and tools                    ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"
dir_fig_out <- "policyBrief_out/"


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


get_logReturn <- function(x){
	if(any(x <= 0, na.rm = TRUE)) stop("Nagative value(s)")
	log(x/lag(x))
}




# RIG colors and theme
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


get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1




#**********************************************************************
#                          Outline                                 ####
#**********************************************************************

# Goals:
  # Path of state tax revenue derived from simulated GDP, stock returns, and estimated elasticities. 
  # What to have in the results
  #   1. a deterministic simulation with constant GDP growth and stock return
  #   2. A single stochastic simulation, how different stylized governments respond to GDP and stocks differently
  #   3. A scenario, if hard to find a stochastic simulation that makes sense, construct a scenario that is similar to history. 
  #   4. Distribution of 2000 simulations (quanitles)
  #   5. Risk measures: probability of sharp decline in tax revenue, and difference stylized governments. 
 

# Calculating growth rates of tax revenues

# Trend growth rates: 
  # - Trend GDP growth rates (real)
  #     - CBO projection: 1.9%
  #     - Need to determined mean (and SD) in recession and expansion. (currently we may want to use SD estimated using historical data)
  # - Trend stock returns (real)
  #     - Trend real return =  Assumed overall nominal mean stock return (expansion and recession) -  assumed inflation  
  #     - Issue:
  #           1. Estimated mean stock return is different from projected mean capital return. When using projected mean, how should the SD be adjusted? 
  #           2. With a target overall mean stock return (and SD), how to determine mean return (and SD) in recession and expansion periods.  
  # - Trend growth of taxes:
  #     - Use assumed trend growth rates
  #     - To ensure that the share of each major tax category stays stable over the long run, 
  #       we assume that the trend growth rates of all tax categories equal the trend GDP growth rates (CBO projection: 1.9%)

# Cyclical growth rates:
  # - Cyclical GDP growth rate: (real)
  #     - Cyclical GDP growth = simulated real rate - assumed overall trend
  #     - Potential issue: Growth will be above trend in expansion periods. (Since recessions are not very often, overall trend should be close to rate in expansion)
  #     - Issue to think about: do we need use a different trend for each single simulation?
  # - Cyclical Stock returns (real)
  #     - Cyclical real stock return = simulated real rate - assumed overall trend
  # - Cyclical tax growth 
  #      - cyclical tax growth = e_GDP*GDP_cycle + e_stock*stock_cycle + recession adjustment 
 
# Converting to nominal rates:
  # - adding assumed inflation rates to simulated real rates. 
  # - Need to check whether the nominal numbers make sense, especially the asset returns (will be used in pension finance model)
 


#**********************************************************************
#     Importing simulations of GDP growth and stock return         ####
#**********************************************************************

# Notes
  # 1. Simulations are generated by Model_simulation(3).R
  # 2. What inputs to include:
  #    - simulated path of real GDP growth
  #    - Recession and expansion periods in each simulation
  #    - simulated path stock return
  #    - simulated path of bond return


# Assumptions:

infl_hist <- 0.022 #(CBO, GDP price index: 1987-2016)
infl_proj <- 0.02  #(CBO, GDP price index: 2017-2047)

infl <- infl_proj


# Loading simulation outputs:

# load("policyBrief_out/sim_results_historical.RData")
# sim_results <- sim_results_historical

load("policyBrief_out/sim_results_forward.RData")
#sim_results <- sim_results_forward

# dfs to use:
  # df_sim_gdp_y
  # df_sim_stockreturn_y
  # df_sim_bondreturn_y


df_sim <- 
	sim_results$df_sim_gdp_regimes_y %>% 
	left_join(sim_results$df_sim_gdp_y         %>% rename(gdp_chg     = return_y)) %>% 
	left_join(sim_results$df_sim_stockreturn_y %>% rename(stockreturn = return_y)) %>% 
	left_join(sim_results$df_sim_bondreturn_y  %>% rename(bondreturn  = return_y)) %>% 
	ungroup() %>% 
	mutate(sim = str_extract(sim, "\\d+") %>% as.numeric) %>% 
	mutate(stockreturn_real = stockreturn  - infl,
				 bondreturn_real        = bondreturn  - infl,
				 recessionYear = recession_nqtr != 0 )

df_sim %>% head


# Calculating geometric mean of GDP growth and stock return

sim_geoMeans <- 
	df_sim %>% 
	group_by(sim) %>% 
	summarise(gdp_chg_geoMean          = get_geoReturn(gdp_chg),
						stockReturn_real_geoMean = get_geoReturn(stockreturn_real),
						bondReturn_real_geoMean  = get_geoReturn(bondreturn_real))

trend_growth_gdp        <- sim_geoMeans$gdp_chg_geoMean %>% mean
trend_growth_stock_real <- sim_geoMeans$stockReturn_real_geoMean %>% mean
trend_growth_bond_real  <- sim_geoMeans$bondReturn_real_geoMean %>% mean


trend_growth_gdp 
trend_growth_stock_real
trend_growth_bond_real

df_sim$stockreturn %>% mean

head(df_sim)



stock_pct <- 0.7
bond_pct  <- 0.3 


penSimInputs_returns <- 
  df_sim %>% 
	  select(sim, year, stockreturn, bondreturn) %>% 
	  mutate(return70_30_nom = stockreturn * stock_pct + bondreturn * bond_pct)

save(penSimInputs_returns, file = "policyBrief_out/penSimInputs_returns.RData")












