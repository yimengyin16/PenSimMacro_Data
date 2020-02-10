# This script is for modeling tax revenue of stylized governments

# 10/30/2019: 
# (5) generate tax revenue using total GDP growth the elasticities 


#**********************************************************************
#                           Packages                               ####
#**********************************************************************

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
# library(plm)
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

library(tidyverse)

options(tibble.print_max = 60, tibble.print_min = 60)



# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

#**********************************************************************
#                     Global settings and tools                    ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"
dir_fig_out <- "outputs_report/"


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
	log(x/dplyr::lag(x))
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

color_GDP = "darkgrey"


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

# Loading simulation outputs:

# load("policyBrief_out/sim_results_historical.RData")
# sim_results <- sim_results_historical

load(paste0(dir_data_out, "MacroModel_sim_results_forward_real.RData"))
#sim_results <- sim_results_forward




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




# dfs to use:
  # df_sim_gdp_y
  # df_sim_stockreturn_y
  # df_sim_bondreturn_y
sim_results$df_sim_gdp_y



df_sim <- 
	sim_results$df_sim_gdp_regimes_y %>% 
	left_join(sim_results$df_sim_gdp_y         %>% rename(gdp_chg     = return_y)) %>% 
	left_join(sim_results$df_sim_stockreturn_y %>% rename(stockreturn_real = return_y)) %>% 
	left_join(sim_results$df_sim_bondreturn_y  %>% rename(bondreturn_real  = return_y)) %>% 
	ungroup() %>% 
	mutate(sim = str_extract(sim, "\\d+") %>% as.numeric) %>% 
	mutate(#stockreturn_real = stockreturn  - infl,
				 #bondreturn_real  = bondreturn  - infl,
				 recessionYear    = recession_nqtr != 0 )

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


#**********************************************************************
#              Assumptions of stylized governments                 ####
#**********************************************************************

# Structure of tax revenue for stylized governments

taxShare_PITState   <- c(PIT = 0.55, 
							  		     salesGen = 0.2,
							  		     salesSel = 0.1,
							  		     other = 0.15,
												 propertyLoc = 0,
												 taxGDP = 0)

taxShare_salesState <- c(PIT = 0, 
									       salesGen = 0.6,
									       salesSel = 0.25,
									       other = 0.15,
												 propertyLoc = 0,
												 taxGDP = 0)


taxShare_GDPState <- c(PIT = 0, 
											 salesGen = 0,
											 salesSel = 0,
											 other = 0,
											 propertyLoc = 0,
											 taxGDP = 1)


taxShare_local <- c(PIT = 0, 
									  salesGen = 0,
									  salesSel = 0,
									  other = 0,
									  propertyLoc = 1,
										taxGDP = 0)




# Assumptions on tax growth

# Assumption 1

#df_sim_gdp_y$return_y %>% mean

trend_growth_gdp 
trend_growth_stock_real

taxGrowth_trend_1 <- 
	c(PIT         = -0.01,# trend_growth_gdp,
		salesGen    = -0.005,# trend_growth_gdp,
		salesSel    =  0.01,# trend_growth_gdp, 
		other       =  0,# trend_growth_gdp,
		propertyLoc = 0,# trend_growth_gdp,
		taxGDP      = 0 # rend_growth_gdp
		) 

taxGrowth_elastGDP_1 <- 
	c(PIT      = 1.1,
		salesGen = 1.3,
		salesSel = 0.5, 
		other    = 1.3,
		propertyLoc = 0,
		taxGDP   = 1) 

taxGrowth_elastStock_1 <- 
	c(PIT      = 0.2,
		salesGen = 0,
		salesSel = 0, 
		other    = 0,
		propertyLoc = 0,
		taxGDP   = 0) 


taxGrowth_adjRecession_1 <- 
	c(PIT      = 0,
		salesGen = 0,
		salesSel = 0, 
		other    = 0,
		propertyLoc = 0,
		taxGDP   = 0) 



# 
# # Assumption 2: sales tax with a constant base growth rate and low elasticity wrt GDP
# 
# taxGrowth_trend_2  <- 
# 	c(PIT      = trend_growth_gdp,
# 		salesGen = trend_growth_gdp,
# 		salesSel = trend_growth_gdp, 
# 		other    = trend_growth_gdp,
# 		propertyLoc = trend_growth_gdp,
# 		taxGDP   =  trend_growth_gdp) 
# 
# taxGrowth_elastGDP_2 <- 
# 	c(PIT      = 1,
# 	  salesGen = 0.9,
# 	  salesSel = 0.5, 
# 	  other    = 1.3,
# 		propertyLoc = 0,
# 		taxGDP   = 1) 
# 
# taxGrowth_elastStock_2 <- 
# 	c(PIT      = 0.2,
# 		salesGen = 0,
# 		salesSel = 0, 
# 		other    = 0,
# 		propertyLoc = 0,
# 		taxGDP   = 0) 
# 
# taxGrowth_adjRecession_2 <- 
# 	c(PIT      = 0,
# 		salesGen = 0,
# 		salesSel = 0, 
# 		other    = 0,
# 		propertyLoc = 0,
# 		taxGDP   = 0) 



#**********************************************************************
#    Calculating tax revenue for stylized governments              ####
#**********************************************************************

df_sim %>% head

# Compute trend and cycle growth for GDP and stock return
df_sim %<>% 
	mutate(gdp_chg_trend = trend_growth_gdp,
				 gdp_chg_cycle = gdp_chg - gdp_chg_trend,
				 stockreturn_real_trend = trend_growth_stock_real,
				 stockreturn_real_cycle = stockreturn_real - stockreturn_real_trend)




# Calculate tax growth
df_sim %<>% 
	mutate(
		     # Real growth rate for tax revenue under A1 
		     growthReal_PIT_a1         = taxGrowth_trend_1["PIT"]      + taxGrowth_elastGDP_1["PIT"]      * gdp_chg + taxGrowth_elastStock_1["PIT"] * stockreturn_real + taxGrowth_adjRecession_1["PIT"], 
				 growthReal_salesGen_a1    = taxGrowth_trend_1["salesGen"] + taxGrowth_elastGDP_1["salesGen"] * gdp_chg + taxGrowth_adjRecession_1["salesGen"],
				 growthReal_salesSel_a1    = taxGrowth_trend_1["salesSel"] + taxGrowth_elastGDP_1["salesSel"] * gdp_chg + taxGrowth_adjRecession_1["salesSel"],
				 growthReal_other_a1       = taxGrowth_trend_1["other"]    + taxGrowth_elastGDP_1["other"]    * gdp_chg + taxGrowth_adjRecession_1["other"],
		     growthReal_taxGDP_a1      = taxGrowth_trend_1["taxGDP"]   + taxGrowth_elastGDP_1["taxGDP"]   * gdp_chg,
				 growthReal_propertyLoc_a1 = trend_growth_gdp #  taxGrowth_trend_1["propertyLoc"] + taxGrowth_elastGDP_1["propertyLoc"] * gdp_chg + taxGrowth_adjRecession_1["propertyLoc"],
				 
		      ) %>% 
	group_by(sim) %>% 
	mutate(gdpLevel = cumprod(1 + gdp_chg),

				 # Future real tax revenue for $1 revenue in t_0.
				 taxLevelReal_PIT_a1      = cumprod(1 + growthReal_PIT_a1),
				 taxLevelReal_salesGen_a1 = cumprod(1 + growthReal_salesGen_a1),
				 taxLevelReal_salesSel_a1 = cumprod(1 + growthReal_salesSel_a1),
				 taxLevelReal_other_a1    = cumprod(1 + growthReal_other_a1),
				 taxLevelReal_propertyLoc_a1 = cumprod(1 + growthReal_propertyLoc_a1),
				 taxLevelReal_taxGDP_a1   = cumprod(1 + growthReal_taxGDP_a1),
				 
				 
				 # taxLevelReal_PIT_a2      = cumprod(1 + growthReal_PIT_a2),
				 # taxLevelReal_salesGen_a2 = cumprod(1 + growthReal_salesGen_a2),
				 # taxLevelReal_salesSel_a2 = cumprod(1 + growthReal_salesSel_a2),
				 # taxLevelReal_other_a2    = cumprod(1 + growthReal_other_a2),
				 # taxLevelReal_propertyLoc_a2 = cumprod(1 + growthReal_propertyLoc_a2),
				 # taxLevelReal_taxGDP_a2   = cumprod(1 + growthReal_taxGDP_a2),
				 
			# For PIT state
			   # tax revenue for each category, A1
				 taxLevelReal_PITState_PIT_a1         = taxShare_PITState["PIT"]      * taxLevelReal_PIT_a1,
				 taxLevelReal_PITState_salesGen_a1    = taxShare_PITState["salesGen"] * taxLevelReal_salesGen_a1,
				 taxLevelReal_PITState_salesSel_a1    = taxShare_PITState["salesSel"] * taxLevelReal_salesSel_a1,
				 taxLevelReal_PITState_other_a1       = taxShare_PITState["other"]    * taxLevelReal_other_a1,
				 taxLevelReal_PITState_propertyLoc_a1 = taxShare_PITState["propertyLoc"] * taxLevelReal_propertyLoc_a1,
				 
			   # total tax, A1
				 taxLevelReal_PITState_tot_a1 = taxLevelReal_PITState_PIT_a1 + taxLevelReal_PITState_salesGen_a1 + taxLevelReal_PITState_salesSel_a1 + 
				                              	taxLevelReal_PITState_other_a1 + taxLevelReal_PITState_propertyLoc_a1,
			   growthReal_tot_PITState_a1   = ifelse(year  == 1, taxLevelReal_PITState_tot_a1 - 1, taxLevelReal_PITState_tot_a1/dplyr::lag(taxLevelReal_PITState_tot_a1) - 1),
			
			
				 # Share of each tax category, A1
				 taxShare_PITState_PIT_a1         = taxLevelReal_PITState_PIT_a1   /     taxLevelReal_PITState_tot_a1,
				 taxShare_PITState_salesGen_a1    = taxLevelReal_PITState_salesGen_a1  / taxLevelReal_PITState_tot_a1,
				 taxShare_PITState_salesSel_a1    = taxLevelReal_PITState_salesSel_a1  / taxLevelReal_PITState_tot_a1,
				 taxShare_PITState_other_a1       = taxLevelReal_PITState_other_a1 /     taxLevelReal_PITState_tot_a1,
				 taxShare_PITState_propertyLoc_a1 = taxLevelReal_PITState_propertyLoc_a1/taxLevelReal_PITState_tot_a1,
				
			#    # tax revenue for each category, A2
			# 	 taxLevelReal_PITState_PIT_a2         = taxShare_PITState["PIT"] * taxLevelReal_PIT_a2,
			# 	 taxLevelReal_PITState_salesGen_a2    = taxShare_PITState["salesGen"] * taxLevelReal_salesGen_a2,
			# 	 taxLevelReal_PITState_salesSel_a2    = taxShare_PITState["salesSel"] * taxLevelReal_salesSel_a2,
			# 	 taxLevelReal_PITState_other_a2       = taxShare_PITState["other"] * taxLevelReal_other_a2,
			# 	 taxLevelReal_PITState_propertyLoc_a2 = taxShare_PITState["propertyLoc"] * taxLevelReal_propertyLoc_a2,
				 
			#    # total tax, A2
			# 	 taxLevelReal_PITState_tot_a2 = taxLevelReal_PITState_PIT_a2 + taxLevelReal_PITState_salesGen_a2 + taxLevelReal_PITState_salesSel_a2 + 
			# 	                                taxLevelReal_PITState_other_a2 + taxLevelReal_PITState_propertyLoc_a2,
			#    growthReal_tot_PITState_a2   = ifelse(year == 1, taxLevelReal_PITState_tot_a2 - 1, taxLevelReal_PITState_tot_a2/dplyr::lag(taxLevelReal_PITState_tot_a2) - 1),
			
			
		#      # Share of each tax category, A2
		# 	   taxShare_PITState_PIT_a1         = taxLevelReal_PITState_PIT_a1   /     taxLevelReal_PITState_tot_a1,
		# 	   taxShare_PITState_salesGen_a1    = taxLevelReal_PITState_salesGen_a1  / taxLevelReal_PITState_tot_a1,
		# 	   taxShare_PITState_salesSel_a1    = taxLevelReal_PITState_salesSel_a1  / taxLevelReal_PITState_tot_a1,
		# 	   taxShare_PITState_other_a1       = taxLevelReal_PITState_other_a1 /     taxLevelReal_PITState_tot_a1,
		# 	   taxShare_PITState_propertyLoc_a1 = taxLevelReal_PITState_propertyLoc_a1/taxLevelReal_PITState_tot_a1,
				 
			# For sales tax state
		    	# tax revenue for each category, A1
		    	taxLevelReal_salesState_PIT_a1         = taxShare_salesState["PIT"]      * taxLevelReal_PIT_a1,
		    	taxLevelReal_salesState_salesGen_a1    = taxShare_salesState["salesGen"] * taxLevelReal_salesGen_a1,
		    	taxLevelReal_salesState_salesSel_a1    = taxShare_salesState["salesSel"] * taxLevelReal_salesSel_a1,
		    	taxLevelReal_salesState_other_a1       = taxShare_salesState["other"]    * taxLevelReal_other_a1,
		    	taxLevelReal_salesState_propertyLoc_a1 = taxShare_salesState["propertyLoc"] * taxLevelReal_propertyLoc_a1,
		    	
		    	# total tax, A1
		    	taxLevelReal_salesState_tot_a1 = taxLevelReal_salesState_PIT_a1 + taxLevelReal_salesState_salesGen_a1 + taxLevelReal_salesState_salesSel_a1 + 
		    		                               taxLevelReal_salesState_other_a1 + taxLevelReal_salesState_propertyLoc_a1,
			    growthReal_tot_salesState_a1   = ifelse(year  == 1, taxLevelReal_salesState_tot_a1 - 1, taxLevelReal_salesState_tot_a1/dplyr::lag(taxLevelReal_salesState_tot_a1) - 1),
			
		    	# Share of each tax category, A1
		    	taxShare_salesState_PIT_a1         = taxLevelReal_salesState_PIT_a1   /     taxLevelReal_salesState_tot_a1,
		    	taxShare_salesState_salesGen_a1    = taxLevelReal_salesState_salesGen_a1  / taxLevelReal_salesState_tot_a1,
		    	taxShare_salesState_salesSel_a1    = taxLevelReal_salesState_salesSel_a1  / taxLevelReal_salesState_tot_a1,
		    	taxShare_salesState_other_a1       = taxLevelReal_salesState_other_a1 /     taxLevelReal_salesState_tot_a1,
		    	taxShare_salesState_propertyLoc_a1 = taxLevelReal_salesState_propertyLoc_a1/taxLevelReal_salesState_tot_a1,
		    	
		#     	# tax revenue for each category, A2
		#     	taxLevelReal_salesState_PIT_a2         = taxShare_salesState["PIT"] * taxLevelReal_PIT_a2,
		#     	taxLevelReal_salesState_salesGen_a2    = taxShare_salesState["salesGen"] * taxLevelReal_salesGen_a2,
		#     	taxLevelReal_salesState_salesSel_a2    = taxShare_salesState["salesSel"] * taxLevelReal_salesSel_a2,
		#     	taxLevelReal_salesState_other_a2       = taxShare_salesState["other"] * taxLevelReal_other_a2,
		#     	taxLevelReal_salesState_propertyLoc_a2 = taxShare_salesState["propertyLoc"] * taxLevelReal_propertyLoc_a2,
		#     	
		#     	# total tax, A2
		#     	taxLevelReal_salesState_tot_a2 = taxLevelReal_salesState_PIT_a2 + taxLevelReal_salesState_salesGen_a2 + taxLevelReal_salesState_salesSel_a2 + 
		#     		                               taxLevelReal_salesState_other_a2 + taxLevelReal_salesState_propertyLoc_a2,
		# 	    growthReal_tot_salesState_a2   = ifelse(year  == 1, taxLevelReal_salesState_tot_a2 - 1, taxLevelReal_salesState_tot_a2/dplyr::lag(taxLevelReal_salesState_tot_a2) - 1),
		# 	    
		# 	
		#     	# Share of each tax category, A2
		#     	taxShare_salesState_PIT_a1         = taxLevelReal_salesState_PIT_a1   /     taxLevelReal_salesState_tot_a1,
		#     	taxShare_salesState_salesGen_a1    = taxLevelReal_salesState_salesGen_a1  / taxLevelReal_salesState_tot_a1,
		#     	taxShare_salesState_salesSel_a1    = taxLevelReal_salesState_salesSel_a1  / taxLevelReal_salesState_tot_a1,
		#     	taxShare_salesState_other_a1       = taxLevelReal_salesState_other_a1 /     taxLevelReal_salesState_tot_a1,
		#     	taxShare_salesState_propertyLoc_a1 = taxLevelReal_salesState_propertyLoc_a1/taxLevelReal_salesState_tot_a1,	 
				 
			  # For local gov
			    # tax revenue for each category, A1
			    taxLevelReal_local_PIT_a1         = taxShare_local["PIT"]      * taxLevelReal_PIT_a1,
			    taxLevelReal_local_salesGen_a1    = taxShare_local["salesGen"] * taxLevelReal_salesGen_a1,
			    taxLevelReal_local_salesSel_a1    = taxShare_local["salesSel"] * taxLevelReal_salesSel_a1,
			    taxLevelReal_local_other_a1       = taxShare_local["other"]    * taxLevelReal_other_a1,
			    taxLevelReal_local_propertyLoc_a1 = taxShare_local["propertyLoc"] * taxLevelReal_propertyLoc_a1,
			    
			    # total tax, A1
			    taxLevelReal_local_tot_a1 = taxLevelReal_local_PIT_a1 + taxLevelReal_local_salesGen_a1 + taxLevelReal_local_salesSel_a1 + 
			                               	taxLevelReal_local_other_a1 + taxLevelReal_local_propertyLoc_a1,
			    growthReal_tot_local_a1   = ifelse(year  == 1, taxLevelReal_local_tot_a1 - 1, taxLevelReal_local_tot_a1/dplyr::lag(taxLevelReal_local_tot_a1) - 1),
			    
			    # Share of each tax category, A1
			    taxShare_local_PIT_a1         = taxLevelReal_local_PIT_a1   /     taxLevelReal_local_tot_a1,
			    taxShare_local_salesGen_a1    = taxLevelReal_local_salesGen_a1  / taxLevelReal_local_tot_a1,
			    taxShare_local_salesSel_a1    = taxLevelReal_local_salesSel_a1  / taxLevelReal_local_tot_a1,
			    taxShare_local_other_a1       = taxLevelReal_local_other_a1 /     taxLevelReal_local_tot_a1,
			    taxShare_local_propertyLoc_a1 = taxLevelReal_local_propertyLoc_a1/taxLevelReal_local_tot_a1,
			    
		# 	    # tax revenue for each category, A2
		# 	    taxLevelReal_local_PIT_a2         = taxShare_local["PIT"] * taxLevelReal_PIT_a2,
		# 	    taxLevelReal_local_salesGen_a2    = taxShare_local["salesGen"] * taxLevelReal_salesGen_a2,
		# 	    taxLevelReal_local_salesSel_a2    = taxShare_local["salesSel"] * taxLevelReal_salesSel_a2,
		# 	    taxLevelReal_local_other_a2       = taxShare_local["other"] * taxLevelReal_other_a2,
		# 	    taxLevelReal_local_propertyLoc_a2 = taxShare_local["propertyLoc"] * taxLevelReal_propertyLoc_a2,
		# 	    
		# 	    # total tax, A2
		# 	    taxLevelReal_local_tot_a2 = taxLevelReal_local_PIT_a2 + taxLevelReal_local_salesGen_a2 + taxLevelReal_local_salesSel_a2 + 
		# 	    	                          taxLevelReal_local_other_a2 + taxLevelReal_local_propertyLoc_a2,
		#     	growthReal_tot_local_a2   = ifelse(year  == 1, taxLevelReal_local_tot_a2 - 1, taxLevelReal_local_tot_a2/dplyr::lag(taxLevelReal_local_tot_a2) - 1),
		# 	
		# 	    # Share of each tax category, A2
		# 	    taxShare_local_PIT_a1         = taxLevelReal_local_PIT_a1   /     taxLevelReal_local_tot_a1,
		# 	    taxShare_local_salesGen_a1    = taxLevelReal_local_salesGen_a1  / taxLevelReal_local_tot_a1,
		# 	    taxShare_local_salesSel_a1    = taxLevelReal_local_salesSel_a1  / taxLevelReal_local_tot_a1,
		# 	    taxShare_local_other_a1       = taxLevelReal_local_other_a1 /     taxLevelReal_local_tot_a1,
		# 	    taxShare_local_propertyLoc_a1 = taxLevelReal_local_propertyLoc_a1/taxLevelReal_local_tot_a1,
			
			# For tax rev that grow proportionally with GDP
			
			# tax revenue for each category, A1
			# taxLevelReal_local_PIT_a1         = taxShare_local["PIT"]      * taxLevelReal_PIT_a1,
			# taxLevelReal_local_salesGen_a1    = taxShare_local["salesGen"] * taxLevelReal_salesGen_a1,
			# taxLevelReal_local_salesSel_a1    = taxShare_local["salesSel"] * taxLevelReal_salesSel_a1,
			# taxLevelReal_local_other_a1       = taxShare_local["other"]    * taxLevelReal_other_a1,
			taxLevelReal_GDPState_taxGDP_a1     = taxShare_GDPState["taxGDP"] * taxLevelReal_taxGDP_a1,
			
			# total tax, A1
			taxLevelReal_GDPState_tot_a1 = taxLevelReal_GDPState_taxGDP_a1,
			growthReal_tot_GDPState_a1   = ifelse(year  == 1, taxLevelReal_GDPState_tot_a1 - 1, taxLevelReal_GDPState_tot_a1/dplyr::lag(taxLevelReal_GDPState_tot_a1) - 1)
			
			# Share of each tax category, A1
			# taxShare_local_PIT_a1         = taxLevelReal_local_PIT_a1   /     taxLevelReal_local_tot_a1,
			# taxShare_local_salesGen_a1    = taxLevelReal_local_salesGen_a1  / taxLevelReal_local_tot_a1,
			# taxShare_local_salesSel_a1    = taxLevelReal_local_salesSel_a1  / taxLevelReal_local_tot_a1,
			# taxShare_local_other_a1       = taxLevelReal_local_other_a1 /     taxLevelReal_local_tot_a1,
			# taxShare_local_propertyLoc_a1 = taxLevelReal_local_propertyLoc_a1/taxLevelReal_local_tot_a1,
			
			# # tax revenue for each category, A2
			# # taxLevelReal_local_PIT_a2         = taxShare_local["PIT"]      * taxLevelReal_PIT_a2,
			# # taxLevelReal_local_salesGen_a2    = taxShare_local["salesGen"] * taxLevelReal_salesGen_a2,
			# # taxLevelReal_local_salesSel_a2    = taxShare_local["salesSel"] * taxLevelReal_salesSel_a2,
			# # taxLevelReal_local_other_a2       = taxShare_local["other"]    * taxLevelReal_other_a2,
			# taxLevelReal_GDPState_taxGDP_a2     = taxShare_GDPState["taxGDP"] * taxLevelReal_taxGDP_a2,
			# 
			# # total tax, A2
			# taxLevelReal_GDPState_tot_a2 = taxLevelReal_GDPState_taxGDP_a2,
			# growthReal_tot_GDPState_a2   = ifelse(year  == 1, taxLevelReal_GDPState_tot_a2 - 1, taxLevelReal_GDPState_tot_a2/dplyr::lag(taxLevelReal_GDPState_tot_a2) - 1)
			
			# Share of each tax category, A2
			# taxShare_local_PIT_a2         = taxLevelReal_local_PIT_a2   /     taxLevelReal_local_tot_a2,
			# taxShare_local_salesGen_a2    = taxLevelReal_local_salesGen_a2  / taxLevelReal_local_tot_a2,
			# taxShare_local_salesSel_a2    = taxLevelReal_local_salesSel_a2  / taxLevelReal_local_tot_a2,
			# taxShare_local_other_a2       = taxLevelReal_local_other_a2 /     taxLevelReal_local_tot_a2,
			# taxShare_local_propertyLoc_a2 = taxLevelReal_local_propertyLoc_a2/taxLevelReal_local_tot_a2
			
	) %>% 
	ungroup()
	
#df_sim %>% select(year, taxLevel_PITState_a1)
	
# Save results

save(df_sim, file = paste0(dir_data_out, "GovFin_sim_forward_tot.RData"))


#**********************************************************************
#       Illustration of a single simulation                        ####
#**********************************************************************

# Example simulation #2:

rec_year <- (df_sim %>% filter(sim == 2, recession_nqtr !=0))$year
rec_year

df_sim2 <- df_sim %>% filter(sim == 2)
save(df_sim2, file = paste0(dir_data_out, "df_sim2_tot.RData"))

fig_singleSim_GDP <- 
df_sim %>% filter(sim == 2) %>% 
	select(sim, year, gdp_chg, stockreturn_real) %>% 
	gather(var, value, -sim, - year) %>% 
	mutate(var = factor(var, levels = c("gdp_chg", "stockreturn_real"), labels = c("Real GDP growth", "Real stock return"))) %>% 
	ggplot(aes(x = year, y = 100 * value, color = var)) + theme_bw() + RIG.themeLite() + 
	geom_line() + 
	geom_point() + 
	geom_vline(xintercept = rec_year, linetype = 2, color = "grey") + 
	geom_hline(yintercept = 0, linetype = 2) +
	scale_x_continuous(breaks = c(1, seq(0, 30, 5)[-1]) ) + 
	scale_color_manual(values = c("darkgrey", RIG.red)) +
	labs(title = "Illustration of a single simulation (#2): real GDP growth and real stock return",
		   x = NULL, 
			 y = "Percent",
			 color = NULL) + 
	theme(legend.position = "bottom")
fig_singleSim_GDP



fig_singleSim_tax_a1 <- 
df_sim %>% filter(sim == 2) %>% 
	select(sim, year, growthReal_tot_PITState_a1, growthReal_tot_salesState_a1) %>% 
	gather(var, value, -sim, - year) %>% 
	mutate(var = factor(var, levels = c("growthReal_tot_PITState_a1", "growthReal_tot_salesState_a1"), 
											     labels = c("Income tax dominant state", "Sales tax dominant state"))) %>% 
	ggplot(aes(x = year, y = 100 * value, color = var, shape = var)) + theme_bw() + RIG.themeLite() + 
	geom_line() + 
	geom_point() + 
	geom_vline(xintercept = rec_year, linetype = 2, color = "grey") + 
	geom_hline(yintercept = 0, linetype = 2) + 
	coord_cartesian(ylim = c(-10, 10)) + 
	scale_x_continuous(breaks = c(1, seq(0, 30, 5)[-1]) ) + 
	scale_color_manual(values = c(RIG.blue, "deepskyblue1" )) +
	labs(title = "Illustration of a single simulation (#2): \nGrowth of real tax revenue for two types of stylized government",
		   x = NULL, 
			 y = "Percent",
			 color = NULL,
			 shape = NULL) + 
	theme(legend.position = "bottom") 
fig_singleSim_tax_a1 


df_sim %>% filter(sim == 2) %>% 
	select(sim, year, gdp_chg, growthReal_tot_PITState_a1, growthReal_tot_salesState_a1) %>% 
	gather(var, value, -sim, - year) %>% 
	mutate(var = factor(var, levels = c("gdp_chg", "growthReal_tot_PITState_a1", "growthReal_tot_salesState_a1"), 
											labels = c("GDP growth", "Income tax dominant state", "Sales tax dominant state"))) %>% 
	ggplot(aes(x = year, y = 100 * value, color = var, shape = var)) + theme_bw() + RIG.themeLite() + 
	geom_line() + 
	geom_point() + 
	geom_vline(xintercept = rec_year, linetype = 2, color = "grey") + 
	geom_hline(yintercept = 0, linetype = 2) + 
	coord_cartesian(ylim = c(-10, 10)) + 
	scale_x_continuous(breaks = c(1, seq(0, 30, 5)[-1]) ) + 
	scale_color_manual(values = c(color_GDP, RIG.blue, "deepskyblue1" )) +
	labs(title = "Illustration of a single simulation (#2): \nGrowth of real tax revenue for two types of stylized government",
			 x = NULL, 
			 y = "Percent",
			 color = NULL,
			 shape = NULL) + 
	theme(legend.position = "bottom") 
# sales tax state is extremely close to the GDP state.


# fig_singleSim_tax_a2 <- 
# df_sim %>% filter(sim == 2) %>% 
# 	select(sim, year, growthReal_tot_PITState_a2, growthReal_tot_salesState_a2) %>% 
# 	gather(var, value, -sim, - year) %>% 
# 	mutate(var = factor(var, levels = c("growthReal_tot_PITState_a2", "growthReal_tot_salesState_a2"), 
# 											labels = c("PIT dominant state", "Sales tax dominant state"))) %>% 
# 	ggplot(aes(x = year, y = 100 * value, color = var)) + theme_bw() + RIG.themeLite() + 
# 	geom_line() + 
# 	geom_point() + 
# 	geom_vline(xintercept = rec_year, linetype = 2, color = "grey") + 
# 	geom_hline(yintercept = 0, linetype = 2) +
# 	coord_cartesian(ylim = c(-10, 10)) + 
# 	scale_x_continuous(breaks = c(1, seq(0, 30, 5)[-1]) ) + 
# 	scale_color_manual(values = c(RIG.blue, "deepskyblue1")) +
# 	labs(title = "Illustration of a single simulation (#2): \nGrowth of real tax revenue of two types of stylized government (Assumption 2)",
# 		   x = NULL, 
# 			 y = "Percent",
# 			 color = NULL) + 
# 	theme(legend.position = "bottom")
# fig_singleSim_tax_a2 

ggsave(paste0(dir_fig_out, "fig_singleSim_GDP.png"), fig_singleSim_GDP, width = 10*0.8, height = 5*0.8)
ggsave(paste0(dir_fig_out, "fig_singleSim_tax_a1_tot.png"), fig_singleSim_tax_a1, width = 10*0.8, height = 5*0.8)
# ggsave(paste0(dir_fig_out, "fig_singleSim_tax_a2.png"), fig_singleSim_tax_a2, width = 10*0.8, height = 5*0.8)



#**********************************************************************
#                Distribution of simulated variables               ####
#**********************************************************************


df_sim %>% head

# quantiles
{ df_sim_quantiles <- 
df_sim %>% 
	group_by(year) %>% 
	summarise(
		gdp_chg_q10 = quantile(gdp_chg, 0.1),
		gdp_chg_q25 = quantile(gdp_chg, 0.25),
		gdp_chg_q50 = quantile(gdp_chg, 0.50),
		gdp_chg_q75 = quantile(gdp_chg, 0.75),
		gdp_chg_q90 = quantile(gdp_chg, 0.90),
		
		# stockreturn_q10 = quantile(stockreturn, 0.1),
		# stockreturn_q25 = quantile(stockreturn, 0.25),
		# stockreturn_q50 = quantile(stockreturn, 0.50),
		# stockreturn_q75 = quantile(stockreturn, 0.75),
		# stockreturn_q90 = quantile(stockreturn, 0.90),
		# 
		GrowthReal_tot_PITState_a1_q10 = quantile(growthReal_tot_PITState_a1, 0.1),
		GrowthReal_tot_PITState_a1_q25 = quantile(growthReal_tot_PITState_a1, 0.25),
		GrowthReal_tot_PITState_a1_q50 = quantile(growthReal_tot_PITState_a1, 0.50),
		GrowthReal_tot_PITState_a1_q75 = quantile(growthReal_tot_PITState_a1, 0.75),
		GrowthReal_tot_PITState_a1_q90 = quantile(growthReal_tot_PITState_a1, 0.90),
		
		# GrowthReal_tot_PITState_a2_q10 = quantile(growthReal_tot_PITState_a2, 0.1),
		# GrowthReal_tot_PITState_a2_q25 = quantile(growthReal_tot_PITState_a2, 0.25),
		# GrowthReal_tot_PITState_a2_q50 = quantile(growthReal_tot_PITState_a2, 0.50),
		# GrowthReal_tot_PITState_a2_q75 = quantile(growthReal_tot_PITState_a2, 0.75),
		# GrowthReal_tot_PITState_a2_q90 = quantile(growthReal_tot_PITState_a2, 0.90),
		# 
		GrowthReal_tot_salesState_a1_q25 = quantile(growthReal_tot_salesState_a1, 0.25),
		GrowthReal_tot_salesState_a1_q50 = quantile(growthReal_tot_salesState_a1, 0.50),
		GrowthReal_tot_salesState_a1_q75 = quantile(growthReal_tot_salesState_a1, 0.75),
		GrowthReal_tot_salesState_a1_q90 = quantile(growthReal_tot_salesState_a1, 0.90),
		GrowthReal_tot_salesState_a1_q10 = quantile(growthReal_tot_salesState_a1, 0.1),
		
		# GrowthReal_tot_salesState_a2_q10 = quantile(growthReal_tot_salesState_a2, 0.1),
		# GrowthReal_tot_salesState_a2_q25 = quantile(growthReal_tot_salesState_a2, 0.25),
		# GrowthReal_tot_salesState_a2_q50 = quantile(growthReal_tot_salesState_a2, 0.50),
		# GrowthReal_tot_salesState_a2_q75 = quantile(growthReal_tot_salesState_a2, 0.75),
		# GrowthReal_tot_salesState_a2_q90 = quantile(growthReal_tot_salesState_a2, 0.90),
		
		taxLevelReal_PITState_tot_a1_q10 = quantile(taxLevelReal_PITState_tot_a1, 0.1),
		taxLevelReal_PITState_tot_a1_q25 = quantile(taxLevelReal_PITState_tot_a1, 0.25),
		taxLevelReal_PITState_tot_a1_q50 = quantile(taxLevelReal_PITState_tot_a1, 0.50),
		taxLevelReal_PITState_tot_a1_q75 = quantile(taxLevelReal_PITState_tot_a1, 0.75),
		taxLevelReal_PITState_tot_a1_q90 = quantile(taxLevelReal_PITState_tot_a1, 0.90),
		
		# taxLevelReal_PITState_tot_a2_q10 = quantile(taxLevelReal_PITState_tot_a2, 0.1),
		# taxLevelReal_PITState_tot_a2_q25 = quantile(taxLevelReal_PITState_tot_a2, 0.25),
		# taxLevelReal_PITState_tot_a2_q50 = quantile(taxLevelReal_PITState_tot_a2, 0.50),
		# taxLevelReal_PITState_tot_a2_q75 = quantile(taxLevelReal_PITState_tot_a2, 0.75),
		# taxLevelReal_PITState_tot_a2_q90 = quantile(taxLevelReal_PITState_tot_a2, 0.90),
		
		taxLevelReal_salesState_tot_a1_q10 = quantile(taxLevelReal_salesState_tot_a1, 0.1),
		taxLevelReal_salesState_tot_a1_q25 = quantile(taxLevelReal_salesState_tot_a1, 0.25),
		taxLevelReal_salesState_tot_a1_q50 = quantile(taxLevelReal_salesState_tot_a1, 0.50),
		taxLevelReal_salesState_tot_a1_q75 = quantile(taxLevelReal_salesState_tot_a1, 0.75),
		taxLevelReal_salesState_tot_a1_q90 = quantile(taxLevelReal_salesState_tot_a1, 0.90),
		
		# taxLevelReal_salesState_tot_a2_q10 = quantile(taxLevelReal_salesState_tot_a2, 0.1),
		# taxLevelReal_salesState_tot_a2_q25 = quantile(taxLevelReal_salesState_tot_a2, 0.25),
		# taxLevelReal_salesState_tot_a2_q50 = quantile(taxLevelReal_salesState_tot_a2, 0.50),
		# taxLevelReal_salesState_tot_a2_q75 = quantile(taxLevelReal_salesState_tot_a2, 0.75),
		# taxLevelReal_salesState_tot_a2_q90 = quantile(taxLevelReal_salesState_tot_a2, 0.90)
	)
	}

df_sim_quantiles


# Box plots

fig_taxGrowth_box_a1 <- 
df_sim %>% 
	# filter(year == 15) %>% 
	select(year, sim,  growthReal_tot_PITState_a1, growthReal_tot_salesState_a1) %>% 
	gather(var, value, -year, -sim) %>% 
	mutate(var = factor(var, levels = c("growthReal_tot_PITState_a1", "growthReal_tot_salesState_a1", "growthReal_tot_local_a1"), 
											     labels = c("Income tax dominant state", "Sales tax dominant state", "Local government"))) %>% 
	ggplot(aes(x = var, y = 100 * value, fill = var)) + theme_bw() + RIG.themeLite() +
	geom_violin() + 
	geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") + 
	scale_fill_manual(values = c(RIG.blue, "deepskyblue1", RIG.yellow.dark)) +
	scale_y_continuous(breaks = seq(-20, 20, 2)) + 
	theme(legend.position = "none") + 
	labs(title = "Distributions of simulated growth in real total tax revenue \nfor the two stylized governments",
		   fill = NULL, 
			 x = NULL,
			 y = "Percent (%)")
fig_taxGrowth_box_a1

# df_sim$growthReal_tot_PITState_a1 %>% median # 2.44%

ggsave(paste0(dir_fig_out, "fig_GovFin_taxGrowth_box_a1_tot.png"), fig_taxGrowth_box_a1, width = 8*0.8, height = 7*0.8)


fig_taxGrowth_box_a1 <- 
	df_sim %>% 
	# filter(year == 15) %>% 
	select(year, sim,  growthReal_tot_PITState_a1, growthReal_tot_salesState_a1, growthReal_tot_GDPState_a1) %>% 
	gather(var, value, -year, -sim) %>% 
	mutate(var = factor(var, levels = c("growthReal_tot_PITState_a1", "growthReal_tot_salesState_a1", "growthReal_tot_GDPState_a1"), 
											labels = c("Income tax dominant state", "Sales tax dominant state", "GDP growth government"))) %>% 
	ggplot(aes(x = var, y = 100 * value, fill = var)) + theme_bw() + RIG.themeLite() +
	geom_violin() + 
	geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") + 
	scale_fill_manual(values = c(RIG.blue, "deepskyblue1", color_GDP)) +
	scale_y_continuous(breaks = seq(-20, 20, 2)) + 
	theme(legend.position = "none") + 
	labs(title = "Distributions of simulated growth in real total tax revenue \nfor the two stylized governments",
			 fill = NULL, 
			 x = NULL,
			 y = "Percent (%)")

fig_taxGrowth_box_a1





# Tax growth in recessions
# Box plots
df_sim %>% 
	filter(recessionYear == TRUE) %>% 
	select(year, sim,  growthReal_tot_PITState_a1, growthReal_tot_salesState_a1) %>% 
	gather(var, value, -year, -sim) %>% 
	ggplot(aes(x = var, y = value)) + 
	geom_boxplot(width = 0.5, outlier.shape = NA)


df_sim %>% 
	#filter(recessionYear == TRUE) %>% 
	select(year, sim,  growthReal_tot_PITState_a2, growthReal_tot_salesState_a2) %>% 
	gather(var, value, -year, -sim) %>% 
	ggplot(aes(x = var, y = value)) + 
	geom_boxplot(width = 0.5, outlier.shape = NA)


# Distribution of tax level

fig_taxLevel_PITState_a1 <- 
df_sim_quantiles %>% 
	select(year, 
				 taxLevelReal_PITState_tot_a1_q10,
				 taxLevelReal_PITState_tot_a1_q25,
				 taxLevelReal_PITState_tot_a1_q50,
				 taxLevelReal_PITState_tot_a1_q75,
				 taxLevelReal_PITState_tot_a1_q90
				 ) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, 
											levels = c("taxLevelReal_PITState_tot_a1_q90",
											           "taxLevelReal_PITState_tot_a1_q75",
											           "taxLevelReal_PITState_tot_a1_q50",
											           "taxLevelReal_PITState_tot_a1_q25",
											           "taxLevelReal_PITState_tot_a1_q10"), 
											labels = c("90th percentile",
																 "75th percentile", 
																 "50th percentile", 
																 "25th percentile",
																 "10th percentile"
											))) %>% 
	ggplot(aes(x = year, y = value, color = var)) +theme_bw() + RIG.themeLite() +
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(1, 3)) + 
	labs(title = "Distribution of simulated tax revenue relative to year 0 \n PIT dominant state (Assumption 1)",
		   x  = "Year",
			 y  = "Tax revenue relative to year 0",
			 color = NULL)
fig_taxLevel_PITState_a1	


fig_taxLevel_salesState_a1 <-
df_sim_quantiles %>% 
	select(year, 
				 taxLevelReal_salesState_tot_a1_q10,
				 taxLevelReal_salesState_tot_a1_q25,
				 taxLevelReal_salesState_tot_a1_q50,
				 taxLevelReal_salesState_tot_a1_q75,
				 taxLevelReal_salesState_tot_a1_q90
	) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, 
											levels = c("taxLevelReal_salesState_tot_a1_q90",
																 "taxLevelReal_salesState_tot_a1_q75",
																 "taxLevelReal_salesState_tot_a1_q50",
																 "taxLevelReal_salesState_tot_a1_q25",
																 "taxLevelReal_salesState_tot_a1_q10"), 
											labels = c("90th percentile",
																 "75th percentile", 
																 "50th percentile", 
																 "25th percentile",
																 "10th percentile"
											))) %>% 
	ggplot(aes(x = year, y = value, color = var)) +theme_bw() + RIG.themeLite() +
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(1, 2.5)) + 
	labs(title = "Distribution of simulated tax revenue relative to year 0 \n sales tax dominant state (Assumption 1)",
		   x  = "Year",
			 y  = "Tax revenue relative to year 0",
			 color = NULL)
fig_taxLevel_salesState_a1





ggsave(paste0(dir_fig_out, "fig_GovFin_taxLevel_PITState_a1_tot.png"),   fig_taxLevel_PITState_a1, width = 8*0.8, height = 6*0.8)
ggsave(paste0(dir_fig_out, "fig_GovFin_taxLevel_salesState_a1_tot.png"), fig_taxLevel_salesState_a1, width = 8*0.8, height = 6*0.8)

# ggsave(paste0(dir_fig_out, "fig_GovFin_taxLevel_PITState_a2.png"),   fig_taxLevel_PITState_a2, width = 8*0.8, height = 6*0.8)
# ggsave(paste0(dir_fig_out, "fig_GovFin_taxLevel_salesState_a2.png"), fig_taxLevel_salesState_a2, width = 8*0.8, height = 6*0.8)





#**********************************************************************
#              Changes in tax structure                            ####
#**********************************************************************
# Because of the "volatility drag", taxes with high volatility (income tax) may grow
# slower than taxes with lower volatility (sales tax, especially selective sales tax)

df_sim %>% 
	select(sim, year, taxShare_PITState_PIT_a1, 
				            taxShare_PITState_salesGen_a1,
				            taxShare_PITState_salesSel_a1,
				            taxShare_PITState_other_a1) %>% 
	group_by(year) %>% 
	summarise(
	share_PIT_q10 = quantile(taxShare_PITState_PIT_a1, 0.1),
 #share_PIT_q25 = quantile(taxShare_PITState_PIT_a1, 0.25),
  share_PIT_q50 = quantile(taxShare_PITState_PIT_a1, 0.50),
 #share_PIT_q75 = quantile(taxShare_PITState_PIT_a1, 0.75),
  share_PIT_q90 = quantile(taxShare_PITState_PIT_a1, 0.90),
	
	share_salesGen_q10 = quantile(taxShare_PITState_salesGen_a1, 0.1),
	#share_salesGen_q25 = quantile(taxShare_PITState_salesGen_a1, 0.25),
	share_salesGen_q50 = quantile(taxShare_PITState_salesGen_a1, 0.50),
	#share_salesGen_q75 = quantile(taxShare_PITState_salesGen_a1, 0.75),
	share_salesGen_q90 = quantile(taxShare_PITState_salesGen_a1, 0.90),
	
	share_salesSel_q10 = quantile(taxShare_PITState_salesSel_a1, 0.1),
 #share_salesGen_q25 = quantile(taxShare_PITState_salesSel_a1, 0.25),
	share_salesSel_q50 = quantile(taxShare_PITState_salesSel_a1, 0.50),
 #share_salesGen_q75 = quantile(taxShare_PITState_salesSel_a1, 0.75),
	share_salesSel_q90 = quantile(taxShare_PITState_salesSel_a1, 0.90)
   )
	
	df_sim %>% 
	select(sim, year, taxShare_salesState_PIT_a1, 
				 taxShare_salesState_salesGen_a1,
				 taxShare_salesState_salesSel_a1,
				 taxShare_salesState_other_a1) %>% 
	group_by(year) %>% 
	summarise(
		share_other_q10 = quantile(taxShare_salesState_other_a1, 0.1),
   #share_other_q25 = quantile(taxShare_salesState_other_a1, 0.25),
		share_other_q50 = quantile(taxShare_salesState_other_a1, 0.50),
   #share_other_q75 = quantile(taxShare_salesState_other_a1, 0.75),
		share_other_q90 = quantile(taxShare_salesState_other_a1, 0.90),
		
		share_salesGen_q10 = quantile(taxShare_salesState_salesGen_a1, 0.1),
		#share_salesGen_q25 = quantile(taxShare_salesState_salesGen_a1, 0.25),
		share_salesGen_q50 = quantile(taxShare_salesState_salesGen_a1, 0.50),
		#share_salesGen_q75 = quantile(taxShare_salesState_salesGen_a1, 0.75),
		share_salesGen_q90 = quantile(taxShare_salesState_salesGen_a1, 0.90),
		
		share_salesSel_q10 = quantile(taxShare_salesState_salesSel_a1, 0.1),
		#share_salesGen_q25 = quantile(taxShare_salesState_salesSel_a1, 0.25),
		share_salesSel_q50 = quantile(taxShare_salesState_salesSel_a1, 0.50),
		#share_salesGen_q75 = quantile(taxShare_salesState_salesSel_a1, 0.75),
		share_salesSel_q90 = quantile(taxShare_salesState_salesSel_a1, 0.90)
	)


df_salesState <- 
df_sim %>% 
	group_by(sim) %>% 
	summarise(geoMean_gdp = get_geoReturn(gdp_chg),
						share_salesGen_salesState = taxShare_salesState_salesGen_a1[30],
						share_salesSel_salesState = taxShare_salesState_salesSel_a1[30],
						share_other_salesState    = taxShare_salesState_other_a1[30]
						) %>% 
	arrange((geoMean_gdp))


df_PITState <- 
df_sim %>% 
	group_by(sim) %>% 
	summarise(geoMean_gdp = get_geoReturn(gdp_chg),
						share_PIT_PITState = taxShare_PITState_PIT_a1[30],
						share_salesGen_PITState = taxShare_PITState_salesGen_a1[30],
						share_salesSel_PITState = taxShare_PITState_salesSel_a1[30],
						share_other_PITState    = taxShare_PITState_other_a1[30]
	) %>% 
	arrange(desc(geoMean_gdp))


df_salesState %>% 
	gather(var, value, -sim, -geoMean_gdp) %>% 
	ggplot(aes(x = geoMean_gdp, y = value, color = var )) + theme_bw() + 
	geom_point(alpha = 0.2) +
	labs(x = "Geometric annual growth in the 30-year period",
			 y = "Share in total tax revenue in year 30") + 
	theme(legend.position = "bottom")


df_salesState %>% 
	mutate(elast = share_salesGen_salesState * 1.2 + 
				 	       share_salesSel_salesState * 0.5 + 
				 	       share_other_salesState * 1.3 ) %>% 
	ggplot(aes(x = geoMean_gdp, y = elast)) + theme_bw() + 
	geom_point(alpha = 0.2) +
	coord_cartesian(ylim = c(0.9, 1.10)) +
	scale_y_continuous(breaks = seq(0, 2, 0.02)) + 
	labs(x = "Geometric annual growth in the 30-year period",
			 y = "Elasticity of total tax wrt GDP growth in year 30")


df_PITState %>% 
	gather(var, value, -sim, -geoMean_gdp) %>% 
	ggplot(aes(x = geoMean_gdp, y = value, color = var )) + theme_bw() + 
	geom_point(alpha = 0.2) +
	labs(x = "Geometric annual growth in the 30-year period",
			 y = "Share in total tax revenue in year 30") + 
	theme(legend.position = "bottom")


df_PITState %>% 
	mutate(elast = share_salesGen_PITState * 1.2 + 
				 	       share_salesSel_PITState * 0.5 + 
				 	       share_other_PITState * 1.3 + 
				 	       share_PIT_PITState * 1) %>% 
	ggplot(aes(x = geoMean_gdp, y = elast)) + theme_bw() + 
	geom_point(alpha = 0.2) +
	coord_cartesian(ylim = c(0.9, 1.10)) +
	scale_y_continuous(breaks = seq(0, 2, 0.02)) + 
	labs(x = "Geometric annual growth in the 30-year period",
			 y = "Elasticity of total tax wrt GDP growth in year 30")






#**********************************************************************
#                Measure of volatility                             ####
#**********************************************************************

# Median standared deviation, Median average growth
df_sim %>% 
	group_by(sim) %>% 
	summarise(avg_taxGrowth_PITState_a1 = mean(growthReal_tot_PITState_a1),
						sd_taxGrowth_PITState_a1  = sd(growthReal_tot_PITState_a1),
						
						avg_taxGrowth_salesState_a1 = mean(growthReal_tot_salesState_a1),
						sd_taxGrowth_salesState_a1  = sd(growthReal_tot_salesState_a1)
						) %>% 
	summarise(Medavg_taxGrowth_PITState_a1 = median(avg_taxGrowth_PITState_a1),
						Medsd_taxGrowth_PITState_a1  = median(sd_taxGrowth_PITState_a1),
						
						Medavg_taxGrowth_salesState_a1 = median(avg_taxGrowth_salesState_a1),
						Medsd_taxGrowth_salesState_a1  = median(sd_taxGrowth_salesState_a1)
						)
						
# df_sim %>% 
# 	group_by(sim) %>% 
# 	summarise(avg_taxGrowth_PITState_a2 = mean(growthReal_tot_PITState_a2),
# 						sd_taxGrowth_PITState_a2  = sd(growthReal_tot_PITState_a2),
# 						
# 						avg_taxGrowth_salesState_a2 = mean(growthReal_tot_salesState_a2),
# 						sd_taxGrowth_salesState_a2  = sd(growthReal_tot_salesState_a2)
# 	) %>% 
# 	summarise(Medavg_taxGrowth_PITState_a2 = median(avg_taxGrowth_PITState_a2),
# 						Medsd_taxGrowth_PITState_a2  = median(sd_taxGrowth_PITState_a2),
# 						
# 						Medavg_taxGrowth_salesState_a2 = median(avg_taxGrowth_salesState_a2),
# 						Medsd_taxGrowth_salesState_a2  = median(sd_taxGrowth_salesState_a2)
# 	)			

# Probability of revenue dropping by more than, 3%, 5%, 10% in any years by year 30
df_prob <- 
df_sim %>% 
	group_by(sim) %>% 
	mutate(tax_drop3pct_PITState_a1   = ifelse(growthReal_tot_PITState_a1   <= -0.03, T, F) %>% cumany,
				 tax_drop3pct_salesState_a1 = ifelse(growthReal_tot_salesState_a1 <= -0.03, T, F) %>% cumany,
				 #tax_drop3pct_PITState_a2   = ifelse(growthReal_tot_PITState_a2   <= -0.03, T, F) %>% cumany,
				 #tax_drop3pct_salesState_a2 = ifelse(growthReal_tot_salesState_a2 <= -0.03, T, F) %>% cumany,
				 
				 tax_drop5pct_PITState_a1   = ifelse(growthReal_tot_PITState_a1   <= -0.05, T, F) %>% cumany,
				 tax_drop5pct_salesState_a1 = ifelse(growthReal_tot_salesState_a1 <= -0.05, T, F) %>% cumany,
				 #tax_drop5pct_PITState_a2   = ifelse(growthReal_tot_PITState_a2   <= -0.05, T, F) %>% cumany,
				 #tax_drop5pct_salesState_a2 = ifelse(growthReal_tot_salesState_a2 <= -0.03, T, F) %>% cumany,
				 
				 tax_drop10pct_PITState_a1   = ifelse(growthReal_tot_PITState_a1   <= -0.10, T, F) %>% cumany,
				 tax_drop10pct_salesState_a1 = ifelse(growthReal_tot_salesState_a1 <= -0.10, T, F) %>% cumany,
				 #tax_drop10pct_PITState_a2   = ifelse(growthReal_tot_PITState_a2   <= -0.10, T, F) %>% cumany,
				 #tax_drop10pct_salesState_a2 = ifelse(growthReal_tot_salesState_a2 <= -0.10, T, F) %>% cumany
				 ) %>% 
	group_by(year) %>% 
	summarize(tax_drop3pct_PITState_a1   = sum(tax_drop3pct_PITState_a1)/n(),
						tax_drop3pct_salesState_a1 = sum(tax_drop3pct_salesState_a1)/n(),
						#tax_drop3pct_PITState_a2   = sum(tax_drop3pct_PITState_a2)/n(),
						#tax_drop3pct_salesState_a2 = sum(tax_drop3pct_salesState_a2)/n(),
						
						tax_drop5pct_PITState_a1   = sum(tax_drop5pct_PITState_a1)/n(),
						tax_drop5pct_salesState_a1 = sum(tax_drop5pct_salesState_a1)/n(),
						#tax_drop5pct_PITState_a2   = sum(tax_drop5pct_PITState_a2)/n(),
						#tax_drop5pct_salesState_a2 = sum(tax_drop5pct_salesState_a2)/n(),

						tax_drop10pct_PITState_a1   = sum(tax_drop10pct_PITState_a1)/n(),
						tax_drop10pct_salesState_a1 = sum(tax_drop10pct_salesState_a1)/n(),
						#tax_drop10pct_PITState_a2   = sum(tax_drop10pct_PITState_a2)/n(),
						#tax_drop10pct_salesState_a2 = sum(tax_drop10pct_salesState_a2)/n()						
						)
	

fig_drop3pct_a1 <- 
df_prob %>% 
	select(year, tax_drop3pct_PITState_a1, tax_drop3pct_salesState_a1) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, levels = c("tax_drop3pct_PITState_a1", "tax_drop3pct_salesState_a1"),
													 labels = c("Income tax dominant state", "Sales tax dominant state")					
																			)) %>% 
	ggplot(aes(x = year, y = 100*value, color = var)) + theme_bw() + RIG.themeLite() + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0, 100)) + 
	scale_color_manual(values = c(RIG.blue, "deepskyblue1")) + 
	scale_x_continuous(breaks = seq(0, 30, 5)) + 
	scale_y_continuous(breaks = seq(0, 100,10)) + 
	theme(legend.position = "bottom") + 
	labs(title = "Probability of a decrease in real total tax revenue of 3% or larger \nup to the given year",
		   x = "Year",
			 y = "Probability (%)",
			 color = NULL)
fig_drop3pct_a1

fig_drop5pct_a1 <- 
df_prob %>% 
	select(year, tax_drop5pct_PITState_a1, tax_drop5pct_salesState_a1) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, 
											levels = c("tax_drop5pct_PITState_a1", "tax_drop5pct_salesState_a1"),
											labels = c("Income tax dominant state", "Sales tax dominant state")					
	)) %>% 
	ggplot(aes(x = year, y = 100*value, color = var)) + theme_bw() + RIG.themeLite() + 
	geom_line() + 
	geom_point()+
	theme(legend.position = "bottom") + 
	coord_cartesian(ylim = c(0, 100)) + 
	scale_color_manual(values = c(RIG.blue, "deepskyblue1")) +
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
	scale_y_continuous(breaks = seq(0, 100,10)) + 
	theme(legend.position = "bottom") + 
	labs(title = "Probability of a decrease in real total tax revenue of 5% or larger \nup to the given year",
		   x = "Year",
			 y = "Probability (%)",
			 color = NULL)
fig_drop5pct_a1






## prob of a drop greater than 10% is every low
# df_prob %>% 
# 	select(year, tax_drop10pct_PITState_a1, tax_drop10pct_salesState_a1) %>% 
# 	gather(var, value, -year) %>% 
# 	ggplot(aes(x = year, y = 100*value, color = var)) + theme_bw() + 
# 	geom_line() + 
# 	geom_point()+
# 	theme(legend.position = "bottom")


# Probability of revenue dropping by more than, 3%, 5%, 10% in recession years by year 30

fn <- function(s){

x <- 0

#s <- c(T, T, F, T,T)
n <- length(s)
m <- numeric(n)

if(s[1] == T) {m[1] <- 1; x<- 1}  else {m[1] <- 0; x <- 0}
	

for(i in 2:n){
	if(s[i] == FALSE) m[i] <- 0
	if(s[i-1] == FALSE & s[i] == TRUE) {x <- x + 1; m[i] <-  x}
  if(s[i-1] == TRUE & s[i] == TRUE)  m[i] <-  x
}
m
}

fn(c(T, T, F, T,T))
fn(c(F, T, F, T,T))

 
df_sim %<>% 
	group_by(sim) %>% 
	mutate(recession_num = fn(recessionYear))

df_sim %>% 
	filter(recessionYear == TRUE) %>% 
	select(sim, year, recessionYear, recession_num, 
				 growthReal_tot_PITState_a1, growthReal_tot_salesState_a1,
				 growthReal_tot_PITState_a2, growthReal_tot_salesState_a2) %>% 
	group_by(sim, recession_num) %>% 
	summarise(chgRec_PITState_a1   = prod(1 + growthReal_tot_PITState_a1) - 1,
						chgRec_salesState_a1 = prod(1 + growthReal_tot_salesState_a1) - 1,
						
						chgRec_PITState_a2   = prod(1 + growthReal_tot_PITState_a2) - 1,
						chgRec_salesState_a2 = prod(1 + growthReal_tot_salesState_a2) - 1
						) %>% 
	group_by(sim) %>% 
	summarise(
		drop5rec_PITState_a1   =  any(chgRec_PITState_a1 <= -0.05),
		drop5rec_salesState_a1 =  any(chgRec_salesState_a1 <= -0.05),
		drop5rec_PITState_a2   =  any(chgRec_PITState_a2 <= -0.05),
		drop5rec_salesState_a2 =  any(chgRec_salesState_a2 <= -0.05),
		
		drop10rec_PITState_a1   =  any(chgRec_PITState_a1 <= -0.1),
		drop10rec_salesState_a1 =  any(chgRec_salesState_a1 <= -0.1),
		drop10rec_PITState_a2   =  any(chgRec_PITState_a2 <= -0.1),
		drop10rec_salesState_a2 =  any(chgRec_salesState_a2 <= -0.1)
	) %>% 
	summarise(
		drop5rec_PITState_a1   =  sum(drop5rec_PITState_a1)/n(),
		drop5rec_salesState_a1 =  sum(drop5rec_salesState_a1)/n(),
		drop5rec_PITState_a2   =  sum(drop5rec_PITState_a2)/n(),
		drop5rec_salesState_a2 =  sum(drop5rec_salesState_a2)/n(),
		
		drop10rec_PITState_a1   =  sum(drop10rec_PITState_a1)/n(),
		drop10rec_salesState_a1 =  sum(drop10rec_salesState_a1)/n(),
		drop10rec_PITState_a2   =  sum(drop10rec_PITState_a2)/n(),
		drop10rec_salesState_a2 =  sum(drop10rec_salesState_a2)/n()
	)
	

ggsave(paste0(dir_fig_out, "fig_GovFin_drop3pct_a1_tot.png"),   fig_drop3pct_a1, width = 8*0.8, height = 8*0.8)
ggsave(paste0(dir_fig_out, "fig_GovFin_drop5pct_a1_tot.png"),   fig_drop5pct_a1, width = 8*0.8, height = 8*0.8)

# ggsave(paste0(dir_fig_out, "fig_GovFin_drop3pct_a2.png"),   fig_drop3pct_a2, width = 8*0.8, height = 8*0.8)
# ggsave(paste0(dir_fig_out, "fig_GovFin_drop5pct_a2.png"),   fig_drop5pct_a2, width = 8*0.8, height = 8*0.8)





# 
# # Growth rate of total tax revenue
# growthReal_tot_PITState_a1   = growthReal_PIT_a1 * taxShare_PITState["share_PIT"] + growthReal_salesGen_a1 * taxShare_PITState["share_salesGen"] + 
# 	growthReal_salesSel_a1 * taxShare_PITState["share_salesSel"] + growthReal_other_a1 * taxShare_PITState["share_other"] + growthReal_propertyLoc_a1 * taxShare_PITState["share_propertyLoc"],
# 
# growthReal_tot_salesState_a1 = growthReal_PIT_a1 * taxShare_salesState["share_PIT"] + growthReal_salesGen_a1 * taxShare_salesState["share_salesGen"] + 
# 	growthReal_salesSel_a1 * taxShare_salesState["share_salesSel"] + growthReal_other_a1 * taxShare_salesState["share_other"] + growthReal_propertyLoc_a1 * taxShare_salesState["share_propertyLoc"],
# 
# growthReal_tot_local_a1      = growthReal_PIT_a1 * taxShare_local["share_PIT"] + growthReal_salesGen_a1 * taxShare_local["share_salesGen"] + 
# 	growthReal_salesSel_a1 * taxShare_local["share_salesSel"] + growthReal_other_a1 * taxShare_local["share_other"] + growthReal_propertyLoc_a1 * taxShare_local["share_propertyLoc"],
# 
# 
# growthReal_tot_PITState_a2   = growthReal_PIT_a2 * taxShare_PITState["share_PIT"] + growthReal_salesGen_a2 * taxShare_PITState["share_salesGen"] + 
# 	growthReal_salesSel_a2 * taxShare_PITState["share_salesSel"] + growthReal_other_a2 * taxShare_PITState["share_other"] + growthReal_propertyLoc_a2 * taxShare_PITState["share_propertyLoc"],
# 
# growthReal_tot_salesState_a2 = growthReal_PIT_a2 * taxShare_salesState["share_PIT"] + growthReal_salesGen_a2 * taxShare_salesState["share_salesGen"] + 
# 	growthReal_salesSel_a2 * taxShare_salesState["share_salesSel"] + growthReal_other_a2 * taxShare_salesState["share_other"] + growthReal_propertyLoc_a2 * taxShare_salesState["share_propertyLoc"],
# 
# growthReal_tot_local_a2      = growthReal_PIT_a2 * taxShare_local["share_PIT"] + growthReal_salesGen_a2 * taxShare_local["share_salesGen"] + 
# 	growthReal_salesSel_a2 * taxShare_local["share_salesSel"] + growthReal_other_a2 * taxShare_local["share_other"] + growthReal_propertyLoc_a2 * taxShare_local["share_propertyLoc"]
# 


# df_temp <- 
# 	df_sim %>% 
# 	group_by(sim) %>% 
# 	summarise(growthReal_tot_PITState_a1        = get_geoReturn(growthReal_tot_PITState_a1),
# 						growthReal_tot_salesState_a1      = get_geoReturn(growthReal_tot_salesState_a1),
# 						growthReal_tot_local_a1      = get_geoReturn(growthReal_tot_local_a1))
# 
# df_temp$growthReal_tot_PITState_a1 %>% mean
# df_temp$growthReal_tot_salesState_a1 %>% mean
# df_temp$growthReal_tot_local_a1 %>% mean
# 
# df_temp$growthReal_tot_PITState_a1 %>% median
# df_temp$growthReal_tot_salesState_a1 %>% median
# df_temp$growthReal_tot_local_a1 %>% median
# 
# df_sim %>% select(growthReal_tot_l)
# 







