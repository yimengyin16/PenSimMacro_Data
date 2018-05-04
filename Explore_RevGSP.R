# This script explore the relationship between GDP and gov revenue and does exploratory modeling 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Relationships to examine:
  # 1. GSP growth and total own soure revenue growth,
  # 2. GSP growth and total tax revenue growth
  # 3. GSP growth and personal income tax growth
  # 4. GSP growth and sales tax growth
  # 5. GSP growth and property tax growth
  # 6. GSP growth and corporate income tax growth
  # 7. GSP growth and growth of total tax revenue minus PIT and sales

  # 8. national GDP growth and GSP growth


# Notes on real and nominal variables
#  - results for nominal variables for 1995-2015 can be compared with Pew results
#  - GDP/GSP are modeled in real terms in the macro model, so the GDP-revenue relationship should be
#    modeld using real term. But note that pension contributions are modeled in nominal terms, which 
#    must be compared against nominal gov revenue. 
#  - Real revenue variables are in 2015 dollar and adjustment factor for inflation is the same across
#    all states, gov levels and types of tax, while real GSPs are in 2009 dollar and adjustment factors 
#    for inflation differ across states.


# How sales taxes are divided between state and local governments:
 # https://taxfoundation.org/state-and-local-sales-tax-rates-in-2017/
 # Five states do not have statewide sales taxes: Alaska, Delaware, Montana, New Hampshire, and Oregon. 
 # Of these, Alaska and Montana allow localities to charge local sales taxes.



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

options(tibble.print_max = 60, tibble.print_min = 60)


# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

#**********************************************************************
#                     Global settings and tools                    ####
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


get_logReturn <- function(x){
	if(any(x <= 0, na.rm = TRUE)) stop("Nagative value(s)")
	log(x/lag(x))
}


#**********************************************************************
#                       Loading Data                               ####
#**********************************************************************
# Loading saved data 
load(paste0(dir_data_out, "data_RevGSP.RData"))
load(paste0(dir_data_out, "dataAll.RData"))


# df_RevGSP loaded

# Variables in df_RevGSP
#  Indices: state, state_abb, year
#  GSP variables
#     - RGSP_SIC:   From BEA,  1963-1997, based on SIC
#     - RGSP_NAICS: From BEA,  1997-2016, based on NAICS
#     - NGSP:       From FRED, 1997-2016
#  Tax and revenue variables: 1977-2015
#     - suffix for gov levels: 
#        - local
#        - state
#        - SL: state and local
#     - suffix for real or nominal: real / nom
# 
#     - variables:   
#      urban code    Var name					  Var description
#        'R01',     'rev_tot',          'Total revenue', 
#        'R02',     'rev_tot_ownSrc',   'Total Rev-Own Sources',
#        'R03',     'rev_gen',          'General Revenue',
#        'R04',     'rev_gen_ownSrc',   'Gen Rev-Own Sources',
#        'R05',     'tax_tot',          'Total Taxes',
#        'R06',     'tax_property',     'Property Taxes',
#        'R08',     'tax_sales_tot',    'Tot Sales & Gr Rec Tax',
#        'R09',     'tax_sales_gen',    'Total Gen Sales Tax (T09)',
#        'R10',     'tax_sales_select', 'Total select Sales Tax',
#        'R27',     'tax_indivIncome',  'Individual Income Tax (T40)',
#        'R28',     'tax_corpIncome',   'Corp Net Income Tax',
#        'R36',     'chgs_Misc',        'Tot Chgs and Misc Rev'
#      - with missing values 


#**********************************************************************
#                      Creating variables                          ####
#**********************************************************************


# Relationship to examine:
# 1. GSP growth and total own soure revenue growth,
# 2. GSP growth and total tax revenue growth
# 3. GSP growth and personal income tax growth
# 4. GSP growth and sales tax growth
# 5. GSP growth and property tax growth
# 6. GSP growth and corporate income tax growth
# 7. GSP growth and growth of total tax revenue minus PIT and sales

# price index variables, just in case
df_inflation <- 
   df_dataAll_y %>% select(year, CPIU_SA_FRED, CPIU_NA_FRED, CPIc_SA_FRED, Inflation_Index)
df_inflation

# Vars in real terms.
df_real <- 
	df_RevGSP %>% 
	select(state_abb, year, 
				 RGSP_SIC, 
				 RGSP_NAICS, 
				 rev_gen_ownSrc_real_SL, 
				 tax_tot_real_SL, 
				 tax_indivIncome_real_SL, 
				 tax_sales_tot_real_SL, 
				 tax_property_real_SL, 
				 tax_corpIncome_real_SL) %>% 
	filter(state_abb %in% df_us_states$state_abb) 
df_real

df_nominal <- 
	df_RevGSP %>% 
	select(state_abb, year, 
				 RGSP_SIC, 
				 RGSP_NAICS, 
				 rev_gen_ownSrc_nom_state, 
				 rev_gen_ownSrc_nom_local, 
				 rev_gen_ownSrc_nom_SL, 
				 tax_tot_nom_SL, 
				 tax_indivIncome_nom_SL, 
				 tax_sales_tot_nom_SL, 
				 tax_property_nom_SL, 
				 tax_corpIncome_nom_SL) %>% 
	filter(state_abb %in% df_us_states$state_abb) 



df_dl_real <-  
  df_real %>% 
	#filter(year>1978) %>% 
	group_by(state_abb) %>% 
	mutate_at(vars(-year), funs(100 * log(./lag(.))))

df_dl_nominal <-  
	df_nominal %>% 
	#filter(year>1978) %>% 
	group_by(state_abb) %>% 
	mutate_at(vars(-year), funs(100 * log(./lag(.))))


# df1 <- 
# 	df_RevGSP %>% 
# 	select(state_abb, year, 
# 				 rev_gen_ownSrc_nom_state, 
# 				 rev_gen_ownSrc_nom_local, 
# 				 rev_gen_ownSrc_nom_SL, 
# 				 
# 				 tax_tot_nom_state,
# 				 tax_tot_nom_local,
# 				 tax_tot_nom_SL,
# 				 
# 				 
# 				 tax_indivIncome_nom_state,
# 				 tax_indivIncome_nom_local,
# 				 tax_indivIncome_nom_SL,
# 				 
# 				 tax_sales_tot_nom_state,
# 				 tax_sales_tot_nom_local, 
# 				 tax_sales_tot_nom_SL, 
# 			
# 				 tax_property_nom_state,
# 				 tax_property_nom_local, 
# 				 tax_property_nom_SL,
# 				 
# 				 tax_corpIncome_nom_state,
# 				 tax_corpIncome_nom_local,
# 				 tax_corpIncome_nom_SL) %>% 
# 	filter(state_abb %in% df_us_states$state_abb) 
# 


#*******************************************************************************
#  1. Revenue structure: General own source revenue and total tax revneu    ####
#*******************************************************************************

# Qustion 1: Comparing own source revenue and Tax revenue
 # Variables: rev_gen_ownSrc_nom_XX, tax_tot_nom_XX

df <- 
  df_RevGSP %>% 
	filter(year>=1977) %>% 
	select(state_abb, year, 
				 rev_gen_ownSrc_nom_SL,
				 rev_gen_ownSrc_nom_state,
				 rev_gen_ownSrc_nom_local,
				 
				 tax_tot_nom_SL,
				 tax_tot_nom_state,
				 tax_tot_nom_local
				 ) %>% 
	mutate(tax_pct_SL    = 100 * tax_tot_nom_SL / rev_gen_ownSrc_nom_SL,
				 tax_pct_state = 100 * tax_tot_nom_state / rev_gen_ownSrc_nom_state,
				 tax_pct_local = 100 * tax_tot_nom_local / rev_gen_ownSrc_nom_local)


df_US <- 
	df %>%  
	filter(state_abb %in% "US") %>% 
	select(state_abb, year, 
				 tax_pct_SL, tax_pct_state, tax_pct_local)

df_pct <- 
df %>%
	group_by(year) %>% 
	summarise(
		tax_pct_SL_p10 = quantile(tax_pct_SL, 0.10, na.rm = T),
		tax_pct_SL_p25 = quantile(tax_pct_SL, 0.25, na.rm = T),
		tax_pct_SL_p50 = quantile(tax_pct_SL, 0.50, na.rm = T),
		tax_pct_SL_p75 = quantile(tax_pct_SL, 0.75, na.rm = T),
		tax_pct_SL_p90 = quantile(tax_pct_SL, 0.90, na.rm = T),
		
		tax_pct_state_p10 = quantile(tax_pct_state, 0.10, na.rm = T),
		tax_pct_state_p25 = quantile(tax_pct_state, 0.25, na.rm = T),
		tax_pct_state_p50 = quantile(tax_pct_state, 0.50, na.rm = T),
		tax_pct_state_p75 = quantile(tax_pct_state, 0.75, na.rm = T),
		tax_pct_state_p90 = quantile(tax_pct_state, 0.90, na.rm = T),
		
		tax_pct_local_p10 = quantile(tax_pct_local, 0.10, na.rm = T),
		tax_pct_local_p25 = quantile(tax_pct_local, 0.25, na.rm = T),
		tax_pct_local_p50 = quantile(tax_pct_local, 0.50, na.rm = T),
		tax_pct_local_p75 = quantile(tax_pct_local, 0.75, na.rm = T),
		tax_pct_local_p90 = quantile(tax_pct_local, 0.90, na.rm = T)
	)


df_US %>% filter(year %in% c(1977, 1990, 2000, 2010, 2015))

df_arrange <- 
  df %>% select(state_abb, year, tax_pct_SL, tax_pct_state, tax_pct_local) %>% 
    filter(year  == 2015) %>% 
    arrange(tax_pct_SL)

 
df %>% select(state_abb, year, tax_pct_SL, tax_pct_state, tax_pct_local) %>% 
	filter(state_abb == "AK") 

## Observations:
 # 1. The share of tax revnue in general own source revenue has been decreasing over the past 40 years. 
 #    For the national total of state and local, the share of tax decreased from about 80% to 70%. 
 #    In 2015, tax accounts for a higher share for state revenue (73%) than for local gov revenue (65%)
 # 2. For the total state and local revenue, the share of tax revenue in general own-source revenue generally ranges from  
 #    60% ~ 80%. 
 # 3. Tax revenue in AK accouts for a very small share of own source revenue (35% in 2015), especially at the state level (20% in 2015). 
 #    The tax share in AK was not so low in 1977, it declined drastically in 1970s and 1980s, mainly due to the drop at state level. The share of tax
 #    at the state level also fluctuate a lot.  

## TODO: make box-whisker plots


#*******************************************************************************
#  2. Revenue structure: share of income tax, sales tax, and property tax   ####
#*******************************************************************************

## State level

df_state <- 
df_RevGSP %>% 
	filter(year >= 1977) %>% 
	select(state_abb, year, 
				 tax_tot_nom_state, 
				 tax_indivIncome_nom_state,
				 tax_sales_tot_nom_state,
         tax_property_nom_state,
         tax_corpIncome_nom_state) %>% 
	mutate(indivIncome_pct = 100 * tax_indivIncome_nom_state/tax_tot_nom_state,
				 sales_pct       = 100 * tax_sales_tot_nom_state/tax_tot_nom_state,
				 property_pct    = 100 * tax_property_nom_state/tax_tot_nom_state,
				 corpIncome      = 100 * tax_corpIncome_nom_state/tax_tot_nom_state) %>% 
  select(state_abb, year,
  			 indivIncome_pct,
  			 sales_pct,
  			 property_pct,
  			 corpIncome)


## Trend sales 
df_state %>% 
	filter(state_abb %in% "US")
# The share of sales tax is quite stable, ~50%
# The share of personal income taxes has increased by half, from 25% in 1977 to 37% in 2015 
# The share of property tax is around 1.5%~2.5%. 
# The share of corp income tax decreased from 9% to 5%




