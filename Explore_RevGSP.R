# This script explore the relationship between GDP and gov revenue and does exploratory modeling 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Relationship to examine:
  # 1. GSP growth and total own soure revenue growth,
  # 2. GSP growth and total tax revenue growth
  # 3. GSP growth and personal income tax growth
  # 4. GSP growth and sales tax growth
  # 5. GSP growth and property tax growth
  # 6. GSP growth and corporate income tax growth
  # 7. GSP growth and growth of total tax revenue minus PIT and sales





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
#     - suffix: 
#        - local
#        - state
#        - SL: state and local
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

df_inflation <- 
   df_dataAll_y %>% select(year, CPIU_SA_FRED, CPIU_NA_FRED, CPIc_SA_FRED, Inflation_Index)
df_inflation


df <- 
	df_RevGSP %>% 
	select(state_abb, year, 
				 RGSP_SIC, RGSP_NAICS, 
				 rev_gen_ownSrc_SL, 
				 tax_tot_SL, tax_indivIncome_SL, tax_sales_tot_SL, tax_property_SL, tax_corpIncome_SL)











