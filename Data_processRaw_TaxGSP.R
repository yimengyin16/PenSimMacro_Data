# Combine data for GSP-tax model
 

#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Combine data the following data
 # 1. Revenue by state from Urban
 # 2. GSP 1963-1997 (SIC) and 1997-2016(NAICS) from BEA



#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(tidyverse)
library(readxl)
library(lubridate)
library(xts)
library(zoo)
library(magrittr)
library(stringr)

library(forecast)

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"


#**********************************************************************
#                     Prepare Data                                 ####
#**********************************************************************

# Loading saved data 
load(paste0(dir_data_raw, "dataRaw_RevGSP.RData"))


df_GSP_FRED
df_RGSP_BEA
df_NGSP_BEA
Rev_urban_tot_nom
Rev_urban_tot_real


Rev_urban_tot_nom1 <- 
  Rev_urban_tot_nom %>% 
	mutate(varname = paste0(varname,"_",nomReal, "_", type)) %>% 
	select(state_abb, year, varname, value) %>% 
	spread(varname, value) 
Rev_urban_tot_nom1

Rev_urban_tot_real1 <- 
	Rev_urban_tot_real %>% 
	mutate(varname = paste0(varname,"_",nomReal, "_", type)) %>% 
	select(state_abb, year, varname, value) %>% 
	spread(varname, value) 
Rev_urban_tot_real1



df_RevGSP <- 
df_RGSP_BEA %>% 
	left_join(df_NGSP_BEA) %>% 
	left_join(df_GSP_FRED %>% select(-RGSP)) %>% 
	left_join(Rev_urban_tot_nom1) %>% 
	left_join(Rev_urban_tot_real1)


df_RevGSP


#**********************************************************************
#                Notes for df_RevGSP                               ####
#**********************************************************************

# Variables
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
#                        Saving Data                               ####
#**********************************************************************

save(df_RevGSP, 
		 df_us_states,
		 file =  paste0(dir_data_out, "Data_RevGSP.RData"))











