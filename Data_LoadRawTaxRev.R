# Loading state and local government finance data and state GDP data


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Goal:
		# 1. Load state and local government finance data
    # 2. Load state GDP data 
    # 3. Standardize data formats

#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(quantmod)
library(Quandl)
library(tidyverse)
library(readxl)
library(lubridate)
library(xts)
library(zoo)
library(magrittr)
library(stringr)

# Intro to quantmod
#    1. quantmod http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
#    2. https://www.quantinsti.com/blog/a-guide-on-r-quantmod-package-how-to-get-started/

# Quandl for R
# https://www.quandl.com/tools/r

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"




#**********************************************************************
#       State and local gov finance data from Urban Institute      ####
#**********************************************************************

#' Source: http://slfdqs.taxpolicycenter.org/pages.cfm


#' State and local government revnue compiled by Urban Institue.
#'   - Data values are slightly different from the Census data but are generally consistent.
#'   - Many government types and revenue variables are available, data loaded here are from
#'     a data query that includes the folling variables:


#' selected series:
#' 	- (R01) Total revenue
#'  - (R02) Total Rev-Own Sources 
#'  - (R03) General Revenue
#'  - (R04) Gen Rev-Own Sources
#'  - (R05) Total Taxes
#'  - (R06) Property Taxes
#'  - (R08) Tot Sales & Gr Rec Tax
#'  - (R09) Total Gen Sales Tax (T09)
#'  - (R10) Total select Sales Tax
#'  - (R27) Individual Income Tax (T40)
#'  - (R28) Corp Net Income Tax 
#'  - (R36) Tot Chgs and Misc Rev
#' Type:
#' 	- State/local: total, nominal
#'  - Local: total, nominal 
#'  - State: total, nominal
#' 

#' Notes:  
#'  - state and local sum up to "state and local " only for 'own source revenue and tax'


# State and local

Rev_urban_SL_tot_nom <- read_excel(paste0(dir_data_raw, 'TaxRev/Urban_SL_tot_nom.xlsx'), skip = 3)
Rev_urban_S_tot_nom  <- read_excel(paste0(dir_data_raw, 'TaxRev/Urban_S_tot_nom.xlsx'),  skip = 3)
Rev_urban_L_tot_nom  <- read_excel(paste0(dir_data_raw, 'TaxRev/Urban_L_tot_nom.xlsx'),  skip = 3)

Rev_urban_SL_tot_nom
Rev_urban_S_tot_nom
Rev_urban_L_tot_nom

fn <- function(df){	
	df %>% filter(!is.na(Year)) %>% 
		gather(varname, value, -State, -Year) %>% 
	  mutate(varname = str_extract(varname,  '\\(([^()]*)\\)'), # '\\(([^)]+)\\)')) # extract text inside (): http://www.rexegg.com/regex-cookbook.html#captureparen
	  			 varname = str_replace(varname, '\\(', ''),
	  			 varname = str_replace(varname, '\\)', ''),
	  			 value   = as.numeric(value),   # DC values for 2013-14 are 'N/A'.
	  			 value   = ifelse(is.na(value), 0, value))    
}
Rev_urban_SL_tot_nom %<>% fn %>% mutate(type = "SL") 
Rev_urban_L_tot_nom  %<>% fn %>% mutate(type = "local") 
Rev_urban_S_tot_nom  %<>% fn %>% mutate(type = "state") 

Rev_urban_tot_nom <-  
	bind_rows(Rev_urban_SL_tot_nom,
						Rev_urban_S_tot_nom,
						Rev_urban_L_tot_nom)



Rev_urban_tot_nom$State %>% unique

# Create and standardized data frame for US states

us_states <- tibble(state = c(state.name, "DC", "United States"), state_abb = c(state.abb, "DC", "US"))


x <- data.frame(Rev_urban_tot_nom$State %>% unique %>% sort, us_states$state %>% sort)
identical(x[[1]], x[[2]])




