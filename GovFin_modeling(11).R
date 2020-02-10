# This script explore the relationship between GSP and state gov revenue and does exploratory modeling 
# It is also for the first draft of the policy brief. 


# 2019/10: Detrending using method described in Hamilton(2017) 'Why You Should Never Use the Hodrick-Prescott Filter'


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# To-do list for GSP and tax revenue section

# What to model:
# 1. Personal income tax: relationship with GSP growth and returns (stock and/or portfolio)
# 2. General Sales tax: relationship with GSP, how the relationship changes during recessions
# 3. Other taxes (non-PIT-non-sales taxes): relationship with GSP
# 4. Construct stylized government types 

# Notes on inflation adustment
#  - Use national level GDP price index or GDP deflator to recalculate real tax revenue. 
#  		 a. The real tax revenue data from Urban are calculated using CPI-U, 
#  		    which is not consistent with the inflation adjustment method for GDP (by chain-type indices)
#  		 b. GDP price index may work better than CPI-U for our purpose. (the basket of goods is fixed in CPI, but changes over time in GDP price index)
#  - Use real values for now. When integrated with macro model, which uses real GDP, 
#    add the assumed inflation rate to the GSP growth rate. So we do not model inflation 
#    in this version. 

# Notes on data
# 1. National GDP from FRED, 2nd quarter data for annual data. This may be more consistent with the timing of FY for most states


# Notes on decomposing GDP and tax revenue

# Other relationships to examine:
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
library(mFilter) # HP filter "hpfilter"

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
				 trough = trough - 0/4)


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


color_PIT <- RIG.green
color_salesgen <- "blue"
color_salessel <- "deepskyblue2"
color_other <- RIG.purple
color_propertyLoc <- RIG.yellow.dark
color_GDP <- "gray50"



#**********************************************************************
#             Data preparation 1: tax revenue and GSP              ####
#**********************************************************************
# 1. Loading saved data 
load(paste0(dir_data_out, "data_RevGSP.RData"))
load(paste0(dir_data_out, "dataAll.RData"))

# Variables in df_RevGSP
#  Indices: state, state_abb, year
#  GSP variables (calendar year)
#     - RGSP_SIC:   From BEA,  1987-1997, based on SIC
#     - RGSP_NAICS: From BEA,  1997-2016, based on NAICS
#     - NGSP_SIC:   From BEA,  1963-1997, based on SIC
#     - NGSP_NAICS: From BEA,  1997-2016, based on NAICS 
#     - NGSP:       From FRED, 1997-2016 (no national level)
#  Tax and revenue variables: 1977-2015 (Fiscal year)
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


# 2. Produce real tax revenue by applying GDP price index 

# Notes: GDP price deflator and GDP chain-type price index are very close.
#        For now, we stick with GDP chain-type price index
# df_dataAll_y %>%
# 	select(year, GDPdeflator_FRED, GDPCTPI_FRED) %>%
# 	mutate_at(vars(-year), funs(100 * log(./lag(.)))) %>% 
# 	mutate(diff = GDPdeflator_FRED - GDPCTPI_FRED)
 
df_RevGDP_GDPCTPI <-   
	df_RevGSP %>% select(state_abb, year, NGSP_SIC,NGSP_NAICS, contains("_nom_")) %>% 
	left_join(df_dataAll_y %>% select(year, GDPCTPI_FRED)) %>% 
	mutate_at(vars(-year, -state_abb), funs(./(GDPCTPI_FRED/100))) %>% 
	rename_all(funs(str_replace(., "_nom_", "_real2_"))) %>% 
	rename_all(funs(str_replace(., "NGSP", "RGSP2"))) 
	


# 3. Real terms based on CPI-U (Urban's calculation BEA real GSP starts from 1987)
df_real <- 
	df_RevGSP %>%
	select(state_abb, year, 
				 RGSP_SIC, RGSP_NAICS, 
				 tax_tot_real_state, 
				 tax_indivIncome_real_state,
				 tax_sales_tot_real_state,
				 tax_sales_gen_real_state,
				 tax_sales_select_real_state,
				 tax_corpIncome_real_state,
				 tax_property_real_state,
				 tax_property_real_local) %>% 
	mutate(tax_other_real_state = tax_tot_real_state - tax_indivIncome_real_state - 
				                        tax_sales_tot_real_state - tax_corpIncome_real_state - tax_property_real_state,
				 tax_nonPITsalesgen_real_state = tax_tot_real_state - tax_indivIncome_real_state - tax_sales_gen_real_state,
				 tax_nonPITsalestot_real_state = tax_tot_real_state - tax_indivIncome_real_state - tax_sales_tot_real_state,
				 RGSPc = ifelse(year >= 1997, RGSP_NAICS, RGSP_SIC),
				 #RGSPc = 1000 * RGSPc,
				 PIT_tax_real = 100 * tax_indivIncome_real_state/tax_tot_real_state, 
				 tax_GSP_real = 100 * tax_tot_real_state / RGSPc)

df_dlreal <- 
	df_real %>% 
	group_by(state_abb) %>% 
	mutate_at(vars(-year, -state_abb), funs(log(./lag(.)))) %>% 
	mutate(RGSP = ifelse(year > 1997, RGSP_NAICS, RGSP_SIC)) %>% 
	ungroup() %>% 
	rename_at(vars(-year, -state_abb), funs(paste0("dl", .) ))
	

df_real
df_dlreal


# 4. Real terms based on GDP price index (SIC from 1963-1997, NAICS from 1997-2016)
df_real2 <- 
	df_RevGDP_GDPCTPI %>%
	select(state_abb, year, 
				 RGSP2_SIC, RGSP2_NAICS, 
				 tax_tot_real2_state, 
				 tax_indivIncome_real2_state,
				 tax_sales_tot_real2_state,
				 tax_sales_gen_real2_state,
				 tax_sales_select_real2_state,
				 tax_corpIncome_real2_state,
				 tax_property_real2_state,
				 tax_property_real2_local) %>% 
	mutate(tax_other_real2_state = tax_tot_real2_state - tax_indivIncome_real2_state - 
				 tax_sales_tot_real2_state - tax_corpIncome_real2_state - tax_property_real2_state,
				 tax_nonPITsalesgen_real2_state = tax_tot_real2_state - tax_indivIncome_real2_state - tax_sales_gen_real2_state,
				 tax_nonPITsalestot_real2_state = tax_tot_real2_state - tax_indivIncome_real2_state - tax_sales_tot_real2_state,
				 RGSP2c = ifelse(year >= 1997, RGSP2_NAICS, RGSP2_SIC),
				 #RGSP2c = 1000 * RGSP2c,
				 PIT_tax_real2 = 100 * tax_indivIncome_real2_state/tax_tot_real2_state, 
				 tax_GSP_real2 = 100 * tax_tot_real2_state / RGSP2c)

df_dlreal2 <- 
	df_real2 %>% 
	group_by(state_abb) %>% 
	mutate_at(vars(-year, -state_abb), funs(log(./lag(.)))) %>% 
	mutate(RGSP = ifelse(year > 1997, RGSP2_NAICS, RGSP2_SIC)) %>% 
	ungroup() %>% 
	rename_at(vars(-year, -state_abb), funs(paste0("dl", .) ))

df_real2
df_dlreal2



# Nominal terms (BEA nominal GSP starts from 1963)
df_nom <- 
	df_RevGSP %>%
	select(state_abb, year, 
				 NGSP_SIC, NGSP_NAICS, 
				 tax_tot_nom_state, 
				 tax_indivIncome_nom_state,
				 tax_sales_tot_nom_state,
				 tax_sales_gen_nom_state,
				 tax_sales_select_nom_state,
				 tax_corpIncome_nom_state,
				 tax_property_nom_state,
				 tax_property_nom_local) %>% 
	mutate(tax_other_nom_state = tax_tot_nom_state - tax_indivIncome_nom_state - 
				 	tax_sales_tot_nom_state - tax_corpIncome_nom_state - tax_property_nom_state,
				 tax_nonPITsalesgen_nom_state = tax_tot_nom_state - tax_indivIncome_nom_state - tax_sales_gen_nom_state,
				 tax_nonPITsalestot_nom_state = tax_tot_nom_state - tax_indivIncome_nom_state - tax_sales_tot_nom_state,
				 NGSPc = ifelse(year > 1997, NGSP_NAICS, NGSP_SIC),
				 # NGSPc = 1000 * NGSPc,
				 PIT_tax_nom = 100 * tax_indivIncome_nom_state/tax_tot_nom_state, 
				 tax_GSP_nom = 100 * tax_tot_nom_state / NGSPc)

df_dlnom <- 
	df_nom %>% 
	group_by(state_abb) %>% 
	mutate_at(vars(-year, -state_abb), funs(log(./lag(.)))) %>% 
	mutate(NGSP = ifelse(year > 1997, NGSP_NAICS, NGSP_SIC)) %>% 
	ungroup() %>% 
	rename_at(vars(-year, -state_abb), funs(paste0("dl", .)))

df_nom
df_dlnom



#**********************************************************************
#             Data preparation 2: Asset returns              ####
#**********************************************************************

# GSP and real tax revenue data
# Notes:
#  GSP from 1988-2016
#  tax from 1978-2015
#  Common period:1988-2015
#  Nominal GSP goes back to 1964, may want to extend the real GSP series by adjusting pre-1988 nominal GSP for inflation   

# df_dlRevGSP_reg <- 
# 	df_dlreal %>% 
# 	select(state_abb, year,
# 				 RGSP,
# 				 tax_tot_real_state, 
# 				 tax_indivIncome_real_state,
# 				 tax_sales_gen_real_state,
# 				 tax_sales_select_real_state,
# 				 tax_sales_tot_real_state,
# 				 tax_corpIncome_real_state,
# 				 tax_property_real_state,
# 				 tax_other_real_state,
# 				 tax_nonPITsalestot_real_state) %>% 
# 	filter(year %in% 1978:2015, state_abb %in% df_us_states$state_abb) %>% 
# 	mutate_at(vars(-year), funs(./100))
# df_dlRevGSP_reg 



# 1. Asset index data from SBBI and national GDP from FRED (check it against the aggregate GSP for all states)
df_SBBI <- 
	df_dataAll_y %>% 
	select(year, month, yearMon, 
				 GDP_FRED,        # National GDP from FRED, in billions
				 CPIU_NA_FRED,    # CPI, urban resident, not seasonally adjusted. FRED
				 GDPCTPI_FRED,    # GDP chain-type price index
				 Inflation_Index, # SBBI, inflation index 
				 LCapStock_TRI,   # SBBI, large cap stock total return index (SP500 total return index)
				 LCapStock_CAI,   # SBBI, large cap stock capital appreciation index (SP500 price index)
				 CBond_TRI,       # SBBI, corp bond total return
				 LTGBond_TRI,     # SBBI, long-term gov bond total return (20y?)
				 MTGBond_TRI,     # SBBI, medium-term gov bond total return
				 LTGBond_Yield    # SBBI, long-term gov bond yield
	) %>%  
	mutate(GDP_FRED = GDP_FRED * 1e6,
				 ) %>% # unit from $billion to $thousand, (tax revenues are in $thousand)
	filter(year %in% 1977:2015) 

# 1.1 Compute asset return and GDP growth (FRED) from indexes
df_dlSBBI <- df_SBBI %>% 
	mutate_at(vars(-year, -month, -yearMon, -LTGBond_Yield), funs(log(./lag(.)))) %>% 
	rename_at(vars(-year, -month, -yearMon, -LTGBond_Yield), funs(paste0("dl", .))  )


# 2. Asset return data from NYU Stern 
df_assetReturn_nyu <- read_excel(paste0(dir_data_raw, "/histretSP_201801.xls"), sheet = "Returns by year", skip = 17)[,1:4]
names(df_assetReturn_nyu) <- c("year", "SP500_return_nyu", "TBill_return_nyu", "TBond_return_nyu")
df_assetReturn_nyu %<>% mutate(year = as.numeric(year)) %>% filter(year <= 2017)
df_assetReturn_nyu


# 3. Captial gain / losses data for states (From Don)
# level in $billions
rlzCapGains_nom <- read_excel(paste0(dir_data_raw, "/NationalCapitalGains_yy.xlsx"), sheet = "CapGains", range = c("A5:C67"))
names(rlzCapGains_nom) <- c("year", "capgains", "capgains_chg") 
rlzCapGains_nom %<>% mutate(capgains = capgains * 1e6) 

rlzCapGains_nom_rev <- read_excel(paste0(dir_data_raw, "/NationalCapitalGains_yy.xlsx"), sheet = "CapGains_rev", range = c("A8:E30"))[,-(2:3)]
rlzCapGains_nom_rev



# 4. combine data 
 # Notes: 1977 to 2015, except for tax_gain and tax_gain_chg

df_returns <- 
	df_SBBI %>%  
	left_join(df_dlSBBI) %>% 
	left_join(df_assetReturn_nyu) %>% 
	left_join(rlzCapGains_nom) %>% 
	left_join(rlzCapGains_nom_rev)



## 5. getting real variables 

df_returns %<>% 
	mutate(# real changes based on CPI
		     dlLCapStock_TRI_real     = dlLCapStock_TRI    - dlInflation_Index,
				 dlLCapStock_CAI_real     = dlLCapStock_CAI    - dlInflation_Index,
				 dlCBond_TRI_real         = dlCBond_TRI        - dlInflation_Index,    
				 dlLTGBond_TRI_real       = dlLTGBond_TRI      - dlInflation_Index,  
				 dlMTGBond_TRI_real       = dlMTGBond_TRI      - dlInflation_Index,
				 SP500_return_nyu_real    = SP500_return_nyu   - dlInflation_Index, 
				 TBill_return_nyu_real    = TBill_return_nyu   - dlInflation_Index, 
				 TBond_return_nyu_real    = TBond_return_nyu   - dlInflation_Index,
				 capgains_chg_real        = capgains_chg       - dlInflation_Index,
				 tax_gains_chg_real       = tax_gains_chg      - dlInflation_Index,
				 
				 # real changes based on GDP price index
				 dlLCapStock_TRI_real2      = dlLCapStock_TRI    - dlGDPCTPI_FRED,
				 dlLCapStock_CAI_real2      = dlLCapStock_CAI    - dlGDPCTPI_FRED,
				 dlCBond_TRI_real2          = dlCBond_TRI        - dlGDPCTPI_FRED,    
				 dlLTGBond_TRI_real2        = dlLTGBond_TRI      - dlGDPCTPI_FRED,  
				 dlMTGBond_TRI_real2        = dlMTGBond_TRI      - dlGDPCTPI_FRED,
				 SP500_return_nyu_real2     = SP500_return_nyu   - dlGDPCTPI_FRED, 
				 TBill_return_nyu_real2     = TBill_return_nyu   - dlGDPCTPI_FRED, 
				 TBond_return_nyu_real2     = TBond_return_nyu   - dlGDPCTPI_FRED,
				 capgains_chg_real2         = capgains_chg       - dlGDPCTPI_FRED,
				 tax_gains_chg_real2        = tax_gains_chg      - dlGDPCTPI_FRED,
				 
				 # real index/amount based on CPI
				 LCapStock_TRI_real     = LCapStock_TRI    / (Inflation_Index/Inflation_Index[year == 2009]),
				 LCapStock_CAI_real     = LCapStock_CAI    / (Inflation_Index/Inflation_Index[year == 2009]),
				 CBond_TRI_real         = CBond_TRI        / (Inflation_Index/Inflation_Index[year == 2009]),    
				 LTGBond_TRI_real       = LTGBond_TRI      / (Inflation_Index/Inflation_Index[year == 2009]),  
				 MTGBond_TRI_real       = MTGBond_TRI      / (Inflation_Index/Inflation_Index[year == 2009]),
				 capgains_real          = capgains     / (Inflation_Index/Inflation_Index[year == 2009]),
				 tax_gains_real         = tax_gains    / (Inflation_Index/Inflation_Index[year == 2009]),
				 
				 # real index/amount based on GDP price index
				 LCapStock_TRI_real2      = LCapStock_TRI  /(GDPCTPI_FRED/100),
				 LCapStock_CAI_real2      = LCapStock_CAI  /(GDPCTPI_FRED/100),
				 CBond_TRI_real2          = CBond_TRI      /(GDPCTPI_FRED/100),    
				 LTGBond_TRI_real2        = LTGBond_TRI    /(GDPCTPI_FRED/100),  
				 MTGBond_TRI_real2        = MTGBond_TRI    /(GDPCTPI_FRED/100),
				 capgains_real2           = capgains   /(GDPCTPI_FRED/100),
				 tax_gains_real2          = tax_gains  /(GDPCTPI_FRED/100)
	) %>% 
	ungroup



#**********************************************************************
#             Data preparation 3: Combine data               ####
#**********************************************************************

df_real    %<>% left_join(df_returns) 
df_dlreal  %<>% left_join(df_returns)
df_real2   %<>% left_join(df_returns)
df_dlreal2 %<>% left_join(df_returns)
df_nom     %<>% left_join(df_returns)
df_dlnom   %<>% left_join(df_returns)




#**********************************************************************************************
#     Regression analysis: decomposition of GDP and tax revenue: using Hamiltion (2017)    ####
#**********************************************************************************************

# Variables to decompose (national)
# Tax in real terms (GDP price index)
# PIT, state
# general sales, state
# selective sales, state
# non-PIT-non-sales, state
# Property, local
# National GDP (FRED)
# Real total stock return (LCapStock_TRI)
# capital gains 

df_decomp_real2 <- 
	df_real2 %>% 
	filter(state_abb == "US", year %in% 1977:2015) %>%  
	select(year, state_abb, year, 
				 tax_indivIncome_real2_state, 
				 tax_sales_gen_real2_state, 
				 tax_sales_tot_real2_state,
				 tax_sales_select_real2_state,
				 tax_nonPITsalestot_real2_state,
				 tax_property_real2_local,
				 GDP_FRED,
				 RGSP2c,
				 LCapStock_TRI_real2,
				 capgains_real2
	) %>% 
	mutate(
		GDP_log  = log(GDP_FRED),
		GDP_logtrend = hpfilter(GDP_log, freq = 100)$trend,
		GDP_logcycle = hpfilter(GDP_log, freq = 100)$cycle,
		GDP_dlog     = GDP_log - dplyr::lag(GDP_log),
		GDP_dlogtrend = GDP_logtrend - dplyr::lag(GDP_logtrend),
		GDP_dlogcycle = GDP_logcycle - dplyr::lag(GDP_logcycle),
		# GDP_cycle_fct   = exp(GDP_logcycle),    
		
		PIT_log = log(tax_indivIncome_real2_state),
		PIT_logtrend = hpfilter(PIT_log, freq = 100)$trend,
		PIT_logcycle = hpfilter(PIT_log, freq = 100)$cycle,
		PIT_dlog     = PIT_log - dplyr::lag(PIT_log),
		PIT_dlogtrend= PIT_logtrend - dplyr::lag(PIT_logtrend),
		PIT_dlogcycle = PIT_logcycle - dplyr::lag(PIT_logcycle),
		
		salesgen_log         = log(tax_sales_gen_real2_state),
		salesgen_logtrend    = hpfilter(salesgen_log, freq = 100)$trend,
		salesgen_logcycle    = hpfilter(salesgen_log, freq = 100)$cycle,
		salesgen_dlog  = salesgen_log - dplyr::lag(salesgen_log),
		salesgen_dlogtrend= salesgen_logtrend - dplyr::lag(salesgen_logtrend),
		salesgen_dlogcycle = salesgen_logcycle - dplyr::lag(salesgen_logcycle),
		
		salessel_log         = log(tax_sales_select_real2_state),
		salessel_logtrend = hpfilter(salessel_log , freq = 100)$trend,
		salessel_logcycle = hpfilter(salessel_log , freq = 100)$cycle,
		salessel_dlog  = salessel_log - dplyr::lag(salessel_log),
		salessel_dlogtrend= salessel_logtrend - dplyr::lag(salessel_logtrend),
		salessel_dlogcycle = salessel_logcycle - dplyr::lag(salessel_logcycle),
		
		nonPITsalestot_log         = log(tax_nonPITsalestot_real2_state),
		nonPITsalestot_logtrend = hpfilter(nonPITsalestot_log, freq = 100)$trend,
		nonPITsalestot_logcycle = hpfilter(nonPITsalestot_log, freq = 100)$cycle,
		nonPITsalestot_dlog  = nonPITsalestot_log - dplyr::lag(nonPITsalestot_log),
		nonPITsalestot_dlogtrend= nonPITsalestot_logtrend - dplyr::lag(nonPITsalestot_logtrend),
		nonPITsalestot_dlogcycle = nonPITsalestot_logcycle - dplyr::lag(nonPITsalestot_logcycle),
		
		propertyLoc_log         = log(tax_property_real2_local),
		propertyLoc_logtrend    = hpfilter(propertyLoc_log, freq = 100)$trend,
		propertyLoc_logcycle    = hpfilter(propertyLoc_log, freq = 100)$cycle,
		propertyLoc_dlog  = propertyLoc_log - dplyr::lag(propertyLoc_log),
		propertyLoc_dlogtrend= propertyLoc_logtrend - dplyr::lag(propertyLoc_logtrend),
		propertyLoc_dlogcycle = propertyLoc_logcycle - dplyr::lag(propertyLoc_logcycle),
		
		
		stockIdx_log         = log(LCapStock_TRI_real2),
		stockIdx_logtrend    = hpfilter(stockIdx_log, freq = 100)$trend,
		stockIdx_logcycle    = hpfilter(stockIdx_log, freq = 100)$cycle,
		stockIdx_dlog  = stockIdx_log - dplyr::lag(stockIdx_log),
		stockIdx_dlogtrend= stockIdx_logtrend - dplyr::lag(stockIdx_logtrend),
		stockIdx_dlogcycle = stockIdx_logcycle - dplyr::lag(stockIdx_logcycle),
		
		LagstockIdx_dlog      = dplyr::lag(stockIdx_dlog),
		LagstockIdx_dlogtrend = dplyr::lag(stockIdx_dlogtrend),
		LagstockIdx_dlogcycle = dplyr::lag(stockIdx_dlogcycle),
		
		
		
		capgains_log         = log(capgains_real2),
		capgains_logtrend    = hpfilter( capgains_log , freq = 100)$trend,
		capgains_logcycle    = hpfilter( capgains_log , freq = 100)$cycle,
		capgains_dlog  = capgains_log - dplyr::lag(capgains_log),
		capgains_dlogtrend= capgains_logtrend - dplyr::lag(capgains_logtrend),
		capgains_dlogcycle = capgains_logcycle - dplyr::lag(capgains_logcycle),
		
		Lagcapgains_dlog      = dplyr::lag(capgains_dlog),
		Lagcapgains_dlogtrend = dplyr::lag(capgains_dlogtrend),
		Lagcapgains_dlogcycle = dplyr::lag(capgains_dlogcycle),
		
		
		PIT_GDP_trend = exp(PIT_logtrend - GDP_logtrend),
		salesgen_GDP_trend = exp(salesgen_logtrend - GDP_logtrend),
		salessel_GDP_trend = exp(salessel_logtrend - GDP_logtrend),
		nonPITsalestot_GDP_trend = exp(nonPITsalestot_logtrend - GDP_logtrend),
		propertyLoc_GDP_trend    = exp(propertyLoc_logtrend - GDP_logtrend)
	)

df_decomp_real2 %>% select(year, GDP_logtrend_H, GDP_dlogtrend_H, GDP_logtrend, GDP_dlogtrend)

df_decomp_real2 %<>% 
	mutate(
		GDP_logtrend_H = c(rep(NA, 5), predict(dynlm(as.ts(GDP_log) ~ L(as.ts(GDP_log), 2:5), data = df_decomp_real2))),
		GDP_logcycle_H = c(rep(NA, 5), (dynlm(as.ts(GDP_log) ~ L(as.ts(GDP_log), 2:5), data = df_decomp_real2))$residuals ),
		GDP_dlogtrend_H = GDP_logtrend_H - dplyr::lag(GDP_logtrend_H),
		GDP_dlogcycle_H = GDP_logcycle_H - dplyr::lag(GDP_logcycle_H),

		PIT_logtrend_H  = c(rep(NA, 5), predict(dynlm(as.ts(PIT_log) ~ L(as.ts(PIT_log), 2:5), data = df_decomp_real2))),
		PIT_logcycle_H  = c(rep(NA, 5), (dynlm(as.ts(PIT_log) ~ L(as.ts(PIT_log), 2:5), data = df_decomp_real2))$residuals),
		PIT_dlogtrend_H = PIT_logtrend_H - dplyr::lag(PIT_logtrend_H),
		PIT_dlogcycle_H = PIT_logcycle_H - dplyr::lag(PIT_logcycle_H),
		

		salesgen_logtrend_H    = c(rep(NA, 5), predict(dynlm(as.ts(salesgen_log) ~ L(as.ts(salesgen_log), 2:5), data = df_decomp_real2))),
		salesgen_logcycle_H    = c(rep(NA, 5), (dynlm(as.ts(salesgen_log) ~ L(as.ts(salesgen_log), 2:5), data = df_decomp_real2))$residuals ),
		salesgen_dlogtrend_H   = salesgen_logtrend_H - dplyr::lag(salesgen_logtrend_H),
		salesgen_dlogcycle_H   = salesgen_logcycle_H - dplyr::lag(salesgen_logcycle_H),
		
		salessel_logtrend_H  = c(rep(NA, 5), predict(dynlm(as.ts(salessel_log) ~ L(as.ts(salessel_log), 2:5), data = df_decomp_real2))),
		salessel_logcycle_H  = c(rep(NA, 5), (dynlm(as.ts(salessel_log) ~ L(as.ts(salessel_log), 2:5), data = df_decomp_real2))$residuals ),
		salessel_dlogtrend_H = salessel_logtrend_H - dplyr::lag(salessel_logtrend_H),
		salessel_dlogcycle_H = salessel_logcycle_H - dplyr::lag(salessel_logcycle_H),
		
		
		nonPITsalestot_logtrend_H = c(rep(NA, 5), predict(dynlm(as.ts(nonPITsalestot_log) ~ L(as.ts(nonPITsalestot_log), 2:5), data = df_decomp_real2))),
		nonPITsalestot_logcycle_H = c(rep(NA, 5), (dynlm(as.ts(nonPITsalestot_log) ~ L(as.ts(nonPITsalestot_log), 2:5), data = df_decomp_real2))$residuals),
		nonPITsalestot_dlogtrend_H = nonPITsalestot_logtrend_H - dplyr::lag(nonPITsalestot_logtrend_H),
		nonPITsalestot_dlogcycle_H = nonPITsalestot_logcycle_H - dplyr::lag(nonPITsalestot_logcycle_H),
		
		
		propertyLoc_logtrend_H    = c(rep(NA, 5), predict(dynlm(as.ts(propertyLoc_log) ~ L(as.ts(propertyLoc_log), 2:5), data = df_decomp_real2))),
		propertyLoc_logcycle_H    = c(rep(NA, 5), (dynlm(as.ts(propertyLoc_log) ~ L(as.ts(propertyLoc_log), 2:5), data = df_decomp_real2))$residuals),
		propertyLoc_dlogtrend_H   = propertyLoc_logtrend_H - dplyr::lag(propertyLoc_logtrend_H),
		propertyLoc_dlogcycle_H   = propertyLoc_logcycle_H - dplyr::lag(propertyLoc_logcycle_H),
		
		
		stockIdx_logtrend_H    = c(rep(NA, 5), predict(dynlm(as.ts(stockIdx_log) ~ L(as.ts(stockIdx_log), 2:5), data = df_decomp_real2))),
		stockIdx_logcycle_H    = c(rep(NA, 5), (dynlm(as.ts(stockIdx_log) ~ L(as.ts(stockIdx_log), 2:5), data = df_decomp_real2))$residuals ),
		stockIdx_dlogtrend_H   = stockIdx_logtrend_H - dplyr::lag(stockIdx_logtrend_H),
		stockIdx_dlogcycle_H   = stockIdx_logcycle_H - dplyr::lag(stockIdx_logcycle_H),
		
		
		LagstockIdx_dlogtrend_H = dplyr::lag(stockIdx_dlogtrend_H),
		LagstockIdx_dlogcycle_H = dplyr::lag(stockIdx_dlogcycle_H),
		
    capgains_logtrend_H    = c(rep(NA, 5), predict(dynlm(as.ts(capgains_log) ~ L(as.ts(capgains_log), 2:5), data = df_decomp_real2))),
    capgains_logcycle_H    = c(rep(NA, 5), (dynlm(as.ts(capgains_log) ~ L(as.ts(capgains_log), 2:5), data = df_decomp_real2))$residuals ),
    capgains_dlogtrend_H   = capgains_logtrend_H - dplyr::lag(capgains_logtrend_H),
    capgains_dlogcycle_H   = capgains_logcycle_H - dplyr::lag(capgains_logcycle_H),
    
    Lagcapgains_dlogtrend_H = dplyr::lag(capgains_dlogtrend_H),
    Lagcapgains_dlogcycle_H = dplyr::lag(capgains_dlogcycle_H)
	)
	

df_decomp_real2$GDP_logtrend_H - dplyr::lag(df_decomp_real2$GDP_logtrend_H)


# level and trend
df_decomp_real2 %>% # GDP
	select(year, GDP_logcycle, GDP_logcycle_H) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)


df_decomp_real2 %>% # GDP
	select(year, GDP_log, GDP_logtrend_H) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2) +
	coord_cartesian(ylim = c(22, 24))



df_decomp_real2 %>% #PIT
	select(year, PIT_dlog, PIT_dlogtrend) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point"))

df_decomp_real2 %>% #gen sales
	select(year, salesgen_dlog, salesgen_dlogtrend) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point"))

df_decomp_real2 %>% #gen sales
	select(year, stockIdx_dlog, stockIdx_dlogtrend) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point"))


# compare cycles

# log cycle
df_decomp_real2 %>%  # GDP, PIT, and gen sales
	select(year, GDP_dlogcycle, PIT_dlogcycle, salesgen_dlogcycle) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)

df_decomp_real2 %>%  # GDP, gen sales and sel sales
	select(year, GDP_dlogcycle, salesgen_dlogcycle, salessel_dlogcycle) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)

df_decomp_real2 %>% # GDP, nonPITsales, and local property
	select(year, GDP_dlogcycle, nonPITsalestot_dlogcycle, propertyLoc_dlogcycle) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)

df_decomp_real2 %>% # GDP, PIT, and stock
	select(year, GDP_dlogcycle, PIT_dlogcycle, stockIdx_dlogcycle) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)


df_decomp_real2 %>% # PIT, and capgains and stock
	select(year, PIT_dlogcycle, stockIdx_dlogcycle, capgains_dlogcycle) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
	geom_hline(yintercept = 0, linetype = 2)


# df_decomp_real2 %>% # PIT, and capgains and stock
# 	mutate() %>% 
# 	select(year, diff_PIT_logcycle, diff_stockIdx_logcycle, diff_capgains_logcycle) %>% 
# 	gather(var, value, -year) %>% 
# 	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) + 
# 	geom_hline(yintercept = 0, linetype = 2)




# Trends
df_decomp_real2 %>% # PIT vs gen sales
	select(year, PIT_logtrend, salesgen_logtrend) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) 
	# geom_hline(yintercept = 1, linetype = 2)


df_decomp_real2 %>% # PIT vs gen sales as % of GDP
		select(year, 
					 PIT_GDP_trend, 
					 salesgen_GDP_trend, 
					 salessel_GDP_trend,
					 nonPITsalestot_GDP_trend,
					 propertyLoc_GDP_trend) %>% 
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, data = ., geom = c("line", "point")) +
	coord_cartesian(ylim = c(0, 0.035))
# geom_hline(yintercept = 1, linetype = 2)


# Check CPI inflation and GDP price index inflation
df_dlreal2 %>%
	filter(state_abb == "US") %>%
	select(state_abb, year, dlInflation_Index, dlGDPCTPI_FRED) %>%
	gather(var, value, -year, -state_abb) %>%
	qplot(x = year, y = value, color = var, data = ., geom = c("line","point"))
# GDP price index inflation is generally lower and less volatile.




#*******************************************************************************
#     Regression analysis: data prep 2 asset return and PIT                 ####
#*******************************************************************************

# Goal: Estimate the relationship between real PIT growth, real GSP growth, and asset return (nominal),
#       and see if there is structure breek around 2000. (if the recent two recessions are different.)

# Notes:
 # Need to first examine the relationship between realized capgains, tax revenue from realized capgains, and asset returns.   
 # Need an argument for using investment returns in the PIT regression




# 1. Asset returns and captial gains, realized captial gains and PIT

df_temp <- 
df_dlreal2 %>% 
	select(state_abb, year, 
				 dlLCapStock_CAI, 
				 dlLCapStock_TRI, 
				 dlLTGBond_TRI,   
				 LTGBond_Yield, 
				 SP500_return_nyu, 
				 TBond_return_nyu, 
				 capgains_chg, 
				 dlLCapStock_CAI_real2, 
				 dlLCapStock_TRI_real2, 
				 dlLTGBond_TRI_real2,   
				 SP500_return_nyu_real2, 
				 TBond_return_nyu_real2, 
				 capgains_chg_real2, 
				 tax_gains_chg,
				 dltax_indivIncome_real2_state) %>% 
	filter(state_abb == "US")

# quick look at how the total bond returns from SBBI and NYU Stern differ
df_temp %>% 
	select(state_abb, year, dlLTGBond_TRI, TBond_return_nyu) %>% 
	gather(var, value, -state_abb, -year) %>% 
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + theme_bw() + 
	scale_x_continuous(breaks = seq(1950, 2020, 5))
# The two sources show very different returns, especially after 2000. 
# We may want to stick with the SBBI return. (SBBI's calculation looks more rigorous) 


df_temp %>% 
	select(state_abb, year, dlLCapStock_TRI, SP500_return_nyu) %>% 
	gather(var, value, -state_abb, -year) %>% 
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + theme_bw() + 
	scale_x_continuous(breaks = seq(1950, 2020, 5))
# It seems there is a lag in SBBI SP500 return


# Plotting asset returns and realized capital gains 
# Stock
fig_returnCapGains_nom <- 
df_temp %>% 
	filter(year >=1977) %>% 
	select(state_abb, year, dlLCapStock_TRI, capgains_chg_real2) %>%  
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("dlLCapStock_TRI", "capgains_chg_real2"),
											     labels = c("SP500 Total Return", "Change in realized capital gains"))) %>% 
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() +  
	
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_color_manual(values = c(RIG.green, "blue")) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Total stock returns and realized capital gains") + 
	theme(legend.position = "bottom")
fig_returnCapGains_nom

fig_returnCapGains_real <- 
	df_temp %>% 
	filter(year >=1977) %>% 
	select(state_abb, year, dlLCapStock_TRI_real2, capgains_chg_real2) %>%  
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("dlLCapStock_TRI_real2", "capgains_chg_real2"),
											labels = c("SP500 real total Return", "Change in real realized capital gains"))) %>% 
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() +  
	
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_color_manual(values = c(RIG.green, "blue")) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Total stock returns and realized capital gains (in real terms)") + 
	theme(legend.position = "bottom")
fig_returnCapGains_real



# Long-term gov bond, SBBI
df_temp %>% 
	select(state_abb, year, dlLTGBond_TRI_real2, capgains_chg_real2) %>%  
	gather(var, value, -state_abb, -year) %>% 
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + theme_bw() + 
	scale_x_continuous(breaks = seq(1950, 2020, 5))
# No obvious correlation 

df_temp %>% 
	select(state_abb, year, dlLTGBond_TRI, capgains_chg) %>%  
	gather(var, value, -state_abb, -year) %>% 
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + theme_bw() + 
	scale_x_continuous(breaks = seq(1950, 2020, 5))
# No obvious correlation 


# Long-term gov bond, NYU
df_temp %>% 
	select(state_abb, year, TBond_return_nyu, capgains_chg) %>%  
	gather(var, value, -state_abb, -year) %>% 
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + theme_bw() + 
	scale_x_continuous(breaks = seq(1950, 2020, 5))
#


# Plotting realized captial gains 
 # High correlation between 1-year lag of realized captial gain and capital gains tax receipts 
 # High correlation between captial gain tax receipt and state PIT
 # Question: the share of capital gain tax in total PIT

# fig_captainsTax <- 
# df_temp	%>% 
# 	select(state_abb, year, capgains_chg, tax_gains_chg, tax_indivIncome_real_state) %>% 
# 	mutate(capgains_chg = lag(capgains_chg)) %>% 
# 	gather(var, value, -state_abb, -year) %>% 
# 	mutate(var = factor(var, levels = c("capgains_chg", "tax_gains_chg", "tax_indivIncome_real_state"),
# 											     labels = c("Change in realized capital gains (1 year lag)", "Change in capital gains tax receipts", "change in state individual tax"))) %>%
# 	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
# 	theme_bw() + RIG.themeLite() + 
# 	geom_hline(yintercept = 0, linetype = 2) + 
# 	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
# 	scale_color_manual(values = c("blue", RIG.red, "darkgrey")) + 
# 	labs(x = NULL, y = "Percent", color = NULL,
# 			 title = "Realized capital gains and tax revenues") + 
# 	theme(legend.position = "bottom")
# fig_captainsTax




fig_capgainsTax_real <- 
	df_temp	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, capgains_chg_real2, dlLCapStock_TRI_real2, dltax_indivIncome_real2_state) %>% 
	mutate(capgains_chg_real2 = lag(capgains_chg_real2),
				 dlLCapStock_TRI_real2 = lag(dlLCapStock_TRI_real2)) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("capgains_chg_real2", "dlLCapStock_TRI_real2", "dltax_indivIncome_real2_state"),
											labels = c("Real change in realized capital gains (1 year lag)", "SP500 total real return (1 year lag)", "Real change in state individual tax"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_color_manual(values = c("blue", RIG.red, "darkgrey")) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Realized capital gains and tax revenues (in real terms)") + 
	theme(legend.position = "bottom")
fig_capgainsTax_real




#*******************************************************************************
#   Figures:tax revenues, GDP and asset returns                             ####
#*******************************************************************************

# Figure: comparing cycles of taxes

df_cyclePct <- 
df_decomp_real2 %>% 
	filter(year >= 1978) %>% 
	select(state_abb, year, 
				 GDP_logtrend, GDP_logcycle,
				 PIT_logtrend, PIT_logcycle,
				 salesgen_logtrend, salesgen_logcycle,
				 salessel_logtrend, salessel_logcycle,
				 nonPITsalestot_logtrend, nonPITsalestot_logcycle,
				 propertyLoc_logtrend, propertyLoc_logcycle) %>% 
	mutate(GDP_cyclePct = exp(GDP_logcycle) - 1,
				 PIT_cyclePct = exp(PIT_logcycle) - 1,
				 salesgen_cyclePct = exp(salesgen_logcycle) - 1,
				 salessel_cyclePct = exp(salessel_logcycle) - 1,
				 nonPITsalestot_cyclePct = exp(nonPITsalestot_logcycle) - 1,
				 propertyLoc_cyclePct = exp(propertyLoc_logcycle) - 1
				 )

fig_cyclePct <- 
df_cyclePct %>% 
	select(year, 
				 GDP_cyclePct, 
				 PIT_cyclePct,
				 salesgen_cyclePct,
				 nonPITsalestot_cyclePct) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, levels = c("GDP_cyclePct", "PIT_cyclePct", "salesgen_cyclePct", "nonPITsalestot_cyclePct"),
	                         labels = c("GDP", "Personal income tax \n(state)", "General sales tax \n(state)", "Non-personal-income-non-sales taxes \n(state)")			
											)) %>% 
	ggplot() + 
	theme_bw() + RIG.themeLite() + 
	geom_line(aes(x = year, y = 100 * value, color = var)) + 
	geom_point(aes(x = year, y = 100 * value, color = var, shape = var)) +
	geom_hline(yintercept = 0, linetype = 2) + 
	geom_rect(data = recessionPeriods[-(1:5),],
						aes(xmin = peak, xmax = trough,
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
  scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_GDP, color_PIT, color_salesgen, color_other)) + 
	scale_shape_manual(values = c(15, 16, 17, 18)) + 
	labs(x = NULL, y = "Percent above or below trend (%)", color = NULL, shape = NULL,
			 title = "Cycles in GDP growth and tax revenues",
			 subtitle = "Calculated using real values (2009 dollar)") + 
	theme(legend.position = "bottom") 
	#guides(col = guide_legend(ncol = 3, byrow = TRUE))
# geom_hline(yintercept = 1, linetype = 2)

fig_cyclePct


fig_cyclePct2 <- # paperFigure 
df_cyclePct %>% 
	select(year, 
				 GDP_cyclePct, 
				 PIT_cyclePct,
				 salesgen_cyclePct,
				 propertyLoc_cyclePct) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, levels = c("GDP_cyclePct", "PIT_cyclePct","salesgen_cyclePct", "propertyLoc_cyclePct"),
											     labels = c("GDP", "Personal income tax \n(state)", "General sales tax \n(state)", 
																      "Property tax\n(local)")			
	)) %>% 
	ggplot() + 
	theme_bw() + RIG.themeLite() + 
	geom_line(aes(x = year, y = 100 * value, color = var)) + 
	geom_point(aes(x = year, y = 100 * value, color = var, shape = var)) +
	geom_hline(yintercept = 0, linetype = 2) + 
	geom_rect(data = recessionPeriods[-(1:5),],
						aes(xmin = peak, xmax = trough,
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_GDP, color_PIT, color_salesgen, color_propertyLoc)) + 
	scale_shape_manual(values = c(15, 16, 17, 18)) + 
	labs(x = NULL, y = "Percent above or below trend (%)", color = NULL, shape = NULL,
			 title    = "Cycles in GDP growth and tax revenues",
			 subtitle = "Calculated using real values (2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, FRED;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances") + 
	theme(legend.position = "bottom") 
# guides(col = guide_legend(ncol = 3, byrow = TRUE))
# geom_hline(yintercept = 1, linetype = 2)
fig_cyclePct2

ggsave(paste0(dir_fig_out, "fig_GovFin_cyclePct.png"), fig_cyclePct,    width = 10*0.8, height = 6*0.8)
ggsave(paste0(dir_fig_out, "fig_GovFin_cyclePct2.png"), fig_cyclePct2 , width = 10*0.8, height = 6*0.85)








# PIT and sales

df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlogcycle, PIT_dlogcycle, salesgen_dlogcycle) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("PIT_dlogcycle", "salesgen_dlogcycle", "GDP_dlogcycle"),
											labels = c("Real change in state individual tax", "Real change in general sales tax","Real GDP growth" ))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_color_manual(values = c("blue", RIG.red, "darkgrey")) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Cyclical components of real growth rates of individual income tax, general sales tax, \nand GDP growth and capital gain tax") + 
	theme(legend.position = "bottom")





# PIT 1: GDP growth, PIT growth and capgains
fig_cycle_PIT <- 
df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlogcycle, PIT_dlogcycle, capgains_dlogcycle) %>% 
	mutate(capgains_dlogcycle = lag(capgains_dlogcycle)) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("PIT_dlogcycle", "GDP_dlogcycle", "capgains_dlogcycle"),
											labels = c("Real change in state individual income tax", "Real GDP growth", "Real change in capital gains (1 year lag)"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 10)) + 
	scale_color_manual(values = c(color_PIT, color_GDP, RIG.red)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title    = "Cyclical changes in state personal income tax, GDP and capital gains",
			 subtitle = "(Real growth rate calculated based on 2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, FRED;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances") + 
	theme(legend.position = "bottom")
fig_cycle_PIT


df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlog, PIT_dlog, capgains_dlog) %>% 
	mutate(capgains_dlog = lag(capgains_dlog)) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("PIT_dlog", "GDP_dlog", "capgains_dlog"),
											labels = c("Real change in state individual tax", "Real GDP growth", "Real change in capital gains tax (1 year lag)"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_color_manual(values = c("blue",  "darkgrey", RIG.red)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of individual income tax, GDP growth and capital gain tax") + 
	theme(legend.position = "bottom")





# PIT 2: cyclical component (?) of Stock return and capgains

fig_stockCaptains <- 
df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, capgains_dlog, stockIdx_dlog) %>% 
	mutate(capgains_dlog = lag(capgains_dlog),
				 stockIdx_dlog = lag(stockIdx_dlog)
				 ) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("capgains_dlog", "stockIdx_dlog"),
											labels = c("Real change in capital gains (1-year lag)", "SP500 total return (1-year lag)"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 10)) + 
	scale_color_manual(values = c("blue", RIG.red)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real changes in capital gains and real stock returns",
			 subtitle = "(Real growth rate calculated based on 2009 dollar)") + 
	theme(legend.position = "bottom")
fig_stockCaptains


# PIT 2: cyclical component (?) of Stock return and capgains

fig_PITstockCaptains <- 
	df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, stockIdx_dlog, capgains_dlog, PIT_dlog) %>% 
	mutate(capgains_dlog = lag(capgains_dlog),
				 stockIdx_dlog = lag(stockIdx_dlog)
	) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("stockIdx_dlog", "capgains_dlog", "PIT_dlog"),
											     labels = c("SP500 total real return (1-year lag)", 
											     					  "% change in inflation-adjusted capital gains (1-year lag)", 
											     					  "% change in inflation-adjusted state individual income tax"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 10)) + 
	scale_color_manual(values = c("blue", RIG.red, color_PIT)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title    = "The stock market, capital gains, and income tax revenue",
			 subtitle = NULL,
			 caption  = "Source:\nSBBI Yearbook 2016;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances") + 
	theme(legend.position = "bottom")
fig_PITstockCaptains



# ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_PIT.png"), fig_cycle_PIT , width = 10*0.8, height = 6*0.8)
# ggsave(paste0(dir_fig_out, "fig_GovFin_stockCaptains.png"), fig_stockCaptains , width = 10*0.8, height = 6*0.8)
# ggsave(paste0(dir_fig_out, "fig_GovFin_PITstockCaptains.png"), fig_PITstockCaptains , width = 12*0.8, height = 7*0.85)



# Sales:  gdp growth, gen sales growth and selective sales growth

df_decomp_real2	%>% 
	filter(year >= 1985) %>% 
	select(state_abb, year, GDP_dlog, salesgen_dlog, salessel_dlog) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c( "salesgen_dlog", "salessel_dlog", "GDP_dlog"),
											labels = c("Real growth of general sales tax", "Real growth of selective sales tax", "Real GDP growth"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_salesgen, color_salessel, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Changes in sales taxes and GDP", 
			 subtitle = "(Real growth rate calculated based on 2009 dollar)") + 
	theme(legend.position = "bottom")


fig_cycle_sales <- # paperFigure
df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlogcycle, salesgen_dlogcycle, salessel_dlogcycle) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c( "salesgen_dlogcycle", "salessel_dlogcycle", "GDP_dlogcycle"),
											labels = c("Real growth of general sales tax", "Real growth of selective sales tax", "Real GDP growth"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_salesgen, color_salessel, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Cyclical changes in sales taxes and GDP", 
			 subtitle = "(Real growth rate calculated based on 2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, Federal Reserve Economic Data;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom")
fig_cycle_sales


ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_sales.png"), fig_cycle_sales, width = 10*0.8, height = 6.5*0.8)




# other:  gdp growth, growth of other taxes

df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlog, nonPITsalestot_dlog) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("nonPITsalestot_dlog", "GDP_dlog"),
											labels = c("Real growth of non-personal-income-non-sales taxes", "GDP_dlog"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c("blue", "lightblue", "darkgrey")) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of non-personal-income-non-sales taxes and GDP") + 
	theme(legend.position = "bottom")


fig_cycle_other <- 
df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlogcycle, nonPITsalestot_dlogcycle) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("nonPITsalestot_dlogcycle", "GDP_dlogcycle"),
											labels = c("Real growth of non-personal-income-non-sales taxes", "Real GDP growth"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_other, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of non-personal-income-non-sales taxes and GDP",
			 subtitle = "(Real growth rate calculated based on 2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, Federal Reserve Economic Data;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom")
fig_cycle_other

ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_other.png"), fig_cycle_other, width = 10*0.8, height = 6.5*0.8)



# Property tax: GDP and local property tax growth

fig_cycle_propertyLoc <- 
df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlogcycle, propertyLoc_dlogcycle) %>% 
	mutate(GDP_dlogcycle = lag(GDP_dlogcycle, 2)) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("propertyLoc_dlogcycle", "GDP_dlogcycle"),
											labels = c("Real growth of local property tax", "Real GDP growth"))) %>%
	qplot(x = year, y = 100 * value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 2)) + 
	scale_color_manual(values = c(color_propertyLoc, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of property tax (local governments) and GDP",
			 subtitle = "(Real growth rate calculated based on 2009 dollar)"
			 ) + 
	theme(legend.position = "bottom")
fig_cycle_propertyLoc

ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_propertyLoc.png"), fig_cycle_propertyLoc, width = 10*0.8, height = 6*0.8)




df_decomp_real2 %>% 
	filter(year %in% c(2009:2010)) %>% 
	select(year, GDP_dlogtrend,  
				       LagstockIdx_dlogtrend,
				       PIT_dlogtrend,
				       salesgen_dlogtrend, 
				       salessel_dlogtrend, 
				       nonPITsalestot_dlogtrend
				       )

df_decomp_real2 %>% 
	filter(year %in% c(2009:2010)) %>% 
	select(year, GDP_dlogcycle, 
				       LagstockIdx_dlogcycle,
				       PIT_dlogcycle, 
				       salesgen_dlogcycle, 
				       salessel_dlogcycle, 
				       nonPITsalestot_dlogcycle)



df_decomp_real2 %>% 
	filter(year %in% c(2002:2003)) %>% 
	select(year, GDP_dlogtrend,  
				 LagstockIdx_dlogtrend,
				 PIT_dlogtrend,
				 salesgen_dlogtrend, 
				 salessel_dlogtrend, 
				 nonPITsalestot_dlogtrend
	)

df_decomp_real2 %>% 
	filter(year %in% c(2002:2003)) %>% 
	select(year, GDP_dlogcycle, 
				 LagstockIdx_dlogcycle,
				 PIT_dlogcycle, 
				 salesgen_dlogcycle, 
				 salessel_dlogcycle, 
				 nonPITsalestot_dlogcycle)








#*******************************************************************************
#   Figures: trend components                                       ####
#*******************************************************************************



# Trend taxes as % of trend GDP

fig_trend_pctGDP <- # paperFigure
df_decomp_real2 %>% # PIT vs gen sales as % of GDP
	select(year, 
				 PIT_GDP_trend, 
				 salesgen_GDP_trend, 
				 salessel_GDP_trend,
				 nonPITsalestot_GDP_trend,
				 propertyLoc_GDP_trend) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, levels = c('PIT_GDP_trend', 
																			'salesgen_GDP_trend', 
																			'salessel_GDP_trend',
																			'nonPITsalestot_GDP_trend',
																			'propertyLoc_GDP_trend'),
	                         labels = c("Personal income tax (state)",
	                         					  "General sales tax (state)",
	                         					  "Selective sales tax (state)",
	                         					  "Non-personal-income-non-sales taxes (state)",
	                         					  "Property tax (local)"))) %>% 
	ggplot(aes(x = year, y = 100*value, color = var)) + theme_bw() + RIG.themeLite() + 
	geom_point() + 
	geom_line() + 
	coord_cartesian(ylim = c(0, 3)) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 0.5)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, color_salessel, color_other, color_propertyLoc)) + 
	labs(x = NULL, y = "Percent of GDP", color = NULL,
			 title = "Trends in taxes as a percentage of GDP ",
			 subtitle = "Calculated using real values (2009 dollar)",
			 caption = "Note: Trends are estimated using HP filter.
Source: 
      Federal Reserve Bank of St. Louis, Federal Reserve Economic Data; 
      U.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom") + 
	guides(col = guide_legend(ncol = 3, byrow = TRUE))
# geom_hline(yintercept = 1, linetype = 2)

fig_trend_pctGDP


# Trend growth of all vars
df_decomp_real2 %>% # 
	select(year, 
				 PIT_dlogtrend, 
				 salesgen_dlogtrend, 
				 salessel_dlogtrend,
				 nonPITsalestot_dlogtrend,
				 GDP_dlogtrend) %>% 
	gather(var, value, -year) %>%  
	filter(year >= 1978) %>% 
	qplot(x = year, y = 100*value, color = var, data = ., geom = c("line", "point")) + theme_bw() + RIG.themeLite() + 
	coord_cartesian(ylim = c(0, 6)) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 0.5)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, color_salessel, color_other, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Trend real growth rate of GDP and taxes") + 
	theme(legend.position = "bottom")



# Trend growth of GDP, PIT, and gen sales
df_decomp_real2 %>% # PIT vs gen sales as % of GDP
	select(year, 
				 PIT_dlogtrend, 
				 salesgen_dlogtrend, 
				 GDP_dlogtrend) %>% 
	gather(vars, value, -year) %>%  
	mutate(vars = factor(vars, levels = c("PIT_dlogtrend", "salesgen_dlogtrend", "GDP_dlogtrend"),
											      labels = c("Personal income tax", "General sales tax", "GDP"))) %>% 
	filter(year >= 1978) %>% 
	ggplot(aes(x = year, y = 100*value, color = vars, shape = vars)) + theme_bw() + RIG.themeLite() + 
	geom_point() + 
	geom_line() + 
	coord_cartesian(ylim = c(0, 6)) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 0.5)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL, shape = NULL,
			 title = "Trend real growth rate of GDP and taxes") + 
	theme(legend.position = "bottom")


# Trend growth rates, grid plot
fig_trend_growth <- # paperFigure
df_decomp_real2 %>% # 
	select(year, 
				 PIT_dlogtrend, 
				 salesgen_dlogtrend, 
				 salessel_dlogtrend,
				 nonPITsalestot_dlogtrend,
				 propertyLoc_dlogtrend) %>% 
	gather(tax_type, tax_growth, -year) %>% 
	left_join(df_decomp_real2 %>% select(year, GDP_dlogtrend)) %>% 
	gather(var, value, -year, -tax_type) %>% 
	mutate(tax_type = factor(tax_type, levels = c("PIT_dlogtrend", 
																				"salesgen_dlogtrend", 
																				"salessel_dlogtrend",
																				"nonPITsalestot_dlogtrend",
																				"propertyLoc_dlogtrend"),
											       labels = c("Personal income tax (state)",
											       					 "General sales tax (state)",
											       					 "Selective sales tax (state)",
											       					 "Non-personal-income-non-sales taxes (state)",
											       					 "Property tax (local)"
											       					 )),
				 var = factor(var, levels = c("tax_growth", "GDP_dlogtrend"),
				 						       labels = c("Real tax growth", "Real GDP growth"))
				 ) %>% 
	
	ggplot(aes(x = year, y = 100*value, color = var, shape = var)) + theme_bw() + RIG.themeLite() + 
	facet_wrap( ~tax_type , ncol = 2) + 
	geom_point() + 
	geom_line()+
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-100, 100, 1)) + 
	scale_color_manual(values = c("blue", color_GDP)) + 
	labs(x = NULL, y = "Growth rate (%)", color = NULL, shape = NULL,
			 title = "Trend real growth of GDP and tax revenues",
			 subtitle = "Real growth calculated based on 2009 dollar",
			 caption = "Note: Trends are estimated using HP filter.
Source: 
      Federal Reserve Bank of St. Louis, Federal Reserve Economic Data; 
      U.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom")
fig_trend_growth


ggsave(paste0(dir_fig_out, "fig_GovFin_trend_pctGDP.png"), fig_trend_pctGDP, width = 10*0.9, height = 7.5*0.9)
ggsave(paste0(dir_fig_out, "fig_GovFin_trend_growth.png"), fig_trend_growth, width = 8*0.9, height = 11*0.9)




#*******************************************************************************
#     Regression analysis US: PIT               ####
#*******************************************************************************

fig_GDP2taxes <- 
	df_decomp_real2 %>% 
	select(year, 
				 GDP_dlog, 
				 PIT_dlog,
				 salesgen_dlog) %>% 
	gather(var, value, -year) %>% 
	mutate(var = factor(var, levels = c("GDP_dlog", "PIT_dlog", "salesgen_dlog"),
											labels = c("GDP", "Personal income tax \n(state)", "General sales tax \n(state)")			
	)) %>% 
	ggplot() + 
	theme_bw() + RIG.themeLite() + 
	geom_line(aes(x = year, y = value, color = var)) + 
	geom_point(aes(x = year, y = value, color = var, shape = var)) +
	geom_hline(yintercept = 0, linetype = 2) + 
	geom_rect(data = recessionPeriods[-(1:5),],
						aes(xmin = peak, xmax = trough,
								ymin = -Inf, ymax = Inf), alpha = 0.4, fill = "grey") +
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-1, 1, 0.02), labels = percent) + 
	scale_color_manual(values = c(color_GDP, color_PIT, color_salesgen, color_other)) + 
	scale_shape_manual(values = c(15, 16, 17, 18)) + 
	labs(x = NULL, y = "Real growth", color = NULL, shape = NULL,
			 title = "Cycles in GDP growth and tax revenues",
			 subtitle = "Calculated using real values (2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, FRED;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances") + 
	theme(legend.position = "bottom") 
#guides(col = guide_legend(ncol = 3, byrow = TRUE))
# geom_hline(yintercept = 1, linetype = 2)

fig_GDP2taxes

ggsave(paste0(dir_fig_out, "fig_GovFin_GDP2taxes.png"), fig_GDP2taxes,    width = 10*0.8, height = 6*0.8)



# # National level regressions
df_reg_US <-
	df_decomp_real2 %>%
	mutate(
		     #Lag_capgains_chg_real2    = lag(capgains_chg_real2),
				 # Lag_dlLCapStock_TRI_real2 = lag(dlLCapStock_TRI_real2),
				 # Lagcapgains_chg_real  = lag(capgains_chg_real),
				 # LagLCapStock_TRI_real = lag(LCapStock_TRI_real),
				 after99 = ifelse(year > 1999, TRUE, FALSE),
				 after00 = ifelse(year > 2000, TRUE, FALSE),
				 after01 = ifelse(year > 2001, TRUE, FALSE),
				 after97 = ifelse(year > 1997, TRUE, FALSE),
				 after95 = ifelse(year > 1995, TRUE, FALSE),
				 after91 = ifelse(year > 1991, TRUE, FALSE),
				 recession = ifelse(year %in% c(2001:2002, 2008:2010), TRUE, FALSE),
				 d87_88 = ifelse(year %in% c(1987, 1988), TRUE, FALSE)
				 #RGSP_after99 = RGSP*after99 
				 ) %>%

	mutate(recession_recent1  = ifelse(year %in% c(2001:2002, 2008:2009), TRUE, FALSE),
				 recession_recent2  = ifelse(year %in% c(2001:2002, 2008:2010), TRUE, FALSE),
				 recession_all1  = ifelse(year %in% c(1980:1982, 1991, 2001:2002, 2008:2009), TRUE, FALSE),
				 recession_all2  = ifelse(year %in% c(1980:1982, 1991, 2001:2002, 2008:2010), TRUE, FALSE),
				 recession_8082 = ifelse(year %in% c(1980:1982), TRUE, FALSE),
				 recession_91   = ifelse(year %in% c(1991), TRUE, FALSE),
				 recession_0102 = ifelse(year %in% c(2001:2002), TRUE, FALSE),
				 recession_0103 = ifelse(year %in% c(2001:2003), TRUE, FALSE),
				 recession_0809 = ifelse(year %in% c(2008:2009), TRUE, FALSE),
				 recession_0810 = ifelse(year %in% c(2008:2010), TRUE, FALSE),
				 before1995     = ifelse(year < 1995 , TRUE, FALSE),
				 before1985     = ifelse(year < 1985 , TRUE, FALSE)
	)



## PIT, National level

## total growth

# GDP only
mod_PIT_GSP_dl <- # total growth 
	df_reg_US%>% 
	lm(PIT_dlog ~ GDP_dlog, data = .) 
mod_PIT_GSP_tot %>% summary

# GDP only, with break point
mod_PIT_GSPd99_dl <-
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + GDP_dlog:after99, data = .) 
mod_PIT_GSPd99_dl %>% summary

# GDP and capital gains
mod_PIT_GSP.Lagcapgain_real_dl <- 
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + Lagcapgains_dlog, data =.)
mod_PIT_GSP.Lagcapgain_real_dl %>% summary


# GDP and capital gains, both with breaking point
mod_PIT_GSPd99.Lagcapgaind99_real_dl <-
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + GDP_dlog:after99 + Lagcapgains_dlog + Lagcapgains_dlog:after99, data =.) 
mod_PIT_GSPd99.Lagcapgaind99_real_dl %>% summary


# GDP and capital gains, captial gains with a breakpoint

mod_PIT_GSP.Lagcapgaind99_real_dl <- 
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + Lagcapgains_dlog + Lagcapgains_dlog:after99, data =.)
mod_PIT_GSP.Lagcapgaind99_real_dl %>% summary


# GDP and real stock return
mod_PIT_GSP.Lagstock_real_dl <- 
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + LagstockIdx_dlog, data = .)
mod_PIT_GSP.Lagstock_real_dl %>% summary



# GDP and real stock return, both with a breakpoint
mod_PIT_GSPd99.Lagstockd99_real_dl <- 
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + GDP_dlog:after99 + LagstockIdx_dlog + LagstockIdx_dlog:after99, data =.)
mod_PIT_GSPd99.Lagstockd99_real_dl %>% summary


# GDP and real stock return, stock return with a breakpoint
mod_PIT_GSP.Lagstockd99_real_dl <- 
	df_reg_US %>% 
	lm(PIT_dlog ~ GDP_dlog + LagstockIdx_dlog + LagstockIdx_dlog:after99, data =.)
mod_PIT_GSP.Lagstockd99_real_dl %>% summary


Table_PIT_param_realStock_dl <-
	bind_rows(
		mod_PIT_GSP_dl                  %>% tidy() %>% mutate(mod = "PIT_GSP"),
		mod_PIT_GSPd99_dl                  %>% tidy() %>% mutate(mod = "PIT_GSPd99"),
		mod_PIT_GSP.Lagstock_real_dl       %>% tidy() %>% mutate(mod = "PIT_GSP.Lagstock"),
		mod_PIT_GSPd99.Lagstockd99_real_dl %>% tidy() %>% mutate(mod = "PIT_GSPd99.Lagstockd99"),
		mod_PIT_GSP.Lagstockd99_real_dl    %>% tidy() %>% mutate(mod = "PIT_GSP.Lagstockd99"),
		
		mod_PIT_GSP.Lagcapgain_real_dl       %>% tidy() %>% mutate(mod = "PIT_GSP.Lagcapgain"),
		mod_PIT_GSPd99.Lagcapgaind99_real_dl %>% tidy() %>% mutate(mod = "PIT_GSPd99.Lagcapgaind99"),
		mod_PIT_GSP.Lagcapgaind99_real_dl    %>% tidy() %>% mutate(mod = "PIT_GSP.Lagcapgaind99")
	)

Table_PIT_glance_realStock_dl <-
	bind_rows(
		mod_PIT_GSP_dl                     %>% glance() %>% mutate(mod = "PIT_GSP"),
		mod_PIT_GSPd99_dl                  %>% glance() %>% mutate(mod = "PIT_GSPd99"),
		mod_PIT_GSP.Lagstock_real_dl       %>% glance() %>% mutate(mod = "PIT_GSP.Lagstock"),
		mod_PIT_GSPd99.Lagstockd99_real_dl %>% glance() %>% mutate(mod = "PIT_GSPd99.Lagstockd99"),
		mod_PIT_GSP.Lagstockd99_real_dl    %>% glance() %>% mutate(mod = "PIT_GSP.Lagstockd99"),
		
		mod_PIT_GSP.Lagcapgain_real_dl        %>% glance() %>% mutate(mod = "PIT_GSP.Lagcapgain"),
		mod_PIT_GSPd99.Lagcapgaind99_real_dl  %>% glance() %>% mutate(mod = "PIT_GSPd99.Lagcapgaind99"),
		mod_PIT_GSP.Lagcapgaind99_real_dl     %>% glance() %>% mutate(mod = "PIT_GSP.Lagcapgaind99")
	)


write.xlsx2(Table_PIT_param_realStock_dl,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_PIT_dl.xlsx"), sheet = "PIT_param" )
write.xlsx2(Table_PIT_glance_realStock_dl, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_PIT_dl.xlsx"), sheet = "PIT_glance", append = TRUE)


# # Quandt likelihood ratio test for structural breaks
m0 <- 
	df_reg_US %>% 
	mutate(breakpoint = ifelse(year > 2000, TRUE, FALSE)) %>% 
	lm(PIT_dlog ~ GDP_dlog + GDP_dlog:breakpoint, data =.)
summary(m0)
# HP filter t-value are high in 1995-1999, 1996 is the highest, significant at 5% with central 70% sample, 
# significant at 10% level for the central 30% sample (about 1991-2002)

# Hamilton Filter: sharp peak in 2001

m1 <- 
	df_reg_US %>% 
	mutate(breakpoint = ifelse(year > 1997, TRUE, FALSE)) %>% 
	lm(PIT_dlog ~ GDP_dlogcycle_H + Lagcapgains_dlog + Lagcapgains_dlog:breakpoint, data =.)
summary(m1)
# HP filter: t-value are high in 1995-1999, 1997 is the highest, significant at 5% with central 70% sample
# Hamilton filter: high t values in 1997 to 2001
# total growth: peak at 1997

m2 <- 
	df_reg_US %>% 
	mutate(breakpoint = ifelse(year > 1999, TRUE, FALSE)) %>% 
	lm(PIT_dlog ~ GDP_dlog + LagstockIdx_dlog + LagstockIdx_dlog:breakpoint, data =.)
summary(m2)
# HP filtert: value are high in 1995-1999, 1997 and 1999 are the highest (1999 slightly higher), significant at 5% with central 70% sample
# Hamilton filter: with sharp peak in 2001  


pi1 <- 14/38
pi2 <- 15/38



fig_PITstockCapgains <- 
	df_decomp_real2	%>% 
	filter(year >= 1988) %>% 
	select(state_abb, year, stockIdx_dlog, capgains_dlog, PIT_dlog) %>% 
	mutate(capgains_dlog = lag(capgains_dlog),
				 stockIdx_dlog = lag(stockIdx_dlog)
	) %>%
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("stockIdx_dlog", "capgains_dlog", "PIT_dlog"),
											labels = c("SP500 total real return (1-year lag)", 
																 "% change in inflation-adjusted capital gains (1-year lag)", 
																 "% change in inflation-adjusted state individual income tax"))) %>%
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-1, 1, 0.1), labels = percent) + 
	scale_color_manual(values = c("blue", RIG.red, color_PIT)) + 
	labs(x = NULL, y = NULL, color = NULL,
			 title    = "The stock market, capital gains, and income tax revenue",
			 subtitle = NULL,
			 caption  = "Source:\nSBBI Yearbook 2016;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances") + 
	theme(legend.position = "bottom")
fig_PITstockCapgains

ggsave(paste0(dir_fig_out, "fig_GovFin_stockCapgains.png"), fig_PITstockCapgains , width = 10*0.8, height = 6*0.8)



# # GSP only
# mod_PIT_GSP <- # cycle growth, HP 
# df_reg_US%>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle, data = .) 
# mod_PIT_GSP %>% summary
# 
# 
# # GSP only, with breakpoint
# mod_PIT_GSPd97 <- # cycle growth, HP 
# df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:after97, data = .) 
# mod_PIT_GSPd97 %>% summary
#   # after 99/95/91 is significant. pre-99 is sig at 10%, pre95/91 not significant
# 
# 
# 
# # GSP and Lag SP500 total return
# mod_PIT_GSP.Lagstock_real <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + LagstockIdx_dlogcycle, data = .)
# mod_PIT_GSP.Lagstock_real %>% summary
# # only both are significant
# 
# mod_PIT_GSP.Lagstock_real_dl <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlog ~ GDP_dlog + LagstockIdx_dlog, data = .)
# mod_PIT_GSP.Lagstock_real_dl %>% summary
# 
# 
# 
#  # with breakpoint dummy for GSP and stock return
# mod_PIT_GSPd97.Lagstockd97_real <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:after97 + LagstockIdx_dlogcycle + LagstockIdx_dlogcycle:after97, data =.)
# mod_PIT_GSPd97.Lagstockd97_real %>% summary
# # both GDP and stock are only sig after 99
# 
# mod_PIT_GSPd97.Lagstockd97_real_dl <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlog ~ GDP_dlog + GDP_dlog:after99 + LagstockIdx_dlog + LagstockIdx_dlog:after99, data =.)
# mod_PIT_GSPd97.Lagstockd97_real_dl %>% summary
# 
# 
# # with breakpoint dummy for stock return
# mod_PIT_GSP.Lagstockd97_real <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + LagstockIdx_dlogcycle + LagstockIdx_dlogcycle:after97, data =.)
# mod_PIT_GSP.Lagstockd97_real %>% summary
# 
# 
# mod_PIT_GSP.Lagstockd99_real_dl <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlog ~ GDP_dlog + LagstockIdx_dlog + LagstockIdx_dlog:after97, data =.)
# mod_PIT_GSP.Lagstockd97_real_dl %>% summary
# 
# 
# 
# 
# 
# # GSP and lag of capital gains
# mod_PIT_GSP.Lagcapgain_real <- 
# df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + Lagcapgains_dlogcycle, data =.)
# mod_PIT_GSP.Lagcapgain_real %>% summary
# 
# 
# 
# # GSP and lag of capital gains, with breakpoints for both
# mod_PIT_GSPd97.Lagcapgaind97_real <-
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:after97 + Lagcapgains_dlogcycle + Lagcapgains_dlogcycle:after97, data =.) 
# mod_PIT_GSPd97.Lagcapgaind97_real %>% summary
# # GDP after 99 not significant
# 
# mod_PIT_GSPd97.Lagcapgaind97_real_H <-
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle_H ~ GDP_dlogcycle_H + GDP_dlogcycle_H:after97 + Lagcapgains_dlogcycle_H + Lagcapgains_dlogcycle_H:after97, data =.) 
# mod_PIT_GSPd97.Lagcapgaind97_real_H %>% summary
# 
# 
# 
# # GSP and lag of capital gains, with breakpoint for capital gains
# mod_PIT_GSP.Lagcapgaind97_real <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlogcycle ~ GDP_dlogcycle + Lagcapgains_dlogcycle + Lagcapgains_dlogcycle:after97, data =.)
# mod_PIT_GSP.Lagcapgaind97_real %>% summary
# 
# mod_PIT_GSP.Lagcapgaind99_real_dl <- 
# 	df_reg_US %>% 
# 	lm(PIT_dlog ~ GDP_dlog + Lagcapgains_dlog + Lagcapgains_dlog:after99, data =.)
# mod_PIT_GSP.Lagcapgaind99_real_dl %>% summary
# 
# 
# 

# 
# 
# ## For draft:
# 
# Table_PIT_param_realStock <-
# 	bind_rows(
# 		mod_PIT_GSP                     %>% tidy() %>% mutate(mod = "PIT_GSP"),
# 		mod_PIT_GSPd97                  %>% tidy() %>% mutate(mod = "PIT_GSPd97"),
# 		mod_PIT_GSP.Lagstock_real       %>% tidy() %>% mutate(mod = "PIT_GSP.Lagstock"),
# 		mod_PIT_GSPd97.Lagstockd97_real %>% tidy() %>% mutate(mod = "PIT_GSPd97.Lagstockd97"),
# 		mod_PIT_GSP.Lagstockd97_real    %>% tidy() %>% mutate(mod = "PIT_GSP.Lagstockd97"),
# 
# 		mod_PIT_GSP.Lagcapgain_real       %>% tidy() %>% mutate(mod = "PIT_GSP.Lagcapgain"),
# 		mod_PIT_GSPd97.Lagcapgaind97_real %>% tidy() %>% mutate(mod = "PIT_GSPd97.Lagcapgaind97"),
# 		mod_PIT_GSP.Lagcapgaind97_real    %>% tidy() %>% mutate(mod = "PIT_GSP.Lagcapgaind97")
# 	)
# 
# Table_PIT_glance_realStock <-
# 	bind_rows(
# 		mod_PIT_GSP                     %>% glance() %>% mutate(mod = "PIT_GSP"),
# 		mod_PIT_GSPd97                  %>% glance() %>% mutate(mod = "PIT_GSPd97"),
# 		mod_PIT_GSP.Lagstock_real       %>% glance() %>% mutate(mod = "PIT_GSP.Lagstock"),
# 		mod_PIT_GSPd97.Lagstockd97_real %>% glance() %>% mutate(mod = "PIT_GSPd97.Lagstockd97"),
# 		mod_PIT_GSP.Lagstockd97_real    %>% glance() %>% mutate(mod = "PIT_GSP.Lagstockd97"),
# 
# 		mod_PIT_GSP.Lagcapgain_real        %>% glance() %>% mutate(mod = "PIT_GSP.Lagcapgain"),
# 		mod_PIT_GSPd97.Lagcapgaind97_real  %>% glance() %>% mutate(mod = "PIT_GSPd97.Lagcapgaind97"),
# 		mod_PIT_GSP.Lagcapgaind97_real     %>% glance() %>% mutate(mod = "PIT_GSP.Lagcapgaind97")
# 	)
# 
# 
# write.xlsx2(Table_PIT_param_realStock,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_PIT.xlsx"), sheet = "PIT_param" )
# write.xlsx2(Table_PIT_glance_realStock, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_PIT.xlsx"), sheet = "PIT_glance", append = TRUE)
# 
# # mod_PIT_GSP
# 


#*******************************************************************************
#     Regression analysis US: General sales        ####
#*******************************************************************************


# total growth
mod_salesgen_GSP_dl <- 
	df_reg_US %>% filter(state_abb == "US", year >=1985) %>% 
	#mutate(RGSP = GDP_FRED) %>%
	lm(salesgen_dlog ~ GDP_dlog, data = .) 
mod_salesgen_GSP_dl %>% summary


mod_salesgen_GSPrec85_dl <- # with recession dummy
	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(salesgen_dlog ~ GDP_dlog + recession_0103 + recession_0810 , data = .) 
mod_salesgen_GSPrec85_dl %>% summary


mod_salesgen_GSPrec95_dl <- # with recession dummy
	df_reg_US %>% filter(state_abb == "US", year >= 1995) %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(salesgen_dlog ~ GDP_dlog + recession_0103 + recession_0810 , data = .) 
mod_salesgen_GSPrec95_dl %>% summary
# 91 recession is not significant


Table_salesgen_new_param_dl <- 
	bind_rows(
		mod_salesgen_GSP_dl             %>% tidy() %>% mutate(mod = "salesgen_GSP"),
		mod_salesgen_GSPrec85_dl        %>% tidy() %>% mutate(mod = "salesgen_GSPrec85"),
		mod_salesgen_GSPrec95_dl        %>% tidy() %>% mutate(mod = "salesgen_GSPrec95")
	)

Table_salesgen_new_glance_dl <- 
	bind_rows(
		mod_salesgen_GSP_dl             %>% glance() %>% mutate(mod = "salesgen_GSP"),
		mod_salesgen_GSPrec85_dl        %>% glance() %>% mutate(mod = "salesgen_GSPrec85"),
		mod_salesgen_GSPrec95_dl        %>% glance() %>% mutate(mod = "salesgen_GSPrec95")
	)

write.xlsx2(Table_salesgen_new_param_dl,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesgen_dl.xlsx"), sheet = "sales_param" )
write.xlsx2(Table_salesgen_new_glance_dl, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesgen_dl.xlsx"), sheet = "sales_glance", append = TRUE)



fig_cycle_sales <- # paperFigure
	df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlog, salesgen_dlog, salessel_dlog) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c( "salesgen_dlog", "salessel_dlog", "GDP_dlog"),
											labels = c("Real growth of general sales tax", "Real growth of selective sales tax", "Real GDP growth"))) %>%
	qplot(x = year, y =  value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-1, 1, 0.02), labels = percent) + 
	scale_color_manual(values = c(color_salesgen, color_salessel, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of sales taxes and GDP", 
			 subtitle = "(Based on 2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, Federal Reserve Economic Data;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom")
fig_cycle_sales

ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_sales_dl.png"), fig_cycle_sales, width = 10*0.8, height = 6.5*0.8)








# ## general sales tax for the paper
# 
# # Cycle growth
# mod_salesgen_GSP <- 
# 	df_reg_US %>% filter(state_abb == "US", year >=1985) %>% 
# 	#mutate(RGSP = GDP_FRED) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle, data = .) 
# mod_salesgen_GSP %>% summary
# 
# 
# mod_salesgen_GSPrec85 <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_0103 + recession_0810 , data = .) 
# mod_salesgen_GSPrec85 %>% summary
# 
# 
# mod_salesgen_GSPrec95 <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1995) %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_0103 + recession_0810 , data = .) 
# mod_salesgen_GSPrec95 %>% summary
# # 91 recession is not significant



# # GDP, dummy for the recent two recessions and interaction with GDP
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(year >= 1985) %>% 	
# 	lm(salesgen_dlogcycle ~ recession_recent2*GDP_dlogcycle, data =.) 
# mod_salesgen_GSPrec %>% summary
# # interaction term is insignificant, not sensitive to sample period, so drop interaction term here after.



# # GDP, dummy for the recent two recessions, with different sample period
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1977) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent1, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1977) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent2, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent1, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent2, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1995) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent1, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1995) %>% 
# 	mutate(RGSP = GDP_dlogcycle) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_recent2, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# # # Observations:
# # 1. The dummy 2008:2010 seems works better than the dummy 2008:2009: higher R^2 and better significance, while with lower GDP elasticity
# # 2. Sample period:
# #      - The more recent the sample period, the lower the GDP elasticity. But the 1995: sampel may be too short.
# #      - The real sales tax revenue were very volatile before 1980, were there policy/rate changes? Reasonable to exclude that period?
# # 3. When the recent-recessions dummy is added, the GDP elasticity becomes considerably lower
# #      - 1985-sample with 2008:2010 dummy: e = 0.70, dummy = -3.7%
# #      - 1985-sample with 2008:2009 dummy: e = 0.82, dummy = -2.6%
# # 
# #      - 1977-sample with 2008:2010 dummy: e = 0.96, dummy = -2.9%
# #      - 1977-sample with 2008:2009 dummy: e = 1.03, dummy = -1.8% (p = 0.15)
# 
# 
# 
# # GDP, dummy for the recent two recessions, GDP interacted with dummy for before 1995
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1977) %>% 
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:before1995 +  recession_recent2, data = .) 
# mod_salesgen_GSPrec %>% summary
#  # interaction term does not significant
# 
# 
# 
# # GDP, dummy for all recessions
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_all2, data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1977) %>% 
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_all2 , data = .) 
# mod_salesgen_GSPrec %>% summary
# 
# 
# # GDP, dummies for single recessions
# mod_salesgen_GSPrec <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_0810 + recession_0102, data = .) 
# mod_salesgen_GSPrec %>% summary
#  # use separate dummies for 2001 and 2008 recessions, only 2008 recession is significant
# 
# 
# # Interaction between GDP and recessions
# mod_salesgen <- # with recession dummy
# 	df_reg_US %>% filter(state_abb == "US", year >= 1985) %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(salesgen_dlogcycle ~ GDP_dlogcycle + recession_0103:GDP_dlogcycle +  recession_0810:GDP_dlogcycle, data = .) 
# mod_salesgen %>% summary
# # interaction terms are not significant
# 



# ## Table for policy brief
# 
# Table_salesgen_new_param <- 
# 	bind_rows(
# 		mod_salesgen_GSP             %>% tidy() %>% mutate(mod = "salesgen_GSP"),
# 		mod_salesgen_GSPrec85        %>% tidy() %>% mutate(mod = "salesgen_GSPrec85"),
# 		mod_salesgen_GSPrec95        %>% tidy() %>% mutate(mod = "salesgen_GSPrec95")
# 	)
# 
# Table_salesgen_new_glance <- 
# 	bind_rows(
# 		mod_salesgen_GSP             %>% glance() %>% mutate(mod = "salesgen_GSP"),
# 		mod_salesgen_GSPrec85        %>% glance() %>% mutate(mod = "salesgen_GSPrec85"),
# 		mod_salesgen_GSPrec95        %>% glance() %>% mutate(mod = "salesgen_GSPrec95")
# 	)
# 
# write.xlsx2(Table_salesgen_new_param,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesgen.xlsx"), sheet = "sales_param" )
# write.xlsx2(Table_salesgen_new_glance, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesgen.xlsx"), sheet = "sales_glance", append = TRUE)
# 



#*******************************************************************************
#     Regression analysis US: Selective sales                               ####
#*******************************************************************************

## selective sales tax for paper

mod_salesselect_GSP_dl <- 
	df_reg_US %>% filter(state_abb == "US", year >=1985) %>% 
	#mutate(RGSP = GDP_FRED) %>%
	lm(salessel_dlog ~ GDP_dlog, data = .) 
mod_salesselect_GSP_dl %>% summary

# GDP with two recessions since 1985
mod_salesselect_GSPrec85_dl <- 
	df_reg_US %>% filter(state_abb == "US", year >=1985) %>% 
	#mutate(RGSP = GDP_FRED) %>%
	lm(salessel_dlog ~ GDP_dlog + recession_0103 + recession_0810, data = .) 
mod_salesselect_GSPrec85_dl %>% summary

# GDP with two recessions since 1995
mod_salesselect_GSPrec95_dl <- 
	df_reg_US %>% filter(state_abb == "US", year >=1995) %>% 
	#mutate(RGSP = GDP_FRED) %>%
	lm(salessel_dlog ~ GDP_dlog + recession_0103 + recession_0810, data = .) 
mod_salesselect_GSPrec95_dl %>% summary


## Table for policy brief
Table_salesselect_new_param_dl <- 
	bind_rows(
		mod_salesselect_GSP_dl             %>% tidy() %>% mutate(mod = "salesselect_GSP"),
		mod_salesselect_GSPrec85_dl        %>% tidy() %>% mutate(mod = "salesselect_GSPrec85"),
		mod_salesselect_GSPrec95_dl        %>% tidy() %>% mutate(mod = "salesselect_GSPrec95")
		# mod_salesselect_GSPrecAll85     %>% tidy() %>% mutate(mod = "salesselect_GSPrecAll85")
	)

Table_salesselect_new_glance_dl <- 
	bind_rows(
		mod_salesselect_GSP_dl             %>% glance() %>% mutate(mod = "salesselect_GSP"),
		mod_salesselect_GSPrec85_dl        %>% glance() %>% mutate(mod = "salesselect_GSPrec85"),
		mod_salesselect_GSPrec95_dl        %>% glance() %>% mutate(mod = "salesselect_GSPrec95")
		# mod_salesselect_GSPrecAll85     %>% glance() %>% mutate(mod = "salesselect_GSPrecAll85")
		
	)

write.xlsx2(Table_salesselect_new_param_dl,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesselect_dl.xlsx"), sheet = "sales_param" )
write.xlsx2(Table_salesselect_new_glance_dl, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_salesselect_dl.xlsx"), sheet = "sales_glance", append = TRUE)





#*******************************************************************************
#     Regression analysis US: Other             ####
#*******************************************************************************

# non PIT/sales National level (corp income + property + other)

# # Cycle growth, HP
# mod_other_GSP <- 
# df_reg_US %>% filter(state_abb == "US") %>% 
# 	mutate(RGSP = GDP_FRED) %>% 
# 	lm(nonPITsalestot_dlogcycle ~ GDP_dlogcycle, data = .) 
# mod_other_GSP %>% summary
# # significant, largely attributable to corp income?
# 
# mod_other_GSPd <- 
# df_reg_US %>% filter(state_abb == "US") %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(nonPITsalestot_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:after00, data = .) 
# mod_other_GSPd  %>% 	summary
# # interaction with after1999 significant
# 
# mod_other_GSPrecInter <- 
# df_reg_US %>% filter(state_abb == "US") %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(nonPITsalestot_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:recession_recent2, data = .) 
# mod_other_GSPrecInter %>%  summary
# # interaction with recession not significant
# 
# mod_other_GSPrecDummy <- 
# 	df_reg_US %>% filter(state_abb == "US") %>% 
# 	mutate(RGSP = GDP_FRED) %>%
# 	lm(nonPITsalestot_dlogcycle ~ GDP_dlogcycle + recession_0102 + recession_0810, data = .) 
# mod_other_GSPrecDummy %>%  summary


# total growth
mod_other_GSP_dl <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(RGSP = GDP_FRED) %>% 
	lm(nonPITsalestot_dlog ~ GDP_dlog, data = .) 
mod_other_GSP_dl %>% summary
# significant, largely attributable to corp income?

mod_other_GSPd_dl <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(nonPITsalestot_dlog ~ GDP_dlog + GDP_dlog:after00, data = .) 
mod_other_GSPd_dl  %>% 	summary
# interaction with after1999 significant

mod_other_GSPrecInter_dl <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(nonPITsalestot_dlog ~ GDP_dlog + GDP_dlog:recession_recent2, data = .) 
mod_other_GSPrecInter_dl %>% summary
# interaction with recession not significant

mod_other_GSPrecDummy_dl <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(nonPITsalestot_dlog ~ GDP_dlog + recession_0102 + recession_0810, data = .) 
mod_other_GSPrecDummy_dl %>% summary




m1 <- 
df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(RGSP = GDP_FRED) %>%
	lm(nonPITsalestot_dlogcycle ~ GDP_dlogcycle + GDP_dlogcycle:recession_recent2 + GDP_dlogcycle:recession_recent2, data = .) 
summary(m1)


# QLR test
m1 <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
  mutate(breakpoint = ifelse(year > 2002, TRUE, FALSE)) %>% 
	lm(nonPITsalestot_dlogcycle_H ~ GDP_dlogcycle_H + GDP_dlogcycle_H:breakpoint, data = .) 
summary(m1)
# HP filter: t-value is high for 1995 to 2001, with 2000 being the highest 
# Hamiltion filter: a modest peak in 2001




Table_other_param_dl <- 
	bind_rows(
		mod_other_GSP_dl               %>% tidy() %>% mutate(mod = "other_GSP"),
		mod_other_GSPd_dl              %>% tidy() %>% mutate(mod = "other_GSPd"),
		mod_other_GSPrecInter_dl       %>% tidy() %>% mutate(mod = "other_GSPrecInter"),
		mod_other_GSPrecDummy_dl       %>% tidy() %>% mutate(mod = "other_GSPrecDummy")
	)

Table_other_glance_dl <- 
	bind_rows(
		mod_other_GSP_dl               %>% glance() %>% mutate(mod = "other_GSP"),
		mod_other_GSPd_dl              %>% glance() %>% mutate(mod = "other_GSPd"),
		mod_other_GSPrecInter_dl       %>% glance() %>% mutate(mod = "other_GSPrecInter"),
		mod_other_GSPrecDummy_dl       %>% glance() %>% mutate(mod = "other_GSPrecDummy")
	)

write.xlsx2(Table_other_param_dl,  file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_other_dl.xlsx"), sheet = "other_param" )
write.xlsx2(Table_other_glance_dl, file = paste0(dir_fig_out, "Table_GovFin_regression_cycle_other_dl.xlsx"), sheet = "other_glance", append = TRUE)


fig_cycle_other <- 
	df_decomp_real2	%>% 
	#filter(year >= 1988) %>% 
	select(state_abb, year, GDP_dlog, nonPITsalestot_dlog) %>% 
	gather(var, value, -state_abb, -year) %>% 
	mutate(var = factor(var, levels = c("nonPITsalestot_dlog", "GDP_dlog"),
											labels = c("Real growth of non-personal-income-non-sales taxes", "Real GDP growth"))) %>%
	qplot(x = year, y = value, color = var, data=.,  geom = c("line", "point")) + 
	theme_bw() + RIG.themeLite() + 
	geom_hline(yintercept = 0, linetype = 2) + 
	scale_x_continuous(breaks = seq(1950, 2020, 5)) + 
	scale_y_continuous(breaks = seq(-1, 1, 0.02), labels = percent) + 
	scale_color_manual(values = c(color_other, color_GDP)) + 
	labs(x = NULL, y = "Percent", color = NULL,
			 title = "Real growth rates of non-personal-income-non-sales taxes and GDP",
			 subtitle = "(Real growth rate calculated based on 2009 dollar)",
			 caption  = "Source: \nFederal Reserve Bank of St. Louis, Federal Reserve Economic Data;\nU.S. Census Bureau, Annual Survey of State and Local Government Finances.") + 
	theme(legend.position = "bottom")
fig_cycle_other

ggsave(paste0(dir_fig_out, "fig_GovFin_cycle_other_dl.png"), fig_cycle_other, width = 10*0.8, height = 6.5*0.8)





#*******************************************************************************
#     Regression analysis US: Property                                      ####
#*******************************************************************************

mod_propertyLoc <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(LagGDP_dlogcycle  = lag(GDP_dlogcycle),
				 Lag2GDP_dlogcycle = lag(GDP_dlogcycle, 3)) %>% 
	lm(propertyLoc_dlogcycle ~ Lag2GDP_dlogcycle , data = .) 
mod_propertyLoc%>%  summary

# GDP growth and lagged GDP growth are not significant. F-test not significant. Extremely low R-squared. 
# Regression results cannot be used for assumptions. 



mod_propertyLoc_dl <- 
	df_reg_US %>% filter(state_abb == "US") %>% 
	mutate(LagGDP_dlog  = dplyr::lag(GDP_dlog),
				 Lag2GDP_dlog = dplyr::lag(GDP_dlog, 3)) %>% 
	lm(propertyLoc_dlog ~ LagGDP_dlog , data = .) 
mod_propertyLoc_dl %>%  summary


# What to do:
  # 1. 0 elasticity
  # 2. Only affected by GDP in severe recessions (need to define severe recessions. )




#*******************************************************************************
#       Regression analysis panel: PIT               ####
#*******************************************************************************
library(plm)


# Continent states only, excluding states with no PIT
df_reg_plm_PIT <-  df_reg_temp %>% 
	filter(!state_abb %in% c("US", "DC", "AL", "HI", "AK", "FL", "NV", "SD", "TX", "WA", "WY")) %>% 
	pdata.frame(index("state_abb", "year"))

# df_reg_plm_PIT %>% filter(is.na(RGSP_ext), year == 2013)


# No breaking point at 1999
df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_indivIncome_real_state ~ RGSP_ext, data =., model = "within") %>% 
	summary
	
df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_indivIncome_real_state ~ RGSP_ext + LCapStock_TRI, data =., model = "within") %>% 
	summary


# breaking point at 1999
df_reg_plm_PIT %>%   # GSP only
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>% 
	plm(tax_indivIncome_real_state ~ RGSP + RGSP:after99, data =., model = "within") %>% 
	summary

df_reg_plm_PIT %>%   # break for GSP and sp500
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>% 
	plm(tax_indivIncome_real_state ~ RGSP + RGSP:after99 + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "within") %>% 
	summary

df_reg_plm_PIT %>%  # break for sp500 only 
	filter(year %in% 1988:2015) %>% 
	#mutate(RGSP = RGSP_ext) %>% 
	plm(tax_indivIncome_real_state ~ RGSP + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "within") %>% 
	summary


# Estimation for states separately
df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	pvcm(tax_indivIncome_real_state ~ RGSP_ext + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "within") 


# Pooling test
df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
  pooltest(tax_indivIncome_real_state ~ RGSP + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "within")
# Cannot reject the H0 (poolable)
# However, when estimated separately, the estimates vary greatly across states


# Hausman test
gw <- 
  df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_indivIncome_real_state ~ RGSP_ext + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "within")

gr <- 
	df_reg_plm_PIT %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_indivIncome_real_state ~ RGSP_ext + LCapStock_TRI + LCapStock_TRI:after99, data =., model = "random")

phtest(gw, gr) # cannot reject H0 that both are consistent (fixed effect prefered.




#*******************************************************************************
#       Regression analysis panel: sales             ####
#*******************************************************************************
# Notes
 # Poolability rejected, FE preferred
 # dummary after 1999 insignificant
 # Elasticity estimated by FE is around 0.77. This is even slightly higher than the FE estimate of the elasticity of PIT
 # Elasticity of select sales tax (FE) is 0.45

# Continent states only, excluding states with no general sales
df_reg_plm_sales <-  df_reg_temp %>% 
	filter(!state_abb %in% c("US", "DC", "AL", "AK", "HI", "DE", "MT", "NH", "OR")) %>% 
	pdata.frame(index("state_abb", "year"))

#df_reg_plm_sales_gen %>% filter(is.na(tax_sales_gen_real_state), year == 2013)


# No breaking point at 1999
df_reg_plm_sales_gen %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_sales_gen_real_state ~ RGSP_ext, data =., model = "within") %>% 
	summary

df_reg_plm_sales_gen %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_sales_gen_real_state ~ RGSP_ext + RGSP_ext:after99, data =., model = "within") %>% 
	summary 
  # dummy after99 not significant

df_reg_plm_sales_gen %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_sales_select_real_state ~ RGSP_ext, data =., model = "within") %>% 
	summary




# Estimation for states separately
df_reg_plm_sales %>% 
	filter(year %in% 1978:2015) %>% 
	pvcm(tax_sales_gen_real_state ~ RGSP_ext , data =., model = "within") %>% 
	summary


# Pooling test
df_reg_plm_sales %>% 
	filter(year %in% 1978:2015) %>% 
	pooltest(tax_sales_gen_real_state~ RGSP_ext, data =., model = "within")
# Poolability is rejected



# Hausman test
gw <- 
	df_reg_plm_sales %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_sales_gen_real_state ~ RGSP_ext, data =., model = "within")

gr <- 
	df_reg_plm_sales %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_sales_gen_real_state ~ RGSP_ext, data =., model = "random")

phtest(gw, gr) # cannot reject H0 that both are consistent at 5% level (fixed effect prefered).
               # but p-value is only 0.05


#*******************************************************************************
#       Regression analysis panel: other             ####
#*******************************************************************************
# Notes
# Poolability rejected, FE preferred
# dummary after 1999 insignificant
# Elasticity estimated by FE is around 0.77. This is even slightly higher than the FE estimate of the elasticity of PIT
# Elasticity of select sales tax (FE) is 0.45

# Continent states only, excluding states with no general sales
df_reg_plm_other <-  df_reg_temp %>% 
	filter(!state_abb %in% c("US", "DC")) %>% 
	pdata.frame(index("state_abb", "year"))

# df_reg_plm_other %>% filter(is.na(tax_nonPITsalestot_real_state), year == 2013)


# No breaking point at 1999
df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_nonPITsalestot_real_state ~ RGSP_ext, data =., model = "within") %>% 
	summary # 0.91

df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	plm(tax_nonPITsalestot_real_state ~ RGSP_ext + RGSP_ext:after99, data =., model = "within") %>% 
	summary 
# dummy after99 significant, would double elasticity 




# Estimation for states separately
df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	pvcm(tax_nonPITsalestot_real_state ~ RGSP_ext , data =., model = "within") %>% 
	summary


# Pooling test
df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	pooltest(tax_nonPITsalestot_real_state~ RGSP_ext, data =., model = "within")
# Poolability cannot be rejected



# Hausman test
gw <- 
	df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_nonPITsalestot_real_state ~ RGSP_ext, data =., model = "within")

gr <- 
	df_reg_plm_other %>% 
	filter(year %in% 1978:2015) %>% 
	mutate(RGSP = RGSP_ext) %>%
	plm(tax_nonPITsalestot_real_state ~ RGSP_ext, data =., model = "random")

phtest(gw, gr) # cannot reject H0 that both are consistent (fixed effect prefered).





#**********************************************************************
#       Descriptive analysis 1: structure of state tax revenue     ####
#**********************************************************************

# For US aggregate:
# The share of sales tax is quite stable, ~total sales 50%, general sales tax 30%. 
# The share of personal income taxes has increased by half, from 25% in 1977 to 37% in 2015 
# The share of property tax is around 1.5%~2.5%. 
# The share of corp income tax decreased from 9% to 5%

## Sales+PIT as a % of total tax revenue.
# Total sales
# tot sales + PIT accounts for 70%+ of tax revenue (most above 80%), 
# States with sales + PIT less than 70% of tax rev are: AK(30%), WY(42%), NH(43%), ND(43%), DE(47%), VT(57%), MT(63%)

# General sales
# general sales + PIT: mostly 60%+ (68% for US)
# States with general sales + PIT less than 50%:  AK, WY, NH, ND, DE, VT, MT


# Year 2015: ordered by share of individual income tax
# Top Five in 2015:       OR(69%), VA(58%), NY(56%), MA(54%), CA(52%)
# states with % PIT < 5%: NH, TN, AK(0), FL, NV, SD, TX WA, WY 


# Year 2015: ordered by share of sales tax
# sales tot
# Top five in 2015: TX(86.5%), SD(82%), FL(82%), NV(80%), WA(79%)
# Lowest five in 2015: OR(14%), DE(14%), MT(21%), AK(30%), NY(31%)   [NH 39%, why]

# sales general
# Top five in 2015: TX(61%), WA(61%), FL(59%), SD(58%), NV(54%)
# Lowest five in 2015(excluding five states with no gen sales): VT(12%), NY(17%), VA(18%), MA(21%), CO(22%)   

# Five states do not have statewide sales taxes: Alaska, Delaware, Montana, New Hampshire, and Oregon. 
# Of these, Alaska and Montana allow localities to charge local sales taxes.



## Structure of state tax revenue 1977-2015, census data (from Urban) based on nominal terms
taxStr_state_nom <- 
	df_RevGSP %>% 
	filter(year %in% 1977:2015, state_abb %in% df_us_states$state_abb) %>% 
	select(state_abb, year, 
				 tax_tot_nom_state, 
				 tax_indivIncome_nom_state,
				 tax_sales_gen_nom_state,
				 tax_sales_select_nom_state,
				 tax_sales_tot_nom_state,
				 tax_property_nom_state,
				 tax_corpIncome_nom_state) %>% 
	mutate(indivIncome_pct = 100 * tax_indivIncome_nom_state/tax_tot_nom_state,
				 salesgen_pct    = 100 * tax_sales_gen_nom_state/tax_tot_nom_state,
				 salesselect_pct = 100 * tax_sales_select_nom_state/tax_tot_nom_state,
				 salestot_pct    = 100 * tax_sales_tot_nom_state/tax_tot_nom_state,
				 property_pct    = 100 * tax_property_nom_state/tax_tot_nom_state,
				 corpIncome_pct  = 100 * tax_corpIncome_nom_state/tax_tot_nom_state,
				 PITsalesgen_pct = indivIncome_pct + salesgen_pct,
				 PITsalestot_pct = indivIncome_pct + salestot_pct,
				 nonPITsalesgen_pct  = 100 - PITsalesgen_pct,
				 nonPITsalestot_pct  = 100 - PITsalestot_pct
	) %>% 
	select(state_abb, year,
				 tax_tot_nom_state,
				 indivIncome_pct,
				 salesgen_pct,
				 salestot_pct,
				 salesselect_pct,
				 property_pct,
				 corpIncome_pct,
				 PITsalesgen_pct,
				 nonPITsalesgen_pct,
				 PITsalestot_pct,
				 nonPITsalestot_pct)


# Share of income tax
taxStr_state_nom %>% 
	filter(year == 2015) %>% 
	select(state_abb, year, indivIncome_pct, salesgen_pct, PITsalesgen_pct, nonPITsalesgen_pct) %>% 
	arrange(desc(indivIncome_pct))

# Share of gen sales tax
taxStr_state_nom %>% 
	filter(year == 2015) %>% 
	select(state_abb, year, indivIncome_pct, salesgen_pct, PITsalesgen_pct, nonPITsalesgen_pct) %>% 
	arrange(desc(salesgen_pct))


## Plotting % of sales against % of PIT
fig_GovFin_sharePITSales <- 
taxStr_state_nom %>% 
	filter(year == 2015, !is.na(state_abb)) %>% 
	ggplot(aes(x = indivIncome_pct, y = salesgen_pct, label = state_abb)) + theme_bw() + RIG.themeLite() +  
	geom_point() +
	geom_text_repel(size = 3.5) + 
	labs(x = "Share of individual income tax (%)",
			 y = "Share of general sales tax (%)",
			 title = "Shares of individual income tax and general sales tax across states in 2015",
			 caption  = "Source: U.S. Census Bureau, Annual Survey of State and Local Government Finances")
fig_GovFin_sharePITSales

ggsave(paste0(dir_fig_out, "fig_GovFin_sharePITSales.png"), fig_GovFin_sharePITSales,  width = 10*0.9, height = 6*0.9)




# tax revenue strucuture structure 

df_temp <- 
	taxStr_state_nom %>% 
	filter(year == 2015) %>% 
	select(state_abb, year, tax_tot_nom_state, indivIncome_pct, salesgen_pct, salesselect_pct, nonPITsalestot_pct)

df_taxRev_str <- 
	bind_rows(
		df_temp %>% arrange(desc(indivIncome_pct)) %>% filter(state_abb == "US"),
		
		df_temp %>% arrange(desc(indivIncome_pct)) %>% '['(1:5,),
		
		df_temp %>% arrange(desc(salesgen_pct)) %>% '['(1:5,)
	)

write.xlsx2(df_taxRev_str, file = "policyBrief_out/Table_taxStr.xlsx")



#*******************************************************************************
#   Descriptive analysis 2:  GSP growth and tax growth                      ####
#*******************************************************************************

## Key points to make
# 1. For the US aggregate, income tax is more responsive to economic conditions than sales tax, 
#    which is especially true during the last two recessions. 
# 2. At state level, we have the same observation. There are variations in the PIT-GSP and sales_GSP relationship across states. 
# 3. The relationship between PIT and GSP may not be linear. Since the share of capital gains and losses have been increasing 
#    in income tax bases, it may make sense to add financial market variables as a determinants PIT. 
#    This will also solve the issue in modeling PIT and GSP with a linear relationship that their growth rates must have the same sign.    
# 4. Is sales tax linear to GSP? 
# 5. About Non-PIT-non_salesgen taxes
#     - select sales tax (about 50% of general sales tax) seems to be more stable than general 
#     - corp income tax is the most volatile component, but its share is small: ~5%
#     - The share of property tax is very small ~1.5%
#     - Other taxes (8~10%), show some cyclicity
#     - For now, may assume a fixed share of non_PIT_sales, say 15% in all taxes. Need to further check how to treat select sales tax 

# Look at total tax revenue of 5 states with the highest and the lowest share of PIT. 

states_PIT    <- c("US", "OR", "NY", "VA", "MA", "CA")
states_sales  <- c("US", "TX", "WA", "FL", "SD", "NV") # sales tax states with zero PIT
states_sales2 <- c("US", "HI", "AZ", "MS", "OH", "IN") # sales tax states with non-zero PIT


## 1. GSP and TOTAL tax revenue over time
# US + PIT states
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_tot_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_PIT) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_PIT)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-15, 15)) + 
	theme(legend.position="bottom") +
	labs(title = "PIT states")


# US + sales states with no PIT
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_tot_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_sales) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_sales)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-15, 15)) + 
	theme(legend.position="bottom") + 
	labs(title = "sales states")

# US + sales states with PIT
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_tot_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_sales2) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_sales2)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-15, 15)) + 
	theme(legend.position="bottom") + 
	labs(title = "sales states")


## 2.  GSP and income tax and sales tax
# Top 5 income tax states
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_indivIncome_real_state, tax_sales_gen_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_PIT) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_PIT)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue", "lightblue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-20, 20)) + 
	theme(legend.position="bottom") +
	labs(title = "PIT")


# Top 5 sales tax states with no PIT
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_indivIncome_real_state, tax_sales_gen_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_sales) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_sales)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue", "lightblue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-15, 15)) + 
	theme(legend.position="bottom") +
	labs(title = "sales1")

# 5 sales tax states with PIT
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_indivIncome_real_state, tax_sales_gen_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_sales2) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_sales2)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue", "lightblue"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-15, 15)) + 
	theme(legend.position="bottom") +
	labs(title = "sales2")


## 3. non-PIT-non-sales 

# Top 5 income tax states
df_dlreal %>% 
	select(state_abb, year, RGSP, tax_indivIncome_real_state, tax_sales_gen_real_state, tax_nonPITsalestot_real_state) %>% 
	filter(year >= 1987, state_abb %in% states_PIT) %>% 
	gather(var, value, -state_abb, -year) %>% 
	ungroup() %>% 
	mutate(state_abb = factor(state_abb, levels = states_PIT)) %>% 
	ggplot(aes(x = year, y = value, color = var ))+ theme_bw() + facet_grid(state_abb~.) + 
	geom_line() +
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) + 
	scale_color_manual(values = c("darkgrey", "blue", "lightblue", "lightgreen"))+
	scale_x_continuous(breaks = seq(1950, 2015, 5)) + 
	coord_cartesian(ylim = c(-20, 20)) + 
	theme(legend.position="bottom") +
	labs(title = "PIT")

# 
fct_order = c( "tax_tot_real_state",
							 "tax_indivIncome_real_state",
							 "tax_sales_gen_real_state",
							 "tax_sales_select_real_state",
							 "tax_corpIncome_real_state",
							 "tax_property_real_state",
							 "tax_other_real_state")

fct_label = c("Total tax",
							"PIT",
							"sales_general",
							"sales_select",
							"corpIncome",
							"property",
							"other")

df <- 
	df_dlreal %>% 
	filter(year %in% 1988:2015, state_abb %in% df_us_states$state_abb) %>% 
	select(state_abb, year, 
				 RGSP,
				 tax_tot_real_state,
				 tax_indivIncome_real_state,
				 tax_sales_gen_real_state,
				 tax_sales_select_real_state,
				 tax_corpIncome_real_state,
				 tax_property_real_state,
				 tax_other_real_state) %>% 
	gather(taxtype, value, -state_abb, -year, -RGSP) %>% 
	mutate(var = "tax")

df1 <- df %>% select(state_abb, year, taxtype, var, value)
df2 <- df %>% mutate(value = RGSP, var = "RGSP") %>% select(state_abb, year, taxtype, var, value) 
df3 <- bind_rows(df1, df2) %>% 
	mutate(taxtype = factor(taxtype, levels = fct_order, labels = fct_label))



df3 %>% filter(state_abb == "US") %>% 
	ggplot(aes(x = year, y = value, color = var)) + theme_bw() + 
	facet_grid(taxtype~.) + 
	geom_line() + 
	geom_point() + 
	geom_hline(yintercept = 1, linetype = 2) +
	scale_color_manual(values = c("darkgrey", "blue"))+
	coord_cartesian(ylim = c(-25, 25))

df_real_pct <- 
	df_real %>% 
	filter(state_abb %in% df_us_states$state_abb) %>% 
	mutate(
		pct_indivIncome  = 100 * tax_indivIncome_real_state/tax_tot_real_state,
		pct_sales_gen    = 100 * tax_sales_gen_real_state/tax_tot_real_state,
		pct_sales_select = 100 * tax_sales_select_real_state/tax_tot_real_state,
		pct_corpIncome   = 100 * tax_corpIncome_real_state/tax_tot_real_state,
		pct_property     = 100 * tax_property_real_state/tax_tot_real_state,
		pct_other        = 100 * tax_other_real_state/tax_tot_real_state ) %>% 
	select(state_abb, year, 
				 pct_indivIncome, 
				 pct_sales_gen,   
				 pct_sales_select,
				 pct_corpIncome,  
				 pct_property,   
				 pct_other) 


df_real_pct %>%
	filter(state_abb == "US")

df_real_pct %>%
	filter(year == 2015, state_abb %in% states_PIT)

df_real_pct %>%
	filter(year == 2015, state_abb %in% states_sales)

df_real_pct %>%
	filter(year == 2015, state_abb %in% states_sales2)





#*******************************************************************************
#                                Saving outputs                             ####
#*******************************************************************************
# 
# ggsave(paste0(dir_fig_out, "fig_returnCapGains.png"), fig_returnCapGains, width = 10*0.8, height = 5*0.8)
# ggsave(paste0(dir_fig_out, "fig_captainsTax.png"),    fig_captainsTax,    width = 10*0.8, height = 5*0.8)
# ggsave(paste0(dir_fig_out, "fig_capgainsTax_real.png"),    fig_capgainsTax_real,    width = 10*0.8, height = 5*0.8)

















