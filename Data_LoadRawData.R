# loading data


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Goal:
		# Load and organize data from each source into the same format that is easy for merge operations. 
		# The format:
		# 	- data frame
		# 	- Three date variables: year, month, date(in Date format)
		# 	- All variables, within and across data sources, have unique names.


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
Quandl.api_key("rsakY2-RD8pa1JNBk9sd") 

# Note for Don: The api key is associated with my Quandl account. 
# You may want to register to obtain you own api key (Account settings -> API Key)




#**********************************************************************
#                     Loading data from FRED                       ####
#**********************************************************************

# http://rstudio-pubs-static.s3.amazonaws.com/24858_1f006c3965614b0099c963913100e9f0.html


# Major economic variables

FRED_vars <- c(
  "GDPC1",           # Quarterly, Seasonally adjusted GDP level, billion
  "A191RL1Q225SBEA", # Quarterly, seasonally adjusted GDP growth, annual rate
  "TB3MS",           # 3-Month Treasury-bill: secondary market rate, monthly
  "GS2",             # 2-Year  Treasury constant maturity rate
  "GS10",            # 10-Year Treasury constant maturity rate
  "GS20",            # 10-Year Treasury constant maturity rate
  "GS30",            # 30-Year Treasury constant maturity rate
  
  "CPIAUCSL",        # CPI-U, seasonally adjusted
  "CPIAUCNS",        # CPI_U, not seasonally adjusted
  "CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
  
  #"AAA",            # Moody's Seasoned Aaa Corporate Bond Yield, monthly not seasonally adjusted
  
  "UNRATE"           # civilian unemployent rate, seasonally adjusted
)

# loading data through Quandl (2000 calls per 10 mins). 
df_FRED <- Quandl(paste0("FRED/", FRED_vars), order = "asc")
names(df_FRED) <- str_replace_all(names(df_FRED), c("FRED." = "", " - Value" = ""))

df_FRED %<>% 
	mutate(year    = year(Date),
				 month   = month(Date),
				 yearMon = as.yearmon(Date)) %>% 
	select(year, month, yearMon, everything(), -Date) %>% 
	rename(GDP_FRED = GDPC1, 
				 GDP_growth_FRED = A191RL1Q225SBEA,
				 TBill3m_FRED  = TB3MS,
				 TBond2y_FRED  = GS2,
				 Tbond10y_FRED = GS10,
				 Tbond30y_FRED = GS30,
				 CPIU_SA_FRED  = CPIAUCSL,
				 CPIU_NA_FRED  = CPIAUCNS,
				 CPIc_SA_FRED  = CPILFESL,
				 unrate_SA_FRED= UNRATE       
	)
names(df_FRED)	

# data through quantmod
macroData <- new.env()
getSymbols("AAA", src = "FRED", env = macroData )
df_AAA <- macroData$AAA %>% as.data.frame()
df_AAA %<>%  
	mutate(Date = row.names(df_AAA),
         year  = year(Date),
				 month   = month(Date),
				 yearMon = as.yearmon(Date)) %>%
	rename(CBond_Yield_AAA = AAA) %>% 
	select(year, month, yearMon, everything(), -Date) 


# Merging data
df_FRED <- full_join(df_FRED, df_AAA)
head(df_FRED)
tail(df_FRED)

# as.yearmon()
# x <- as.yearmon(date("2001-01-01"))
# x %>% month
# as.Date(x)
# as.yearmon("2010-1")

#

df_FRED  %<>% group_by(year) %>% 
	mutate(
		GDP_FRED = case_when(
			month %in% 1:3   ~ as.numeric(GDP_FRED[ 1]),
			month %in% 4:6   ~ as.numeric(GDP_FRED[ 4]),
			month %in% 7:9   ~ as.numeric(GDP_FRED[ 7]),
			month %in% 10:12 ~ as.numeric(GDP_FRED[ 10])
	),
  	GDP_growth_FRED = case_when(
			month %in% 1:3   ~ GDP_growth_FRED[1],
			month %in% 4:6   ~ GDP_growth_FRED[4],
			month %in% 7:9   ~ GDP_growth_FRED[7],
			month %in% 10:12 ~ GDP_growth_FRED[10]
	)
)




#**********************************************************************
#                     Loading data from SBBI Yearbook              ####
#**********************************************************************

# Data from SBBI2016 yearbook appendix B: 1/1926~12/2015
SBBI_AppendB_vars <- c("LCapStock_TRI",
										 	"LCapStock_CAI",
										 	"SCapStock_TRI",
										 	"CBond_TRI",
										 	"LTGBond_TRI",
										 	"MTGBond_TRI",
										 	"TBills_TRI",
										 	"Inflation_Index")  

SBBI_AppendA_vars <- c("LTGBond_TRI",
	                     "MTGBond_TRI",
	                     "LTGBond_Yield",
											 "MTGBond_Yield",
											 "CBond_TR") 

SBBI_AppendB_file <- paste0(dir_data_raw, "SBBI2016_AppendixB.xlsx")
SBBI_AppendB_cell <- "A3:M94"

SBBI_AppendA_file <- paste0(dir_data_raw, "SBBI2016_AppendixA.xlsx")
SBBI_AppendA_cell <- "A3:M93"



fn <- function(fileName, varName, cells){
	  # a function to read a single sheet and convert it into long format
	  read_excel(fileName, varName, cells) %>% 
		gather(month, var, -Year) %>% 
		mutate(month   = match(month, substr(month.name, 1, 3)),
					 varName = varName) %>% 
		arrange(Year, month, var) %>% 
		rename(year = Year)
}


df_SBBI_AppendB <- 
		sapply(SBBI_AppendB_vars, fn, fileName = SBBI_AppendB_file, cells = SBBI_AppendB_cell, simplify = FALSE) %>% 
		bind_rows() %>%
	  mutate(varName = factor(varName, levels = SBBI_AppendB_vars)) %>% 
	  spread(varName, var) %>% 
	  mutate(yearMon = as.yearmon(paste(year,  month, sep =  "-"))) %>% 
	  select(year, month, yearMon, everything())

df_SBBI_AppendA <- 
	sapply(SBBI_AppendA_vars, fn, fileName = SBBI_AppendA_file, cells = SBBI_AppendA_cell, simplify = FALSE) %>% 
	bind_rows() %>%
	mutate(varName = factor(varName, levels = SBBI_AppendA_vars)) %>% 
	spread(varName, var) %>% 
	mutate(yearMon = as.yearmon(paste(year,  month, sep =  "-"))) %>% 
	select(year, month, yearMon, everything())


head(df_SBBI_AppendB)
tail(df_SBBI_AppendB)

head(df_SBBI_AppendA)
tail(df_SBBI_AppendA)

#**********************************************************************
#                     Loading R. Shiller data                      ####
#**********************************************************************
df_Shiller <- read_xls(paste0(dir_data_raw,"RShiller_data.xls"), sheet = "Data", skip = 7) %>% 
	mutate(yearMon = str_replace(as.character(Date), "\\.", "-") %>% as.yearmon(),
				 year  = year(yearMon),
				 month = month(yearMon)) %>% 
	filter(!is.na(year)) %>% 
	select(year, month, yearMon, everything(), -Date)
	
head(df_Shiller)
tail(df_Shiller)



#**********************************************************************
#                     Loading yahoo finance data                    ####
#**********************************************************************

yahoo_vars <- c("^GSPC",    #SP500 index (price only)
						 "^SP500TR"  #SP500 total return 
				   	 #"^W5000",
						 #"^RUA"    # Russell 3000 Index 
)

# use quantmod package
env_yahoo <- new.env()
getSymbols(yahoo_vars, src = "yahoo", env = env_yahoo, 
					 from = "1900-01-01", periodicity = "monthly")

SP500   <- env_yahoo$GSPC %>% Cl     # get closing prices
SP500TR <- env_yahoo$SP500TR %>% Cl  # get closing prices

df_yahoo <- cbind(SP500, SP500TR) %>% as.data.frame
df_yahoo %<>%
	mutate(Date    = row.names(df_yahoo),
				 year    = year(Date),
				 month   = month(Date),
				 yearMon = as.yearmon(Date)) %>%
	rename(SP500 = GSPC.Close,
				 SP500TR = SP500TR.Close) %>% 
	select(year, month, yearMon, everything(), -Date)
	
head(df_yahoo)
tail(df_yahoo)

# # get monthly returns
# GSPC.pctchange <- ClCl(GSPC)
# GSPC.pctchange %>% head()


#**********************************************************************
#               Loading estimated monthly GDP data                 ####
#**********************************************************************

# Stock and Watson estimated monthly real GDP: 1/1959 - 6/2010
df_GDPmonthly_StockWatson <- read_excel(paste0(dir_data_raw, "MonthlyGDP_StockWatson.xlsx"), "InterpolatedGDP_GDI_rename")

df_GDPmonthly_StockWatson %<>% 
	mutate(yearMon = paste(year, month, sep = "-") %>% as.yearmon()) %>% 
  rename(GDP_qtr_SW = RealGDP_Q,
  			 GDP_mon_SW = RealGDP_M) %>% 
	mutate(GDP_qtr_SW = as.numeric(GDP_qtr_SW),
				 GDP_mon_SW = as.numeric(GDP_mon_SW)) %>% 
	select(year, month, yearMon, GDP_mon_SW, GDP_qtr_SW)


# Macroeconomic Advisors estimated monthly real GDP 1/1992 - 12/2017
df_GDPmonthly_MA <- read_excel(paste0(dir_data_raw, "MonthlyGDP_MA.xlsx"), "Data")

df_GDPmonthly_MA %<>% 
	separate(Date, c("year", "month"), " - " , convert = TRUE) %>% 
	mutate(yearMon = paste(month, year) %>% as.yearmon(),
				 month   = month(yearMon)) %>% 
	rename(GDP_mon_MA = `Monthly Real GDP Index`,
				 GDP_nominal_mon_MA = `Monthly Nominal GDP Index`) %>% 
	select(year, month, yearMon, everything())
	
head(df_GDPmonthly_MA)

save(df_FRED, 
		 df_SBBI_AppendA,
		 df_SBBI_AppendB, 
		 df_Shiller, df_yahoo,
		 df_GDPmonthly_StockWatson, df_GDPmonthly_MA,
		 file = paste0(dir_data_raw, "dataRaw.RData"))





