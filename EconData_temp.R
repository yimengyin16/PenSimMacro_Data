# loading data



#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(quantmod)
library(Quandl)
library(tidyverse)
library(readxl)

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
Quandl.api_key("rsakY2-RD8pa1JNBk9sd")



#**********************************************************************
#                     Loading data from FRED                       ####
#**********************************************************************

# Loading FRED data ####
# http://rstudio-pubs-static.s3.amazonaws.com/24858_1f006c3965614b0099c963913100e9f0.html


# Major economic variables

FRED_vars <- c(
  "GDPC1",           # Quarterly, Seasonally adjusted GDP level, billion
  "A191RL1Q225SBEA", # Quarterly, seasonally adjusted GDP growth, annual rate
  "TB3MS",           # 3-Month Treasury-bill: secondary market rate, monthly
  "GS2",             # 2-Year  Treasury constant maturity rate
  "GS10",            # 10-Year Treasury constant maturity rate
  "GS30",            # 30-Year Treasury constant maturity rate
  
  "CPIAUCSL",        # CPI-U, seasonally adjusted
  "CPIAUCNS",        # CPI_U, not seasonally adjusted
  "CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
  
  #"AAA",             # Moody's Seasoned Aaa Corporate Bond Yield, monthly not seasonally adjusted
  
  "UNRATE"           # civilian unemployent rate, seasonally adjusted
)

# loading data through Quandl (2000 calls per 10 mins). 
df_FRED <- Quandl(paste0("FRED/", FRED_vars), order = "asc")


# data through quantmod
macroData <- new.env()
getSymbols("AAA", src = "FRED", env = macroData )
AAA <- macroData$AAA


# GDP_ql <- macroData$GDPC1
# GDP_qr <- macroData$A191RL1Q225SBEA
# 
# TRate_3m  <- macroData$TB3MS
# TRate_2y  <- macroData$GS2
# TRate_10y <- macroData$GS10
# TRate_30y <- macroData$GS30
# 
# CPI_U 	 <- macroData$CPIAUCSL
# CPI_core <- macroData$CPILFESL
# 
# SP500 <- macroData$SP500
# 
# bind_cols(GDP_ql, GDP_qr) 



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

SBBI_AppendB_file <- paste0(dir_data_raw, "SBBI2016_AppendixB.xlsx")
SBBI_AppendB_cell <- "A3:M94"

fn <- function(fileName, varName, cells){
	  # a function to read a single sheet and convert it into long format
	  read_excel(fileName, varName, cells) %>% 
		gather(month, var, -Year) %>% 
		mutate(month   = match(month, substr(month.name, 1, 3)),
					 varName = varName) %>% 
		arrange(Year, month, var) %>% 
		rename(year = Year)
}

df_SBBI_AppenB <- 
		sapply(SBBI_AppendB_vars, fn, fileName = SBBI_AppendB_file, cells = SBBI_AppendB_cell, simplify = FALSE) %>% 
		bind_rows() %>%
	  mutate(varName = factor(varName, levels = SBBI_AppendB_vars)) %>% 
	  spread(varName, var)
	
df_SBBI_AppenB


#**********************************************************************
#                     Loading R. Shiller data                      ####
#**********************************************************************
df_ShillerData <- read_xls(paste0(dir_data_raw,"RShiller_data.xls"), sheet = "Data", skip = 7)





#**********************************************************************
#                     Loading other data                    ####
#**********************************************************************

# Loading yahoo finance  ####
finVars <- c(c("^GSPC", 
							 "^W5000",
							 "^RUA",    # Russell 3000 Index 
							 "^SP500TR"
))

financial <- new.env()
getSymbols(finVars, src = "yahoo", env=financial, 
					 from = as.Date("1960-01-04"), periodicity = "monthly")

SP500 <-   financial$GSPC
W5000 <-   financial$W5000
R3000 <-   financial$RUA
SP500TR <- financial$SP500TR %>% Cl

Cl(financial$SP500TR)

head(SP500)
head(W5000)
head(R3000)

SP500TR

# get closing price
Cl(GSPC)

# get monthly returns
GSPC.pctchange <- ClCl(GSPC)
GSPC.pctchange %>% head()



#**********************************************************************
#               Loading estimated monthly GDP data                 ####
#**********************************************************************

# Stock and Watson estimated monthly real GDP: 1/1959 - 6/2010
df_GDPmonthly_StockWatson <- read_excel(paste0(dir_data_raw, "MonthlyGDP_StockWatson.xlsx"), "InterpolatedGDP_GDI_rename")

# Macroeconomic Advisors estimated monthly real GDP 1/1992 - 12/2017
df_GDPmonthly_MA <- read_excel(paste0(dir_data_raw, "MonthlyGDP_MA.xlsx"), "Data")


df_GDPmonthly_MA









