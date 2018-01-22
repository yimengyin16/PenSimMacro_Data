# loading data

library(quantmod)
library(tidyverse)
library(readxl)

# Intro to quantmod
#    1. quantmod http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
#    2. https://www.quantinsti.com/blog/a-guide-on-r-quantmod-package-how-to-get-started/

# Intro to zee  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


# Loading stock prices  ####
finVars <- c(c("^GSPC", 
							 "^W5000",
							 "^RUA"    # Russell 3000 Index 
							 ))

financial <- new.env()
getSymbols(finVars, src = "yahoo", env=financial, 
           from = as.Date("1960-01-04"), periodicity = "monthly")

SP500 <- financial$GSPC
W5000 <- financial$W5000
R3000 <- financial$RUA

head(SP500)
head(W5000)
head(R3000)

# get closing price
Cl(GSPC)

# get monthly returns
GSPC.pctchange <- ClCl(GSPC)
GSPC.pctchange %>% head()



# Shiller data
dir_data_raw <- "data_raw/"
ShillerData <- read_xls(paste0(dir_data_raw,"RShiller_data.xls"), sheet = "Data", skip = 7)






# Loading FRED data
# http://rstudio-pubs-static.s3.amazonaws.com/24858_1f006c3965614b0099c963913100e9f0.html

# Major economic variables

econVars <- c(
  "GDPC1",           # Quarterly, Seasonally adjusted GDP level, billion
  "A191RL1Q225SBEA", # Quarterly, seasonally adjusted GDP growth, annual rate
  "TB3MS",           # 3-Month Treasury-bill: secondary market rate, monthly
  "GS2",             # 2-Year Treasury constant maturity rate
  "GS10",            # 10-Year Treasury constant maturity rate
  "GS30",            # 30-Year Treasury constant maturity rate
  
  "CPIAUCSL",        # CPI-U, seasonally adjusted
  "CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
  
  "UNRATE"           # civilian unemployent rate, seasonally adjusted
  
)


macroData <- new.env()
getSymbols(econVars, src = "FRED", env = macroData )

GDP_ql <- macroData$GDPC1
GDP_qr <- macroData$A191RL1Q225SBEA

TRate_3m  <- macroData$TB3MS
TRate_2y  <- macroData$GS2
TRate_10y <- macroData$GS10
TRate_30y <- macroData$GS30

CPI_U 	 <- macroData$CPIAUCSL
CPI_core <- macroData$CPILFESL

SP500 <- macroData$SP500















