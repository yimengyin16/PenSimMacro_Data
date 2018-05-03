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
Rev_urban_tot_nom 

Rev_urban_tot_nom1 <- 
  Rev_urban_tot_nom %>% 
	mutate(varname = paste0(varname,"_", type)) %>% 
	select(state_abb, year, varname, value) %>% 
	spread(varname, value) 
Rev_urban_tot_nom1


df_RevGSP <- 
df_RGSP_BEA %>% 
	left_join(df_GSP_FRED %>% select(-RGSP)) %>% 
	left_join(Rev_urban_tot_nom1)


#**********************************************************************
#                 Examine and combine stock series                 ####
#**********************************************************************

# Large Cap stock total return index.
df1 <- df_dataAll %>% select(yearMon, LCapStock_TRI, SP500TR) %>% 
	filter(yearMon >= as.yearmon("1988-1")) %>% 
	mutate(LCapStock_TRI = LCapStock_TRI * SP500TR[1]/LCapStock_TRI[1],
				 diff = LCapStock_TRI / SP500TR - 1) 
df1$diff %>% range(na.rm = T)

# Large Cap stock price index.
df2 <- df_dataAll %>% select(yearMon, LCapStock_CAI, SP500) %>% 
	filter(yearMon >= as.yearmon("1950-1")) %>% 
	mutate(LCapStock_CAI = LCapStock_CAI * SP500[1]/LCapStock_CAI[1],
				 diff = LCapStock_CAI / SP500 - 1) 
df2$diff %>% range(na.rm = T)


# LCapStock_TRI and SP500TR are consistent. SP500TR can be used to update LCapStock_TRI
# with data after 2016-1

# LCapStock_CAI and SP500 are consistent. SP500 can be used to update LCapStock_CAI
# with data after 2016-1


df_dataAll %<>% 
	mutate(LCap_TRI = ifelse(yearMon <= as.yearmon("2015-12"),
															 LCapStock_TRI,
													     SP500TR * (LCapStock_TRI/SP500TR)[yearMon == as.yearmon("2015-12")]),
				 LCap_CAI = ifelse(yearMon <= as.yearmon("2015-12"),
													 		 LCapStock_CAI,
													     SP500 * (LCapStock_CAI/SP500)[yearMon == as.yearmon("2015-12")])
				 # LCap_DI  = LCap_TRI/lag(LCap_TRI) * lag(LCap_CAI) - LCap_CAI
	)




#**********************************************************************
#                 Examine and combine GDP                          ####
#**********************************************************************

df3 <- df_dataAll %>%
	select(yearMon, GDP_FRED, GDP_qtr_SW, GDP_mon_SW, GDP_mon_MA) %>% 
	filter(yearMon >= as.yearmon("1947-1")) %>% 
	mutate(diff_SW = 100 * GDP_mon_SW / GDP_FRED - 100,
				 diff_MA  = 100 * GDP_mon_MA / GDP_FRED - 100 )

df3 %>% select(yearMon, diff_SW, diff_MA) %>% 
	gather(var, value, -yearMon) %>% 
	qplot(x = as.Date(yearMon), y = value, data = .,  color = var, geom = "line")

# S-W GDP are about 9-10% lower than the official GDP, and the difference increases over time.
# MA GDP are quite close to the official GDP. Differences generally within 1%

# Monthly GDP seems not reliable enough. 
# For now, should focus on quartly and annual data. 




#**********************************************************************
#                 Examine bond return and yield data               ####
#**********************************************************************

df4 <- 
	df_dataAll %>% 
	select(yearMon, CBond_TR, CBond_TRI, CBond_Yield_AAA)



#**********************************************************************
#                Construct quarterly and annual data               ####
#**********************************************************************
df_dataAll_q <- df_dataAll %>% filter(month %in% c(3, 6, 9, 12))
df_dataAll_y <- df_dataAll %>% filter(month %in% 6)





#**********************************************************************
#                 save data               ####
#**********************************************************************

save(df_dataAll, df_dataAll_q, df_dataAll_y, 
		 file =  paste0(dir_data_out, "dataAll.RData"))




























