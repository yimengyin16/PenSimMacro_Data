# Estimation and simulation based on HSZ2016 paper


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************
# A discussion of ARIMA transfer function models:
# https://robjhyndman.com/hyndsight/arimax/


## Issues:
# Simulate ARIMA process with initial values and regressors

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

# packages for 
library(zoo)
library(xts)

library(timetk)
library(tidyquant)

library(lubridate)

# check tidyquant, timetk, sweep (broom ), tibbletime
# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
# sweep: http://www.business-science.io/code-tools/2017/07/09/sweep-0-1-0.html

get_ewa <- function(x, d, to_xts = TRUE){
	# compute exponentially weighted average
	# x: time series in xts 
	# d: weight of current period
	
	#x <- df_inflation_y[, "dl_inflation"]
	#d <- 0.38
	n <- length(x)  
	ewa <- numeric(n)
	
	ewa[1] <- x[1]
	for(i in 2:n) ewa[i] <- (1 - d) * ewa[i - 1] + d * x[i]
	
	if(to_xts) ewa <- xts(ewa, index(x)) else ewa
}



#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"



#**********************************************************************
#                     Loading Data                                 ####
#**********************************************************************
# Loading saved data 
load(paste0(dir_data_out, "dataAll.RData"))


#**********************************************************************
#                 Selecting data                             ####
#**********************************************************************

# Use annual data

# Data needed:
# 1. Inflation:
#   - SBBI inflation index (starts from 1926)
#   - FRED CPI-U, seasonally adjusted (starts from 194?)
# 2. Equity total return index (large cap)
#   - SBBI total return index + SP500TR index 
# 3. Equity price index
#   - SBBI capital appreciation index + SP500 index
# 4. Long-term corp bond yield index
#   - Moody's long-term bond yield
# 5. GDP
#   - FRED quarterly GDP 
# 6. Long-term bond total return (not used in modeling)
#   - SBBI long-term corb bond total return index
#   - SBBI long-term gov bond  total return index


df_wilkie <- 
	df_dataAll_y %>% 
	select( year, yearMon,
					
		cpi = Inflation_Index,       # SBBI price index, based on CPI-U, not seasonally adjusted
		# cpi = CPIU_SA_FRED,     # CPI-U, seasonally adjusted
		
		lcap_TRI = LCap_TRI,                      # SBBI Large Cap total return
		lcap_CAI = LCap_CAI,                      # SBBI Large Cap capital appreciation
		
		cbond_yield = CBond_Yield_AAA, # Moody's AAA long-term bond yield
		
		gdp = GDP_FRED,                # GDP
		
		cbond_TRI   = CBond_TRI,       # SBBI corporate bond total return index
		gbond_TRI   = LTGBond_TRI      # SBBI government bond total return index
	)


#**********************************************************************
#                    Calc derived data                             ####
#**********************************************************************

df_wilkie %<>% 
	mutate(
		     dl_gdp  = log(gdp/lag(gdp)),
				 
		     infl     = log(cpi / lag(cpi)),
		     
		     lcap_divI   = (lcap_TRI/lag(lcap_TRI)) * lag(lcap_CAI) - lcap_CAI, 
		     lcap_divY   = lcap_divI / lcap_CAI,
				 
		     l_lcap_divI  = log(lcap_divI),
		     l_lcap_divY  = log(lcap_divY),
		     dl_lcap_divI = l_lcap_divI - lag(l_lcap_divI),
		     dl_lcap_divY = l_lcap_divY - lag(l_lcap_divY),
		     
		     cbond_yield  = cbond_yield/100,
         
		     dl_cbond = log(cbond_TRI/lag(cbond_TRI)),
		     dl_gbond = log(gbond_TRI/lag(gbond_TRI))
				 ) %>% 
  tk_xts(date_var = yearMon) # convert to xts for easier subsetting

df_wilkie <-  df_wilkie["1928/", ] 

Index_wilkie <- index(df_wilkie)



#**********************************************************************
#     Inflationary and real parts of long-term corp bond rate      ####
#**********************************************************************
dc    <-  0.058
c_min <-  0.005
# Notes:
 # set c_min, so that
 #  if the real component of long-term bond yield is smaller than c_min, then
 #  forces the real component to be c_min, and the inflationary component to be 
 #  the total rate minus c_min, where the total rate is the sum of unadjusted real
 #  and inflationary components 
 #  This ensures that simulated the real part is always positive and can take log. 


decomp_cbondY <- function(infl, yield_tot, dc, c_min){
	## compute inflationary and real components of corp bond yield based on p9 HSZ2016
	# infl:      inflation in xts 
	# yield_tot: weight of current period
	# dc: weight
	# c_min: min of real component of yield
	
	# infl      <- df_cbond_y[, "dl_inflation"]
	# yield_tot <- df_cbond_y[,  "CBond_Yield_AAA"]
	# dc <- 0.058 
	# c_min <- 0.005
	
	Index <- index(infl)
	non_NA <- (!is.na(infl) & !is.na(yield_tot)) %>% as.vector
	
	infl      <- infl[non_NA, ]
	yield_tot <- yield_tot[non_NA, ]
	
	n <- length(infl)  
	yield_infl <- numeric(n)
	yield_real <- numeric(n)
	
	yield_infl[1] <- min(infl[1], yield_tot[1] - c_min)
	for(i in 2:n) yield_infl[i] <- min((1 - dc) * yield_infl[i - 1] + dc * infl[i], yield_tot[i] - c_min)
	yield_infl <- xts(yield_infl, index(yield_tot))
	yield_real <- yield_tot - yield_infl
	colnames(yield_real) <- "yield_real"
	
	yield_decomp <- merge(merge(yield_real, yield_infl), Index)
	yield_decomp	
}

cbondY_comp_y <- decomp_cbondY(df_wilkie[, "infl"], df_wilkie[, "cbond_yield"], dc, c_min)
df_wilkie     <- merge(df_wilkie, cbondY_comp_y)

cbondY_comp_y %>% plot
df_wilkie[, c("infl", "cbond_yield")] %>% plot

df_wilkie %<>% 
	as.data.frame() %>% 
	mutate(l_yield_real  = log(yield_real),
				 dl_yield_real = l_yield_real - lag(l_yield_real)) %>% 
	tk_xts(order.by = Index_wilkie)


#**********************************************************************
#                     Modeling: inflation                          ####
#**********************************************************************

## Inflation models:
  #1a. AR(1) of inflation rate on 1951-2014
  #1b. AR(1) of inflation rate on 1991-2014
  #2a. HSZ2016 (Hardy, Saunders, Zhang, 2016) estimates on 1951-2014
  #2b. HSZ2016 (Hardy, Saunders, Zhang, 2016) estimates on 1991-2014

mdl_infl_y1a <- Arima(df_wilkie["1951/2014" , "infl"],  c(1, 0, 0)) # AR(1)
mdl_infl_y1b <- Arima(df_wilkie["1991/2014" , "infl"],  c(1, 0, 0)) # AR(1)

mdl_infl_y1a
mdl_infl_y1b

# HSZ2016 estimate on 1951-2014
mdl_infl_y2a <- mdl_infl_y1a
mdl_infl_y2a$coef   <- c(ar1 = 0.7575, intercept = 0.0338)
mdl_infl_y2a$sigma2 <- 0.0171^2 # 

# HSZ2016 estimate on 1991-2014
mdl_infl_y2b <- mdl_infl_y1a
mdl_infl_y2b$coef   <- c(ar1 = 0, intercept = 0.0244 )
mdl_infl_y2b$sigma2 <- 0.0111^2 # 



#**********************************************************************
#                     Modeling: dividend yield                    ####
#**********************************************************************

# Dividend yield models
# 1a. AR(1) of log div yield on 1951-1984, with inflation as regressor
# 1b. AR(1) of log div yield on 1991-2014, with inflation as regressor
# 2a. HSZ2016 par values: AR(1) of log div yield  on 1951-2014, with inflation as regressor
# 2b. HSZ2016 par values: AR(1) of log div yield  on 1991-2014, with inflation as regressor

mdl_divY_y1a <- Arima(df_wilkie["1951/2014", "l_lcap_divY"], xreg = df_wilkie["1951/2014", "infl"], order = c(1, 0, 0))
mdl_divY_y1b <- Arima(df_wilkie["1991/2014", "l_lcap_divY"], xreg = df_wilkie["1991/2014", "infl"], order = c(1, 0, 0))

mdl_divY_y1a
mdl_divY_y1b

mdl_divY_y2a <- mdl_divY_y1a
mdl_divY_y2a$coef <- c(ar1 = 0.9582, intercept = log(0.0331), dl_inflation = 0.0504)
mdl_divY_y2a$sigma2 <- 0.131^2

mdl_divY_y2b <- mdl_divY_y1a
mdl_divY_y2b$coef <- c(ar1 = 0.9112, intercept = log(0.0252), dl_inflation = -4.5762 )
mdl_divY_y2b$sigma2 <- 0.1159^2


#**********************************************************************
#                     Modeling: dividend growth                    ####
#**********************************************************************

# Dividend growth models
# 1a. MA(1) of dividend growth on 1951-1984, with    div yield shock as regressor 
# 1b. MA(1) of dividend growth on 1991-2014, without div yield shock as regressor
# 2a. HSZ2016 par values for 1a
# 2b. HSZ2016 par values for 1b

mdl_divY_y1a$residuals
mdl_divY_y1b$residuals

dd <- 0.38
wd <- 1

df_wilkie <- cbind(df_wilkie, infl_ewa =  get_ewa(df_wilkie[, "infl"], dd))


# regressors 
infl_ewa_a <- df_wilkie["1951/2014", "infl_ewa"]
divY_res_a <- xts(mdl_divY_y1a$residuals, index(infl_ewa_a)) %>% lag; colnames(divY_res_a) <- "divY_res"
xreg_a <- cbind(infl_ewa_a, divY_res_a)[-1, ]

infl_ewa_b <- df_wilkie["1991/2014", "infl_ewa"]
divY_res_b <- xts(mdl_divY_y1b$residuals, index(infl_ewa_b)) %>% lag; colnames(divY_res_b) <- "divY_res"
xreg_b <- cbind(infl_ewa_b, divY_res_b)[-1, ]

# Wilkie specification: MA(1) + regressors 

mdl_divG_y1a <- Arima(df_wilkie["1952/2014", "dl_lcap_divI"], xreg = xreg_a, order = c(0, 0, 1), fixed = c(NA, NA, wd, NA))
mdl_divG_y1a; mdl_divG_y1a$sigma2^0.5

mdl_divG_y1b <- Arima(df_wilkie["1992/2014", "dl_lcap_divI"], xreg = xreg_b, order = c(0, 0, 1), fixed = c(NA, NA, wd, NA))
mdl_divG_y1b; mdl_divG_y1b$sigma2^0.5



mdl_divG_y2a <- mdl_divG_y1a
mdl_divG_y2a$coef <- c(ma1 = -.3468, intercept = 0.0202 , dl_inflation_NA_ewa = 1,  divY_res = 0.2138 )
mdl_divG_y2a$sigma2 <- 0.1146^2

mdl_divG_y2b <- mdl_divG_y1b
mdl_divG_y2b$coef <- c(ma1 = 0.1215, intercept = 0.0306 , dl_inflation_NA_ewa = 1,  divY_res = 0.1567)
mdl_divG_y2b$sigma2 <- 0.1097^2


#**********************************************************************
#             Modeling: corp bond yield, real part                 ####
#**********************************************************************

# Long-term corp yield models: real part 
  # 1a. AR(1) of real cbond yield on 1951-1984, with div yield shock as regressor (regressor may be excluded in simulation)
  # 1b. AR(1) of real cbond yield on 1991-2014, with div yield shock as regressor (regressor may be excluded in simulation)
  # 2a. HSZ2016 par values for 1a
  # 2b. HSZ2016 par values for 1b

# Regressors 
divY_res_a <- xts(mdl_divY_y1a$residuals, index(infl_ewa_a)); colnames(divY_res_a) <- "divY_res"
xreg_a     <- divY_res_a

divY_res_b <- xts(mdl_divY_y1b$residuals, index(infl_ewa_b)); colnames(divY_res_b) <- "divY_res"
xreg_b     <- divY_res_b


# Wilkie model

mdl_cbond_real_y1a <- Arima(df_wilkie["1951/2014", "l_yield_real"], xreg = xreg_a,  order = c(1, 0, 0))
mdl_cbond_real_y1b <- Arima(df_wilkie["1991/2014", "l_yield_real"], xreg = xreg_b,  order = c(1, 0, 0))

mdl_cbond_real_y1a
mdl_cbond_real_y1a$coef[2] %>% exp; mdl_cbond_real_y1a$sigma2^0.5 

mdl_cbond_real_y1b
mdl_cbond_real_y1b$coef[2] %>% exp; mdl_cbond_real_y1b$sigma2^0.5 


mdl_cbond_real_y2a <- mdl_cbond_real_y1a
mdl_cbond_real_y2a$coef   <- c(ar1 = 0.9109, intercept = log(0.0213) , divY_res = -0.1958)
mdl_cbond_real_y2a$sigma2 <- 0.3157^2

mdl_cbond_real_y2b <- mdl_cbond_real_y1b
mdl_cbond_real_y2b$coef <- c(ar1 = 0.8020, intercept = log(0.0338), divY_res = -0.2785)
mdl_cbond_real_y2b$sigma2 <- 0.2538^2


#**********************************************************************
#                  Simulation            ####
#**********************************************************************

# Simulation
nsim <- 2000
nyear_sim <- 100

mdl_infl <- mdl_infl_y1a 
mdl_divY <- mdl_divY_y1a
mdl_divG <- mdl_divG_y1a
mdl_cbond_real <- mdl_cbond_real_y1a

mdl_name <- "y1a"


set.seed(1234); sim_infl <- replicate(nsim, simulate(mdl_infl, nyear_sim, future = TRUE)) %>% as.tibble()
set.seed(1234); sim_divY <- sapply(seq_len(nsim), function(x) {simulate(mdl_divY, nyear_sim, xreg = sim_infl[, x], future = TRUE) %>% exp %>% as.numeric}) %>% as.tibble()
set.seed(1234); sim_divG <- sapply(seq_len(nsim), function(x) {simulate(mdl_divG, nyear_sim, xreg = cbind(sim_infl[,x], sim_divY[,x]), future = TRUE) %>% as.numeric})  %>% as.tibble()
set.seed(1234); sim_cbond_real <- sapply(seq_len(nsim), function(x) {simulate(mdl_cbond_real, nyear_sim, xreg = sim_divY[, x], future = TRUE) %>% as.numeric}) %>% exp  %>% as.tibble()


# Calculate stock returns from dividend yield and dividend growth

# total return(t) = (price(t) + div(t)) / price(t-1)
#                = (D(t)/Y(t) + D(t)) / (D(t - 1) / Y(t-1))


# recover dividend index from dividend growth (assuming dividend at t = 0 is 1)
sim_divI <- sim_divG[, seq_len(nsim)] %>% as.tibble %>% 
	mutate_all(funs(cumprod(1+.))) %>% as.matrix

# recover stock price and lagged price from dividend index and yield
sim_eqtprice     <- sim_divI / as.matrix(sim_divY[, seq_len(nsim)])
sim_eqtprice_lag <- sim_eqtprice %>% as.tibble() %>% mutate_all(funs(lag)) %>% as.matrix

# compute stock return from price and dividend index
sim_eqtreturn    <- (sim_eqtprice + sim_divI) / sim_eqtprice_lag - 1

sim_eqtreturn %>% as.tibble %>% head

sim_eqtprice <- sim_eqtprice %>% as.tibble 
sim_divI     <- sim_divI %>% as.tibble 



# Examine stock returns
sim_eqtreturn[-1, ] %>% mean
sim_eqtreturn[-1, ] %>% sd


# Inflationary part of cbond yield
sim_cbond_infl <- 
	sim_infl %>% mutate_all(funs(get_ewa(., dc, FALSE)))


# Total cbond yield
sim_cbond_totY <- as.matrix(sim_cbond_real) + as.matrix(sim_cbond_infl) %>% as.tibble()


add_index <- function(df) df %>% mutate(mdl = mdl_name, year = seq_len(nyear_sim))

sim_infl %<>% add_index() 
sim_divY %<>% add_index() 
sim_divG %<>% add_index() 
sim_divI %<>% add_index() 
sim_eqtprice %<>% add_index() 
sim_cbond_real %<>% add_index() 
sim_cbond_infl %<>% add_index() 
sim_cbond_totY %<>% add_index() 






#**********************************************************************
#            Plotting and validating data         ####
#**********************************************************************

# Produce plots similar to fig 2 and fig 3 in HSZ2016 p10, and compare plots

df <- left_join(df_dividend_y %>% as.data.frame %>% select(year, dl_inflation, dl_inflation_NA, LCap_DivY, dl_LCap_DivI),
								df_cbond_y    %>% as.data.frame %>% select(year, CBond_Yield_AAA, yield_real_NA)) %>% 
	left_join(df_GDP_y      %>% as.data.frame %>% select(year, dl_GDP))

# fig2
df %>% 
	select(year, dl_inflation_NA, LCap_DivY, CBond_Yield_AAA, dl_GDP) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05))+
	scale_x_continuous(breaks = seq(1930, 2015, 10))


# fig3
df %>% 
	select(year, dl_inflation_NA, LCap_DivY, dl_LCap_DivI) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.8, 0.6, 0.1)) +
	scale_x_continuous(breaks = seq(1930, 2015, 10))

cor(
	df_inflation_y["1950/2015", "dl_inflation"],
	df_GDP_y["1950/2015", "dl_GDP"])


df %>% 
	select(year, dl_inflation_NA, LCap_DivY) %>%
	gather(var, value, -year) %>% 
	qplot(x = year, y = value, color = var, geom = "line", data =.) + theme_bw() +
	scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05))+
	scale_x_continuous(breaks = seq(1930, 2015, 10))




