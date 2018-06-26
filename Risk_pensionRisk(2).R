# This script is for modeling tax revenue of stylized governments


#**********************************************************************
#                           Packages                               ####
#**********************************************************************


# library(plyr)
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
IO_folder <- "C:/Git/PenSim-Projects/Model_Main/IO_penSimMacro"

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


get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1


color_PIT <- RIG.green
color_salesgen <- "blue"
color_salessel <- "deepskyblue2"
color_other <- RIG.purple
color_propertyLoc <- RIG.yellow.dark
color_GDP <- "gray50"



#**********************************************************************
#                          Outline                                 ####
#**********************************************************************



#**********************************************************************
#                         Loading data                             ####
#**********************************************************************

# Economic and revenue data: simulation with forward-looking assumptions
load(paste0(dir_data_out, "GovFin_sim_forward.RData"))

# Outputs of pension Finance  
IO_folder

get_results <- function(IO_folder, Pattern = "^Outputs"){
	
	fn <- function(x) {
		load(paste0(IO_folder, "/", x))
		
		# if("results.t7" %in% names(outputs_list)){
		#   df_out <- bind_rows(outputs_list$results,
		#                       outputs_list$results.t7,
		#                       outputs_list$results.xt7)
		#   return(df_out)
		# } else {
		#   return(outputs_list$results)
		# }
		
		return(outputs_list$results)
		
	}
	
	file_select <- dir(IO_folder, Pattern)
	results_all <- plyr::adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder) %>% select(runname, sim, year, everything())



#**********************************************************************
#                         Organizing data                          ####
#**********************************************************************

# Assumption on ERC as a percent of government tax revenue 
ERC_taxRev_init <- 0.05

infl <- 0.02

df_sim %>% head




df_taxRev_real <- 
df_sim %>% 
	select(sim, year, 
				 taxLevelReal_PITState_tot_a1,
				 taxLevelReal_PITState_tot_a2,
				 
				 taxLevelReal_salesState_tot_a1,
				 taxLevelReal_salesState_tot_a2,
				 
				 taxLevelReal_local_tot_a1,
				 taxLevelReal_local_tot_a2) %>% 
	mutate_at(
		vars(
			taxLevelReal_PITState_tot_a1,
			taxLevelReal_PITState_tot_a2,
			
			taxLevelReal_salesState_tot_a1,
			taxLevelReal_salesState_tot_a2,
			
			taxLevelReal_local_tot_a1,
			taxLevelReal_local_tot_a2),
		
		funs(ifelse(year == 1, 1, lag(.)))
	)
	
	

df_pension <- 
	results_all %>% 
	  filter(sim>0) %>% 
		select(runname, sim, year, C, ERC)

# assuming the ERC under slow repayment of UAAL and 7.5% discount rate is 3% of total tax revenue in year 1
v <- df_pension %>% filter(year == 1, sim %in% 1)
v
taxRev_init <- v[v$runname == "A_O30pA5_port70_30","ERC"]/ERC_taxRev_init

v %>% mutate(pct = 100* ERC / taxRev_init)





df <- 
	df_pension %>% 
	left_join(df_taxRev_real) %>% 
	group_by(runname, sim) %>% 
	mutate(# Contributions in real terms
		     price_index = (1+infl)^(year-1),
				 C_real   = C / price_index,
				 ERC_real = ERC/ price_index
				 ) %>% 
	mutate_at(
			vars(# Tax revenue 
				taxLevelReal_PITState_tot_a1,
				taxLevelReal_PITState_tot_a2,
				
				taxLevelReal_salesState_tot_a1,
				taxLevelReal_salesState_tot_a2,
				
				taxLevelReal_local_tot_a1,
				taxLevelReal_local_tot_a2),
			
			funs(. * taxRev_init)
	) %>% 
	mutate(
		ERC_tax_PITState_a1   = ERC_real / taxLevelReal_PITState_tot_a1,
		ERC_tax_PITState_a2   = ERC_real / taxLevelReal_PITState_tot_a2,
		
		ERC_tax_salesState_a1 = ERC_real / taxLevelReal_salesState_tot_a1,
		ERC_tax_salesState_a1 = ERC_real / taxLevelReal_salesState_tot_a2,
		
		ERC_tax_local_a1      = ERC_real / taxLevelReal_local_tot_a1,
		ERC_tax_local_a1      = ERC_real / taxLevelReal_local_tot_a2
	)

x <- df %>% filter(sim %in% 1:3, year == 1)

na2zero <- function(x){replace(x, is.na(x), 0)}


df_risk <- 
	df %>%
	group_by(runname, sim) %>%
	mutate(
	
		ERC_tax_PITState_a1_high1   = cumany(ERC_tax_PITState_a1 >= ERC_tax_PITState_a1[year == 1] + 0.05 ),
		ERC_tax_PITState_a1_high2   = cumany(ERC_tax_PITState_a1 >= 0.12),
		ERC_tax_PITState_a1_hike    = cumany(na2zero(ERC_tax_PITState_a1 - lag(ERC_tax_PITState_a1, 2) >= 0.03)),

		ERC_tax_salesState_a1_high1 = cumany(ERC_tax_salesState_a1>= ERC_tax_salesState_a1[year == 1] + 0.05),
		ERC_tax_salesState_a1_high2 = cumany(ERC_tax_salesState_a1>= 0.12),
		ERC_tax_salesState_a1_hike  = cumany(na2zero(ERC_tax_salesState_a1 - lag(ERC_tax_salesState_a1, 2) >= 0.03)),

		ERC_tax_localState_a1_high1 = cumany(ERC_tax_local_a1 >= ERC_tax_local_a1[year == 1] + 0.05),
		ERC_tax_localState_a1_high2 = cumany(ERC_tax_local_a1 >= 0.12),
		ERC_tax_localState_a1_hike  = cumany(na2zero(ERC_tax_local_a1 - lag(ERC_tax_local_a1, 2) >= 0.03))
		     
		# ERC_tax_PITState_a1_high1  = cumany(ERC_tax_PITState_a1   >= 0.1 ),
		# ERC_tax_PITState_a1_high2  = cumany(ERC_tax_PITState_a1   >= 0.15),
		# ERC_tax_PITState_a1_hike   = cumany(na2zero(ERC_tax_PITState_a1 - lag(ERC_tax_PITState_a1, 2) >= 0.03)),
		# 
		# ERC_tax_salesState_a1_high1 = cumany(ERC_tax_salesState_a1>= 0.1),
		# ERC_tax_salesState_a1_high2 = cumany(ERC_tax_salesState_a1>= 0.15),
		# ERC_tax_salesState_a1_hike  = cumany(na2zero(ERC_tax_salesState_a1 - lag(ERC_tax_salesState_a1, 2) >= 0.03)),
		# 
		# ERC_tax_localState_a1_high1 = cumany(ERC_tax_local_a1 >= 0.1),
		# ERC_tax_localState_a1_high2 = cumany(ERC_tax_local_a1 >= 0.15),
		# ERC_tax_localState_a1_hike  = cumany(na2zero(ERC_tax_local_a1 - lag(ERC_tax_local_a1, 2) >= 0.03))
		
				 ) %>%
	group_by(runname, year) %>%
	dplyr::summarize(
		ERC_tax_PITState_a1_high2 =  sum(ERC_tax_PITState_a1_high2, na.rm = T)/n(),
		ERC_tax_PITState_a1_hike =  sum(ERC_tax_PITState_a1_hike, na.rm = T)/n(),
		ERC_tax_PITState_a1_high1 =  sum(ERC_tax_PITState_a1_high1, na.rm = T)/n(),
		
		
		ERC_tax_salesState_a1_high2 =  sum(ERC_tax_salesState_a1_high2, na.rm = T)/n(),
		ERC_tax_salesState_a1_hike =  sum(ERC_tax_salesState_a1_hike, na.rm = T)/n(),
		ERC_tax_salesState_a1_high1 =  sum(ERC_tax_salesState_a1_high1, na.rm = T)/n(),
		
		ERC_tax_local_a1_high2   =  sum(ERC_tax_localState_a1_high2, na.rm = T)/n(),
		ERC_tax_local_a1_hike   =  sum(ERC_tax_localState_a1_hike, na.rm = T)/n(),
		ERC_tax_local_a1_high1 =  sum(ERC_tax_localState_a1_high1, na.rm = T)/n(),
		
		
		ERC_tax_PITState_a1.q10 = quantile(ERC_tax_PITState_a1, 0.1,na.rm = T),
		ERC_tax_PITState_a1.q25 = quantile(ERC_tax_PITState_a1, 0.25,na.rm = T),
		ERC_tax_PITState_a1.q50 = quantile(ERC_tax_PITState_a1, 0.5,na.rm = T),
		ERC_tax_PITState_a1.q75 = quantile(ERC_tax_PITState_a1, 0.75,na.rm = T),
		ERC_tax_PITState_a1.q90 = quantile(ERC_tax_PITState_a1, 0.9,na.rm = T),
		
		ERC_tax_salesState_a1.q10 = quantile(ERC_tax_salesState_a1, 0.1,na.rm = T),
		ERC_tax_salesState_a1.q25 = quantile(ERC_tax_salesState_a1, 0.25,na.rm = T),
		ERC_tax_salesState_a1.q50 = quantile(ERC_tax_salesState_a1, 0.5,na.rm = T),
		ERC_tax_salesState_a1.q75 = quantile(ERC_tax_salesState_a1, 0.75,na.rm = T),
		ERC_tax_salesState_a1.q90 = quantile(ERC_tax_salesState_a1, 0.9,na.rm = T),
		
		ERC_tax_local_a1.q10 = quantile(ERC_tax_local_a1, 0.1,na.rm = T),
		ERC_tax_local_a1.q25 = quantile(ERC_tax_local_a1, 0.25,na.rm = T),
		ERC_tax_local_a1.q50 = quantile(ERC_tax_local_a1, 0.5,na.rm = T),
		ERC_tax_local_a1.q75 = quantile(ERC_tax_local_a1, 0.75,na.rm = T),
		ERC_tax_local_a1.q90 = quantile(ERC_tax_local_a1, 0.9,na.rm = T)) %>%
	ungroup()





#**********************************************************************
#                         Analysis: Fiscal                                 ####
#**********************************************************************

# Dimensions to compare
  # 1. Types of Stylized governments
  # 2. MS returns vs normal returns
  # 3. Amortization method
  # 4. discout rate


## 1. Risk implied by Normally distributed returns and Markov switching returns

df_risk %>% 
	filter(year == 30) %>% 
	select(runname, year, contains("_high1") )


df_risk %>% 
	filter(year == 30) %>% 
	select(runname, year, contains("_high2") )
	


df_risk %>% 
	filter(year == 30) %>% 
	select(runname, year, contains("_hike") )

 # Results: 
  # Risk measures are significantly higher in simulations with synergy between investment returns and economic conditions


# 2. Types of stylized governments

df_risk %>% 
	filter(year == 30, str_detect(runname, "port")) %>% 
	select(runname, year, contains("_high") )

df_risk %>% 
	filter(year == 30, str_detect(runname, "port")) %>% 
	select(runname, year, contains("_hike") )

 # As expected, PIT dominant state faces much greater risk than sales tax state, and benchmark state,
 # especially under very backloaded amortization method

# 3. Amortization method and discount rate

df_risk %>% 
	filter(year == 10, str_detect(runname, "port")) %>% 
	select(runname, year, contains("_high") )

df_risk %>% 
	filter(year == 10, str_detect(runname, "port")) %>% 
	select(runname, year, contains("_hike") )

  # Backloaded amortization method leads to lower contribution risks for government, at the expense of higher risk of severe underfunding,
  # Lower discount rate leads to lower contribution risks for government, at the expense of much higher near-term contributions. 



# Figures 


df_risk %<>%  mutate(
	DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
	DiscRate = factor(DiscRate, levels = c("Discount rate = 7.5%", "Discount rate = 6.0%")),
	Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL"),
	Amort    = factor(Amort, levels = c("Fast repayment of UAAL", "Slow repayment of UAAL")),
	ReturnDist = ifelse(str_detect(runname,"normal"), "normal", "port70_30"),
	ReturnDist = factor(ReturnDist, levels = c("port70_30", "normal"))
	
	)



## Fig: High ERC as a % of total tax revenue: above 12 percent of payroll

df_fig1 <- 
df_risk %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																	"B_O30pA5_port70_30", "B_C15d_port70_30")) %>% 
          	select(year, DiscRate, Amort,  year, ERC_tax_PITState_a1_high2, ERC_tax_salesState_a1_high2, ERC_tax_local_a1_high2) 

df_fig2 <- 
  df_risk %>% filter(runname %in% c("A_O30pA5_normal", "A_C15d_normal",
																	  "B_O30pA5_normal", "B_C15d_normal")) %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_local_a1_high2) %>% 
	rename(ERC_tax_local_a1_high2_normal = ERC_tax_local_a1_high2)
df_fig2
	
df_fig <- left_join(df_fig1, df_fig2)
df_fig

fig.title <- "Probability of employer contribution as a percentage of total tax revenue \nabove 12 percent at any time up to a given year"
fig.subtitle <- NULL #"Employer contribution is 5% of total tax revenue in year 1" 
fig_ERC_tax_high2 <- 
	df_fig %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_PITState_a1_high2, ERC_tax_salesState_a1_high2, 
				                                 ERC_tax_local_a1_high2, ERC_tax_local_a1_high2_normal) %>% 
	gather(type, value, -DiscRate, -Amort, -year) %>% 
	mutate(type = factor(type, levels = c("ERC_tax_PITState_a1_high2", "ERC_tax_salesState_a1_high2", "ERC_tax_local_a1_high2", "ERC_tax_local_a1_high2_normal"),
				                     labels = c("Income tax dominant state \nsimulated returns", "Sales tax dominant state \nsimulated returns", 
				                     					  "Baseline:\nconstant tax revenue growth \nsimulated returns", "Baseline:\nconstant tax revenue growth \nnormal returns"))) %>% 
	ggplot(aes(x = year, y = 100 * value,
						 color = type,
						 shape = type)) + 
  theme_bw() + 
	facet_grid(DiscRate ~ Amort) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,40)) + 
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 5)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, "gray50", "black"),  name = NULL) + 
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Probability (%)") + 
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme() + 
	theme(legend.position = "bottom")
fig_ERC_tax_high2
fig_ERC_tax_high2$data


##  Fig: High ERC as a % of total tax revenue: rising 5 percentage points

df_fig1 <- 
	df_risk %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																		"B_O30pA5_port70_30", "B_C15d_port70_30")) %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_PITState_a1_high1, ERC_tax_salesState_a1_high1, ERC_tax_local_a1_high1) 

df_fig2 <- 
	df_risk %>% filter(runname %in% c("A_O30pA5_normal", "A_C15d_normal",
																		"B_O30pA5_normal", "B_C15d_normal")) %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_local_a1_high1) %>% 
	rename(ERC_tax_local_a1_high1_normal = ERC_tax_local_a1_high1)
df_fig2

df_fig <- left_join(df_fig1, df_fig2)
df_fig

fig.title <- "Probability of employer contribution as a percentage of total tax revenue \nbeing more than 5 percentage points above the year-1 level at any time up to a given year"
fig.subtitle <- NULL #"Employer contribution is 5% of total tax revenue in year 1" 
fig_ERC_tax_high1 <- 
	df_fig %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_PITState_a1_high1, ERC_tax_salesState_a1_high1, 
				 ERC_tax_local_a1_high1, ERC_tax_local_a1_high1_normal) %>% 
	gather(type, value, -DiscRate, -Amort, -year) %>% 
	mutate(type = factor(type, levels = c("ERC_tax_PITState_a1_high1", "ERC_tax_salesState_a1_high1", "ERC_tax_local_a1_high1", "ERC_tax_local_a1_high1_normal"),
											 labels = c("Income tax dominant state \nsimulated returns", "Sales tax dominant state \nsimulated returns", 
											 					 "Baseline:\nconstant tax revenue growth \nsimulated returns", "Baseline:\nconstant tax revenue growth \nnormal returns"))) %>% 
	ggplot(aes(x = year, y = 100 * value,
						 color = type,
						 shape = type)) + 
	theme_bw() + 
	facet_grid(DiscRate ~ Amort) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,40)) + 
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 5)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, "gray50", "black"),  name = NULL) + 
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Probability (%)") + 
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme() + 
	theme(legend.position = "bottom")
fig_ERC_tax_high1
fig_ERC_tax_high1$data




## Fig ERC increasing fast relatively to tax revenue 

df_fig1 <- 
	df_risk %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																		"B_O30pA5_port70_30", "B_C15d_port70_30")) %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_PITState_a1_hike, ERC_tax_salesState_a1_hike, ERC_tax_local_a1_hike) 

df_fig2 <- 
	df_risk %>% filter(runname %in% c("A_O30pA5_normal", "A_C15d_normal",
																		"B_O30pA5_normal", "B_C15d_normal")) %>% 
	select(year, DiscRate, Amort,  year, ERC_tax_local_a1_hike) %>% 
	rename(ERC_tax_local_a1_hike_normal = ERC_tax_local_a1_hike)
df_fig2

df_fig <- left_join(df_fig1, df_fig2)
df_fig


fig.title <- "Probability of employer contribution rising more than 3 percent of total tax revenue\n in a 2-year period at any time up to a given year"
fig.subtitle <- NULL # "Employer contribution is 5% of total tax revenue in year 1" 
fig_ERC_tax_hike <- 
	df_fig %>% 
	select(DiscRate, Amort, year, ERC_tax_PITState_a1_hike, ERC_tax_salesState_a1_hike, ERC_tax_local_a1_hike, ERC_tax_local_a1_hike_normal) %>% 
	gather(type, value, -DiscRate, -Amort, -year) %>% 
	mutate(type = factor(type,    levels = c("ERC_tax_PITState_a1_hike", "ERC_tax_salesState_a1_hike", "ERC_tax_local_a1_hike", "ERC_tax_local_a1_hike_normal"),
											          labels = c("Income tax dominant state \nsimulated returns", "Sales tax dominant state \nsimulated returns", 
											 					  					"Baseline:\nconstant tax revenue growth \nsimulated returns", "Baseline:\nconstant tax revenue growth \nnormal returns"))) %>%
	#mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>% 
	ggplot(aes(x = year, y = 100 * value,
						 color = type,
						 shape = type)) + 
	theme_bw() + 
	facet_grid(DiscRate ~ Amort) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,60)) + 
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 10)) + 
	scale_color_manual(values = c(color_PIT, color_salesgen, "gray50", "black"),  name = NULL) + 
	scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Probability (%)") + 
	theme(axis.text.x = element_text(size = 8)) + 
	RIG.theme() + 
	theme(legend.position = "bottom")
fig_ERC_tax_hike
fig_ERC_tax_hike$data


# Saving results

ggsave(paste0(dir_fig_out, "fig_RiskGov_ERC_tax_high1.png"), fig_ERC_tax_high1, width = 8, height = 9 )
ggsave(paste0(dir_fig_out, "fig_RiskGov_ERC_tax_high2.png"),  fig_ERC_tax_high2,  width = 8, height = 9 )
ggsave(paste0(dir_fig_out, "fig_RiskGov_ERC_tax_hike.png"),   fig_ERC_tax_hike,   width = 8, height = 9 )


Table_ERC_tax_high1 <- 
	df_risk %>% 
	filter(year == 30) %>% 
	select(ReturnDist, DiscRate, Amort, year, contains("_high1") ) %>% 
	arrange(ReturnDist, DiscRate, Amort)

Table_ERC_tax_high2 <- 
df_risk %>% 
	filter(year == 30) %>% 
	select(ReturnDist, DiscRate, Amort, contains("_high2")) %>% 
	arrange(ReturnDist, DiscRate, Amort)
Table_ERC_tax_high2


Table_ERC_tax_hike <- 
df_risk %>% 
	filter(year == 30) %>% 
	select(ReturnDist, DiscRate, Amort, year, contains("_hike") ) %>% 
	arrange(ReturnDist, DiscRate, Amort)


write.xlsx2(Table_ERC_tax_high1,  file = paste0(dir_fig_out, "Table_RiskGov_ERC_tax_risk.xlsx"), sheetName = "ERC_tax_high1")
write.xlsx2(Table_ERC_tax_high2,  file = paste0(dir_fig_out, "Table_RiskGov_ERC_tax_risk.xlsx"), sheetName = "ERC_tax_high2", append = TRUE)
write.xlsx2(Table_ERC_tax_hike,   file = paste0(dir_fig_out, "Table_RiskGov_ERC_tax_risk.xlsx"), sheetName = "ERC_tax_hike" , append = TRUE)




#**********************************************************************
#                         Analysis: pension finance                ####
#**********************************************************************

df_all.stch <-
	results_all %>%
	filter(sim >= 0, year <= 30)

df_7p5 <- 
results_all %>% 
	filter(runname == "A_O30pA5_port70_30", sim == 1) %>% 
	select(year, AL_7p5 = AL)


df_all.stch %<>%
	select(runname, sim, year, AL, MA, EEC, PR, ERC_PR) %>%
	left_join(df_7p5) %>% 
	group_by(runname, sim) %>%
	mutate(FR_MA     = 100 * MA / AL_7p5,
				 FR40less   = cumany(FR_MA <= 40),
				 FR100more  = cumany(FR_MA >= 100),
				 FR100more2 = FR_MA >= 100,
				 ERC_high   = cumany(ERC_PR >= 50),
				 ERC_hike   = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>%
	group_by(runname, year) %>%
	summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
						FR100more = 100 * sum(FR100more, na.rm = T)/n(),
						FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
						ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
						ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
						
						FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
						FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
						FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
						FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
						FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
						
						ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
						ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
						ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
						ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
						ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
						
						
	) %>%
	ungroup()


df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_O30pA5_port70_30")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_C15d_port70_30" )
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_O30pA5_normal")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_C15d_normal" )


df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_O30pA5_port70_30")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_C15d_port70_30" )
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_O30pA5_normal")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_C15d_normal" )






#*********************************************************************************************************
#  figures 
#*********************************************************************************************************


# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio" 
fig_FRdist <- 
	df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																				"B_O30pA5_port70_30", "B_C15d_port70_30"
	)) %>% 
	select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
	gather(type, value, -runname, -year) %>% 
	mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
				 DiscRate = factor(DiscRate, levels = c("Discount rate = 7.5%", "Discount rate = 6.0%")),
				 Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
	) %>% 
	mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>% 
	ggplot(aes(x = year, y = value,
						 color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")),
						 shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
	)) + theme_bw() + 
	facet_grid(DiscRate ~ Amort) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(40,180)) + 
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 20)) + 
	scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"), 
										 label  = c("75th percentile", "50th percentile", "25th percentile")) + 
	scale_shape_manual(values = c(15, 16, 17, 18), 
										 label  = c("75th percentile", "50th percentile", "25th percentile")) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Percent",
			 color = NULL, shape = NULL) + 
	theme(axis.text.x = element_text(size = 8)) + 
	guides(color = guide_legend(keywidth = 2, keyheight = 1))+
	theme(legend.position = "bottom") +
	RIG.theme()
fig_FRdist
fig_FRdist$data

df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30") )




# Distribution of ERC as % Payroll
fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_ERCdist <- 
	df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																				"B_O30pA5_port70_30", "B_C15d_port70_30"
	)) %>% 
	select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
	gather(type, value, -runname, -year) %>% 
	mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
				 DiscRate = factor(DiscRate, levels = c("Discount rate = 7.5%", "Discount rate = 6.0%")),
				 Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
	) %>% 
	mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>% 
	
	# mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
	ggplot(aes(x = year, y = value,
						 color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
						 shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
	theme_bw() + 
	facet_grid(DiscRate ~ Amort) +
	geom_line() + 
	geom_point(size = 2) + 
	geom_hline(yintercept = 100, linetype = 2, size = 1) +
	coord_cartesian(ylim = c(0,50)) + 
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_y_continuous(breaks = seq(0, 500, 5)) + 
	scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"), 
										 label  = c("75th percentile", "50th percentile", "25th percentile")) + 
	scale_shape_manual(values = c(17, 16, 15, 18), 
										 label  = c("75th percentile", "50th percentile", "25th percentile")) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Percent of payroll",
			 color = NULL, shape = NULL) + 
	theme(axis.text.x = element_text(size = 8)) + 
	guides(color = guide_legend(keywidth = 2, keyheight = 1))+
	theme(legend.position = "bottom") +
	RIG.theme()
fig_ERCdist


# Risk of low funded ratio
fig.title <- "Probabilities of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_FR40less <- 
	
	df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																				"B_O30pA5_port70_30", "B_C15d_port70_30"
	)) %>% 
	select(runname, year, FR40less) %>%
	gather(type, value, -runname, -year) %>% 
	mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
				 DiscRate = factor(DiscRate, levels = c("Discount rate = 7.5%", "Discount rate = 6.0%")),
				 Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
	) %>% 
	mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>%
	
	# mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
	#mutate(FR40less.det = 0) %>% 
	#gather(variable, value, -year) %>% 
	ggplot(aes(x = year, y = value, color = Amort, shape = Amort)) + 
	# color = runname, shape = runname)) + 
	theme_bw() + 
	facet_grid(.~DiscRate) + 
	geom_point(size = 2) + 
	geom_line() + 
	coord_cartesian(ylim = c(0,35)) + 
	scale_y_continuous(breaks = seq(0,200, 5)) +
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red)) + 
	scale_shape_manual(values = c(17,16, 15)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Probability (%)",
			 color = NULL, shape = NULL) + 
	guides(color = guide_legend(keywidth = 3, keyheight = 1))+
	theme(legend.position = "bottom") + 
	RIG.theme()
fig_FR40less
fig_FR40less$data %>% filter(year == 2046)


# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time up to the given year"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_ERChike <- 
	
	df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
																				"B_O30pA5_port70_30", "B_C15d_port70_30"
	)) %>% 
	select(runname, year, ERC_hike) %>% 
	gather(type, value, -runname, -year) %>% 
	mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
				 DiscRate = factor(DiscRate, levels = c("Discount rate = 7.5%", "Discount rate = 6.0%")),
				 Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
	) %>% 
	mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>%
	
	ggplot(aes(x = year, y = value, color = Amort, shape = Amort)) + theme_bw() + 
	facet_grid(.~DiscRate) + 
	geom_point(size = 2) + geom_line() + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous(breaks = seq(0,200, 10)) +
	scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
	scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red)) + 
	scale_shape_manual(values = c(17,16, 15, 18, 19)) +
	labs(title = fig.title,
			 subtitle = fig.subtitle,
			 x = "Year", y = "Probability (%)",
			 color = NULL, shape = NULL) + 
	guides(color = guide_legend(keywidth = 3, keyheight = 1))+
	theme(legend.position = "bottom") + 
	RIG.theme()
fig_ERChike
fig_ERChike$data %>% filter(year == 2046)


#*********************************************************************************************************
#  Table  
#*********************************************************************************************************

df_all.stch %>% names

Table_risk_DC75 <- 
	df_all.stch %>%
	select(runname, year, FR40less, ERC_hike, 
				 FR.q10, FR.q25, FR.q50, FR.q75, FR.q90,    
				 ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
	filter(year %in% c(1, 30), str_detect(runname, "A_")) %>% 
	arrange(year)
Table_risk_DC75

Table_risk_DC60 <-
	df_all.stch %>%
	select(runname, year, FR40less, ERC_hike, 
				 FR.q10, FR.q25, FR.q50, FR.q75, FR.q90,    
				 ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
	filter(year %in% c(1, 30), str_detect(runname, "B_")) %>% 
	arrange(year)
Table_risk_DC60

results_all %>% 
	filter(sim >0, year %in% 1:15, str_detect(runname, "port70_30")) %>%
	group_by(runname, sim) %>% 
	summarise(ERC = sum(ERC/(1.075)^(year - 1))) %>% 
	group_by(runname) %>% 
	summarise(ERC = median(ERC))

results_all %>% 
	filter(sim >0, year %in% 15:30, str_detect(runname, "port70_30")) %>%
	group_by(runname, sim) %>% 
	summarise(ERC = sum(ERC/(1.075)^(year - 1))) %>% 
	group_by(runname) %>% 
	summarise(ERC = median(ERC))

	



#*********************************************************************************************************
#  save
#*********************************************************************************************************

# dir_out <- "policyBrief_out/"

write.xlsx2(Table_risk_DC60, file = paste0(dir_fig_out, "Table_RiskPen.xlsx"), sheetName = "DC60")
write.xlsx2(Table_risk_DC75, file = paste0(dir_fig_out, "Table_RiskPen.xlsx"), sheetName = "DC75", append = TRUE)

ggsave(fig_FRdist, file  = paste0(dir_fig_out,  "fig_RiskPen_FRdist.png"), width = 8, height = 8 )
ggsave(fig_ERCdist, file = paste0(dir_fig_out,  "fig_RiskPen_ERCdist.png"), width = 8, height = 8 )

ggsave(fig_FR40less, file = paste0(dir_fig_out, "fig_RiskPen_FR40less.png"), width = 8, height = 5 )
ggsave(fig_ERChike,  file = paste0(dir_fig_out, "fig_RiskPen_ERChike.png"),  width = 8, height = 5 )



results_all %>% filter(year == 1, sim ==1) %>% select(runname, FR_MA)




























	
	
	
	
	
	
	
	
	
	
	
	
	
	
	





	
	
	
	
	













				 
				 








