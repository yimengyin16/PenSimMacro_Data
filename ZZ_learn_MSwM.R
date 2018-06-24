# Learn package MSwM 


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************




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
library(MSwM)     # Markov Switching model


# packages for 
library(zoo)
library(xts)

library(timetk)
library(tidyquant)

library(lubridate)


#**********************************************************************
#                    Package examples                              ####
#**********************************************************************

data(example)
plot(ts(example))

# linear model
mod <- lm(y~x, example)
summary(mod)

x <- lm(y~1, example)
summary(x)


# Markov Switching model

mod.mswm = msmFit(mod, k = 2, p = 1, sw = c(T, T, T, T), control = list(parallel=F))
summary(mod.mswm)

plotProb(mod.mswm, which = 1)
plotProb(mod.mswm, which = 2)
plotReg(mod.mswm, expl = "x" )


# Daily Traffic Caualities by car accidents in Spain
data(traffic)

model <- glm(NDead~Temp+Prec, traffic, family = "poisson")
summary(model)

m1 <- msmFit(model, k = 2, sw = c(T, T, T), family = "poisson", control = list(parallel = F))
summary(m1)
intervals(m1)























