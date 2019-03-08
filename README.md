# Analyzing the Interplay between Public Pension Finances and Governmental Finances: Lessons from Linking an Economic Model to a Pension Fund Model


## About the project
Public pension funds invest in stock, bonds, and other assets in an effort to keep the costs to governments of funding pensions low. Researchers have examined the investment-related risks to public pension funds, to the governments that contribute to them, and to stakeholders in pension funds and governments, using stochastic simulation models of pension fund finances.

These models generally use simple investment return assumptions, such as that returns are drawn from a normal distribution, are independent from year to year, and are not correlated with the governmental tax revenue needed to pay pension contributions. Because investment returns and tax revenue may both be correlated with underlying economic conditions, this could understate the risks that future increased contributions will be difficult to afford: a poor economy may dampen investment returns and cause tax revenue to fall short, compounding fiscal pressures on governments if the increases come when tax revenues are low.

This project addresses this issue by using a regime-switching macroeconomic model of recession and growth to drive (1) a model of pension fund returns driven by stock and bond returns, and (2) models of state tax revenue driven by real GDP and stock returns, which influence the capital gains component of income taxes. It is parameterized using retrospective and prospective estimates. It shows that the correlated risk is real and significant, meaning that political risks related to pensions are greater than often understood. The models show that these risks are greatest for governments that rely heavily on income taxes.

For more details about the project, please visit https://www.albany.edu/slgf/Yin%20Boyd%20SLGFP%20PensionSimulationMacroLinkagePaper_February2019.pdf


## About the repository
This repo contains the code for 1) importing macroeconomic and tax revenue data; 2) estimating parameters of the regime-switching model and the regression models of the of the relationship between state tax revenue and economic variables; 3) joint simulation of economic variables, investment returns, and state tax revenue.

The simulation of pension finance uses code from the repo https://github.com/marshallpku/Model_Main

Note that the estimation of the regime-switching model is done with python code because of the the limited capacity of the package of regime-switching model in R. 
