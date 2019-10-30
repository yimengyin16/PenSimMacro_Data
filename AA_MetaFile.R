

# 0. Data
  # 0.1 Data_LoadRawData.R:        outputs: data_raw/dataRaw.RData 
  # 0.2 Data_LoadRawTaxRev.R       outputs: data_raw/dataRaw_RevGSP.RData
  # 0.3 Data_processRaw.R          outputs: data_out/dataAll.RData
  # 0.4 Data_processRaw_RevGSP.R   outputs: data_out/Data_RevGSP.RData


# 1. Macro model: GDP growth and asset returns
  # 1.0 MacroModel_Data_Description.R     
	   # - loads data_out/dataAll.RData
     # - outputs: figures and tables for data description
  # 1.1 MacroModel_explore.R:             Explore modeling approach;   loads data_out/dataAll.RData
  # 1.2 MacroModel_explore_sim.R:         Explore simulation approach; loads data_out/dataAll.RData; outputs: data_out/df_stock_q.feather
  # 1.3 MacroModel_regimeSwitching.py:    Python code for regime-switching model of GDP and stock. loads data_out/df_stock_q.feather
  # 1.4 MacroModel_simulation(x)_forward: Simulation; 
			# - loads data_out/dataAll.RData; 
      # - outputs: 
      #   - data_out/df_stock_q.feather; 
      #   - data_out/MacroModel_sim_results_forward.RData
  # 1.5 MacroModel_simAnalysis_forward.R: Analysis of simulation results. based on 1.4 
      # - loads: data_out/MacroModel_sim_results_forward.RData, data_out/dataAll.RData
      # - outputs: figures and tables of the distributions of simulated variables
  

# 2. Finance of stylized governments
  # 2.1 GovFin_modeling(x).R：   Regression analysis; 
      # loads data_out/dataAll.RData, and data_RevGSP.RData
      # outputs: tables for regression analysis, figures for trends and cycles
  # 2.2 GovFin_simulation(x).R   (need inputs from 1.4); df_sim saved for 4
      # loads data_out/MacroModel_sim_results_forward.RData, data_out/dataAll.RData
      # outputs: data_out/GovFin_sim_forward.RData; figures of distributions and risk measures


# 3. Simulation of Pension Finance 
  # Need pension simulatoin model for prototypical plan
  # PenFin_pensionSimInputs.R: 
     # loads: data_out/MacroModel_sim_results_forward.RData
     # outputs: penSimInputs_returns.RData saved to data_out/ and C:/Git/PenSim-Projects/Model_Main/IO_penSimMacro
  


# 4. Impact of pension contributions on government finance 
  # Risk_pensionRisk.R
    # - loads "C:/Git/PenSim-Projects/Model_Main/IO_penSimMacro"; data_out/GovFin_sim_forward.RData
    # - figures and tables in outputs_out/. 

# 5. Impact of non-normal distribution of asset returns
  # XX_nonNormal_fatTails(x).R


# Wilkie-like model
  # W.1 Wilkie_explore.R
  # W.2 Wilkie_HSZ2016.R










