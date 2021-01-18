#####################################################################################################################################################################################
#
# Project: NESP Tropical Water Quality Hub: Project 3.1.6
#
# Funders: This project is jointly funded through the Queensland Government: Office of the Great Barrier Reef 
#          and the Australian Government's National Environmental Science Programme
#
# Purpose: R-code to produce supply curves for DIN credits from fertiliser practice improvement in sugarcane production
#          in main river catchments in Queensland's Wet Tropics Region
#
# Version: Final
# Date: 4th December 2020
#
# Author 1: James C.R. Smart 
# Institution: Australian Rivers Institute, Griffith University, Australia
# Email: j.smart@griffith.edu.au
#
# Author 2: Syezlin Hasan
# Institution: Australian Rivers Institute, Griffith University, Australia
# Email: s.hasan@griffith.edu.au
#
# Inputs: Tab-delimited text data files as follows
#
#         The following five datafiles are derived from the Queensland Government's Paddock to Reef (P2R)modelling 
#         Project 3.1.6 does not own these data. They were provided by P2R for use in this project
#         
#         P2R_combined_Avg_file_to_read = "P2R_Avg_1987_2013_wide.txt"
#         P2R_DINLoss_file_to_read = "P2R_WetTropics_Av_of_DIN_Loss_trimmed_max_to_min.txt"
#         P2R_Herbert_Avg_file_to_read = "P2R_Averaged_data_in_Herbert_only.txt"
#         P2R_FWCane_file_to_read = "P2R_WetTropics_Av_of_CaneFW_trimmed_max_to_min.txt"
#         CVaR_file_to_read = "P2R_CVaR_by_DMU_Best_Worst_10_percent_1987_2013.txt"   # This file gives the average FW cane yield for the top 10 percentile of years 1987-2013
#
#         The following four datafiles were obtained from public data and published literature
#
#         Urea_prices_to_read = "Urea_Prices_in_2018_19_AUD.txt"  #Sourced by Lin Hasan from ABS and inflated to FY2018-2019 AUD$
#         Sugar_prices_to_read = "Sugar_Prices_in_2018_19_AUD.txt" #Sourced by Lin Hasan from ABS data (that were originally sources from QLD MSF Sugar) and inflated to FY2018-2019 AUD$
#         Transition_costs_to_read = "Transition_Costs_between_Cane_Mgmt_Practices_v040520.txt"
#         Transaction_costs_to_read = "Transaction_Cost_Data_Coggan_2014.txt"
#
# Outputs: The software generates plots of supply curves for DIN credits for the main catchments in the Wet Tropics
#
#################################################################################################################################################################


#number_of_loops <- 1
number_of_loops <- 50
Seed_number <- 123456 #Set seed for random number generation

  
for (loop in 1:number_of_loops){
 #Setup overall definitions here
  #Seed_number <- 123456 #Set seed for random number generation
  
  #Practices for N_applied
  N_practices <- c("Df","Cp","Cf","Bp","Bf","Af") #from P2R document from Mel Shaw @ DNRME
  N_applied <- c(147,130,123,118,113,107) # Application rates in kgN/ha following stated practice in P2R document from Mel Shaw @ DNRME. 
  mean_Bf_fert_applic_by_soil_type <- c(74.24,75,76.05,69.71,81.66,75.07) #sequence here is coom eton mari stew thor todd. These data are used to estimate mean Bf fert applications for the Herbert catchment
  #For other catchments, DMU-specific Bf and Af fertiliser applications are read in via P2R_WT_other_info using DMU-specific results from the 1987-2013 simulations
  #for Bf for Eton use rough average of Bf for coom and mari based on Wood et al 2003 Soil-specific management guidelines for sugar cane production
  Af_fert_applic_from_Bf_fert_applic <- 0.95 #5% reduction in fertiliser requirement Af compared with Bf
   #These are the annual averages corresponding to each practice level over the 6-year plant, ratoon, cowpea fallow cane rotation set out in Mel Shaw's description
  #of the cane cycle that was assumed when running APSIM based on the 2016 Report Card
  N_content_in_Urea <- 46/100 # nitrogen content in Urea = 46%. Data from Impact Fertilisers https://impactfertilisers.com.au/products/straights/urea/
  Harvesting_cost <- 7.50 #AUD/tonne FW cane harvested (from Kandulu et al 2018)
  Transaction_cost <- 2084 # in 2018 AUD$ This is the annualised equivalent of the average transaction cost reported to be incurred by cane farmers for a Management Class Change - from Coggan et al 2014 (table 5, p512). Annualisation over a 6-year cane cycle at 6% discount rate
  #Transaction_cost <- 8960 #AUD This is the average transaction cost reported to be incurred by cane farmers for a Management Class Change - from Coggan et al 2014, Table 4, p510 inflated from 2014 to 2018 AUD$
  num_farm_types <- 3 # Uses the farm size classifications from van Grieken et al 2019 (& earlier) to categorise transistion cost
  farm_types <- c(1,2,3)
  Transport_via_GW <- 0.6 # A uniform transport factor of 0.6 (i.e. 60%) is assumed for all DIN that follows the groundwater path from paddock to receptor (after Webster et al 2012, as cited in van Grieken et al 2019, Sec 3.3, p 4)
  Very_Small_Area <- 0.01 #to discard DMUs with an area < 0.01ha
  Very_Small_DIN_Reduction <-  0.00001 #Use this to artificially create a very high price for practice changes that produce zero DIN reduction
  Very_High_Price <- 50000 #Replace 'Inf' with Very_High_Price so that convexifier will reject tranche steps that don't provide any reduction in DIN loss
  Very_Small_Number <- 1E-5 # use to detect 'close enough' equivalency
  
  EarlyAdopter_sell_unused_init <- FALSE
  Include_CVaR_cost <- TRUE
  #Include_CVaR_cost <- FALSE
  Include_transition_cost <- TRUE
  Include_transaction_cost <- TRUE
  #Uniform_baseline_practice <- TRUE
  Uniform_baseline_practice <- FALSE
  #Uniform_farm_type <- TRUE
  Uniform_farm_type <- FALSE
  
  clamp_at_BasePractice <- FALSE
  save_output = TRUE
  produce_plots = FALSE
  
  area_tolerance <- .01 #aim to get within x% of the reported area of the Focus_Catchment under each practice and each farm type
  
  
  init_practice <- "Df"
  #init_practice <- "Cp"
  #init_practice <- "Cf"
  #init_practice <- "Bp"
  #init_practice <- "Bf"
  #init_practice <- "Af"
  
  #Focus_Catchment <- "Daintree"
  #Focus_Catchment <- "Mossman"
  #Focus_Catchment <- "Barron"
  #Focus_Catchment <- "Mulgrave-Russell"
  Focus_Catchment <- "Johnstone"
  #Focus_Catchment <- "Tully"
  #Focus_Catchment <- "Murray"
  #Focus_Catchment <- "Herbert"
  
  if (Focus_Catchment == "Herbert"){
    Include_CVaR_cost <- FALSE
  }
  
  #Here to set uniform_baseline_prac_ptr
  uniform_baseline_prac_ptr <- 1 #"Df"
  #uniform_baseline_prac_ptr <- 2 #"Cp"
  #uniform_baseline_prac_ptr <- 3 #"Cf"
  #uniform_baseline_prac_ptr <- 4 #"Bp"
  #uniform_baseline_prac_ptr <- 5 #"Bf"
  #uniform_baseline_prac_ptr <- 6 #"Af"
  
  
  Practice_Level_probabilities <- as.data.frame(matrix(0,nrow=8,ncol=(length(N_practices)+1)))
  Cp_pc <- 0.75 # Proportion of 'Moderate Risk' = Class C land assumed to be at APSIM P2R level Cp
  Cf_pc <- 0.25 # Proportion of 'Moderate Risk' = Class C land assumed to be at APSIM P2R level Cf
  Bp_pc <- 0.75 # Proportion of 'Moderate-low Risk' = Class B land assumed to be at APSIM P2R level Bp
  Bf_pc <- 0.25 # Proportion of 'Moderate-low Risk' = Class B land assumed to be at APSIM P2R level Bf
  names(Practice_Level_probabilities)<- c("Catchment",N_practices)
  #Practice Probabilities match the reported percentages of cane land area in each catchment at practice levels D, C, B & A in the 2017 & 2018 Reef Report Cards
  #Report Card does not distinguish between Cp, Cf or Bp, Bf. Assume that 75% of 'C' is at Cp and the remaining 25% is at Cf. Assume similarly for 'B'
  Practice_Level_probabilities[1,] <- c("Daintree",.482,(.316*Cp_pc),(.316*Cf_pc),(.13*Bp_pc),(.13*Bf_pc),.072)#Insert probabilities for % of cane area at practice level Df, Cp, Cf, Bp, Bf, Af for this catchment from Reef Water Quality Report Card 2017 & 2018
  Practice_Level_probabilities[2,] <- c("Mossman",.294,(.416*Cp_pc),(.416*Cf_pc),(.186*Bp_pc),(.186*Bf_pc),.104)
  Practice_Level_probabilities[3,] <- c("Barron",.420,(.283*Cp_pc),(.283*Cf_pc),(.15*Bp_pc),(.15*Bf_pc),.148)
  Practice_Level_probabilities[4,] <- c("Mulgrave-Russell",.649,(.211*Cp_pc),(.211*Cf_pc),(.092*Bp_pc),(.092*Bf_pc),.049)
  Practice_Level_probabilities[5,] <- c("Johnstone",.454,(.373*Cp_pc),(.373*Cf_pc),(.10*Bp_pc),(.10*Bf_pc),.073)
  Practice_Level_probabilities[6,] <- c("Tully",.462,(.278*Cp_pc),(.278*Cf_pc),(.148*Bp_pc),(.148*Bf_pc),.112)
  Practice_Level_probabilities[7,] <- c("Murray",.504,(.270*Cp_pc),(.270*Cf_pc),(.138*Bp_pc),(.138*Bf_pc),.088)
  Practice_Level_probabilities[8,] <- c("Herbert",.516,(.134*Cp_pc),(.134*Cf_pc),(.203*Bp_pc),(.203*Bf_pc),.1475)
  
  #uniform_farm_type_ptr <- 1 #Large farm type
  uniform_farm_type_ptr <- 2 #Med farm type
  #uniform_farm_type_ptr <- 3 #Small farm type
  
  Farm_Type_areas <- as.data.frame(matrix(0,nrow=8,ncol=(num_farm_types+1)))# Use the distribution of farm sizes by area within each catchment from Sing & Barron (2014)
  names(Farm_Type_areas)<- c("Catchment",c("large","med","small")) # large = >200ha, med between 101 - 200ha, small < 101ha
  mean_farm_sizes <- c(428,115,40) #ha mean quoted for large, med, small size categories used for transition cost estimation by van Grieken et al (2019)
  Farm_Type_areas[1,] <- c("Daintree",0.505,0.32,0.175) #Ordering here is large med small
  Farm_Type_areas[2,] <- c("Mossman",0.505,0.32,0.175)
  Farm_Type_areas[3,] <- c("Barron",0.438,0.327,0.235)
  Farm_Type_areas[4,] <- c("Mulgrave-Russell",0.488,0.335,0.177)
  Farm_Type_areas[5,] <- c("Johnstone",0.484,0.252,0.262)
  Farm_Type_areas[6,] <- c("Tully",0.769,0.151,0.08)
  Farm_Type_areas[7,] <- c("Murray",0.769,0.151,0.08)
  Farm_Type_areas[8,] <- c("Herbert",0.593,0.278,0.129)
  
  
  #Focus_Year <- "1987"
  #Focus_Year <- "2013"
  
  
  Dir_In <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Input Data"
  Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
  
  P2R_combined_Avg_file_to_read = "P2R_Avg_1987_2013_wide.txt"
  P2R_DINLoss_file_to_read = "P2R_WetTropics_Av_of_DIN_Loss_trimmed_max_to_min.txt"
  P2R_Herbert_Avg_file_to_read = "P2R_Averaged_data_in_Herbert_only.txt"
  P2R_FWCane_file_to_read = "P2R_WetTropics_Av_of_CaneFW_trimmed_max_to_min.txt"
  CVaR_file_to_read = "P2R_CVaR_by_DMU_Best_Worst_10_percent_1987_2013.txt"   # This file gives the average FW cane yield for the top 10 percentile of years 1987-2013
  Urea_prices_to_read = "Urea_Prices_in_2018_19_AUD.txt"  #Sourced by Lin Hasan from ABS and inflated to FY2018-2019 AUD$
  Sugar_prices_to_read = "Sugar_Prices_in_2018_19_AUD.txt" #Sourced by Lin Hasan from ABS data (that were originally sources from QLD MSF Sugar) and inflated to FY2018-2019 AUD$
  Transition_costs_to_read = "Transition_Costs_between_Cane_Mgmt_Practices_v040520.txt"
  Transaction_costs_to_read = "Transaction_Cost_Data_Coggan_2014.txt"
  
  setwd(Dir_In)
  
  if(Focus_Catchment != "Herbert"){
    if (file.exists(P2R_combined_Avg_file_to_read)){  
      P2R_WT_Avg_data_in <- read.table(file = P2R_combined_Avg_file_to_read,header = TRUE, na.strings="-999", sep ="\t")
    } else {
      stop("\t P2R Wet Tropics DMU FW Cane nfo is NOT in the working directory ?")
    }
    #names(P2R_WT_Avg_data_in)
    #str(P2R_WT_Avg_data_in)
    #Remove rows with missing data
    P2R_WT_Avg_data_in <- P2R_WT_Avg_data_in[!(is.na(P2R_WT_Avg_data_in$Concat_12)),]
  }
  
  if(Focus_Catchment == "Herbert"){ # no year by year 1987 - 2013 modelling data for the Herbert catchment, so use pre-averaged data on DIN Loss from Mel Shaw instead
    if (file.exists(P2R_DINLoss_file_to_read)){  
      P2R_WT_Avg_data_in <- read.table(file = P2R_Herbert_Avg_file_to_read,header = TRUE, na.strings="-999", sep ="\t")
    } else {
      stop("\t P2R Wet Tropics DMU DIN Loss info is NOT in the working directory ?")
    }
    #names(P2R_WT_DINLoss_data_in)
    #str(P2R_WT_DINLoss_data_in)
    #Remove rows with missing data
    P2R_WT_Avg_data_in <- P2R_WT_Avg_data_in[!(is.na(P2R_WT_Avg_data_in$Concat_12)),]
  }
  
  
  
  if (file.exists(P2R_FWCane_file_to_read)){  
    P2R_WT_CaneFW_data_in <- read.table(file = P2R_FWCane_file_to_read,header = TRUE, na.strings="NA", sep ="\t")
  } else {
    stop("\t P2R Wet Tropics DMU FW Cane nfo is NOT in the working directory ?")
  }
  #names(P2R_WT_CaneFW_data_in)
  #str(P2R_WT_CaneFW_data_in)
  
  P2R_WT_other_info <-  P2R_WT_CaneFW_data_in[,!grepl("_FW",names(P2R_WT_CaneFW_data_in)[1:length(names(P2R_WT_CaneFW_data_in))])]
  #Write in Bf and Af fertiliser application rates for the Herbert catchment. Use the average fertiliser application rates per soil type
  #We do not have individual year 1987-2013 P2R APSIM data for management units in the Herbert, so use these averages instead
  
  for(soil in 1:length(levels(P2R_WT_other_info$APSIMSoil))){
    P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$Basin_Clip=="Herbert"&P2R_WT_other_info$APSIMSoil==(levels(P2R_WT_other_info$APSIMSoil)[seq(1:length(levels(P2R_WT_other_info$APSIMSoil)))==soil])] <- mean_Bf_fert_applic_by_soil_type[seq(1:length(levels(P2R_WT_other_info$APSIMSoil)))==soil]
  }
  P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$Basin_Clip=="Herbert"] <- P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$Basin_Clip=="Herbert"] * Af_fert_applic_from_Bf_fert_applic
  
  # Trim to include only DMUs in the focus catchment
  P2R_WT_other_info <-  P2R_WT_other_info[P2R_WT_other_info$Basin_Clip==Focus_Catchment,]
  
  #Remove rows with missing data
  P2R_WT_other_info <- P2R_WT_other_info[!(is.na(P2R_WT_other_info$Concat_12)),]
  P2R_WT_other_info <- P2R_WT_other_info[!(is.na(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle)),]
  P2R_WT_other_info$Concat_12_FID <- as.factor(paste(P2R_WT_other_info$Concat_12,P2R_WT_other_info$FID,sep="_"))
  
  #Merge Avg_data_in and other_info into a single dataframe
  P2R_WT_all_data_in <- merge(P2R_WT_Avg_data_in,P2R_WT_other_info)
  
  #Calculate average transport factors to apply to TotDIN_Loss at each practice level
  #After van Grieken et al 2019 Section 3.3, p4 [which cites Furnas & Mitchell (2001) and Webster et al (2012) as sources]
  #DIN from Runoff is transported with the RSDR transport factor as used by Source [these are almost all very close to 1]
  #DIN via Drainage is tranported with Transport_via_GW = 0.6 (after Webster et al (2012))
  P2R_WT_all_data_in$BpAfAf_Avg_Transport <- (P2R_WT_all_data_in$BpAfAf_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpAfAf_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpAfAf_Avg_DINTotper_ha
  P2R_WT_all_data_in$BpBfBf_Avg_Transport <- (P2R_WT_all_data_in$BpBfBf_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpBfBf_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpBfBf_Avg_DINTotper_ha
  P2R_WT_all_data_in$BpBpBp_Avg_Transport <- (P2R_WT_all_data_in$BpBpBp_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpBpBp_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpBpBp_Avg_DINTotper_ha
  P2R_WT_all_data_in$BpCfCf_Avg_Transport <- (P2R_WT_all_data_in$BpCfCf_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpCfCf_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpCfCf_Avg_DINTotper_ha
  P2R_WT_all_data_in$BpCpCp_Avg_Transport <- (P2R_WT_all_data_in$BpCpCp_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpCpCp_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpCpCp_Avg_DINTotper_ha
  P2R_WT_all_data_in$BpDfDf_Avg_Transport <- (P2R_WT_all_data_in$BpDfDf_Avg_DINRunoffper_ha*P2R_WT_all_data_in$RSDR_DIN_Runoff + P2R_WT_all_data_in$BpDfDf_Avg_DINDrainageper_ha*Transport_via_GW)/P2R_WT_all_data_in$BpDfDf_Avg_DINTotper_ha

  P2R_WT_all_data_names <- names(P2R_WT_all_data_in)
  
  All_Avg_Transport<- rep(0,length(P2R_WT_all_data_in$Concat_12))
  for (i in 1:length(P2R_WT_all_data_in$Concat_12)){
    All_Avg_Transport[i] <- mean(as.numeric(as.vector(P2R_WT_all_data_in[i,seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="BpAfAf_Avg_Transport"]:seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="BpDfDf_Avg_Transport"]])))
  }
  P2R_WT_all_data_in$All_Avg_Transport <- All_Avg_Transport

  P2R_WT_all_data_in <- P2R_WT_all_data_in[order(P2R_WT_all_data_in$FID),]
  P2R_WT_all_data_names <- names(P2R_WT_all_data_in)
  
  #Setup easy to read numbering for the columns of P2R_WT_all_data_in
  DMU_no <-(seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="FID"])
  x <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="POINT_X"])
  y <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="POINT_Y"])
  Area_ha <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="Area_ha"])
  Main_Catch <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="Basin_Clip"])
  Sub_Catch <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="Subcatch"])
  Transport <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="All_Avg_Transport"])
  Soil_Type <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="APSIMSoil"])
  Permeability <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="APSIMPerm"])
  Bf_fert <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="Bf_fert_applic_avg_over_cycle"])
  Af_fert <- (seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="Af_fert_applic_avg_over_cycle"])
  
  #Calculate Cane_FWPer_ha,(Total Cane)FW, DINLoss_per_ha and(Total)DINLoss
  DMU_FW_per_ha <- P2R_WT_all_data_in[,grepl("_FW",P2R_WT_all_data_names[1:length(P2R_WT_all_data_names)])]
  DMU_FW <- DMU_FW_per_ha*P2R_WT_all_data_in[,Area_ha]   #Cane FW in tonnes per DMU
  names(DMU_FW) <- paste(N_practices,"_FW",sep="") 
  #DMU_GMs <- (DMU_FWs*Cane_Price*GM_percentage/100)*P2R_WT_CaneFW_data_in[,Area_ha] #more sophisticated calc of gross margin now included below
  DMU_DINLoss_per_ha <- P2R_WT_all_data_in[,grepl("_DINTotper_ha",P2R_WT_all_data_names[1:length(P2R_WT_all_data_names)])]
  DMU_DINLoss <- DMU_DINLoss_per_ha*P2R_WT_all_data_in[,Area_ha] #DIN Loss in kg per DMU
  names(DMU_DINLoss) <- paste(N_practices,"_DIN_loss",sep="") 
  
  
  #################################### Plotting #################################################
  
  if (produce_plots){
  # Generate plots
  #setwd(Dir_Out)
  #png(paste(Focus_Catchment,"_Cane_FW_per_ha_Df.png",sep=""), width=15,height=12, unit="in", res=600) #This plots to a file instead of the screen
  #par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

  with(P2R_WT_all_data_in, plot(BpDfDf_Avg_FWper_ha, xlab = "Management Unit", ylab = "Cane FW (tonnes per ha)",
        main=paste(Focus_Catchment,": Cane FW per ha Df managment practice",sep=""),pch = c(8,1,12,20,17)[as.numeric(APSIMSoil)],col = c("red", "brown","blue")[as.numeric(APSIMPerm)]))
  
  legend(
    x ="topleft",
    legend = c("Soil",levels(P2R_WT_all_data_in$APSIMSoil)), 
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    legend = c("           H-p"), 
    text.col = "red",
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    legend = c("                  M-p"), 
    text.col = c("blue"),
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    legend = c("                          L-p"), 
    text.col = c("brown"),
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    inset = c(0.0925,0.045),
    legend = c(NA,NA,NA,NA,NA),                         
    pch = c(8,1,12,20,17),
    col = "red",
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    inset = c(0.135,0.045),
    legend = c(NA,NA,NA,NA,NA),                         
    pch = c(8,1,12,20,17),
    col = "blue",
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  legend(
    x ="topleft",
    inset = c(0.1775,0.045),
    legend = c(NA,NA,NA,NA,NA),                         
    pch = c(NA,1,NA,20,NA),
    col = "brown",
    bty = "n",
    cex = 0.8 # scale the legend to look attractively sized
  )
  
  segments(x0=-5,y0=78,x1=25,y1=78, col = "black", lty = 1)
  segments(x0=25,y0=78,x1=25,y1=87, col = "black", lty = 1)
  
  #dev.off() # This sets plotting back to the screen
  } #end of 'produce_plots
  ######################################################### End of plotting #######################
  
  
  if (file.exists(Urea_prices_to_read)){  #Read in time series data on Urea prices
    Urea_prices <- read.table(file = Urea_prices_to_read,header = TRUE, na.strings="-999", sep ="\t")
  } else {
    stop("\t Urea_prices_to_readis NOT in the working directory ?")
  }
  Avg_Urea_price <- mean(Urea_prices$Urea_Price_AUD_per_tonne) #mean 1987-2013 Urea price in AUD/tonne [Presume these prices are inflation-corrected ?]
  Avg_N_price <- Avg_Urea_price/N_content_in_Urea*1/1000 #N_price in AUD/kgN
  Avg_N_cost_per_ha <- Avg_N_price*N_applied #Using the P2R standard application rates for each practice
  DMU_Fertiliser_Cost <- matrix(0,nrow = length(P2R_WT_all_data_in$Area_ha),ncol=length(Avg_N_cost_per_ha))
  DMU_Fertiliser_Cost[,seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]<- P2R_WT_all_data_in$Area_ha %*% t(Avg_N_cost_per_ha[seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]) 
  DMU_Fertiliser_Cost[,seq(1:length(N_practices))[N_practices=="Bf"]] <- P2R_WT_all_data_in$Area_ha * as.numeric(as.vector(P2R_WT_all_data_in[,Bf_fert])) * Avg_N_price
  DMU_Fertiliser_Cost[,seq(1:length(N_practices))[N_practices=="Af"]] <- P2R_WT_all_data_in$Area_ha * as.numeric(as.vector(P2R_WT_all_data_in[,Af_fert])) * Avg_N_price
  
  #names(DMU_Fertiliser_Cost) <- paste(N_practices,"_Fert_cost",sep="")

  if (file.exists(Sugar_prices_to_read)){  
    Sugar_prices <- read.table(file = Sugar_prices_to_read,header = TRUE, na.strings="-999", sep ="\t")
  } else {
    stop("\t P2R Wet Tropics DMU FW Cane nfo is NOT in the working directory ?")
  }
  Avg_Sugar_price <- mean(Sugar_prices$QSL_Pool_Price) #mean 1987-2013 Sugar price in AUD/tonne FW cane [Presume these prices are inflation-corrected ?]
  Avg_CCS <- mean(Sugar_prices$CCS) #mean CCS 1987-2013 
  Avg_Cane_price <- Avg_Sugar_price * 0.009 * (Avg_CCS-4) + 0.662 #Avg cane price in AUD/tonne FW cane. Using cane pricing formula from Lin Hasan via MSF Sugar
  
  DMU_Farm_Returns <- DMU_FW * Avg_Cane_price
  names(DMU_Farm_Returns) <- paste(N_practices,"_Farm_Return",sep="")
  
  DMU_Harvest_Cost <- DMU_FW * Harvesting_cost
  names(DMU_Harvest_Cost) <- paste(N_practices,"_Harvest_Cost",sep="")
  
  DMU_Net_Return <- DMU_Farm_Returns - DMU_Fertiliser_Cost - DMU_Harvest_Cost
  names(DMU_Net_Return) <- paste(N_practices,"_Net_Return",sep="")
  
  DMU_Net_Return_per_ha <- DMU_Net_Return/P2R_WT_all_data_in$Area_ha
  
  # #Separate calculations to plot Opportunity Costs of practice change
  # Opp_cost <- DMU_Net_Return[,1]- DMU_Net_Return
  # Opp_cost_per_ha <- Opp_cost/P2R_WT_all_data_in$Area_ha
  # Opp_cost_per_ha<- data.frame(Opp_cost_per_ha,P2R_WT_other_info$Basin_Clip,P2R_WT_other_info$APSIMSoil,P2R_WT_other_info$APSIMPerm,P2R_WT_other_info$Subcatch,P2R_WT_other_info$Area_ha)
  # Opp_cost_per_ha_all_WT <- rbind(Opp_cost_per_ha,Opp_cost_per_ha_all_WT)
  # 
  
  #Separate calculations to plot FW reduction following practice change
  FW_Redn <- DMU_FW[,1]- DMU_FW
  FW_Redn_per_ha <- FW_Redn/P2R_WT_all_data_in$Area_ha
  FW_Redn_per_ha<- data.frame(FW_Redn_per_ha,P2R_WT_other_info$Basin_Clip,P2R_WT_other_info$APSIMSoil,P2R_WT_other_info$APSIMPerm,P2R_WT_other_info$Subcatch,P2R_WT_other_info$Area_ha)
  #FW_Redn_per_ha_all_WT <- rbind(FW_Redn_per_ha,FW_Redn_per_ha_all_WT)

  
  # #Calculate average Bf and Af fertiliser requirements by soil type across all catchments except the Herbert
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="coom"],na.rm=TRUE)
  # #74.24343
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="mari"],na.rm=TRUE)
  # #76.04793
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="stew"],na.rm=TRUE)
  # #69.7147
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="thor"],na.rm=TRUE)
  # #81.66128
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="todd"],na.rm=TRUE)
  # #69.36471
  # mean(P2R_WT_other_info$Bf_fert_applic_avg_over_cycle,na.rm=TRUE)
  # #75.06581
  # 
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="coom"],na.rm=TRUE)
  # #70.53125
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="mari"],na.rm=TRUE)
  # #72.24553
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="stew"],na.rm=TRUE)
  # #66.22897
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="thor"],na.rm=TRUE)
  # #77.57822
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle[P2R_WT_other_info$APSIMSoil=="todd"],na.rm=TRUE)
  # #65.89647
  # mean(P2R_WT_other_info$Af_fert_applic_avg_over_cycle,na.rm=TRUE)
  # #71.31252
  # 
  # #Calculate total area of each soil types in the whole of the Wet Tropics
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="coom"],na.rm=TRUE)
  # #28,807 ha
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="mari"],na.rm=TRUE)
  # #118,951 ha
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="eton"],na.rm=TRUE)
  # #12,419 ha
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="stew"],na.rm=TRUE)
  # #4,859 ha
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="thor"],na.rm=TRUE)
  # #6,285 ha
  # sum(P2R_WT_other_info$Area_ha[P2R_WT_other_info$APSIMSoil=="todd"],na.rm=TRUE)
  # #8,581 ha
  
  
  
  
  
  
  if (produce_plots){
    Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
    setwd(Dir_Out)
    
    old_par<- par()
    png("BoxPlots_Avg_OppCost_per_ha_of_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
    par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot(Opp_cost_per_ha_all_WT$Cf_Net_Return~Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Df to Cf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,20),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Opportunity cost per ha ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Opp_cost_per_ha_all_WT$Bp_Net_Return-Opp_cost_per_ha_all_WT$Cf_Net_Return)~Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Cf to Bp",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,20),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Opportunity cost per ha ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Opp_cost_per_ha_all_WT$Bf_Net_Return-Opp_cost_per_ha_all_WT$Bp_Net_Return)~Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Bp to Bf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,20),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Opportunity cost per ha ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Opp_cost_per_ha_all_WT$Af_Net_Return-Opp_cost_per_ha_all_WT$Bf_Net_Return)~Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Bf to Af",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,20),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(Opp_cost_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Opportunity cost per ha ($ per ha)", side=1, line=2.5)
    mtext("Average Opportunity Cost of Practice Change 1987-2013 Wet Tropics", side=3, line=1,outer=TRUE,cex=1.4,font=2)
    #par(old_par)
    dev.off()
    
    ############################## Plots of changes in average cane FW for changes in fertiliser management practice, grouped by changes in soil type
    
    Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
    setwd(Dir_Out)
    
    old_par<- par()
    png("BoxPlots_Avg_Redn_Cane_FW_per_ha_of_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
    par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot(FW_Redn_per_ha_all_WT$Cf_FW~FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Df to Cf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-1,3),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Reduction in FW Cane Yield per ha (tonnes per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((FW_Redn_per_ha_all_WT$Bp_FW-FW_Redn_per_ha_all_WT$Cf_FW)~FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Cf to Bp",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-1,3),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Reduction in FW Cane Yield per ha (tonnes per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((FW_Redn_per_ha_all_WT$Bf_FW-FW_Redn_per_ha_all_WT$Bp_FW)~FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Bp to Bf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-1,3),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Reduction in FW Cane Yield per ha (tonnes per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((FW_Redn_per_ha_all_WT$Af_FW-FW_Redn_per_ha_all_WT$Bf_FW)~FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil,
            yaxt="n",
            main = "Practice change Bf to Af",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-1,3),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5,6),labels=levels(FW_Redn_per_ha_all_WT$P2R_WT_other_info.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Reduction in FW Cane Yield per ha (tonnes per ha)", side=1, line=2.5)
    mtext("Average Reduction in Cane Fresh Weight Yield 1987-2013 Wet Tropics", side=3, line=1,outer=TRUE,cex=1.4,font=2)
    #par(old_par)
    dev.off()
    
    
  }
  
  
  
  
  
  
  #Convert each of the above matrices to dataframes [Not sure if this is really necessary ?]
  DMU_Farm_Returns_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_Farm_Returns)
  names(DMU_Farm_Returns_df) <- c("Concat_12_FID",paste(N_practices,"_Farm_Returns",sep=""))
  DMU_Fertiliser_Cost_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_Fertiliser_Cost)
  names(DMU_Fertiliser_Cost_df) <- c("Concat_12_FID",paste(N_practices,"_Fert_cost",sep=""))
  DMU_Harvest_Cost_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_Harvest_Cost) 
  names(DMU_Harvest_Cost_df) <- c("Concat_12_FID",paste(N_practices,"_Harvest_cost",sep=""))
  DMU_Net_Return_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_Net_Return)
  names(DMU_Net_Return_df) <- c("Concat_12_FID",paste(N_practices,"_Net_Return",sep=""))
  DMU_DINLoss_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_DINLoss)
  names(DMU_DINLoss_df) <- c("Concat_12_FID",paste(N_practices,"_DINLoss",sep=""))
  
  
  #################################### Conversion Factors kg N applied to kg DIN at 'end of paddock' and kg DIN at end of catchment ##################################
  
  
  DMU_Fertiliser_N_Applied_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,DMU_Fertiliser_Cost_df[,2:7]/Avg_N_price)
  names(DMU_Fertiliser_N_Applied_df) <- c("Concat_12_FID",paste(N_practices,"_Fert_N_Applied_kg",sep=""))
  
  Ratio_N_Applied_to_DIN_from_paddock <- DMU_Fertiliser_N_Applied_df[,2:7]/DMU_DINLoss_df[,2:7]
  names(Ratio_N_Applied_to_DIN_from_paddock) <- c(paste(N_practices,"_Ratio_N_Applied_to_DIN_from_paddock",sep=""))
  Ratio_N_Applied_to_DIN_from_paddock_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,P2R_WT_all_data_in$POINT_X,P2R_WT_all_data_in$POINT_Y,P2R_WT_all_data_in$APSIMSoil,P2R_WT_all_data_in$APSIMPerm,P2R_WT_all_data_in$Area_ha,Ratio_N_Applied_to_DIN_from_paddock )
  names(Ratio_N_Applied_to_DIN_from_paddock_df) <- c("Concat_12_FID","POINT_X","POINT_Y","APSIMSoil","APSIMPerm","Area_ha",names(Ratio_N_Applied_to_DIN_from_paddock))
  Ratio_N_Applied_to_DIN_at_End_of_Catchment <- DMU_Fertiliser_N_Applied_df[,2:7]/(DMU_DINLoss * P2R_WT_all_data_in[,seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="BpDfDf_Avg_Transport"]:seq(1:length(P2R_WT_all_data_names))[P2R_WT_all_data_names =="BpAfAf_Avg_Transport"]])
  names(Ratio_N_Applied_to_DIN_at_End_of_Catchment) <- c(paste(N_practices,"_Ratio_N_Applied_to_DIN_at_End_of_Catchment",sep=""))
  Ratio_N_Applied_to_DIN_at_End_of_Catchment_df <- data.frame(P2R_WT_all_data_in$Concat_12_FID,P2R_WT_all_data_in$POINT_X,P2R_WT_all_data_in$POINT_Y,P2R_WT_all_data_in$APSIMSoil,P2R_WT_all_data_in$APSIMPerm,P2R_WT_all_data_in$Area_ha,Ratio_N_Applied_to_DIN_at_End_of_Catchment)
  names(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df) <- c("Concat_12_FID","POINT_X","POINT_Y","APSIMSoil","APSIMPerm","Area_ha",names(Ratio_N_Applied_to_DIN_at_End_of_Catchment))

  
  #For each level of management practice, calculate mean ratio of N_applied to DIN_from_paddock and DIN_at_end_of_catchment by soil type and by soil permeability
 #tapply(Ratio_N_Applied_to_DIN_from_paddock_df$Df_Ratio_N_Applied_to_DIN_from_paddock,Ratio_N_Applied_to_DIN_from_paddock_df$APSIMSoil,mean)
  
  Df_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Df_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))
  Cp_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Cp_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))
  Cf_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Cf_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))
  Bp_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Bp_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))
  Bf_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Bf_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))
  Af_Ratio_N_Applied_to_DIN_from_paddock <- with(Ratio_N_Applied_to_DIN_from_paddock_df, tapply(Af_Ratio_N_Applied_to_DIN_from_paddock, list(APSIMPerm,APSIMSoil),mean))

  Df_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Df_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  Cp_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Cp_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  Cf_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Cf_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  Bp_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Bp_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  Bf_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Bf_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  Af_Ratio_N_Applied_to_DIN_at_End_of_Catchment <- with(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df, tapply(Af_Ratio_N_Applied_to_DIN_at_End_of_Catchment, list(APSIMPerm,APSIMSoil),mean))
  
  ############################################################################################################################################################################################################
  
  #Now calculate net returns for the CVaR FWper_ha results
  #This gives the net return expected for a top-X-percentile 'good year', based on performance between 1987-2013
  #These top-X-percentile CVaR data have been calculated separately in function P2R_WT_Panel_MultiYear_data_reader v200819.R
  #Refer to the naming of the data input file (specified in CVaR_file_to_read) to see the percentile for which these CVaR values were calculated
  # ONLY DO THE CVaR CALCULATIONS IF Focus_Catchment is NOT the Herbert
  
  if ((Focus_Catchment!="Herbert") && (Include_CVaR_cost)){
   if (file.exists(CVaR_file_to_read)){  
      P2R_WT_not_Herbert_CVaR_data_in <- read.table(file = CVaR_file_to_read,header = TRUE, na.strings="-999", sep ="\t")
    } else {
      stop("\t P2R Wet Tropics DMU DIN Loss info is NOT in the working directory ?")
    }
    #names(P2R_WT_not_Herbert_CVaR_data_in)
    #str(P2R_WT_not_Herbert_CVaR_data_in)
  
    # Trim to include only DMUs in the focus catchment
    # P2R_WT_not_Herbert_CVaR_data_in <-  P2R_WT_not_Herbert_CVaR_data_in[P2R_WT_not_Herbert_CVaR_data_in$Basin_Clip==Focus_Catchment,]
 
    #Assign CVaR X-percentile cane FW yield data to each concat in the main data file.
    #Use the Concat_12 DMU key to do this
    #Separate FID blocks within the same Concat_12 DMU will be assigned the same CVaR X-percentile cane FW yield data. The separate FIDs are indistinguishable in P2R APSIM modelling
  
    P2R_WT_all_data_in$CVaR_best_Df_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Df_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_best_Cp_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Cp_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_best_Cf_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Cf_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_best_Bp_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Bp_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_best_Bf_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Bf_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_best_Af_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_best_Af_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    
    P2R_WT_all_data_in$CVaR_worst_Df_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Df_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_worst_Cp_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Cp_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_worst_Cf_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Cf_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_worst_Bp_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Bp_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_worst_Bf_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Bf_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
    P2R_WT_all_data_in$CVaR_worst_Af_FWper_ha <- P2R_WT_not_Herbert_CVaR_data_in$CVaR_worst_Af_FWper_ha[match(P2R_WT_all_data_in$Concat_12, P2R_WT_not_Herbert_CVaR_data_in$Concat_12)]
  
    #Discard DMUs from the Herbert catchment. We don't have CVaR data for Herbert 
    #Discard DMUs with an area of less than Very_Small_Area
    P2R_WT_not_Herbert_data <- P2R_WT_all_data_in[(P2R_WT_all_data_in$Basin_Clip!="Herbert"),]
    P2R_WT_not_Herbert_data <- P2R_WT_not_Herbert_data[(P2R_WT_not_Herbert_data$Area_ha >= Very_Small_Area),]
  
    # Remove CVaR observations with NAs 
    #It would be good to check back to the multi-year panel data file from which the CVaR results were generated to see why 101 Concat_12_FIDs have NAs in the CVaR data
    #101 of them with NAs seems a bit high ?
    P2R_WT_not_Herbert_data <- P2R_WT_not_Herbert_data[!(is.na(P2R_WT_not_Herbert_data$CVaR_best_Df_FWper_ha)),]
    P2R_WT_not_Herbert_data_names <- names(P2R_WT_not_Herbert_data)
  
  
    CVaR_best_FW_per_ha <- P2R_WT_not_Herbert_data[,grepl("CVaR_best",P2R_WT_not_Herbert_data_names[1:length(P2R_WT_not_Herbert_data_names)])]
    CVaR_best_FW <- CVaR_best_FW_per_ha*P2R_WT_not_Herbert_data$Area_ha   #CVaR_best Cane FW in tonnes per DMU
    names(CVaR_best_FW) <- paste(N_practices,"_CVaR_best_FW",sep="") 
    # Calculate the assumed value of CVaR top-X percentiles for the different practices. Assume that farmers value this for its beneficial impact on gross margins
    Revenue_CVaR_best_FW <- CVaR_best_FW * Avg_Cane_price
    CVaR_best_Fert_Cost <- matrix(0,nrow = length(P2R_WT_not_Herbert_data$Area_ha),ncol=length(Avg_N_cost_per_ha))
    CVaR_best_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]<- P2R_WT_not_Herbert_data$Area_ha %*% t(Avg_N_cost_per_ha[seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]) 
    CVaR_best_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Bf"]] <- P2R_WT_not_Herbert_data$Area_ha * as.numeric(as.vector(P2R_WT_not_Herbert_data[,Bf_fert])) * Avg_N_price
    CVaR_best_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Af"]] <- P2R_WT_not_Herbert_data$Area_ha * as.numeric(as.vector(P2R_WT_not_Herbert_data[,Af_fert])) * Avg_N_price
    #CVaR_best_Fert_Cost <- P2R_WT_not_Herbert_data$Area_ha %*% t(Avg_N_cost_per_ha)
    CVaR_best_Harvest_Cost <- CVaR_best_FW * Harvesting_cost
    Net_Revenue_CVaR_best_FW <- Revenue_CVaR_best_FW - CVaR_best_Fert_Cost - CVaR_best_Harvest_Cost
    Net_Revenue_CVaR_best_FW_df <- data.frame(P2R_WT_not_Herbert_data$Concat_12_FID,Net_Revenue_CVaR_best_FW)
    names(Net_Revenue_CVaR_best_FW_df) <- c("Concat_12_FID",paste(N_practices,"_Net_Revenue_CVaR_best_FW",sep=""))
    
    CVaR_worst_FW_per_ha <- P2R_WT_not_Herbert_data[,grepl("CVaR_worst",P2R_WT_not_Herbert_data_names[1:length(P2R_WT_not_Herbert_data_names)])]
    CVaR_worst_FW <- CVaR_worst_FW_per_ha*P2R_WT_not_Herbert_data$Area_ha   #CVaR_worst Cane FW in tonnes per DMU
    names(CVaR_worst_FW) <- paste(N_practices,"_CVaR_worst_FW",sep="") 
    # Calculate the assumed value of CVaR bottom-X percentiles for the different practices. Assume that farmers value avoiding this low crop because of its adverse impact on gross margin
    Revenue_CVaR_worst_FW <- CVaR_worst_FW * Avg_Cane_price
    CVaR_worst_Fert_Cost <- matrix(0,nrow = length(P2R_WT_not_Herbert_data$Area_ha),ncol=length(Avg_N_cost_per_ha))
    CVaR_worst_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]<- P2R_WT_not_Herbert_data$Area_ha %*% t(Avg_N_cost_per_ha[seq(1:length(N_practices))[N_practices=="Df"]: seq(1:length(N_practices))[N_practices=="Bp"]]) 
    CVaR_worst_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Bf"]] <- P2R_WT_not_Herbert_data$Area_ha * as.numeric(as.vector(P2R_WT_not_Herbert_data[,Bf_fert])) * Avg_N_price
    CVaR_worst_Fert_Cost[,seq(1:length(N_practices))[N_practices=="Af"]] <- P2R_WT_not_Herbert_data$Area_ha * as.numeric(as.vector(P2R_WT_not_Herbert_data[,Af_fert])) * Avg_N_price
    CVaR_worst_Harvest_Cost <- CVaR_worst_FW * Harvesting_cost
    Net_Revenue_CVaR_worst_FW <- Revenue_CVaR_worst_FW - CVaR_worst_Fert_Cost - CVaR_worst_Harvest_Cost
    Net_Revenue_CVaR_worst_FW_df <- data.frame(P2R_WT_not_Herbert_data$Concat_12_FID,Net_Revenue_CVaR_worst_FW)
    names(Net_Revenue_CVaR_worst_FW_df) <- c("Concat_12_FID",paste(N_practices,"_Net_Revenue_CVaR_worst_FW",sep=""))
    
    P2R_WT_not_Herbert_data <- cbind(P2R_WT_not_Herbert_data,Net_Revenue_CVaR_best_FW_df[,-1],Net_Revenue_CVaR_worst_FW_df[,-1])
    
  } else if ((Focus_Catchment =="Herbert") || !Include_CVaR_cost){
    P2R_WT_not_Herbert_data <- P2R_WT_all_data_in[(P2R_WT_all_data_in$Area_ha >= Very_Small_Area),] # the name of this dataframe is confusing in this instance - amend this later if time available
  } 
  
  
  
  
  
  ############################## Plots of changes in CVaR for changes in fertiliser management practice, grouped by changes in soil type
  
 if (produce_plots){
  
  Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
  setwd(Dir_Out)
  
  old_par<- par()
  png("BoxPlots_CVaR_90_FW_per_ha_for_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
  par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot(CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Cf_FWper_ha~CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Df to Cf",
          #xlab = "Average 90%-ile Cane FW reduction (tonne FW per ha)",
          #ylab = "Soil Type",
          ylim = c(-2,4),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 90%-ile 'good years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Bp_FWper_ha-CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Cf_FWper_ha)~CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Cf to Bp",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-2,4),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 90%-ile 'good years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Bf_FWper_ha-CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Bp_FWper_ha)~CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Bp to Bf",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-2,4),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 90%-ile 'good years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Af_FWper_ha-CVaR_best_FW_Redn_per_ha_all_WT$CVaR_best_Bf_FWper_ha)~CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoi,
          yaxt="n",
          main = "Practice change Bf to Af",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-2,4),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 90%-ile 'good years' (tonnes per ha)", side=1, line=2.5)
  mtext("Average Reduction in Cane Fresh Weight Yield for 90%-ile 'good years' 1987-2013 Wet Tropics (excld. Herbert)", side=3, line=1,outer=TRUE,cex=1.4,font=2)
  #par(old_par)
  dev.off()
  
  
  Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
  setwd(Dir_Out)
  
  old_par<- par()
  png("BoxPlots_CVaR_10_FW_per_ha_for_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
  par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot(CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Cf_FWper_ha~CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Df to Cf",
          #xlab = "Average 90%-ile Cane FW reduction (tonnes FW per ha)",
          #ylab = "Soil Type",
          ylim = c(-4,6),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 10%-ile 'bad years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Bp_FWper_ha-CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Cf_FWper_ha)~CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Cf to Bp",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-4,6),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 10%-ile 'bad years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Bf_FWper_ha-CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Bp_FWper_ha)~CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
          yaxt="n",
          main = "Practice change Bp to Bf",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-4,6),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 10%-ile 'bad years' (tonnes per ha)", side=1, line=2.5)
  #par(old_par)
  
  #old_par<- par()
  #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
  boxplot((CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Af_FWper_ha-CVaR_worst_FW_Redn_per_ha_all_WT$CVaR_worst_Bf_FWper_ha)~CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoi,
          yaxt="n",
          main = "Practice change Bf to Af",
          #xlab = "DIN Loss at paddock (kg DIN per ha)",
          #ylab = "Soil Type",
          ylim = c(-4,6),
          col = "grey",
          border = "black",
          horizontal = TRUE,
          notch = FALSE
  )
  axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
  mtext("Soil Type", side=2, line=3.5)
  mtext("Average reduction in Cane FW for 10%-ile 'bad years' (tonnes per ha)", side=1, line=2.5)
  mtext("Average Reduction in Cane Fresh Weight Yield for 10%-ile 'bad years' 1987-2013 Wet Tropics (excld. Herbert)", side=3, line=1,outer=TRUE,cex=1.4,font=2)
  #par(old_par)
  dev.off()
  
 }  
  
  
  ############################## Plots of changes in gross margin CVaR for changes in fertiliser management practice, grouped by changes in soil type
  
  if (produce_plots){
    
    Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
    setwd(Dir_Out)
    
    old_par<- par()
    png("BoxPlots_CVaR_90_GM_per_ha_for_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
    par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot(Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Cf_CVaR_best_FW~Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Df to Cf",
            #xlab = "Average 90%-ile Cane FW reduction (tonne FW per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,150),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 90%-ile 'good years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Bp_CVaR_best_FW-Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Cf_CVaR_best_FW)~Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Cf to Bp",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,150),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 90%-ile 'good years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Bf_CVaR_best_FW-Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Bp_CVaR_best_FW)~Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Bp to Bf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,150),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 90%-ile 'good years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Af_CVaR_best_FW-Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$Bf_CVaR_best_FW)~Net_Revenue_CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Bf to Af",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-100,150),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_best_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 90%-ile 'good years' ($ per ha)", side=1, line=2.5)
    mtext("Average Reduction in Gross Margin per hecatare for 90%-ile 'good years' 1987-2013 Wet Tropics (excld. Herbert)", side=3, line=1,outer=TRUE,cex=1.4,font=2)
    #par(old_par)
    dev.off()
    
    
    
    
    Dir_Out <- "C:/My Docs et al - copy for backing up/Documents/Research/NESP Tropical Water Quality - 2017/NESP N-trading R code/Revised Code for 2017 NESP Project/Output Results"
    setwd(Dir_Out)
    
    old_par<- par()
    png("BoxPlots_CVaR_10_GM_per_ha_for_prac_change_across_soils.png",width=1000,height=765) #This plots to a file instead of the screen. width=1000, height=765 works well for two columns on A4 paper
    par(mfrow=c(2,2),mar=c(3.1,5.6,3.1,1.1),oma=c(1,0,3,1),cex=1.15) #This gives a bit more space on the left hand margin for the vertical axis label
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot(Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Cf_CVaR_worst_FW~Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Df to Cf",
            #xlab = "Average 90%-ile Cane FW reduction (tonne FW per ha)",
            #ylab = "Soil Type",
            ylim = c(-160,200),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 10%-ile 'bad years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Bp_CVaR_worst_FW-Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Cf_CVaR_worst_FW)~Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Cf to Bp",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-160,200),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 10%-ile 'bad years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Bf_CVaR_worst_FW-Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Bp_CVaR_worst_FW)~Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Bp to Bf",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-160,200),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 10%-ile 'bad years' ($ per ha)", side=1, line=2.5)
    #par(old_par)
    
    #old_par<- par()
    #par(mar=c(5.1, 8.1, 4.1, 2.1)) 
    boxplot((Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Af_CVaR_worst_FW-Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$Bf_CVaR_worst_FW)~Net_Revenue_CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil,
            yaxt="n",
            main = "Practice change Bf to Af",
            #xlab = "DIN Loss at paddock (kg DIN per ha)",
            #ylab = "Soil Type",
            ylim = c(-160,200),
            col = "grey",
            border = "black",
            horizontal = TRUE,
            notch = FALSE
    )
    axis(2, las=1,at=c(1,2,3,4,5),labels=levels(CVaR_worst_FW_Redn_per_ha_all_WT$P2R_WT_not_Herbert_data.APSIMSoil))
    mtext("Soil Type", side=2, line=3.5)
    mtext("Average reduction in Gross Margin for 10%-ile 'bad years' ($ per ha)", side=1, line=2.5)
    mtext("Average Reduction in Gross Margin per hecatare for 10%-ile 'bad years' 1987-2013 Wet Tropics (excld. Herbert)", side=3, line=1,outer=TRUE,cex=1.4,font=2)
    #par(old_par)
    dev.off()
    
  }  
  
  
  
  
  #end of the CVaR blocks - go straight here if we're dealing with the Herbert catchment
  
  #Read in data on Transition Costs by Farm Type
  #Costs derived from van Grieken et al 2010 and 2019.
  #Costs vary by farm size range: Type 1 (largest) 180-1230ha; Type 2 80 - 180ha; Type 3 20 - 80ha
  #Costs from van Grieken et al 2010 inflated to 2018 $ by Lin Hasan
  #Annualised transition cost expressed in 2018$, annualised over a 6-year cane cycle at a discount rate of 7% p.a.
  
  setwd(Dir_In)
  
  if (Include_transition_cost){
    if (file.exists(Transition_costs_to_read)){  
      Transition_cost_data_in <- read.table(file = Transition_costs_to_read,header = TRUE, na.strings="-999", sep ="\t")
    } else {
      stop("\t P2R Wet Tropics DMU DIN Loss info is NOT in the working directory ?")
    }
  }
  
  
  if (Include_transaction_cost){
    if (file.exists(Transaction_costs_to_read)){  
      Transaction_cost_data_in <- read.table(file = Transaction_costs_to_read,header = TRUE, na.strings="-999", sep ="\t")
    } else {
      stop("\t P2R Wet Tropics DMU DIN Loss info is NOT in the working directory ?")
    }
  }
  
  
  #Sum all the separate elements of cost to arrive at the total cost associated with each level of cane practice
  #Separate elements of cost are:
  # 1. Opportunity cost expressed as reduction in average net revenue from cane production 
  # 2. Cost of bearing risk associated with reduced cane yield in a 'good year': expressed as (CVaR X-percentile cane FW * average cane price)
  # 3. Cost of transitioning between cane practices (primarily equipment-based) [from van Grieken et al 2019]
  # 4. Transaction cost [to be derived from papers by Anthea Coggan and Romi Griener]
 
  # Merge Total DIN losses and Total Net Returns into P2R_WT_not_Herbert_data 
  # This picks up relevant data for the DMU_FIDs for which we have the CVaR results
  P2R_WT_not_Herbert_data <- merge(P2R_WT_not_Herbert_data,DMU_DINLoss_df, by.x="Concat_12_FID", by.y="Concat_12_FID")
  P2R_WT_not_Herbert_data <- merge(P2R_WT_not_Herbert_data,DMU_Net_Return_df, by.x="Concat_12_FID", by.y="Concat_12_FID")
  col_names <- names(P2R_WT_not_Herbert_data)
  
  #identify Total_DIN_loss (kg DIN) and Total_All_Revenues for each practice level
  optn_Tot_DIN_loss <- P2R_WT_not_Herbert_data[,grepl("_DINLoss",col_names[1:length(col_names)])]
  optn_Tot_All_Revenues <- P2R_WT_not_Herbert_data[,grepl("_Net_Return",col_names[1:length(col_names)])] 
  
  # Read the names of the management practices directly from the Tot_DIN_Loss data 
  practices <- rep(" ",length(names(optn_Tot_DIN_loss)))
  for (i in 1:ncol(optn_Tot_DIN_loss)){
    practices[i] <- strsplit(names(optn_Tot_DIN_loss), split="_")[[i]][1]
  }
  names(optn_Tot_All_Revenues) <- paste(practices,"_Tot_All_Revenues",sep="") #correct naming for columns of optn_Tot_All_Revenues
  
  
  #BaseLeach generator
  Base_Practice_dmus <- matrix(0,nrow=length(N_practices),ncol=length(P2R_WT_not_Herbert_data$Concat_12_FID))
  
  if (!Uniform_baseline_practice){# randomly assign baseline practice across DMUs so that the area of caneland under each practice matches that reported for the Focus_Catchment in the 2017 & 2018 Reef Report Cards 
    #set.seed(Seed_number)
    dmu_ptr <- seq(1:length(P2R_WT_not_Herbert_data$Concat_12_FID))
    mean_dmu_area <- mean(P2R_WT_not_Herbert_data$Area_ha)
    median_dmu_area <- median(P2R_WT_not_Herbert_data$Area_ha)
    tot_dmu_area <- sum(P2R_WT_not_Herbert_data$Area_ha)
    num_dmus <- length(P2R_WT_not_Herbert_data$Concat_12_FID)
    catchment_prac_proportions <- as.numeric(as.vector(Practice_Level_probabilities[(Practice_Level_probabilities[,1]==Focus_Catchment),-1]))#Read practice proportions for Focus_Catchment. Don't pick up the catchment name.
    trip_out <- FALSE #Initialise trip_out
  
    for (prac in 1:length(N_practices)){
      if (prac == 1){
        dmu_select <- sample(dmu_ptr,round(catchment_prac_proportions[prac]*num_dmus))
      } else if (prac < length(N_practices)){
        if(round(catchment_prac_proportions[prac]*num_dmus)<=length(dmu_ptr[! dmu_ptr %in% Base_Practice_dmus[1:(prac-1),]])){
          dmu_select <- sample(dmu_ptr[! dmu_ptr %in% Base_Practice_dmus[1:(prac-1),]],round(catchment_prac_proportions[prac]*num_dmus))
        } else {
          dmu_select <- sample(dmu_ptr[! dmu_ptr %in% Base_Practice_dmus[1:(prac-1),]],length(dmu_ptr[! dmu_ptr %in% Base_Practice_dmus[1:(prac-1),]]))
        }
      } else { #Here for the final practice
        dmu_select <- dmu_ptr[! dmu_ptr %in% Base_Practice_dmus[1:(prac-1),]]
      }
      prac_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
      error <- catchment_prac_proportions[prac]-prac_area_proportion
      while_count<-1
      trip_out <- FALSE
      while ((abs(error)>area_tolerance) && (length(dmu_ptr[! dmu_ptr %in% c(Base_Practice_dmus[1:prac,],dmu_select)])>0) && !trip_out){#too big an area under this practice
        if (error < 0){#too large an area under this practice currently
          if (length(dmu_select)>1){
          dmu_select <- dmu_select[1:(length(dmu_select)-1)]
          prac_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
          error <- catchment_prac_proportions[prac]-prac_area_proportion
          } else {
            trip_out <- TRUE
          }
        } else if (error > 0){#too small an area under this practice currently
          dmu_select <- c(dmu_select,sample((dmu_ptr[! dmu_ptr %in% c(Base_Practice_dmus[1:prac,],dmu_select)]),1))
          prac_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
          error <- catchment_prac_proportions[prac]-prac_area_proportion
        }
        while_count<- while_count+1
        if(while_count>50){
          trip_out <- TRUE
        }
      }
      if(length(dmu_select)>0){
        Base_Practice_dmus[prac,1:length(dmu_select)] <- dmu_select
      }
    }
  
    prac_area_proportion_check<-rep(0,6)
    for (prac in 1:6){
     prac_area_proportion_check[prac] <- sum(P2R_WT_not_Herbert_data$Area_ha[Base_Practice_dmus[prac,(Base_Practice_dmus[prac,]!=0)]])/tot_dmu_area
    }

    #Now allocate DMUs to base practice levels using the area allocations shown in Base_Practice_dmus[]
    BasePractice_ptr <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
    for (prac in 1:6){
      BasePractice_ptr[Base_Practice_dmus[prac,(Base_Practice_dmus[prac,]!=0)]] <- prac #DMUs at appropriate BasePractice Level
    }
  } else {# here if uniform baseline practice has been specified
    BasePractice_ptr <- rep(uniform_baseline_prac_ptr,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  }
  
  #Remove any DMUs that have not been assigned a base practice level.
  #There should only be a few of these per catchment
  
  P2R_WT_not_Herbert_data <- P2R_WT_not_Herbert_data[(BasePractice_ptr!=0),]
  optn_Tot_All_Revenues <- optn_Tot_All_Revenues[(BasePractice_ptr!=0),]
  optn_Tot_DIN_loss <- optn_Tot_DIN_loss[(BasePractice_ptr!=0),]
  # if ((Focus_Catchment =="Herbert") || !Include_CVaR_cost){
  #   Net_Revenue_CVaR_FW <- Net_Revenue_CVaR_FW[(BasePractice_ptr!=0),]
  #   Net_Revenue_CVaR_FW_df <- Net_Revenue_CVaR_FW_df[(BasePractice_ptr!=0),]
  #   CVaR_FW <- CVaR_FW[(BasePractice_ptr!=0),]
  #   CVaR_FW_per_ha <- CVaR_FW_per_ha[(BasePractice_ptr!=0),]
  # }
  # 
  #Trim BasePractice_ptr appropriately too
  BasePractice_ptr <- BasePractice_ptr[BasePractice_ptr!=0]
  
  BasePractice <- practices[BasePractice_ptr]
  P2R_WT_not_Herbert_data$BasPractice <- BasePractice
  Base_Leaching <- optn_Tot_DIN_loss[cbind(1:length(P2R_WT_not_Herbert_data$Concat_12_FID), BasePractice_ptr)]
  Base_Revenues <- optn_Tot_All_Revenues[cbind(1:length(P2R_WT_not_Herbert_data$Concat_12_FID), BasePractice_ptr)]
  P2R_WT_not_Herbert_data$BasLeach_kgDIN <- Base_Leaching
  P2R_WT_not_Herbert_data$BasRevenue_dollar <- Base_Revenues
  BaseLeach <- (seq(1:length(names(P2R_WT_not_Herbert_data)))[names(P2R_WT_not_Herbert_data) =="BasLeach_kgDIN"])
  BaseReven <- (seq(1:length(names(P2R_WT_not_Herbert_data)))[names(P2R_WT_not_Herbert_data) =="BasRevenue_dollar"])
  
  #Farm size generator ('Farm Type')
  #Farm Type is used to allocate transition and transaction costs to DMUs
  
  if (!Uniform_farm_type){
    Farm_Type_dmus <- matrix(0,nrow=num_farm_types,ncol=length(P2R_WT_not_Herbert_data$Concat_12_FID))
    #set.seed(Seed_number)
    dmu_ptr <- seq(1:length(P2R_WT_not_Herbert_data$Concat_12_FID))
    mean_dmu_area <- mean(P2R_WT_not_Herbert_data$Area_ha)
    median_dmu_area <- median(P2R_WT_not_Herbert_data$Area_ha)
    tot_dmu_area <- sum(P2R_WT_not_Herbert_data$Area_ha)
    num_dmus <- length(P2R_WT_not_Herbert_data$Concat_12_FID)
    catchment_type_proportions <- as.numeric(as.vector(Farm_Type_areas[(Farm_Type_areas[,1]==Focus_Catchment),-1]))#Read practice proportions for Focus_Catchment. Don't pick up the catchment name.
    trip_out <- FALSE #Initialise trip_out
  
    for (farmtype in 1:num_farm_types){
      if (farmtype == 1){
        dmu_select <- sample(dmu_ptr,round(catchment_type_proportions[farmtype]*num_dmus))
      } else if (farmtype < num_farm_types){
        if(round(catchment_type_proportions[farmtype]*num_dmus)<=length(dmu_ptr[! dmu_ptr %in% Farm_Type_dmus[1:(farmtype-1),]])){
          dmu_select <- sample(dmu_ptr[! dmu_ptr %in% Farm_Type_dmus[1:(farmtype-1),]],round(catchment_type_proportions[farmtype]*num_dmus))
        } else {
          dmu_select <- sample(dmu_ptr[! dmu_ptr %in% Farm_Type_dmus[1:(farmtype-1),]],length(dmu_ptr[! dmu_ptr %in% Farm_Type_dmus[1:(farmtype-1),]]))
        }
      } else { #Here for the final farm type
        dmu_select <- dmu_ptr[! dmu_ptr %in% Farm_Type_dmus[1:(farmtype-1),]]
      }
      type_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
      error <- catchment_type_proportions[farmtype]-type_area_proportion
      while_count<-1
      trip_out <- FALSE
      while ((abs(error)>area_tolerance) && (length(dmu_ptr[! dmu_ptr %in% c(Farm_Type_dmus[1:farmtype,],dmu_select)])>0) && !trip_out){#too big an area under this practice
        if (error < 0){#too large an area under this practice currently
          if (length(dmu_select)>1){
            dmu_select <- dmu_select[1:(length(dmu_select)-1)]
            type_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
            error <- catchment_type_proportions[farmtype]-type_area_proportion
          } else {
            trip_out <- TRUE
          }
        } else if (error > 0){#too small an area under this practice currently
          dmu_select <- c(dmu_select,sample((dmu_ptr[! dmu_ptr %in% c(Farm_Type_dmus[1:farmtype,],dmu_select)]),1))
          type_area_proportion <- sum(P2R_WT_not_Herbert_data$Area_ha[dmu_select])/tot_dmu_area
          error <- catchment_type_proportions[farmtype]-type_area_proportion
        }
        while_count<- while_count+1
        if(while_count>50){
          trip_out <- TRUE
        }
      }
      if(length(dmu_select)>0){
        Farm_Type_dmus[farmtype,1:length(dmu_select)] <- dmu_select
      }
    }
  
    # type_area_proportion_check<-rep(0,num_farm_types)
    # for (type in 1:num_farm_types){
    # type_area_proportion_check[type] <- sum(P2R_WT_not_Herbert_data$Area_ha[Farm_Type_dmus[type,(Farm_Type_dmus[type,]!=0)]])/tot_dmu_area
    # }
  
    #Now allocate DMUs to farm types using the area allocations shown in Farm_Type_dmus[]
    FarmType_ptr <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
    for (type in 1:num_farm_types){
      FarmType_ptr[Farm_Type_dmus[type,(Farm_Type_dmus[type,]!=0)]] <- type #DMUs of appropriate Farm Type
    }
  } else {#Assign a uniform farm type to all farms
    FarmType_ptr <- rep(uniform_farm_type_ptr,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  }
  
  #Remove any DMUs that have not been assigned a farm type
  #There should only be a few of these per catchment
  
  P2R_WT_not_Herbert_data <- P2R_WT_not_Herbert_data[(FarmType_ptr!=0),]
  optn_Tot_All_Revenues <- optn_Tot_All_Revenues[(FarmType_ptr!=0),]
  optn_Tot_DIN_loss <- optn_Tot_DIN_loss[(FarmType_ptr!=0),]
  # if ((Focus_Catchment =="Herbert") || !Include_CVaR_cost){
  #   Revenue_CVaR_FW <- Revenue_CVaR_FW[(FarmType_ptr!=0),]
  #   Revenue_CVaR_FW_df <- Revenue_CVaR_FW_df[(FarmType_ptr!=0),]
  #   CVaR_FW <- CVaR_FW[(FarmType_ptr!=0),]
  #   CVaR_FW_per_ha <- CVaR_FW_per_ha[(FarmType_ptr!=0),]
  # }
  
  #Trim FarmType_ptr appropriately too
  FarmType_ptr <- FarmType_ptr[FarmType_ptr!=0]
  
  FarmType <- farm_types[FarmType_ptr]
  P2R_WT_not_Herbert_data$FarmType <- FarmType
  
  T_cost_multiplier <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  T_cost_multiplier <- P2R_WT_not_Herbert_data$Area_ha/mean_farm_sizes[FarmType]
  P2R_WT_not_Herbert_data$T_cost_mult <- T_cost_multiplier
  
  #Setup easy to read numbering for the columns of P2R_WT_not_Herbert_data
  P2R_WT_not_Herbert_data_names <- names(P2R_WT_not_Herbert_data)
  DMU_no <-(seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="Concat_12_FID"])
  x <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="POINT_X"])
  y <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="POINT_Y"]) 
  Area_ha <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="Area_ha"])
  Main_Catch <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="Basin_Clip"])
  Sub_Catch <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="Subcatch"])
  Transport <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="All_Avg_Transport"])
  Soil_Type <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="APSIMSoil"])
  Permeability <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="APSIMPerm"])
  Farm_Type <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="FarmType"])
  Base_Pract <- (seq(1:length(P2R_WT_not_Herbert_data_names))[P2R_WT_not_Herbert_data_names =="BasPractice"])
  
  
  # As a starting point,calculate optn_Tot_Abate_Cost as the total cost incurred in moving to each of the improved practices from the baseline practice
  # N.B. this is referred to as 'Total Abatement Cost' for compatibility with the Limfjorden software
  # N.B. these data are called TOTAL abatement cost because they report the total cost incurred in moving all the way from baseline practice to the stated practice
  # Subsequently, these total abatement costs will be converted into incremental abatement costs that report the incremental (i.e. stepwise) cost of practice improvement
  
  #Modify optn_Tot_Abate_Cost and optn_Tot_DIN_loss to account correctly for Revenues and DIN loss from Base_Leaching practice
  optn_Tot_Abate_Cost <- P2R_WT_not_Herbert_data$BasRevenue_dollar - optn_Tot_All_Revenues #Column $BasRevenue_dollar holds the revenues associated with the Base_Leaching practice
  names(optn_Tot_Abate_Cost) <- paste(practices,"_Tot_Abate_Cost",sep="") #correct naming for columns of optn_Tot_Abate_Cost
  
  # Calculate CVaR-derived production risk in good and bad years, knowing baseline practice
  #Separate calculations to plot Net Revenue CVaR best and worst following practice change
  
  if (Include_CVaR_cost){
  
    Net_Revenue_CVaR_best_FW_Redn<- matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=length(practices))
    Net_Revenue_CVaR_best_FW <- P2R_WT_not_Herbert_data[,grepl("_Net_Revenue_CVaR_best_FW",names(P2R_WT_not_Herbert_data)[1:length(names(P2R_WT_not_Herbert_data))])] 
    for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      Net_Revenue_CVaR_best_FW_Redn[DMU,] <- as.numeric(Net_Revenue_CVaR_best_FW[DMU,BasePractice_ptr[DMU]] - Net_Revenue_CVaR_best_FW[DMU,])
    }
    Net_Revenue_CVaR_best_FW_Redn_per_ha <- Net_Revenue_CVaR_best_FW_Redn/P2R_WT_not_Herbert_data$Area_ha
    Net_Revenue_CVaR_best_FW_Redn_per_ha_df<- data.frame(Net_Revenue_CVaR_best_FW_Redn_per_ha,P2R_WT_not_Herbert_data$Basin_Clip,P2R_WT_not_Herbert_data$APSIMSoil,P2R_WT_not_Herbert_data$APSIMPerm,P2R_WT_not_Herbert_data$Subcatch,P2R_WT_not_Herbert_data$Area_ha)
    names(Net_Revenue_CVaR_best_FW_Redn_per_ha_df) <- c(paste(practices,"_Net_Rev_Redn_CVaR_best",sep=""),"Basin_Clip","APSIMSoil","APSIMPerm","Subcatch","Area_ha")
  
    Net_Revenue_CVaR_worst_FW_Redn<- matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=length(practices))
    Net_Revenue_CVaR_worst_FW <- P2R_WT_not_Herbert_data[,grepl("_Net_Revenue_CVaR_worst_FW",names(P2R_WT_not_Herbert_data)[1:length(names(P2R_WT_not_Herbert_data))])] 
    for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      Net_Revenue_CVaR_worst_FW_Redn[DMU,] <- as.numeric(Net_Revenue_CVaR_worst_FW[DMU,BasePractice_ptr[DMU]] - Net_Revenue_CVaR_worst_FW[DMU,])
    }
    Net_Revenue_CVaR_worst_FW_Redn_per_ha <- Net_Revenue_CVaR_worst_FW_Redn/P2R_WT_not_Herbert_data$Area_ha
    Net_Revenue_CVaR_worst_FW_Redn_per_ha_df<- data.frame(Net_Revenue_CVaR_worst_FW_Redn_per_ha,P2R_WT_not_Herbert_data$Basin_Clip,P2R_WT_not_Herbert_data$APSIMSoil,P2R_WT_not_Herbert_data$APSIMPerm,P2R_WT_not_Herbert_data$Subcatch,P2R_WT_not_Herbert_data$Area_ha)
    names(Net_Revenue_CVaR_worst_FW_Redn_per_ha_df) <- c(paste(practices,"_Net_Rev_Redn_CVaR_worst",sep=""),"Basin_Clip","APSIMSoil","APSIMPerm","Subcatch","Area_ha")
  } 
  
  #If required, add in cost of exposure to production risk in good years and bad years
  if (Include_CVaR_cost){
    Prod_Risk_CVaR_best_FW_Redn <- matrix(0,nrow = length(P2R_WT_not_Herbert_data$Area_ha),ncol=length(Avg_N_cost_per_ha))
    for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      if(BasePractice_ptr[DMU]<6){
        Prod_Risk_CVaR_best_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)][Net_Revenue_CVaR_best_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)]>0] <- as.numeric(Net_Revenue_CVaR_best_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)])[Net_Revenue_CVaR_best_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)]>0] 
      }
    }
    Prod_Risk_CVaR_worst_FW_Redn <- matrix(0,nrow = length(P2R_WT_not_Herbert_data$Area_ha),ncol=length(Avg_N_cost_per_ha))
    for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      if(BasePractice_ptr[DMU]<6){
        Prod_Risk_CVaR_worst_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)][Net_Revenue_CVaR_worst_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)]>0] <- as.numeric(Net_Revenue_CVaR_worst_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)])[Net_Revenue_CVaR_worst_FW_Redn[DMU,(BasePractice_ptr[DMU]+1):length(practices)]>0] 
      } 
    }
    optn_Tot_Abate_Cost <- optn_Tot_Abate_Cost + Prod_Risk_CVaR_best_FW_Redn + Prod_Risk_CVaR_worst_FW_Redn
  }
  
  #If required, add in cost of practice transition
  if (Include_transition_cost){
    for (DMU in  1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      DMU_transition_cost <- Transition_cost_data_in[,FarmType_ptr[DMU]+1]
      if (BasePractice_ptr[DMU]<6){
        if (BasePractice_ptr[DMU] ==5){ #Farm is already at practice Bf
          optn_Tot_Abate_Cost[DMU,6] <- optn_Tot_Abate_Cost[DMU,6]+ (DMU_transition_cost[6] * T_cost_multiplier[DMU]) #only include transition cost for transitioning from practice Bf to practice Af
        } else if (BasePractice_ptr[DMU]==4) { #Farm is at practice class Bp
          optn_Tot_Abate_Cost[DMU,6] <- optn_Tot_Abate_Cost[DMU,6]+ (DMU_transition_cost[5] * T_cost_multiplier[DMU]) #cost for transitioning from practice class Bp to practice class Af
          optn_Tot_Abate_Cost[DMU,6] <- optn_Tot_Abate_Cost[DMU,5]+ (DMU_transition_cost[4] * T_cost_multiplier[DMU]) #cost for transitioning from practice class Bp to practice class Bf
        } else { # Farm is at practice class Df, Cp or Cf
          optn_Tot_Abate_Cost[DMU,6] <- optn_Tot_Abate_Cost[DMU,6] + DMU_transition_cost[3] * T_cost_multiplier[DMU] #include transition cost from class D or C to class Af
          optn_Tot_Abate_Cost[DMU,5] <- optn_Tot_Abate_Cost[DMU,5] + DMU_transition_cost[2] * T_cost_multiplier[DMU] #include transition cost from class D or C to class Bf
          optn_Tot_Abate_Cost[DMU,4] <- optn_Tot_Abate_Cost[DMU,4] + DMU_transition_cost[1] * T_cost_multiplier[DMU] #include transition cost from class D or C to class Bp
        }
      }
    }
  }
  
  
  #If required, add in transaction costs based on findings from Coggan et al 2013, as explained on Rolfe & Windle NESP Project 3.10 Report, p46
  #Increase the 'abatement cost' associated with all practices other than the baseline practice by the amount of the transaction cost. 
  #Any move away from the baseline position (offering to sell credits, or bidding to buy, credits) will incur transaction cost
  #Thus, transaction costs will arise the first time a DMU enters the market. i.e. as soon as they move away from their initial allocation of credits
  if (Include_transaction_cost){
    for (DMU in  1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      optn_Tot_Abate_Cost[DMU,seq(1:length(N_practices))[N_practices != BasePractice[DMU]]] <- optn_Tot_Abate_Cost[DMU,seq(1:length(N_practices))[N_practices != BasePractice[DMU]]] + (Transaction_cost*T_cost_multiplier[DMU]) #only include transition cost for transitioning from practice class B to practice class A
    }
  }
  
 if(clamp_at_BasePractice){
   #Modification here to 'saturate' Total Abatement Cost and Total_DIN_Loss for practices that are 'worse' than BasePractice
    for (DMU in  1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
      optn_Tot_Abate_Cost[DMU,(seq(1:length(practices))<BasePractice_ptr[DMU])] <- optn_Tot_Abate_Cost[DMU,][BasePractice_ptr[DMU]]
      optn_Tot_DIN_loss[DMU,(optn_Tot_DIN_loss[DMU,]>t(rep((optn_Tot_DIN_loss[DMU,][BasePractice_ptr[DMU]]),length(practices))))] <- optn_Tot_DIN_loss[DMU,][BasePractice_ptr[DMU]]
    }
 }
  
  # This section of code calculates the incremental costs of changing practice, step by step i.e. Df to Cp, Cp to Cf, Cf to Bp, Bp to Bf, Bf to Af
  
  #Need to think about how to handle negative optn_Tot_Abate_Cost data - Will have to come back to that in a bit
  #For now, just try to get incremental abatement cost data running for DMUs that don't have any negative optn_Tot_Abate_Cost data 
 
  optn_Inc_DIN_Reduct <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])-1)))
  optn_Inc_Abate_Cost <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_Abate_Cost[1,])-1)))
  #names(optn_Inc_DIN_Reduct) <- practices
  #names(optn_Inc_Abate_Cost) <- practices
  optn_Bid_Qs <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])-1))) #Will hold incremental steps in DIN Loss i.e. bid quantities (positive for 'buy' tranches, negative for 'sell' tranches)
  optn_Bid_Ps <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])-1))) #Will hold incremental DIN prices (these are always positive, whether buying or selling DIN)
  
  #For P2R data, the highest level of leaching is the leaching associated with the Df practice
  #If Tot_DIN_Loss and Tot_Abatement_Cost have been clamped at 'BasePractice' [lines 645-651 above] then DIN Loss between ''baseleaching'leaching' any practices that are 'worse' than the BaseLeach practice will be zero
  for (pract in 2:length(optn_Tot_DIN_loss[1,])){
    optn_Inc_DIN_Reduct[,pract-1] <- optn_Tot_DIN_loss[,pract-1]-optn_Tot_DIN_loss[,pract]  
  }
  optn_Bid_Qs <- optn_Inc_DIN_Reduct
  #zero_reductions <- optn_Bid_Qs[,2:length(practices)]==0
  #zero_reductions <- optn_Bid_Qs==0
  #optn_Bid_Qs[,2:length(practices)][zero_reductions] <- Very_Small_DIN_Reduction #This removes a 'divide by zero' problem for optn_Bid_Ps when the DIN reduction between practice steps is near zero
  #optn_Bid_Qs[zero_reductions] <- Very_Small_DIN_Reduction #This removes a 'divide by zero' problem for optn_Bid_Ps when the DIN reduction between practice steps is near zero
  
  #Same again here for incremental abatement costs
  for (pract in 2:length(optn_Tot_Abate_Cost[1,])){
    optn_Inc_Abate_Cost[,pract-1] <- optn_Tot_Abate_Cost[,pract]-optn_Tot_Abate_Cost[,pract-1] 
  }
  optn_Bid_Ps <- optn_Inc_Abate_Cost/optn_Bid_Qs
  #optn_Bid_Ps[is.na(optn_Bid_Ps)] <- 0   #Will this be sufficient to handle negative abatement costs ? Need to check 
  #optn_Bid_Ps$Cf[!is.finite(optn_Bid_Ps$Cf)] <- Very_High_Price - shouldn't need this now optn_Bid_Qs has been set non-zero for anything other than the first tranche
  
  #Save 'raw data' i.e. un-convexified steps in DIN abatement and Abatement Cost
  unconvex_optn_Bid_Ps <- optn_Bid_Ps
  unconvex_optn_Bid_Qs <- optn_Bid_Qs
  
  min_DIN_unconvex <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  min_DIN_unconvex <- (P2R_WT_not_Herbert_data$BasLeach_kgDIN - rowSums(unconvex_optn_Bid_Qs))/P2R_WT_not_Herbert_data$Area_ha
  #summary(min_DIN_unconvex[])
  #which(min_DIN_unconvex<0)
  
  
  ################################ CONVEXIFICATION STARTS HERE ###########################################################################
  
  # Start here by ensuring that Total Abatement Cost vs Total DIN Loss is convex for all farms
  # Force this by ensuring that slope between adjacent data points gets steeper and steeper as DIN reduction increases.
  
  seq_DIN_loss <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  seq_Abate_cost <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  seq_avg_slope <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  prac_change_tranche_pointer <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  prac_change_tranche_cost <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  prac_change_tranche_DIN <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  first_slope_DIN_loss <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)
  first_slope_Abate_cost <- matrix(0,nrow=length(optn_Bid_Qs[,1]),ncol=length(practices)-1)

  #DMU<-1
  #i<-1
  #i<-2
  #i<-3
  #i<-4
  #i<-5
  #i<-6
  #i<-7
  #i<-8
  
  #DMU<-180
  for (DMU in 1:length(optn_Bid_Qs[,1])){
    seq_DIN_loss[DMU,] <- as.numeric(as.vector(optn_Inc_DIN_Reduct[DMU,]))
    seq_Abate_cost[DMU,] <- as.numeric(as.vector(optn_Inc_Abate_Cost[DMU,]))
    for (i in 1:(length(practices)-1)){
      first_slope_DIN_loss[DMU,i] <- sum(seq_DIN_loss[DMU,(1:i)])
      first_slope_Abate_cost[DMU,i] <- sum(seq_Abate_cost[DMU,(1:i)])
      #if (optn_Inc_DIN_Reduct[DMU,i]!=0){
        #seq_DIN_loss[DMU,i] <- seq_DIN_loss[DMU,(i-1)] + optn_Inc_DIN_Reduct[DMU,i]
        #seq_Abate_cost[DMU,i] <- seq_Abate_cost[DMU,(i-1)] + optn_Inc_Abate_Cost[DMU,i]
      #}
    }
  }
  first_avg_slope <- first_slope_Abate_cost/first_slope_DIN_loss
  
  #DMU<-1
  #DMU<-180
  for (DMU in 1:length(optn_Tot_DIN_loss[,1])){
    another_point <- TRUE
    DMU_seq_avg_slope <- first_avg_slope[DMU,]
    DMU_seq_DIN_incs <- as.numeric(as.vector(optn_Inc_DIN_Reduct[DMU,]))
    DMU_seq_Abate_Cost_incs <- as.numeric(as.vector(optn_Inc_Abate_Cost[DMU,]))
    #i<-1
    #i<-2
    for(i in 1:(length(practices)-1)){ #looking for a maximum of (length(practices)-1) data points per farm
      if (another_point){
        if (sum(is.na(DMU_seq_avg_slope))<(length(practices)-1)){
          prac_change_tranche_pointer[DMU,i] <- which(DMU_seq_avg_slope == min(DMU_seq_avg_slope, na.rm = TRUE))[1] #using position 1 here will break a tie if any tie emerges here
        if (i == 1){#Dealing with the first data point
            prac_change_tranche_DIN[DMU,i] <- sum(DMU_seq_DIN_incs[(1):(prac_change_tranche_pointer[DMU,i])])
            prac_change_tranche_cost[DMU,i] <- sum(DMU_seq_Abate_Cost_incs[(1):prac_change_tranche_pointer[DMU,i]])
          } else {#dealing with a later offer tranche
            prac_change_tranche_DIN[DMU,i] <- prac_change_tranche_DIN[DMU,(i-1)] + DMU_seq_DIN_incs[(prac_change_tranche_pointer[DMU,i])]
            prac_change_tranche_cost[DMU,i] <- prac_change_tranche_cost[DMU,(i-1)] + DMU_seq_Abate_Cost_incs[prac_change_tranche_pointer[DMU,i]]
          }
        } else {another_point <- FALSE} #here if we've already handled all of the available data points for this DMU
      }
      DMU_seq_DIN_incs <- rep(0,(length(practices)-1))#initialise
      DMU_seq_Abate_Cost_incs <- rep(0,(length(practices)-1))#initialise
      DMU_seq_avg_slope <- rep(0,(length(practices)-1))#initialise
      if((prac_change_tranche_pointer[DMU,i] < ((length(practices)-1)))&&(another_point)) { #if so, then look for next point on convex curve
        DMU_seq_DIN_incs[prac_change_tranche_pointer[DMU,i]+1] <- (optn_Bid_Qs[DMU,prac_change_tranche_pointer[DMU,i]+1])
        DMU_seq_Abate_Cost_incs[(prac_change_tranche_pointer[DMU,i]+1)] <- optn_Inc_Abate_Cost[DMU,(prac_change_tranche_pointer[DMU,i]+1)] 
        if (prac_change_tranche_pointer[DMU,i]<((length(practices)-2))) {
          #j<-4
          #j<-5
          #j<-6
          #j<-7
          #j<-8
          for (j in (prac_change_tranche_pointer[DMU,i]+2):(length(practices)-2)){
            #if (optn_Bid_Qs[DMU,j]!=0){
              DMU_seq_DIN_incs[j] <- DMU_seq_DIN_incs[j-1] + optn_Bid_Qs[DMU,j]
              DMU_seq_Abate_Cost_incs[j] <- DMU_seq_Abate_Cost_incs[j-1] + optn_Inc_Abate_Cost[DMU,j] 
            #}
          }
        }
      }
      DMU_seq_avg_slope <- DMU_seq_Abate_Cost_incs/DMU_seq_DIN_incs
    }
  }
  
 #Fully convex Total DIN Loss and Total Abatement Cost curves are now given by: 
 # Tot_DIN_loss[DMU,] <-  -1*prac_change_tranche_DIN[DMU,] + Limfjorden_data_in[DMU,BaseLeach]
 # Tot_Abate_Cost <- prac_change_tranche_cost
 # Also, prac_change_tranche_pointer indicates which of the INCREMENTS between the practices will be retained in the convexified bid and offer schedule
 # The nitrogen practice level attained after a practice change step has been taken is given by N_practices[(prac_change_tranche_pointer[DMU,step_i])+1]
  
 #Copied code here modified for Wet Tropics P2R data 
  Tot_DIN_Loss <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=length(optn_Tot_DIN_loss[1,])))
  #names(Tot_DIN_Loss)<- practices
  #DMU<-1
  #practice<-1
  for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
    Tot_DIN_Loss[DMU,1]<-optn_Tot_DIN_loss[DMU,1]
    for (practice in 1:(length(practices)-1)){
      if ((prac_change_tranche_DIN[DMU,practice]==0)&&(Tot_DIN_Loss[DMU,practice]==optn_Tot_DIN_loss[DMU,1])){#no reduction for this DMU
        Tot_DIN_Loss[DMU,(1+practice)] <- optn_Tot_DIN_loss[DMU,1]
      } else {
        if (prac_change_tranche_DIN[DMU,practice]!=0){
          Tot_DIN_Loss[DMU,(1+practice)] <-  optn_Tot_DIN_loss[DMU,1] - prac_change_tranche_DIN[DMU,practice]
        }
      }
    }
  }
  
  #names(Tot_DIN_Loss) <- paste("DIN_Loss_",names(Tot_DIN_Loss),sep="")
  prac_change_tranche_cost_df <- data.frame(prac_change_tranche_cost)
  #names(prac_tranche_cost_df) <- paste("Cost_",names(prac_tranche_cost_df),sep="")
  
  #Check how low DIN can get with the convexified Bid_Qs BEFORE init allocs have been taken into account
  convex_optn_Bid_Qs <- optn_Bid_Qs
  
  min_DIN_convex <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  min_DIN_convex <- (P2R_WT_not_Herbert_data$BasLeach_kgDIN - rowSums(abs(convex_optn_Bid_Qs)))/P2R_WT_not_Herbert_data$Area_ha
  #summary(min_DIN_convex)
  #which(min_DIN_unconvex<0)
  #summary(min_DIN_convex[-5053])
  #min(optn_Bid_Ps[(optn_Bid_Qs<0)])
  
#Write out the convex curves for checking
Convex_Data_Inputs <- cbind(P2R_WT_not_Herbert_data[,c(DMU_no,x,y,Area_ha,BaseLeach)],optn_Tot_DIN_loss,optn_Tot_Abate_Cost,Tot_DIN_Loss,prac_change_tranche_cost_df,prac_change_tranche_pointer)
Abatement_cost_and_area <- cbind(optn_Tot_Abate_Cost,P2R_WT_not_Herbert_data$Area_ha)
setwd(Dir_Out)
write.table(Convex_Data_Inputs,file="Convexified_for_checking_040520.txt",sep="\t",row.names=FALSE,na="NA")
write.table(Abatement_cost_and_area,file="Abatement_Cost_and_Area_040520.txt",sep="\t",row.names=FALSE,na="NA")

  # if(save_output == TRUE){
  #   setwd(Dir_Out)
  #   write.table(optn_Inc_Abate_Cost,file="Limfjorden_Inc_Abate_Cost.txt",sep="\t",row.names=FALSE)
  #   write.table(optn_Bid_Qs,file="Limfjorden_BidQs.txt",sep="\t",row.names=FALSE)
  #   write.table(optn_Bid_Ps,file="Limfjorden_BidPs.txt",sep="\t",row.names=FALSE)
  # }
  
  ###################################### INTRODUCE AN INITIAL PRACTICE ALLOCATION HERE ##########################################
  #
  ###################################### N.B. INITIAL PRACTICE ALLOCATION CAN BE DIFFERENT TO BASELINE PRACTICE #################
  #
  ###################################### Initial Allocation determines the intial allocation of credits #########################
  ###################################### Baseline Practice indicates which level of practice the managment unit has already attained ############
  
  #Now introduce an init_Alloc based on a particular practice level
  
  #init_permit_alloc <- as.integer(strsplit(init_practice,"P")[[1]][2])
  #init_prac_col <- (seq(1:length(names(optn_Bid_Qs)))[names(optn_Bid_Qs) == init_practice])#This gives column position for init_practice
  #init_alloc <- optn_Tot_DIN_loss[,names(optn_Tot_DIN_loss)==paste(init_practice,"_DINLoss",sep="")] #init_alloc holds the initial DIN allowance of each farm at the specific initial practice level
  init_alloc <- rep(0,length(optn_Tot_DIN_loss[,1])) 
  for (farm in 1:length(init_alloc)){
    init_alloc[farm] <- unlist(optn_Tot_DIN_loss[farm,][names(optn_Tot_DIN_loss)==paste(BasePractice[farm],"_DINLoss",sep="")],use.names=FALSE)
  }
  #init_alloc holds the initial DIN allowance of each farm at the specific initial practice level

  #prac_ptr_store <-matrix(99,nrow=length(Limfjorden_data_in[,Farm]),ncol=(length(optn_Tot_DIN_loss[1,])))
  offer_tranche_marker <-matrix(1,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))) #Offer tranche marker is initialised to 1s i.e. to denote bids
  Unused_alloc <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
  starting_position <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID)) #Starting position (in terms of kg DIN emitted) is where the DMU has to start from (on its convexified bid, offer schedule), given its designated Init_Alloc 
  
  #These loops run on the convexified Tot_DIN_Loss and prac_change_tranche_cost dataframes. They produce results for:
  # offer_tranche_marker[DMU,] (where a -1 indicates and 'offer to sell' trance and a +1 indicates a bid to buy tranche)
  # starting_position[DMU] (which identifies the DIN load kg position from which the DMU will start, given its convexified Tot_DIN_Loss vector and the designated init_alloc)
  # Unused_alloc[DMU] (which contains any unused DIN allocation that can be offered for sale on market startup, given the DMU's init_alloc and starting_position in terms of DIN load kg)
  
  #DMU<-1
  for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
    first_offer_marker <- 0
      #cntr <- 2
      #cntr <- 3
      #cntr <- 4
      #cntr <- 5
      for (cntr in 2:length(Tot_DIN_Loss[1,])){
        if ((Tot_DIN_Loss[DMU,cntr]<init_alloc[DMU])&&((Tot_DIN_Loss[DMU,cntr-1]<init_alloc[DMU])||((Tot_DIN_Loss[DMU,cntr-1]-init_alloc[DMU])<Very_Small_Number))){# This identifies 'clear' offer tranches 
          offer_tranche_marker[DMU,cntr-1]<- -1
          if (first_offer_marker==0){
            starting_position[DMU]<-Tot_DIN_Loss[DMU,cntr-1]
            first_offer_marker <- 1
          }
        } else if ((Tot_DIN_Loss[DMU,cntr]<init_alloc[DMU])&&(Tot_DIN_Loss[DMU,cntr-1]>init_alloc[DMU])){# This identifies the tranche which 'straddles' the initial allocation position
          if (Tot_DIN_Loss[DMU,cntr]!= 0){ #checks that the lower DIN end of the tranche that straddles the init alloc position is a feasible position for this DMU. If it isn't then Tot_DIN_Loss[DMU,cntr] will be zero
            if (first_offer_marker==0){
              starting_position[DMU]<-Tot_DIN_Loss[DMU,cntr]
              first_offer_marker <- 1
            }
          #What happens to offer_tranche_marker here ?
            #Leave offer_tranche_marker =1 for a straddling tranche. Change the sign of the 'offer' side of the straddling tranche to 
          } 
        } # The other condition, which occurs when cntr and cntr-1 are both >= init_alloc[DMU], does not require a change to the initialised settings of starting_position,Unused_alloc or offer_tranche_marker. So no need to do anything here 
      }
    Unused_alloc[DMU]<-starting_position[DMU]-init_alloc[DMU]
  }
  
 #These next blocks and loop calculate Inc_DIN_Reduction and Inc_Abatement Cost for steps on the convexified Tot_DIN_loss and prac_change_tranche_DIN vectors for each DMU
  
 optn_Inc_DIN_Reduct <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 optn_Inc_Abate_Cost <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_Abate_Cost[1,]))))
 #names(optn_Inc_DIN_Reduct) <- practices
 #names(optn_Inc_Abate_Cost) <- practices
 optn_Bid_Qs <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 optn_Bid_Ps <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 
 #optn_Inc_DIN_Reduct[,1] <- P2R_WT_not_Herbert_data[,"BasLeach_kgDIN"]-Tot_DIN_Loss[,2]
 for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
  for (cntr in 1:(length(Tot_DIN_Loss[1,])-1)){
    if (Tot_DIN_Loss[DMU,cntr+1]!=0){
      optn_Inc_DIN_Reduct[DMU,cntr] <- Tot_DIN_Loss[DMU,cntr]-Tot_DIN_Loss[DMU,(cntr+1)]
    }
  }
 }
 optn_Bid_Qs <- optn_Inc_DIN_Reduct * offer_tranche_marker
 
 optn_Inc_Abate_Cost[,1] <- prac_change_tranche_cost[,1]
 for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
  for (cntr in 2:length(prac_change_tranche_cost[1,])){
    if (prac_change_tranche_cost[DMU,cntr]!=0){
    optn_Inc_Abate_Cost[DMU,cntr] <- prac_change_tranche_cost[DMU,cntr]-prac_change_tranche_cost[DMU,cntr-1]
    }
  }
 }
 optn_Bid_Ps <- optn_Inc_Abate_Cost/optn_Inc_DIN_Reduct  #Careful with intpretation of negative prices here
 #How to interpret negative prices?
 #For Bids (i.e. when offer_tranche_marker == 1)
 #Negative price => higher cost after nitrogen purchase => farmer would be worse off after purchase => max WTP = $0 => replace negative price with $0
 #
 #For Offers (i.e. when offer tranche marker == -1)
 #Negative price => lower cost after nitrogen sale => farmer would be better off after purchase, and so would move to lower N position voluntarily => min WTA = $0 => replace negative price with $0
 #
 #What about positive prices?
 #For Bids (i.e. when offer_tranche_marker == 1)
 #Positive price => lower cost after N purchase => farmer would be better off after purchase => positive price correctly reflects farmers max WTP => no need to change the price
 #
 #For Offers (i.e. when offer tranche marker == -1)
 #Positive price => higher cost after nitrogen sale => farmer would be worse off after purchase => positive price correctly reflects farmers min WTA => no need to change the price
 
 
 optn_Bid_Ps[is.na(optn_Bid_Ps)] <- 0 #NAs here were from 0/0, so just replace them with 0 here to indicate that the bid/offer price associated with these tranches is $0
 optn_Bid_Ps[(optn_Bid_Ps <0)] <- 0
  #optn_Bid_Ps <- abs(optn_Inc_Abate_Cost/optn_Bid_Qs)
 
 #optn_Bid_Ps <- optn_Inc_Abate_Cost/optn_Bid_Qs
 #optn_Bid_Ps[is.na(optn_Bid_Ps)] <- 0
 
 #summary(optn_Bid_Ps[optn_Bid_Qs!=0])
# which((optn_Bid_Ps[optn_Bid_Qs!=0])==0)
 
 #Check how low DIN can get with the convexified Bid_Qs after init allocs have been taken into account
 
 convex_init_optn_Bid_Qs <- optn_Bid_Qs
 
 min_DIN_convex_init <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
 min_DIN_convex_init <- (P2R_WT_not_Herbert_data$BasLeach_kgDIN - rowSums(abs(convex_init_optn_Bid_Qs)))/P2R_WT_not_Herbert_data$Area_ha
 #summary(min_DIN_convex_init)
 #which(min_DIN_convex_init<0)
 #summary(min_DIN_convex_init[-5053])
 min(optn_Bid_Ps[(optn_Bid_Qs<0)])
 which(optn_Bid_Ps[(optn_Bid_Qs<0)]==0)
 
 # Now adjust Bid and Offer tranches appropriately to allow for unused initial allocation
 # Follow the scheme used by Prabodanie et al 2010 (or maybe 2011 ??)
 # Also identify practices at the start and end of each trade tranche
 
 #For sale of unused initial alloc, ensure that this pricing ($/kg DIN) is sufficient to recover the transaction cost
 unused_alloc_price <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
 unused_alloc_price[Unused_alloc!=0] <- Transaction_cost/(-1*Unused_alloc[Unused_alloc!=0])
 
 first_bid_tranche_P_reduction <- 0.000001 #reduce price of first bid tranche by this amount
 first_bid_ptr <- rep(0,length(P2R_WT_not_Herbert_data$Concat_12_FID))
 
 tranche_start_practice <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 tranche_end_practice <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 tranche_Start_to_End_prac <- data.frame(matrix(0,nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,]))))
 
 tranche_start_practice[,2:length(tranche_start_practice)] <- prac_change_tranche_pointer+1 #start practice for each tranche #Start practice sequence = c("Df","Cp","Cf","Bp","Bf","Af")
 tranche_start_practice[,1] <- 1 #start practice for the very first tranche will always be Df 
 
 tranche_end_practice[,1:(length(tranche_start_practice)-1)] <- prac_change_tranche_pointer+1 #prac_change_tranche_pointer already holds the 'end' practice of each tranche #End practice sequence = c("Df","Cp","Cf","Bp","Bf","Af")
 
 
for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
   #DMU <- 2
    straddling <- 0
    cntr <- 2
    while ((straddling == 0) && (cntr <= length(prac_change_tranche_pointer[1,]))){
        if (((Tot_DIN_Loss[DMU,cntr]<init_alloc[DMU])||(Tot_DIN_Loss[DMU,cntr]-init_alloc[DMU]<Very_Small_Number))&&(Tot_DIN_Loss[DMU,cntr-1]>init_alloc[DMU])){# This identifies the (convexified) tranche that straddles init alloc
         first_bid_ptr[DMU]<-cntr-1
         straddling <- 1
        } else { 
          cntr <- cntr+1
        } 
    }
    if ((Tot_DIN_Loss[DMU,cntr]!=0) && (straddling ==1 && ((init_alloc[DMU] - Tot_DIN_Loss[DMU,cntr])>Very_Small_Number))){# This tranche is genuinely 'straddling'
      if (((starting_position[DMU]>init_alloc[DMU]))||((starting_position[DMU]<init_alloc[DMU]) && EarlyAdopter_sell_unused_init)){ #Starting position leaching sits above init_alloc OR stating position is below init_alloc and sell_unused_init == TRUE
        if (first_bid_ptr[DMU]+2 <= length(optn_Bid_Qs[1,])){
          optn_Bid_Qs[DMU,((first_bid_ptr[DMU]+2):(length(optn_Bid_Qs[1,])))] <- optn_Bid_Qs[DMU,((first_bid_ptr[DMU]+1):(length(optn_Bid_Qs[1,])-1))] #shift existing offer Q columns one place to the right
          optn_Bid_Ps[DMU,((first_bid_ptr[DMU]+2):(length(optn_Bid_Ps[1,])))] <- optn_Bid_Ps[DMU,((first_bid_ptr[DMU]+1):(length(optn_Bid_Ps[1,])-1))] #shift existing offer P columns one place to the right
          tranche_start_practice[DMU,((first_bid_ptr[DMU]+2):(length(optn_Bid_Ps[1,])))] <- tranche_start_practice[DMU,((first_bid_ptr[DMU]+1):(length(optn_Bid_Ps[1,])-1))] #shift existing start_practice columns one place to the right
          tranche_end_practice[DMU,((first_bid_ptr[DMU]+1):(length(optn_Bid_Ps[1,])))] <- tranche_end_practice[DMU,(first_bid_ptr[DMU]:(length(optn_Bid_Ps[1,])-1))] #shift existing offer P columns one place to the right
          
          optn_Bid_Qs[DMU,(first_bid_ptr[DMU]+1)] <- Unused_alloc[DMU] #first offer is to sell unused initial allocation [this will automatically produce a negative Q for this tranche]
          optn_Bid_Ps[DMU,(first_bid_ptr[DMU]+1)] <- unused_alloc_price[DMU]  #pricing here recovers transaction cost
          tranche_start_practice[DMU,(first_bid_ptr[DMU]+1)] <- seq(1:length(N_practices))[init_practice == N_practices]  #unused alloc tranche 'starts' at init_practice
          tranche_end_practice[DMU,first_bid_ptr[DMU]] <- seq(1:length(N_practices))[init_practice == N_practices]  #buyback tranche from init_practice ends at init_practice
          
          optn_Bid_Qs[DMU,first_bid_ptr[DMU]] <- optn_Bid_Qs[DMU,first_bid_ptr[DMU]] + Unused_alloc[DMU]
          optn_Bid_Ps[DMU,first_bid_ptr[DMU]] <- optn_Bid_Ps[DMU,first_bid_ptr[DMU]] - first_bid_tranche_P_reduction
      } 
     } #There's an implicit 'else' here for DMUs for which starting_position == init_alloc. The bid offer schedule for these DMUs should be ok as it is, so no need to do anything with these ones
    }
 }
 
 tranche_start_practice_char <- matrix("0",nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])))
 tranche_end_practice_char <- matrix("0",nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])))
 tranche_Start_to_End_practice_char <- matrix("0",nrow=length(P2R_WT_not_Herbert_data$Concat_12_FID),ncol=(length(optn_Tot_DIN_loss[1,])))
 
 
 for (DMU in 1:length(P2R_WT_not_Herbert_data$Concat_12_FID)){
   
   tranche_start_practice[DMU,seq(1:6)[prac_change_tranche_pointer[DMU,]==0]]<-0
   tranche_start_practice[DMU,(length(prac_change_tranche_pointer[DMU,])+1)]<-0
   tranche_end_practice[DMU,seq(1:6)[prac_change_tranche_pointer[DMU,]==0]]<-0
   
   tranche_start_practice_char[DMU,(tranche_start_practice[DMU,]!=0)] <- N_practices[unlist(tranche_start_practice[DMU,(seq(1:length(tranche_start_practice[1,]))[tranche_start_practice[DMU,]!=0])], use.names = FALSE)]
   tranche_end_practice_char[DMU,(tranche_end_practice[DMU,]!=0)] <- N_practices[unlist(tranche_end_practice[DMU,(seq(1:length(tranche_end_practice[1,]))[tranche_end_practice[DMU,]!=0])], use.names = FALSE)]
   
   i <- 1
   while ((unlist(tranche_start_practice_char[DMU,i],use.names=FALSE)!="0")&&(i < length(tranche_Start_to_End_prac[1,]))) {
      tranche_Start_to_End_practice_char[DMU,i] <- paste(unlist(tranche_start_practice_char[DMU,i],use.names=FALSE),unlist(tranche_end_practice_char[DMU,i],use.names=FALSE),sep="")
      i <- i+1
   }
}
 

 #Check how low DIN can get with the convexified Bid_Qs after init allocs have been taken into account
 convex_init_optn_Bid_Qs <- optn_Bid_Qs
 
 #min_DIN_convex_init <- rep(0,length(Limfjorden_data_in$Farm))
 #min_DIN_convex_init <- (Limfjorden_data_in$BasLeach_kgDIN - rowSums(abs(convex_init_optn_Bid_Qs)))/Limfjorden_data_in$TotArea_ha
 #summary(min_DIN_convex_init)
 #which(min_DIN_convex_init<0)
 #length(which(min_DIN_convex_init<0))
 
 # Now pack up the fully revised Bid Quantities and Bid Prices for use in the smart market optimiser
 optn_Bid_Qs_df <- as.data.frame(optn_Bid_Qs)
 names(optn_Bid_Qs_df) <- paste(names(optn_Bid_Qs_df),"_buy",sep="")
 optn_Bid_Ps_df <- as.data.frame(optn_Bid_Ps)
 names(optn_Bid_Ps_df) <- paste(names(optn_Bid_Ps_df),"_p",sep="")
 init_alloc_df <- data.frame(init_alloc)
 Unused_alloc_df <- data.frame(Unused_alloc)
 starting_position_df <- data.frame(starting_position)
 tranche_start_practice_df <- as.data.frame(tranche_start_practice)
 names(tranche_start_practice_df)<- paste("Start",as.character(seq(1:6)),sep="")
 tranche_end_practice_df <- as.data.frame(tranche_end_practice)
 names(tranche_end_practice_df)<- paste("End",as.character(seq(1:6)),sep="")
 tranche_Start_to_End_practice_char_df <- as.data.frame(tranche_Start_to_End_practice_char)
 names(tranche_Start_to_End_practice_char_df)<- paste("Start_End",as.character(seq(1:6)),sep="")
 
 # optn_Bid_Qs_df <- as.data.frame(recon_Inc_DIN_Reduct)
 # names(optn_Bid_Qs_df) <- paste(names(optn_Bid_Qs_df),"_buy",sep="")
 # optn_Bid_Ps_df <- as.data.frame(recon_Inc_Abate_Cost)
 # names(optn_Bid_Ps_df) <- paste(names(optn_Bid_Ps_df),"_p",sep="")
 # init_alloc_df <- data.frame(init_alloc)
 # Unused_alloc_df <- data.frame(Unused_alloc)
 # starting_position_df <- data.frame(starting_position)
 
    
  #Add N_transport and package up ready for optimiser
  #Tranches_Transport_recon <- cbind(Limfjorden_data_in[,c(Farm,x,y,Area,Transport,BaseLeach)],optn_Bid_Qs_df,optn_Bid_Ps_df,init_alloc_df,Unused_alloc_df)
  #Tranches_Transport_sell_unused_init <- cbind(Limfjorden_data_in[,c(Farm,x,y,Area,Transport,BaseLeach,SubCatch,CropType,FarmType,Irrigated,Manured,SoilType,Subcatchment_factor)],optn_Bid_Qs_df,optn_Bid_Ps_df)
  Tranches_Transport_sell_unused_init <- cbind(P2R_WT_not_Herbert_data[,c(DMU_no,x,y,Area_ha,Transport,Main_Catch,Sub_Catch,Soil_Type,Permeability,Farm_Type,Base_Pract)],optn_Bid_Qs_df,optn_Bid_Ps_df)
  
  
  
  # if(save_output == TRUE){
  #   setwd(Dir_Out)
  #   write.table(Tranches_Transport_sell_unused_init,file="P2R_Wet_Tropics_not_Herbert_Tranches_Transport_19092019_360_DMUs_Init_Prac_Cp.txt",sep="\t",row.names=FALSE,na="NA")
  #   #write.table(starting_position, file="starting_position_200_Farms.txt",sep="\t",row.names=FALSE)
  # }
  # 
  
  
  #Code added here to plot out demand and supply curves for permits in the trading market
  
  #For_Demand_and_Supply_Curves <- cbind(P2R_WT_not_Herbert_data[,c(DMU_no,Main_Catch,Soil_Type,Permeability,Base_Pract,x,y,Area_ha,Transport,Farm_Type)],optn_Bid_Qs_df,optn_Bid_Ps_df)
  For_Demand_and_Supply_Curves <- cbind(P2R_WT_not_Herbert_data[,c(DMU_no,Main_Catch,Soil_Type,Permeability,Base_Pract,x,y,Area_ha,Transport,Farm_Type)],optn_Bid_Qs_df,optn_Bid_Ps_df,tranche_Start_to_End_practice_char_df)
  For_D_and_S_Curves_Names <- names(For_Demand_and_Supply_Curves)
  
  #For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Daintree",]
  # For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Mossman",]
  # For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Barron",]
  # For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Mulgrave-Russell",]
  #first_bid_ptr <- first_bid_ptr[For_Demand_and_Supply_Curves$Basin_Clip==Focus_Catchment]
  #For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip==Focus_Catchment,]
  # For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Tully",]
  # For_Demand_and_Supply_Curves <-  For_Demand_and_Supply_Curves[For_Demand_and_Supply_Curves$Basin_Clip=="Murray",]
  
  For_D_and_S_num_copy <- For_Demand_and_Supply_Curves
  For_D_and_S_char_copy <- For_Demand_and_Supply_Curves
  

  #Start by working on bids to buy DIN. This will produce a demand curve for DIN for the catchment concerned
  #Here construct a long-format list of the individual (quantity,price) bids to buy DIN permits from each of the farms
  #Column layout here is: Farm_ID, X_Coord, Y_Coord, SubCatchment_factor, Bid_Q_farm, Bid_Q_receptor, Bid_P_farm etc.
  #So there are 13 columns in all
  Bid_List_Col_Names <- c("DMU_no","Basin_Clip","Soil","Perm","BasePract","X_coord","Y_coord","All_Avg_Transport","FarmType","Bid_Q_farm","Bid_Q_receptor","Bid_P_farm","Start_end_pract")
  Bid_List <- data.frame(matrix(0,nrow=(length(For_Demand_and_Supply_Curves$Concat_12_FID)*length(practices)),ncol=(length(Bid_List_Col_Names))))
  names(Bid_List)<- Bid_List_Col_Names
  Offer_List <- data.frame(matrix(0,nrow=(length(For_Demand_and_Supply_Curves$Concat_12_FID)*length(practices)),ncol=(length(Bid_List_Col_Names))))
  names(Offer_List)<- Bid_List_Col_Names #Offer list has exactly the same columns as Bid_List
  
  bid_col_names <- names(Bid_List)
  #Column numbers for char data to be stored in DMU_char_info
  bid_char_DMU <-1
  bid_char_MainCatch <-2
  bid_char_Soil <- 3
  bid_char_Perm <- 4
  bid_char_BasePract <- 5
  #Column numbers for num data to be stored in DMU_num_info
  bid_num_x <- 1
  bid_num_y <- 2
  bid_num_Transport <- 3
  bid_num_FarmType <- 4
  
  bid_Bid_Q_farm <- (seq(1:length(bid_col_names))[bid_col_names =="Bid_Q_farm"])
  bid_Bid_Q_receptor <- (seq(1:length(bid_col_names))[bid_col_names =="Bid_Q_receptor"])
  bid_Bid_P_farm <- (seq(1:length(bid_col_names))[bid_col_names =="Bid_P_farm"])
  bid_Start_End_prac <- (seq(1:length(bid_col_names))[bid_col_names =="Start_end_pract"])
  demand_and_supply_first_bid_Q_col <- (seq(1:length(names(For_Demand_and_Supply_Curves)))[names(For_Demand_and_Supply_Curves) =="X1_buy"])
  demand_and_supply_first_bid_P_col <- (seq(1:length(names(For_Demand_and_Supply_Curves)))[names(For_Demand_and_Supply_Curves) =="X1_p"])
  demand_and_supply_first_start_end_prac_col <- (seq(1:length(names(For_Demand_and_Supply_Curves)))[names(For_Demand_and_Supply_Curves) =="Start_End1"])
  
  
  DMU_num_info <- rep(0,4) #Stores x, y, All_Avg_Transport, FarmType
  DMU_char_info <- rep("0",5) #Stores DMU_no, Basin_Clip, Soil, Perm, BasePract
  #library(magrittr) #need 'pipe' command shortly
  #library(hablar)#easy conversion from factors to chars for relevant f For_D_and_S_char_copy
  #convert relevant columns of For_D_and_S_char_copy from factors to chars 
  factor_cols <- sapply(For_D_and_S_char_copy, is.factor)
  For_D_and_S_char_copy[factor_cols] <- lapply(For_D_and_S_char_copy[factor_cols], as.character)
  
  
  bid_row <- 1 # initialise bid_row pointer
  for (farm in 1:length(For_Demand_and_Supply_Curves$Concat_12_FID)){
    if(first_bid_ptr[farm]>0){ #there are some bids to buy from this farm
      #Log basic info for this farm
      #Extract the info from the relevant data frame, using the column names directly
      DMU_char_info[bid_char_DMU]<- For_D_and_S_char_copy$Concat_12_FID[farm]
      DMU_num_info[bid_num_x]<- For_Demand_and_Supply_Curves$POINT_X[farm]
      DMU_num_info[bid_num_y]<- For_Demand_and_Supply_Curves$POINT_Y[farm]
      DMU_char_info[bid_char_MainCatch]<- For_D_and_S_char_copy$Basin_Clip[farm]
      DMU_num_info[bid_num_Transport]<- For_Demand_and_Supply_Curves$All_Avg_Transport[farm]
      DMU_char_info[bid_char_Soil]<- For_D_and_S_char_copy$APSIMSoil[farm]
      DMU_char_info[bid_char_Perm]<- For_D_and_S_char_copy$APSIMPerm[farm]
      DMU_char_info[bid_char_BasePract]<- For_D_and_S_char_copy$BasPractice[farm]
      DMU_num_info[bid_num_FarmType]<- For_Demand_and_Supply_Curves$FarmType[farm]
      
      bid <- 1 #log the first bid from this farm
      while(bid <= first_bid_ptr[farm]){
        Bid_List[bid_row,(1:length(DMU_char_info))]<- DMU_char_info
        Bid_List[bid_row,((length(DMU_char_info)+1):(length(DMU_char_info)+length(DMU_num_info)))]<- DMU_num_info
        Bid_List[bid_row,bid_Bid_Q_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+(bid-1)]
        Bid_List[bid_row,bid_Bid_Q_receptor]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+(bid-1)]*Bid_List[bid_row,(length(DMU_char_info)+bid_num_Transport)]
        Bid_List[bid_row,bid_Bid_P_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_P_col+(bid-1)]
        Bid_List[bid_row,bid_Start_End_prac]<- unlist(For_D_and_S_char_copy[farm,demand_and_supply_first_start_end_prac_col+(bid-1)],use.names=FALSE)
        bid<-bid+1
        bid_row <- bid_row+1
      }
    }
  }
  Bid_List <- Bid_List[Bid_List$DMU_no!=0,]
  if (length(Bid_List$DMU_no)>0){
    Bid_List$Bid_P_receptor <- Bid_List$Bid_P_farm/Bid_List$All_Avg_Transport
    #Bid_List$Subcatchment_factor <- factor(Bid_List$Subcatchment_Line,levels=c("156","157","158"),labels = c("Northern Limfjorden","Skive_Lovns_Risgaarde","Hjarbaek"))
    Bids_decreasing_index <- order(Bid_List$Bid_P_receptor,decreasing=TRUE)
    Bids_decreasing_order <- Bid_List[Bids_decreasing_index,]
    cumulative_Bid_Q_receptor <- rep(0,length(Bid_List$DMU_no))
    
    for(i in 2:length(Bid_List$DMU_no)){
      cumulative_Bid_Q_receptor [i] <- cumulative_Bid_Q_receptor [i-1] + Bids_decreasing_order$Bid_Q_receptor[i-1]
    }
    Bids_decreasing_order <- cbind(Bids_decreasing_order,cumulative_Bid_Q_receptor)
  }
  
  
  #Now work on offers to supply credits. This will produce the supply curve for the catchment concerned
  DMU_num_info <- rep(0,4) #Stores x, y, All_Avg_Transport, FarmType
  DMU_char_info <- rep("0",5) #Stores DMU_no, Basin_Clip, Soil, Perm, BasePract
  
  offer_row <- 1 # initialise offer row pointer
  for (farm in 1:length(For_Demand_and_Supply_Curves$Concat_12_FID)){
      #Log basic info for this farm
      DMU_char_info[bid_char_DMU]<- For_D_and_S_char_copy$Concat_12_FID[farm]
      DMU_num_info[bid_num_x]<- For_Demand_and_Supply_Curves$POINT_X[farm]
      DMU_num_info[bid_num_y]<- For_Demand_and_Supply_Curves$POINT_Y[farm]
      DMU_char_info[bid_char_MainCatch]<- For_D_and_S_char_copy$Basin_Clip[farm]
      DMU_num_info[bid_num_Transport]<- For_Demand_and_Supply_Curves$All_Avg_Transport[farm]
      DMU_char_info[bid_char_Soil]<- For_D_and_S_char_copy$APSIMSoil[farm]
      DMU_char_info[bid_char_Perm]<- For_D_and_S_char_copy$APSIMPerm[farm]
      DMU_char_info[bid_char_BasePract]<- For_D_and_S_char_copy$BasPractice[farm]
      DMU_num_info[bid_num_FarmType]<- For_Demand_and_Supply_Curves$FarmType[farm]
      
      if (first_bid_ptr[farm]==0){#there are no bids from this farm
        offer <- 1 #log the first offer from this farm
       while(For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+(offer-1)] < 0){ # there are still offers from this farm
          Offer_List[offer_row,(1:length(DMU_char_info))]<- DMU_char_info
          Offer_List[offer_row,((length(DMU_char_info)+1):(length(DMU_char_info)+length(DMU_num_info)))]<- DMU_num_info
          Offer_List[offer_row,bid_Bid_Q_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+(offer-1)]*-1
          Offer_List[offer_row,bid_Bid_Q_receptor]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+(offer-1)]*Offer_List[offer_row,(length(DMU_char_info)+bid_num_Transport)]*-1
          Offer_List[offer_row,bid_Bid_P_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_P_col+(offer-1)]
          Offer_List[offer_row,bid_Start_End_prac]<- unlist(For_D_and_S_char_copy[farm,demand_and_supply_first_start_end_prac_col+(offer-1)],use.names=FALSE)
          offer <- offer+1
          offer_row <- offer_row+1
       }
      } else { #This farm had bids as well as offers, so the first offer is in position (first_bid_ptr[farm]+1)
        offer <- 1 #log the first offer from this farm
        while(For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+first_bid_ptr[farm]+(offer-1)] < 0){ # there are still offers from this farm
          Offer_List[offer_row,(1:length(DMU_char_info))]<- DMU_char_info
          Offer_List[offer_row,((length(DMU_char_info)+1):(length(DMU_char_info)+length(DMU_num_info)))]<- DMU_num_info
          Offer_List[offer_row,bid_Bid_Q_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+first_bid_ptr[farm]+(offer-1)]*-1
          Offer_List[offer_row,bid_Bid_Q_receptor]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_Q_col+first_bid_ptr[farm]+(offer-1)]*Offer_List[offer_row,(length(DMU_char_info)+bid_num_Transport)]*-1
          Offer_List[offer_row,bid_Bid_P_farm]<- For_Demand_and_Supply_Curves[farm,demand_and_supply_first_bid_P_col+first_bid_ptr[farm]+(offer-1)]
          Offer_List[offer_row,bid_Start_End_prac]<- unlist(For_D_and_S_char_copy[farm,demand_and_supply_first_start_end_prac_col+first_bid_ptr[farm]+(offer-1)],use.names=FALSE)
          offer<-offer+1
          offer_row <- offer_row+1
        } 
      }
  }
  Offer_List <- Offer_List[Offer_List$Farm!=0,]
  Offer_List$Bid_P_receptor <- Offer_List$Bid_P_farm/Offer_List$All_Avg_Transport
  #Offer_List$Subcatchment_factor <- factor(Offer_List$Subcatchment_Line,levels=c("156","157","158"),labels = c("Northern Limfjorden","Skive_Lovns_Risgaarde","Hjarbaek"))
  Offers_increasing_index <- order(Offer_List$Bid_P_receptor,decreasing=FALSE)
  Offers_increasing_order <- Offer_List[Offers_increasing_index,]
  cumulative_Offer_Q_receptor <- rep(0,length(Offer_List$Farm))
  for(i in 2:length(Offer_List$Farm)){
    cumulative_Offer_Q_receptor[i] <- cumulative_Offer_Q_receptor[i-1] + Offers_increasing_order$Bid_Q_receptor[i-1]
  }
  
  Offers_increasing_order <- cbind(Offers_increasing_order,cumulative_Offer_Q_receptor)
  
  if (loop == 1){
    Offers_increasing_order_1 <- Offers_increasing_order
  } else if (loop == 2){
    Offers_increasing_order_2 <- Offers_increasing_order
  } else if (loop == 3){
    Offers_increasing_order_3 <- Offers_increasing_order
  } else if (loop == 4){
    Offers_increasing_order_4 <- Offers_increasing_order
  } else if (loop == 5){
    Offers_increasing_order_5 <- Offers_increasing_order
  } else if (loop == 5){
    Offers_increasing_order_5 <- Offers_increasing_order
  } else if (loop == 6){
    Offers_increasing_order_6 <- Offers_increasing_order
  } else if (loop == 7){
    Offers_increasing_order_7 <- Offers_increasing_order
  } else if (loop == 8){
    Offers_increasing_order_8 <- Offers_increasing_order
  } else if (loop == 9){
    Offers_increasing_order_9 <- Offers_increasing_order
  } else if (loop == 10){
    Offers_increasing_order_10 <- Offers_increasing_order
  } else if (loop == 11){
    Offers_increasing_order_11 <- Offers_increasing_order
  } else if (loop == 12){
    Offers_increasing_order_12 <- Offers_increasing_order
  } else if (loop == 13){
    Offers_increasing_order_13 <- Offers_increasing_order
  } else if (loop == 14){
    Offers_increasing_order_14 <- Offers_increasing_order
  } else if (loop == 15){
    Offers_increasing_order_15 <- Offers_increasing_order
  } else if (loop == 16){
    Offers_increasing_order_16 <- Offers_increasing_order
  } else if (loop == 17){
    Offers_increasing_order_17 <- Offers_increasing_order
  } else if (loop == 18){
    Offers_increasing_order_18 <- Offers_increasing_order
  } else if (loop == 19){
    Offers_increasing_order_19 <- Offers_increasing_order
  } else if (loop == 20){
    Offers_increasing_order_20 <- Offers_increasing_order
  } else if (loop == 21){
    Offers_increasing_order_21 <- Offers_increasing_order
  } else if (loop == 22){
    Offers_increasing_order_22 <- Offers_increasing_order
  } else if (loop == 23){
    Offers_increasing_order_23 <- Offers_increasing_order
  } else if (loop == 24){
    Offers_increasing_order_24 <- Offers_increasing_order
  } else if (loop == 25){
    Offers_increasing_order_25 <- Offers_increasing_order
  } else if (loop == 26){
    Offers_increasing_order_26 <- Offers_increasing_order
  } else if (loop == 27){
    Offers_increasing_order_27 <- Offers_increasing_order
  } else if (loop == 28){
    Offers_increasing_order_28 <- Offers_increasing_order
  } else if (loop == 29){
    Offers_increasing_order_29 <- Offers_increasing_order
  } else if (loop == 30){
    Offers_increasing_order_30 <- Offers_increasing_order
  } else if (loop == 31){
    Offers_increasing_order_31 <- Offers_increasing_order
  } else if (loop == 32){
    Offers_increasing_order_32 <- Offers_increasing_order
  } else if (loop == 33){
    Offers_increasing_order_33 <- Offers_increasing_order
  } else if (loop == 34){
    Offers_increasing_order_34 <- Offers_increasing_order
  } else if (loop == 35){
    Offers_increasing_order_35 <- Offers_increasing_order
  } else if (loop == 36){
    Offers_increasing_order_36 <- Offers_increasing_order
  } else if (loop == 37){
    Offers_increasing_order_37 <- Offers_increasing_order
  } else if (loop == 38){
    Offers_increasing_order_38 <- Offers_increasing_order
  } else if (loop == 39){
    Offers_increasing_order_39 <- Offers_increasing_order
  } else if (loop == 40){
    Offers_increasing_order_40 <- Offers_increasing_order
  } else if (loop == 41){
    Offers_increasing_order_41 <- Offers_increasing_order
  } else if (loop == 42){
    Offers_increasing_order_42 <- Offers_increasing_order
  } else if (loop == 43){
    Offers_increasing_order_43 <- Offers_increasing_order
  } else if (loop == 44){
    Offers_increasing_order_44 <- Offers_increasing_order
  } else if (loop == 45){
    Offers_increasing_order_45 <- Offers_increasing_order
  } else if (loop == 46){
    Offers_increasing_order_46 <- Offers_increasing_order
  } else if (loop == 47){
    Offers_increasing_order_47 <- Offers_increasing_order
  } else if (loop == 48){
    Offers_increasing_order_48 <- Offers_increasing_order
  } else if (loop == 49){
    Offers_increasing_order_49 <- Offers_increasing_order
  } else if (loop == 50){
    Offers_increasing_order_50 <- Offers_increasing_order
  }
  
  
  # write.table(Offers_increasing_order,file=paste("Offers_",Focus_Catchment,"_101220.txt",sep =""),sep="\t",row.names=FALSE,na="NA")
  # write.table(Offers_increasing_order_7,file=paste("Offers_7_",Focus_Catchment,"_111220.txt",sep =""),sep="\t",row.names=FALSE,na="NA")
  # write.table(DMU_Fertiliser_N_Applied_df,file="DMU_Fertiliser_N_Applied_051220.txt",sep="\t",row.names=FALSE,na="NA")
  # write.table(Ratio_N_Applied_to_DIN_at_End_of_Catchment_df,file="Ratio_N_Applied_to_DIN_at_End_of_Catchment_051220.txt",sep="\t",row.names=FALSE,na="NA")
  # write.table(Ratio_N_Applied_to_DIN_from_paddock_df,file="Ratio_N_Applied_to_DIN_from_paddock_051220.txt",sep="\t",row.names=FALSE,na="NA")
  # 
  # P2R_WT_not_Herbert_data <- merge(P2R_WT_not_Herbert_data,DMU_DINLoss_df, by.x="Concat_12_FID", by.y="Concat_12_FID")
  # 
  # jim_test <- strsplit(Offers_increasing_order$Start_end_pract,split="")
  # Start_Prac <- rep("0",length(Offers_increasing_order$DMU_no))
  # End_Prac <- rep("0",length(Offers_increasing_order$DMU_no))
  # Start_N <- rep("0",length(Offers_increasing_order$DMU_no))
  # End_N <- rep("0",length(Offers_increasing_order$DMU_no))
  # for (i in 1:length(Offers_increasing_order$DMU_no)){
  #   Start_Prac[i] <- paste(jim_test[[i]][1],jim_test[[i]][2],sep="")
  #   End_Prac[i] <- paste(jim_test[[i]][3],jim_test[[i]][4],sep="")
  #   Start_N
  # }
  
  # library(pastecs)
  # fert_check <- data.frame(BpFert_ha = (DMU_Fertiliser_N_Applied_df$Bp_Fert_N_Applied_kg/P2R_WT_all_data_in$Area_ha),BfFert_ha = P2R_WT_all_data_in$Bf_fert_applic_avg_over_cycle)
  # fert_check$Fert_save_ha = fert_check$BpFert_ha-fert_check$BfFert_ha
  # fert_check$FertCost_save_ha = fert_check$Fert_save_ha*Avg_N_price
  # fert_check$CaneFW_redn_BptoBf_ha = P2R_WT_all_data_in$BpBpBp_Avg_FWper_ha - P2R_WT_all_data_in$BpBfBf_Avg_FWper_ha
  # fert_check$OppCost_BptoBf_ha = fert_check$CaneFW_redn_BptoBf_ha *(Avg_Cane_price-Harvesting_cost) - fert_check$FertCost_save_ha
  #fert_check
  
  #   setwd(Dir_Out)
  # if(Include_CVaR_cost){
  #   write.table(Offers_increasing_order ,file=paste("Offers_",Focus_Catchment,"_init_",init_practice,".txt",sep=""),sep="\t",row.names=FALSE,na="NA")
  #   write.table(Bids_decreasing_order ,file=paste("Bids_",Focus_Catchment,"_init_",init_practice,".txt",sep=""),sep="\t",row.names=FALSE,na="NA")
  # } else if(!Include_CVaR_cost){
  #   write.table(Offers_increasing_order ,file=paste("Offers_",Focus_Catchment,"_init_",init_practice,"_no_CVaR.txt",sep=""),sep="\t",row.names=FALSE,na="NA")
  #   write.table(Bids_decreasing_order ,file=paste("Bids_",Focus_Catchment,"_init_",init_practice,"_no_CVaR.txt",sep=""),sep="\t",row.names=FALSE,na="NA")
  # }
 
 # Total_Receptor_Load_at_Df <- sum(P2R_WT_not_Herbert_data$Df_DINLoss * P2R_WT_not_Herbert_data$BpDfDf_Avg_Transport)
 # Total_Receptor_Load_at_Af <- sum(P2R_WT_not_Herbert_data$Af_DINLoss * P2R_WT_not_Herbert_data$BpAfAf_Avg_Transport)
 # Total_Cane_Area_modelled <- sum(P2R_WT_not_Herbert_data$Area_ha)
  
  
}#end of for loop   
  
################################# Plotting out supply curves for permit sales at the receptor - below here ############################
  
###################  Plot Supply curve with randomised spread as a background

# Johnstone plot

setwd(Dir_Out)
old_par <- par()
png(paste(Focus_Catchment,"_50_randomised_DIN_supply_curve_with_target",".png",sep=""), width=579,height=560, unit="mm", res=600) #This plots to a file instead of the screen
par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

plot(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,
     xlab="DIN load reduction at EoC (tonnes)",ylab="Supply (AUD$/kg DIN EoC)",main = paste("DIN Credit Supply: ",Focus_Catchment,sep=""),
     type="n",lwd=1,col="blue",ylim=c(-5,300),xlim=c(0,450),xaxs = "i",yaxs = "i",cex = 0.9)
points(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_3$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_3$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_4$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_4$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_5$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_5$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_6$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_6$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_7$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_7$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_8$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_8$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_9$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_9$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_11$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_11$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_12$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_12$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_13$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_13$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_14$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_14$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_15$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_15$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_17$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_17$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_18$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_18$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_19$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_19$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_20$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_20$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_21$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_21$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_22$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_22$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_23$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_23$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_24$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_24$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_25$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_25$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_26$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_26$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_27$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_27$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_28$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_28$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_29$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_29$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_30$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_30$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_31$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_31$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_32$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_32$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_33$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_33$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_34$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_34$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_35$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_35$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_36$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_36$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_37$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_37$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_38$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_38$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_39$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_39$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_40$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_40$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_41$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_41$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_42$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_42$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_43$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_43$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_44$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_44$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_46$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_46$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_47$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_47$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_48$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_48$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_49$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_49$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_50$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_50$Bid_P_receptor,pch=16,col="grey")
lines(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,lwd=2,col="blue",lty=1)
abline(a=60.28,b=0,lwd=2,col="black",lty=2)
points(x=292.54,y=60.28,pch=16,col="blue",cex=1.5)
segments(x0=292.54,y0=-5,y1=60.28,col="black",lty=3)
text(x=100,y=70,"60 AUD$/kg DIN EoC")
abline(v=350,lwd=2,col="red",lty=3)
mtext("350",at=350,side=1,col="red")
mtext("293",at=290,side=1,line=0.15,col="black")

par(old_par) 
dev.off() # This sets plotting back to the screen

#Mulgrave-Russell plot

setwd(Dir_Out)
old_par <- par()
png(paste(Focus_Catchment,"_50_randomised_DIN_supply_curve_with_target",".png",sep=""), width=579,height=560, unit="mm", res=600) #This plots to a file instead of the screen
par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

plot(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,
     xlab="DIN load reduction at EoC (tonnes)",ylab="Supply (AUD$/kg DIN EoC)",main = paste("DIN Credit Supply: ",Focus_Catchment,sep=""),
     type="n",lwd=1,col="blue",ylim=c(-5,300),xlim=c(0,450),xaxs = "i",yaxs = "i",cex = 0.9)
points(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_3$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_3$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_4$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_4$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_5$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_5$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_6$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_6$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_7$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_7$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_8$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_8$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_9$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_9$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_11$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_11$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_12$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_12$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_13$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_13$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_14$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_14$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_15$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_15$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_17$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_17$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_18$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_18$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_19$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_19$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_20$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_20$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_21$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_21$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_22$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_22$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_23$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_23$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_24$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_24$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_25$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_25$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_26$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_26$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_27$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_27$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_28$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_28$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_29$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_29$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_30$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_30$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_31$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_31$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_32$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_32$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_33$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_33$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_34$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_34$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_35$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_35$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_36$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_36$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_37$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_37$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_38$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_38$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_39$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_39$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_40$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_40$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_41$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_41$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_42$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_42$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_43$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_43$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_44$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_44$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_46$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_46$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_47$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_47$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_48$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_48$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_49$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_49$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_50$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_50$Bid_P_receptor,pch=16,col="grey")
lines(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,lwd=2,col="blue",lty=1)
abline(a=53.8,b=0,lwd=2,col="black",lty=2)
points(x=270.2,y=53.8,pch=16,col="blue",cex=1.5)
segments(x0=270.2,y0=-5,y1=53.8,col="black",lty=3)
text(x=100,y=64,"54 AUD$/kg DIN EoC")
abline(v=300,lwd=2,col="red",lty=3)
mtext("300",at=300,side=1,line=1,col="red")
mtext("270",at=270,side=1,line=0,col="black")

par(old_par) 
dev.off() # This sets plotting back to the screen


#Tully plot

setwd(Dir_Out)
old_par <- par()
png(paste(Focus_Catchment,"_randomised_DIN_supply_curve_with_target",".png",sep=""), width=579,height=560, unit="mm", res=600) #This plots to a file instead of the screen
par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

plot(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,
     xlab="DIN load reduction at EoC (tonnes)",ylab="Supply (AUD$/kg DIN EoC)",main = paste("DIN Credit Supply: ",Focus_Catchment,sep=""),
     type="n",lwd=1,col="blue",ylim=c(-5,300),xlim=c(0,225),xaxs = "i",yaxs = "i",cex = 0.9)
points(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_3$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_3$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_4$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_4$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_5$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_5$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_6$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_6$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_7$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_7$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_8$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_8$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_9$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_9$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_11$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_11$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_12$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_12$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_13$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_13$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_14$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_14$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_15$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_15$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_17$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_17$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_18$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_18$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_19$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_19$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_20$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_20$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_21$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_21$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_22$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_22$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_23$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_23$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_24$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_24$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_25$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_25$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_26$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_26$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_27$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_27$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_28$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_28$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_29$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_29$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_30$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_30$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_31$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_31$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_32$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_32$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_33$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_33$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_34$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_34$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_35$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_35$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_36$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_36$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_37$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_37$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_38$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_38$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_39$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_39$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_40$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_40$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_41$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_41$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_42$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_42$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_43$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_43$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_44$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_44$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_46$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_46$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_47$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_47$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_48$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_48$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_49$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_49$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_50$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_50$Bid_P_receptor,pch=16,col="grey")

lines(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,lwd=2,col="blue",lty=1)
abline(a=50.823,b=0,lwd=2,col="black",lty=2)
points(x=178.388,y=50.823,pch=16,col="blue",cex=1.5)
segments(x0=178.388,y0=-5,y1=50.823,col="black",lty=3)
text(x=51,y=60.823,"51 AUD$/kg DIN EoC")
abline(v=190,lwd=2,col="red",lty=3)
mtext("190",at=190,side=1,line=0,col="red")
mtext("175",at=175,side=1,line=0,col="black")


par(old_par) 
dev.off() # This sets plotting back to the screen


#Murray plot

setwd(Dir_Out)
old_par <- par()
png(paste(Focus_Catchment,"_randomised_DIN_supply_curve_with_target",".png",sep=""), width=579,height=560, unit="mm", res=600) #This plots to a file instead of the screen
par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

plot(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,
     xlab="DIN load reduction at EoC (tonnes)",ylab="Supply (AUD$/kg DIN EoC)",main = paste("DIN Credit Supply: ",Focus_Catchment,sep=""),
     type="n",lwd=1,col="blue",ylim=c(-5,300),xlim=c(0,225),xaxs = "i",yaxs = "i",cex = 0.9)
points(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_3$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_3$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_4$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_4$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_5$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_5$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_6$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_6$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_7$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_7$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_8$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_8$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_9$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_9$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_11$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_11$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_12$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_12$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_13$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_13$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_14$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_14$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_15$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_15$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_17$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_17$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_18$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_18$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_19$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_19$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_20$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_20$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_21$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_21$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_22$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_22$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_23$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_23$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_24$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_24$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_25$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_25$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_26$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_26$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_27$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_27$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_28$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_28$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_29$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_29$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_30$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_30$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_31$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_31$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_32$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_32$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_33$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_33$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_34$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_34$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_35$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_35$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_36$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_36$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_37$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_37$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_38$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_38$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_39$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_39$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_40$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_40$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_41$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_41$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_42$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_42$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_43$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_43$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_44$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_44$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_46$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_46$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_47$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_47$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_48$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_48$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_49$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_49$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_50$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_50$Bid_P_receptor,pch=16,col="grey")

lines(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,lwd=2,col="blue",lty=1)
abline(a=65.858,b=0,lwd=2,col="black",lty=2)
points(x=144.869,y=65.858,pch=16,col="blue",cex=1.5)
segments(x0=144.869,y0=-5,y1=65.858,col="black",lty=3)
text(x=50,y=75.858,"66 AUD$/kg DIN EoC")
abline(v=120,lwd=2,col="red",lty=3)
mtext("120",at=120,side=1,line=0,col="red")
mtext("145",at=143,side=1,line=0,col="black")

par(old_par) 
dev.off() # This sets plotting back to the screen


# Herbert plot

setwd(Dir_Out)
old_par <- par()
png(paste(Focus_Catchment,"_randomised_DIN_supply_curve_with_target",".png",sep=""), width=579,height=560, unit="mm", res=600) #This plots to a file instead of the screen
par(mar=c(5.1,6.1,4.1,2.1)) #This gives a bit more space on the left hand margin for the vertical axis label

plot(x=Offers_increasing_order_1$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_1$Bid_P_receptor,
     xlab="DIN load reduction at EoC (tonnes)",ylab="Supply (AUD$/kg DIN EoC)",main = paste("DIN Credit Supply: ",Focus_Catchment,sep=""),
     type="n",lwd=1,col="blue",ylim=c(-5,300),xlim=c(0,700),xaxs = "i",yaxs = "i",cex = 0.9)
points(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_3$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_3$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_4$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_4$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_5$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_5$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_6$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_6$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_7$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_7$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_8$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_8$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_9$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_9$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_10$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_10$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_11$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_11$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_12$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_12$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_13$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_13$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_14$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_14$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_15$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_15$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_16$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_16$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_17$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_17$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_18$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_18$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_19$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_19$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_20$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_20$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_21$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_21$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_22$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_22$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_23$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_23$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_24$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_24$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_25$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_25$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_26$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_26$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_27$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_27$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_28$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_28$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_29$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_29$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_30$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_30$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_31$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_31$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_32$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_32$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_33$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_33$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_34$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_34$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_35$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_35$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_36$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_36$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_37$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_37$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_38$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_38$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_39$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_39$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_40$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_40$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_41$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_41$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_42$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_42$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_43$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_43$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_44$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_44$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_45$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_45$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_46$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_46$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_47$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_47$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_48$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_48$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_49$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_49$Bid_P_receptor,pch=16,col="grey")
points(x=Offers_increasing_order_50$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_50$Bid_P_receptor,pch=16,col="grey")

lines(x=Offers_increasing_order_2$cumulative_Offer_Q_receptor/1E3,y=Offers_increasing_order_2$Bid_P_receptor,lwd=2,col="blue",lty=1)
abline(a=48.667,b=0,lwd=2,col="black",lty=2)
points(x=509.118,y=48.667,pch=16,col="blue",cex=1.5)
text(x=150,y=58,"49 AUD$/kg DIN EoC")
abline(v=620,lwd=2,col="red",lty=3)
segments(x0=510,y0=-5,y1=48.667,col="black",lty=3)
mtext("620",at=620,side=1,line=0,col="red")
mtext("510",at=520,side=1,line=0,col="black")

par(old_par) 
dev.off() # This sets plotting back to the screen





