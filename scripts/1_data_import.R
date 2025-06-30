#This script creates the following subset dfs:
  # base_
  # base_othername
  # base_othername2
  # base_asmanyasyouneed

# Also combines the above and adds in summary veg change data for survey soums/bags

#NOTE: If bringing in new data exported from QGis remember to adjust
  # place names so that they match
  # e.g. #sb values: "Dundgovi"    "Govi-Sumber" "Tuv aimag"  
         #veg values: "Dundgobi"   "Gobisumber" "Tuv"



#SET ENV------- 
##pkgs-----
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(dplyr)
library(rvest)
library(ggplot2) 
library(rlang) 
library(readr)


##set wd -----------------------------------------------------------------------
setwd("C:/Users/jessi/OneDrive - Cornell University/Documents/MSPhD/AllingtonLab_LCLUC/RStudio_WorkingDirectory")

#CODE---------------------------------------------------------------------------
##IMPORT DATA FILE -------------------------------------------------------------
# this is the latest file from the SUMR proj, dated 16 Aug 2024:
base_ <- read_csv("MG_LCLUC_Household_Survey_TRANSLATED_02172024.csv", 
                           col_names = TRUE, 
                           trim_ws = TRUE)

names(base_) <- gsub("\\.x$", "", names(base_))

str(base_)
base_ <- as.data.frame(base_)
base_ <- base_ %>% rename(Ref = '_1_idInfo_survey_ref_') %>%
                                   mutate(across(Ref, as.numeric))

view(base_)




##Subsetting ----- 
# the full spreadsheet into smaller dataframes--- 
# and renaming the super long col names 
# see the codebook to cross-reference names to data types


###BASE DEMOGRAPHICS------------------------------------------------------------
base_DEMOGRAPHICS <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    hhSize = '_1_HHDems_num_HHmembers_',
    kidsCamp = '_5_HHDems_num_HHmems_sub16_camp_tot_',
    kidsSoum = '_5_a_HHDems_num_HHmems_sub16_soum_',
    kidsAimag = '_5_b_HHDems_num_HHmems_sub16_aimag_',
    kidsUB = '_5_c_HHDems_num_HHmems_sub16_ub_',
    yaCamp = '_6_HHDems_num_HHmems_16to30_camp_tot_',
    yaSoum = '_6_a_HHDems_num_HHmems_16to30_soum_', 
    yaAimag =  '_6_b_HHDems_num_HHmems_16to30_aimag_', 
    yaUB =  '_6_c_HHDems_num_HHmems_16to30_ub_',
    adCamp = '_7_HHDems_num_HHmems_30to60_camp_tot_',
    adSoum = '_7_a_HHDems_num_HHmems_30to60_soum_', 
    adAimag = '_7_b_HHDems_num_HHmems_30to60_aimag_', 
    adUB = '_7_c_HHDems_num_HHmems_30to60_ub_',
    oldCamp = '_8_HHDems_num_HHmems_60plus_camp_tot_',
    oldSoum = '_8_a_HHDems_num_HHmems_60plus_soum_', 
    oldAimag = '_8_b_HHDems_num_HHmems_60plus_aimag_', 
    oldUB = '_8_c_HHDems_num_HHmems_60plus_ub_', 
    khotAil_num_hh = '_9_HHDems_num_HH_khotAil_',
    khotAil_num_ppl = '_10_HHDems_num_people_khotAil_',
    khotAil_stay_tog = '_11_HHDems_do_HH_stayTogether_'
    )
view(base_DEMOGRAPHICS)



###BASE DEMOGRAPHICS BY AIMAG---------------------------------------------------
base_DEMOGRAPHICS_aimag <- mutate(base_DEMOGRAPHICS, newAimag = case_when(Aimag == "Tuv aimag" ~ "Tuv", 
                                                      Aimag == "Govi-Sumber" ~ "Govisumber",
                                                      Aimag == "Dundgovi" ~ "Dundgovi"
)) %>%
  mutate(across(newAimag, as.factor)) %>%
  mutate(across(c(1:4), as.factor)) %>%
  mutate(concated_loc = paste(newAimag, Soum, bag, sep = '_')
  )  
view(base_DEMOGRAPHICS_aimag)



###BASE LABOR ------------------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_LABOR <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    labor_whoMovesDaily = '_1_labor_HHmems_movesAnimals_daily_',
    labor_whoMigrates = '_2_labor_HHmems_seasonalMig_toCamp_',
    labor_numMigrates = '_2_a_labor_HHmems_seasonalMig_toCamp_num_',
    labor_migImpactLabor = '_5_labor_HHmems_mig_impHerdLabor_',
    labor_migImpactPract = '_6_labor_HHmems_mig_impHerdPractices_',
    labor_hire = '_7_labor_hireLabor_',
    labor_hire_dailyMove = '_7_a_labor_hireLabor_dailyHerdMvmts_', 
    labor_hire_bigMove =  '_7_b_labor_hireLabor_moveToPasture_', 
    labor_hire_forOtor =  '_7_c_labor_hireLabor_assistOtor_',
    labor_hire_Other = '_8_a_labor_ifYes_how_text_'
  )
view(base_LABOR)




###BASE TENURE -----------------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_TENURE <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    wintCamp = '_1_tenure_HH_winterCamp_',
    wintContract = '_2_tenure_HH_winterCamp_contract_',
    whoWintContract = '_2_a_tenure_ifYes_whoHolds_contract_',
    wintPas = '_3_tenure_HH_winterPasture_',
    wintPasContract = '_4_tenure_HH_winterPasture_contract_',
    sameCamp = '_5_tenure_isWinterCamp_springCamp_',
    sprCamp = '_6_tenure_HH_springCamp_', 
    sprCampContract = '_7_tenure_HH_springCamp_contract_', 
    sprPasContract = '_8_tenure_HH_springPasture_contract_'
  )
view(base_TENURE)



###BASE ALT LIVELIHOODS --------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_ALTLIFE <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    altLife_nonHerdWork = '_1_altLivelihoods_HHmems_nonHerdingWork_',
    altLife_whoNoHerdwork = '_1_a_i_altLivelihoods_IfYes_who_',
    altLife_noHerdWhatWork = '_1_a_i_altLivelihoods_IfYes_what_',
    altLife_otherInc = '_1_b_altLivelihoods_otherInc_',
    altLife_whereLoans = '_2_altLivelihoods_loans_whereToGet_',
    altLife_loansPerYr = '_3_altLivelihoods_loans_howOften_yr_',
    altLife_loansMin = '_4_altLivelihoods_loans_min_millions_', 
    altLife_loansMax = '_4_altLivelihoods_loans_max_millions_',
    altLife_loansWhenNeed = '_5_altLivelihoods_finNeed_difTime_'
  )
view(base_ALTLIFE)




###BASE HERDING MANAGEMENT -----------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_HERDMGMT <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    herdMgmt_dailyDist = '_1_herdMgmt_dailyHerd_dist_km_',
    herdMgmt_sumDailyDist = '_1_a_herdMgmt_dailyHerd_summer_dist_km_',
    herdMgmt_wintDailyDist = '_1_b_herdMgmt_dailyHerd_winter_dist_km_',
    herdMgmt_sumCampDist = '_2_herdMgmt_summerCamp_distFrom_km_',
    herdMgmt_wintCampDist = '_3_herdMgmt_winterCamp_distFrom_km_',
    herdMgmt_timesMoved_thisYr = '_4_herdMgmt_thisYr_timesMoved_camp_',
    herdMgmt_timesMoves_lastYr = '_5_herdMgmt_lastYr_timesMoved_camp_', 
    herdMgmt_avgDistMoves = '_6_herdMgmt_lastYr_avgMoveDist_km_', 
    herdMgmt_totDistMoves = '_7_herdMgmt_lastYr_totMoveDist_km_', 
    herdMgmt_10yrs_avgMoveDist = '_8_herdMgmt_10yrsAgo_avgMoveDist_km_',
    herdMgmt_lastYr_Otor = '_9_herdMgmt_lastYr_otor_yn_',
    herdMgmt_thisYr_Otor = '_10_herdMgmt_thisYr_otor_yn_',
    herdMgmt_thisYr_wintPast = '_11_herdMgmt_thisYr_winterPasture_res_yn_',
    herdMgmt_lastYr_wintPast = '_12_herdMgmt_lastYr_winterPasture_res_yn_',
    herdMgmt_thisYr_springPast = '_13_herdMgmt_thisYr_springPasture_res_yn_',
    herdMgmt_lastYr_springPast = '_14_herdMgmt_lastYr_springPasture_res_yn_',
    herdMgmt_thisYr_DzudPast = '_15_herdMgmt_thisYr_dzudPasture_res_yn_', 
    herdMgmt_lastYr_DzudPast = '_16_herdMgmt_lastYr_dzudPasture_res_yn_', 
    herdMgmt_past5yrs_resPast = '_17_herdMgmt_past5yrs_resPasture_grazed_yn_', 
    herdMgmt_10yrsAgo_herdTravel = '_18_a_herdMgmt_10yrsAgo_dailyHerd_means_',
    herdMgmt_5yrsAgo_herdTravel = '_18_b_herdMgmt_5yrsAgo_dailyHerd_means_',
    herdMgmt_lastYr_herdTravel = '_18_c_herdMgmt_lastYr_dailyHerd_means_',
    herdMgmt_10yrsAgo_dailyDist = '_19_herdMgmt_10yrsAgo_dailyHerd_distTrav_km_',
    herdMgmt_5yrsAgo_dailyDist = '_20_herdMgmt_5yrsAgo_dailyHerd_distTrav_km_',
    herdMgmt_lastYr_dailyDist = '_21_herdMgmt_lastYr_dailyHerd_distTrav_km_',
    herdMgmt_past5Yrs_mgmtChanges = '_22_herdMgmt_past5yrs_mgmtPractices_chgd_yn_',
    herdMgmt_mgmtChanges_what = '_22_a_herdMgmt_ifYes_how_', 
    herdMgmt_next5Yrs_mgmtChanges = '_23_herdMgmt_next5Yrs_mgmtPractices_chgs_yn_', 
    herdMgmt_futureChg_what = '_23_a_herdMgmt_ifYes_how_', 
    herdMgmt_whatChanges_cantMake = '_24_herdMgmt_mgmtPractices_chgs_cantMake_',
    herdMgmt_chg_limitingFactor = '_25_a_herdMgmt_ifYes_lmtngFactor_', 
    herdMgmt_pastureCon_chg_yn = '_1_resUse_past3yrs_pastureCon_chg_', 
    herdMgmt_pastureCon_chg_deg = '_1_a_resUse_past3yrs_pastureCon_chg_deg_'
  )
view(base_HERDMGMT)




###BASE LIVESTOCK --------------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_LIVESTOCK <- base_ %>%
  select(
    Ref = 'Ref',
    Aimag = '_2_idInfo_aimag_',
    Soum = '_3_idInfo_soum_',
    bag = '_4_idInfo_bagCode_',
    livestock_2023_camel = '_1_livestock_EOY_2023_camel_tot_',
    livestock_2023_cow = '_2_livestock_EOY_2023_cow_tot_',
    livestock_2023_horse = '_3_livestock_EOY_2023_horse_tot_',
    livestock_2023_sheep = '_4_livestock_EOY_2023_sheep_tot_',
    livestock_2023_goat = '_5_livestock_EOY_2023_goat_tot_',
    livestock_2019_camel = '_1_1_livestock_EOY_5yrsAgo_camel_tot_',
    livestock_2019_cow = '_2_1_livestock_EOY_5yrsAgo_cow_tot_', 
    livestock_2019_horse = '_3_1_livestock_EOY_5yrsAgo_horse_tot_', 
    livestock_2019_sheep = '_4_1_livestock_EOY_5yrsAgo_sheep_tot_', 
    livestock_2019_goat = '_5_1_livestock_EOY_5yrsAgo_goat_tot_',
    livestock_lastYr_fodder = '_7_livestock_lastYr_suppFodder_yn_',
    livestock_thisYr_fodder = '_8_livestock_thisYr_suppFodder_yn_',
    livestock_vegShifts_yn = '_9_livestock_vegForageShifts_longTerm_yn_',
    livestock_vegShifts_quanQual = '_9_a_livestock_vegForageShifts_ifSo_quanQual_',
    livestock_vegShifts_type = '_9_b_livestock_vegForageShifts_ifSo_chgType_',
    livestock_5yrs_herdsize = '_10_livestock_past5yrs_herdSzChg_',
    livestock_5yrs_herdInc = '_10_a_livestock_past5Yrs_herdSzChg_inc_reason_', 
    livestock_5yrs_herdDec = '_10_b_livestock_past5yrs_herdSzChg_dec_reason_', 
    livestock_nextYr_herdChg = '_11_livestock_nextYr_herdSzChg_plans_yn_',
    herdMgmt_pastureCon_chg_yn = '_1_resUse_past3yrs_pastureCon_chg_', 
    herdMgmt_pastureCon_chg_deg = '_1_a_resUse_past3yrs_pastureCon_chg_deg_'
  )
view(base_LIVESTOCK)




#VEG CHANGE FROM QGIS-----------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_VEG <- base_ %>%
  select(Ref, 
         Aimag = `_2_idInfo_aimag_`, 
         Soum = `_3_idInfo_soum_`,
         bag = `_4_idInfo_bagCode_`,
         cov23mean = `_cov23mean`,
         cov23median = `_cov23medi`,
         cov19mean = `_cov19mean`,
         cov19median = `_cov19medi`,
         cov_chng = `cov_chng_19_23`
  ) 

str(base_VEG)
base_VEG <- base_VEG %>%
  mutate(newAimag = case_when(Aimag == "Tuv" ~ "Tuv", 
                              Aimag == "Gobisumber" ~ "Govisumber",
                              Aimag == "Dundgobi" ~ "Dundgovi",
                              TRUE ~ Aimag # Keep other values unchanged
  )) %>%
  mutate(concated_loc = paste(newAimag, Soum, bag, sep = '_')) %>%
  mutate(across(c(cov23mean, cov23median, cov19mean, cov19median, cov_chng), as.factor))

View(base_VEG)






#COMBINED BASE DFs -------------------------------------------------------------
###I chose these variables based on what I thought might be most important to the admins and officials we'll be reporting to. 
base_COMBINED <- base_DEMOGRAPHICS %>% 
  left_join(base_LABOR, by = 'Ref') %>% 
  left_join(base_TENURE, by = 'Ref') %>% 
  left_join(base_ALTLIFE, by = 'Ref') %>% 
  left_join(base_HERDMGMT, by = 'Ref') %>% 
  left_join(base_LIVESTOCK, by = 'Ref') %>%
  left_join(base_VEG, by = 'Ref') %>% 
  select(-ends_with(".y")) %>%   # remove all .y columns
  select(-ends_with(".x.x")) %>% 
  select(-ends_with(".x"))

View(base_COMBINED)





#EXTRAS----------                    
# library(purrr)
# ## calculate how many NAs there are in each variable 
# base_movement %>%
# map(is.na) %>% map(sum)    

