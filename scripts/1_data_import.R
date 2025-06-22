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


##set wd ----------------------------------------------------------------------
setwd("C:/Users/jessi/OneDrive - Cornell University/Documents/MSPhD/AllingtonLab_LCLUC/RStudio_WorkingDirectory")

#CODE-------------------------------------------------------------------------

##IMPORT DATA FILE ------------------------------------
# this is the latest file from the SUMR proj, dated 16 Aug 2024:
original_file <- read_csv("MG_LCLUC_Household_Survey_ZSL_TRANSLATION_20240604.csv", 
                           col_names = TRUE, 
                           trim_ws = TRUE)

names(base_) <- gsub("\\.x$", "", names(base_))
view(base_)


str(base_)
base_ <- as.data.frame(base_)
base_ <- base_ %>% rename(Ref = '_1_idInfo_survey_ref_') %>%
                                   mutate(across(Ref, as.factor))
##Subsetting ----- 
# the full spreadsheet into smaller dataframes--- 
# and renaming the super long col names 
# see the codebook to cross-reference names to data types


###BASE DEMOGRAPHICS----
base_demog <- base_ %>%
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
    yaSoum = `_6_a_HHDems_num_HHmems_16to30_soum_`, 
    yaAimag =  `_6_b_HHDems_num_HHmems_16to30_aimag_`, 
    yaUB =  `_6_c_HHDems_num_HHmems_16to30_ub_`,
    adCamp = '_7_HHDems_num_HHmems_30to60_camp_tot_',
    adSoum = `_7_a_HHDems_num_HHmems_30to60_soum_`, 
    adAimag = `_7_b_HHDems_num_HHmems_30to60_aimag_`, 
    adUB = `_7_c_HHDems_num_HHmems_30to60_ub_`,
    oldCamp = '_8_HHDems_num_HHmems_60plus_camp_tot_',
    oldSoum = `_8_a_HHDems_num_HHmems_60plus_soum_`, 
    oldAimag = `_8_b_HHDems_num_HHmems_60plus_aimag_`, 
    oldUB = `_8_c_HHDems_num_HHmems_60plus_ub_`, 
    khotAil_num_hh = '_9_HHDems_num_HH_khotAil_',
    khotAil_num_ppl = '_10_HHDems_num_people_khotAil_',
    khotAil_stay_tog = '_11_HHDems_do_HH_stayTogether_'
  ) 

View(base_demog)

