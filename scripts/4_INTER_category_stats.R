# INTER-categorical comparison analyses
# exploring questions across SEPARATE dataframes
# All dataframes for comparison created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)

##LABOR_3A vs. HERDMGMT_1A---------------------------------------
#How many people undertake migrations? vs. Distance for daily herding, generally
#Columns: labor_numMigrates/altLife_nonHerdWork


##LABOR_3A vs. HERDMGMT_9A---------------------------------------
#How many people undertake migrations? vs. In the past five years, have you changed your herding management practices?
#Columns: labor_numMigrates/herdMgmt_past5Yrs_mgmtChanges


##LABOR_3A vs. HERDMGMT_10A---------------------------------------
#How many people undertake migrations? vs. Do you have plans to make changes to management practices in the next five years? 
#Columns: labor_numMigrates/herdMgmt_next5Yrs_mgmtChanges


##LABOR_3A vs. LIVESTOCK_2A---------------------------------------
#How many people undertake migrations? vs. Overall SFU
#Columns: labor_numMigrates/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on


##LABOR_4A vs. ALTLIVELIHOODS_1A---------------------------------------
#Does migration impact labor? vs. Is someone in the household doing non-herding work?
#Columns: labor_migImpactLabor/altLife_nonHerdWork


##LABOR_4A vs. ALTLIVELIHOODS_3A---------------------------------------
#Does migration impact labor? vs. Number of loans taken out per year?
#Columns: labor_migImpactLabor/altLife_loansPerYr


##LABOR_4A vs. HERDMGMT_12---------------------------------------
#Does migration impact labor? vs. Condition and degree of pastoral change
#Columns: labor_migImpactLabor/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg


##LABOR_4A vs. LIVESTOCK_3A---------------------------------------
#Does migration impact labor? vs. Did you purchase supplemental fodder last year?
#Columns: labor_migImpactLabor/lastYr_fodder


##LABOR_4A vs. LIVESTOCK_4A---------------------------------------
#Does migration impact labor? vs. Do you plan to purchase supplemental fodder this year?
#Columns: labor_migImpactLabor/thisYr_fodder


##LABOR_4A vs. LIVESTOCK_6A---------------------------------------
# Does migration impact labor? vs. Has your herd size changed over the last five years?
#Columns: labor_migImpactLabor/past5yrs_herdsize


##LABOR_4A vs. LIVESTOCK_9A---------------------------------------
#Does migration impact labor? vs. Do you have plans to substantially change the size of your herd?
#Columns: labor_migImpactLabor/nextYr_herdChg


##LABOR_4A vs. LIVESTOCK_10A---------------------------------------
#Does migration impact labor? vs. If you are changing the herd composition, what are you doing?
#Columns: labor_migImpactLabor/livestock_nextYr_what


##LABOR_5A vs. ALTLIVELIHOODS_1A---------------------------------------
#Do you hire labor? vs. Is someone in the household doing non-herding work?
#Columns: labor_hire/altLife_nonHerdWork


##LABOR_5A vs. ALTLIVELIHOODS_3A---------------------------------------
#Do you hire labor? vs. Number of loans taken out per year?
#Columns: labor_hire/altLife_loansPerYr


##LABOR_5A vs. HERDMGMT_9A---------------------------------------
#Do you hire labor? vs. In the past five years, have you changed your herding management practices?
#Columns: labor_hire/herdMgmt_past5Yrs_mgmtChanges


##LABOR_5A vs. HERDMGMT_10A---------------------------------------
#Do you hire labor? vs. Do you have plans to make changes to management practices in the next 5years?
#Columns: labor_hire/herdMgmt_next5Yrs_mgmtChanges


##LABOR_5A vs. LIVESTOCK_5A---------------------------------------
#Do you hire labor? vs. Have you noticed any long term shifts in vegetation/forage?
#Columns: labor_hire/vegShifts_yn/vegShifts_quanQual


##LABOR_5A vs. LIVESTOCK_6A---------------------------------------
#Do you hire labor? vs. Has your herd size changed over the last five years?
#Columns: labor_hire/past5yrs_herdsize


##LABOR_5A vs. LIVESTOCK_9A---------------------------------------
#Do you hire labor? vs. Do you have plans to substantially change the size of your herd? 
#Columns: labor_hire/nextYr_herdChg


##LABOR_5A vs. LIVESTOCK_10A---------------------------------------
#Do you hire labor? vs. If you are changing the herd composition, what are you doing?
#Columns: labor_hire/livestock_nextYr_what


##ALTLIVELIHOODS_1A vs. LIVESTOCK_2A---------------------------------------
#Is someone in the household doing non-herding work? vs. OVERALL SFU.
#Columns: altLife_nonHerdWork/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on


##ALTLIVELIHOODS_1A vs. LIVESTOCK_6A---------------------------------------
#Is someone in the household doing non-herding work? vs. Has your herd size changed over the last five years?
#Columns: altLife_nonHerdWork/past5yrs_herdsize


##ALTLIVELIHOODS_1A vs. LIVESTOCK_9A---------------------------------------
#Is someone in the household doing non-herding work? vs. Do you have plans to substantially change the size of your herd? 
#Columns: altLife_nonHerdWork/nextYr_herdChg


##ALTLIVELIHOODS_1A vs. LIVESTOCK_10A---------------------------------------
#Is someone in the household doing non-herding work? vs. If you are changing the herd composition, what are you doing?
#Columns: altLife_nonHerdWork/livestock_nextYr_what


##ALTLIVELIHOODS_3A vs. HERDMGMT_9A---------------------------------------
#Number of loans taken out per year? vs. In the past five years, have you changed your herding management practices?
#Columns: altLife_loansPerYr/herdMgmt_past5Yrs_mgmtChanges


##ALTLIVELIHOODS_3A vs. LIVESTOCK_3A---------------------------------------
#Number of loans taken out per year? vs. Did you purchase supplemental fodder last year?
#Columns: altLife_loansPerYr/lastYr_fodder


##ALTLIVELIHOODS_3A vs. LIVESTOCK_4A---------------------------------------
#Number of loans taken out per year? vs. Do you plan to purchase supplemental fodder this year? 
#Columns: altLife_loansPerYr/thisYr_fodder


##ALTLIVELIHOODS_3A vs. LIVESTOCK_6A---------------------------------------
#Number of loans taken out per year? vs. Has your herd size changed over the last five years?
#Columns: altLife_loansPerYr/past5yrs_herdsize


##HERDMGMT_1A vs. LIVESTOCK_5A---------------------------------------
#Distance for daily herding, generally vs. Have you noticed any long term shifts in vegetation/forage?
#Columns: herdMgmt_dailyDist/vegShifts_yn/vegShifts_quanQual


##HERDMGMT_9A vs. LIVESTOCK_2A---------------------------------------
#In the past five years, have you changed your herding management practices? vs.  OVERALL SFU
#Columns: herdMgmt_past5Yrs_mgmtChanges/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on


##HERDMGMT_9A vs. LIVESTOCK_2D---------------------------------------
#In the past five years, have you changed your herding management practices? vs. SFU by Soum
#Columns: herdMgmt_past5Yrs_mgmtChanges/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on


##HERDMGMT_9A vs. LIVESTOCK_5A---------------------------------------
#In the past five years, have you changed your herding management practices? vs. Have you noticed any long term shifts in vegetation/forage?
#Columns: herdMgmt_past5Yrs_mgmtChanges/vegShifts_yn/vegShifts_quanQual


##HERDMGMT_9A vs. LIVESTOCK_6A---------------------------------------
#In the past five years, have you changed your herding management practices? vs. Has your herd size changed over the last five years?
#Columns: herdMgmt_past5Yrs_mgmtChanges/past5yrs_herdsize


##HERDMGMT_11A vs. LIVESTOCK_6A---------------------------------------
#Are there changes you want to make to your management practices but can’t? vs. Has your herd size changed over the last five years?
#Columns: herdMgmt_whatChanges_cantMake/past5yrs_herdsize


##HERDMGMT_11A vs. LIVESTOCK_9A---------------------------------------
#Are there changes you want to make to your management practices but can’t? vs. Do you have plans to substantially change the size of your herd? 
#Columns: herdMgmt_whatChanges_cantMake/nextYr_herdChg












