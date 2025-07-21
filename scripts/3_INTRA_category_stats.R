# INTRA-categorical comparison analyses
# exploring questions within the SAME dataframe
# All dataframes for comparison created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)


###LABOR------------------------------------------------------------------------
#-------------------------------------------------------------------------------
##3A vs. 4A---------------------------------------------------------------------
  #How many people undertake migrations? vs. Does migration impact labor?
  #Columns: labor_whoMigrates/labor_migImpactLabor



##3A vs 4B----------------------------------------------------------------------
  #How many people undertake migrations? vs. Does migration impact herding practices?
  #Columns: labor_whoMigrates/labor_migImpactPract



##3A vs. 5A---------------------------------------------------------------------
  #How many people undertake migrations? vs. Do you hire labor?
  #Columns: labor_whoMigrates/labor_hire







#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

###ALTERNATIVE LIVELIHOODS------------------------------------------------------
#-------------------------------------------------------------------------------
##1A vs. 3A---------------------------------------------------------------------
  #Is someone in the household doing non-herding work?  vs. Number of loans taken out per year?
  #Columns: altLife_nonHerdWork/altLife_loansPerYr



##1A vs. 3B---------------------------------------------------------------------
  #Is someone in the household doing non-herding work?  vs. Loan Sizes (min, max, mean, median, range)
  #Columns: altLife_nonHerdWork/altLife_loansMin/altLife_loansMax



##1A vs. 3C---------------------------------------------------------------------
  #Is someone in the household doing non-herding work?  vs. When do you typically need loans?
  #Columns: altLife_nonHerdWork/altLife_loansWhenNeed








#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

###HERD MANAGEMENT--------------------------------------------------------------
#-------------------------------------------------------------------------------
##5B vs. 9A---------------------------------------------------------------------
  # What is the average distance of moves, now vs. 10yrs ago?  vs. Changed management practices (yes/no):
  # Columns: herdMgmt_avgDistMoves/herdMgmt_10yrs_avgMoveDist/herdMgmt_past5Yrs_mgmtChanges



##9A vs. 11A--------------------------------------------------------------------
  # Changed management practices (yes/no):  vs. Are there changes you want to make to your management practices but can’t?
  # Columns: herdMgmt_past5Yrs_mgmtChanges/herdMgmt_whatChanges_cantMake



##10A vs. 12--------------------------------------------------------------------
  # Planning to change management practices (yes/no):  vs. Condition and degree of pastoral change:
  # Columns: herdMgmt_next5Yrs_mgmtChanges/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg



##11A vs. 12--------------------------------------------------------------------
  # 11A. Are there changes you want to make to your management practices but can’t? vs. Condition and degree of pastoral change:
  # Columns: herdMgmt_whatChanges_cantMake/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg






#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

### LIVESTOCK-------------------------------------------------------------------
#-------------------------------------------------------------------------------
##1E vs. 3A--------------------------------------------------------------------
  # Have certain types of livestock increased or decreased? by Soum: vs. Did you purchase supplemental fodder last year?
  # Columns: livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_goat
  # livestock_2019_camel/livestock_2019_cow/livestock_2019_horse/livestock_2019_sheep/livestock_2019_goat/lastYr_fodder



##1E vs. 4A--------------------------------------------------------------------
  # Have certain types of livestock increased or decreased? by Soum: vs. Do you plan to purchase supplemental fodder this year?
  # Columns: livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_goat
  # livestock_2019_camel/livestock_2019_cow/livestock_2019_horse/livestock_2019_sheep/livestock_2019_goat/thisYr_fodder



##1E vs. 4A--------------------------------------------------------------------
  # Have certain types of livestock increased or decreased? by Soum: vs. Have you noticed any long term shifts in vegetation/forage?
  # Columns: livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_goat
  # livestock_2019_camel/livestock_2019_cow/livestock_2019_horse/livestock_2019_sheep/livestock_2019_goat/vegShifts_yn/vegShifts_quanQual



##6A vs. 9A--------------------------------------------------------------------
# Has your herd size changed over the last five years? Overall vs. Do you have plans to substantially change the size of your herd?
# Columns: past5yrs_herdsize/nextYr_herdChg



##3A vs. 4A--------------------------------------------------------------------
#  Did you purchase supplemental fodder last year? vs. 4A. Do you plan to purchase supplemental fodder this year?
# Columns: lastYr_fodder/nextYr_herdChg/thisYr_fodder










































