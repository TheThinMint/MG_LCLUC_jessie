# INTER-categorical comparison analyses
# exploring questions across SEPARATE dataframes
# All dataframes for comparison created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)

##LABOR_3A vs. HERDMGMT_1A---------------------------------------
#How many people undertake migrations? vs. Distance for daily herding, generally
#Columns: labor_numMigrates/herdMgmt_dailyDist
count_numMigrates <- base_LABOR %>%
  select(Ref, labor_numMigrates)
laborHerdMgmt_3a1a <- count_numMigrates %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_dailyDist), by = "Ref")
count_laborHerdMgmt_3a1a <- laborHerdMgmt_3a1a %>%
  count(labor_numMigrates, herdMgmt_dailyDist, sort = TRUE)
kable(count_laborHerdMgmt_3a1a)



##LABOR_3A vs. HERDMGMT_9A---------------------------------------
#How many people undertake migrations? vs. In the past five years, have you changed your herding management practices?
#Columns: labor_numMigrates/herdMgmt_past5Yrs_mgmtChanges
count_numMigrates2 <- base_LABOR %>%
  select(Ref, labor_numMigrates)
laborHerdMgmt_3a9a <- count_numMigrates2 %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref")
count_laborHerdMgmt_3a9a <- laborHerdMgmt_3a9a %>%
  count(labor_numMigrates, herdMgmt_past5Yrs_mgmtChanges, sort = TRUE)
kable(count_laborHerdMgmt_3a9a)



##LABOR_3A vs. HERDMGMT_10A---------------------------------------
#How many people undertake migrations? vs. Do you have plans to make changes to management practices in the next five years? 
#Columns: labor_numMigrates/herdMgmt_next5Yrs_mgmtChanges
count_numMigrates3 <- base_LABOR %>%
  select(Ref, labor_numMigrates)
laborHerdMgmt_3a10a <- count_numMigrates3 %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_next5Yrs_mgmtChanges), by = "Ref")
count_laborHerdMgmt_3a10a <- laborHerdMgmt_3a10a %>%
  count(labor_numMigrates, herdMgmt_next5Yrs_mgmtChanges, sort = TRUE)
kable(count_laborHerdMgmt_3a10a)



##LABOR_3A vs. LIVESTOCK_2A---------------------------------------
#How many people undertake migrations? vs. Overall SFU
#Columns: labor_numMigrates/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
SFU_count <- base_LIVESTOCK %>%
  pivot_longer(
    cols = starts_with("livestock_"),
    names_to = c("year", "livestock_type"),
    names_pattern = "livestock_(\\d{4})_(.*)",
    values_to = "count_raw"
  ) %>%
  mutate(
    year = as.integer(year),
    livestock_type = str_to_lower(livestock_type),
    count = parse_number(as.character(count_raw)),
    sfu_factor = case_when(
      livestock_type == "sheep" ~ 1,
      livestock_type == "goat"  ~ 0.9,
      livestock_type == "cow"   ~ 6,
      livestock_type == "horse" ~ 7,
      livestock_type == "camel" ~ 5,
      TRUE ~ NA_real_
    ),
    sfu_total = count * sfu_factor
  ) %>%
  group_by(Ref, year) %>%
  summarise(SFU = sum(sfu_total, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

tol <- 0  
SFU_count <- SFU_count %>%
  mutate(
    delta = SFU_2023 - SFU_2019,
    SFU_comparison = case_when(
      is.na(SFU_2019) & is.na(SFU_2023) ~ NA_character_,
      is.na(SFU_2019) & !is.na(SFU_2023) ~ "2023: greater SFU",
      !is.na(SFU_2019) & is.na(SFU_2023) ~ "2023: less SFU",
      delta >  tol ~ "2023: greater SFU",
      delta < -tol ~ "2023: less SFU",
      TRUE ~ "2023: same SFU"
    )
  )

laborLivestock_3a2a <- SFU_count %>%
  left_join(base_LABOR %>% select(Ref, labor_numMigrates), by = "Ref") %>%
  mutate(labor_numMigrates = str_to_lower(str_trim(labor_numMigrates)))

count_laborLivestock_3a2a <- laborLivestock_3a2a %>%
  count(labor_numMigrates, SFU_2023, sort = TRUE)

kable(count_laborLivestock_3a2a)




##LABOR_3A vs. LIVESTOCK_2A---------------------------------------
#Simplified version
#How many people undertake migrations? vs. Overall SFU
#Columns: labor_numMigrates/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
SFU_count <- base_LIVESTOCK %>%
  pivot_longer(
    cols = starts_with("livestock_"),
    names_to = c("year", "livestock_type"),
    names_pattern = "livestock_(\\d{4})_(.*)",
    values_to = "count_raw"
  ) %>%
  mutate(
    year = as.integer(year),
    livestock_type = str_to_lower(livestock_type),
    count = parse_number(as.character(count_raw)),
    sfu_factor = case_when(
      livestock_type == "sheep" ~ 1,
      livestock_type == "goat"  ~ 0.9,
      livestock_type == "cow"   ~ 6,
      livestock_type == "horse" ~ 7,
      livestock_type == "camel" ~ 5,
      TRUE ~ NA_real_
    ),
    sfu_total = count * sfu_factor
  ) %>%
  group_by(Ref, year) %>%
  summarise(SFU = sum(sfu_total, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

tol <- 0  
SFU_count <- SFU_count %>%
  mutate(
    delta = SFU_2023 - SFU_2019,
    SFU_comparison = case_when(
      is.na(SFU_2019) & is.na(SFU_2023) ~ NA_character_,
      is.na(SFU_2019) & !is.na(SFU_2023) ~ "2023: greater SFU",
      !is.na(SFU_2019) & is.na(SFU_2023) ~ "2023: less SFU",
      delta >  tol ~ "2023: greater SFU",
      delta < -tol ~ "2023: less SFU",
      TRUE ~ "2023: same SFU"
    )
  )

laborLivestock_3a2a <- SFU_count %>%
  left_join(base_LABOR %>% select(Ref, labor_numMigrates), by = "Ref") %>%
  mutate(labor_numMigrates = str_to_lower(str_trim(labor_numMigrates)))

count2_laborLivestock_3a2a <- laborLivestock_3a2a %>%
  count(labor_numMigrates, SFU_comparison, sort = TRUE)

kable(count2_laborLivestock_3a2a)




##LABOR_4A vs. ALTLIVELIHOODS_1A---------------------------------------
#Does migration impact labor? vs. Is someone in the household doing non-herding work?
#Columns: labor_migImpactLabor/altLife_nonHerdWork
count_migImpactLabor <- base_LABOR %>%
  select(Ref, labor_migImpactLabor)
laborAltLife_4a1a <- count_migImpactLabor %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_nonHerdWork), by = "Ref")

count_laborAltLife_4a1a <- laborAltLife_4a1a %>%
  count(labor_migImpactLabor, altLife_nonHerdWork, sort = TRUE)
kable(count_laborAltLife_4a1a)






##LABOR_4A vs. ALTLIVELIHOODS_3A---------------------------------------
#Does migration impact labor? vs. Number of loans taken out per year?
#Columns: labor_migImpactLabor/altLife_loansPerYr
count_migImpactLabor2 <- base_LABOR %>%
  select(Ref, labor_migImpactLabor)
laborAltLife_4a3a <- count_migImpactLabor2 %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_loansPerYr), by = "Ref")

count_laborAltLife_4a3a <- laborAltLife_4a3a %>%
  count(labor_migImpactLabor, altLife_loansPerYr, sort = TRUE)
kable(count_laborAltLife_4a3a)




##LABOR_4B vs. HERDMGMT_12---------------------------------------
#Does migration impact herding practices? vs. Condition and degree of pastoral change
#Columns: labor_migImpactPract/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg
pastureCondition <- base_HERDMGMT %>%
  select(Ref, herdMgmt_pastureCon_chg_yn, herdMgmt_pastureCon_chg_deg) %>% 
  mutate(
    condition_comparison = case_when(
      herdMgmt_pastureCon_chg_yn == "Degraded" & herdMgmt_pastureCon_chg_deg == "Slight" ~ "Slightly Degraded",
      herdMgmt_pastureCon_chg_yn == "Degraded" & herdMgmt_pastureCon_chg_deg == "Medium" ~ "Moderately Degraded",
      herdMgmt_pastureCon_chg_yn == "Degraded" & herdMgmt_pastureCon_chg_deg == "Substantial" ~ "Substantially Degraded",
      herdMgmt_pastureCon_chg_yn == "No change" ~ "No change",
      herdMgmt_pastureCon_chg_yn == "Improved" & herdMgmt_pastureCon_chg_deg == "Slight" ~ "Slightly Improved",
      herdMgmt_pastureCon_chg_yn == "Improved" & herdMgmt_pastureCon_chg_deg == "Medium" ~ "Moderately Improved",
      herdMgmt_pastureCon_chg_yn == "Improved" & herdMgmt_pastureCon_chg_deg == "Substantial" ~ "Substantially Improved"
    )
  )

laborHerdMgmt_4a12 <- pastureCondition %>%
  left_join(base_LABOR %>% select(Ref, labor_migImpactPract), by = "Ref")

count_laborAltLife_4a12 <- laborHerdMgmt_4a12 %>%
  count(labor_migImpactPract, condition_comparison, sort = TRUE)
kable(count_laborAltLife_4a12)




##LABOR_4A vs. LIVESTOCK_3A---------------------------------------
#Does migration impact labor? vs. Did you purchase supplemental fodder last year?
#Columns: labor_migImpactLabor/lastYr_fodder
count_migImpactLabor3 <- base_LABOR %>%
  select(Ref, labor_migImpactLabor)

laborLivestock_4a3a <- count_migImpactLabor3 %>%
  left_join(base_LIVESTOCK %>% select(Ref, lastYr_fodder), by = "Ref")

count_laborLivestock_4a3a <- laborLivestock_4a3a %>%
  count(labor_migImpactLabor, lastYr_fodder, sort = TRUE)

kable(count_laborLivestock_4a3a)







##LABOR_4A vs. LIVESTOCK_4A---------------------------------------
#Does migration impact labor? vs. Do you plan to purchase supplemental fodder this year?
#Columns: labor_migImpactLabor/thisYr_fodder
count_migImpactLabor4 <- base_LABOR %>%
  select(Ref, labor_migImpactLabor)

laborLivestock_4a4a <- count_migImpactLabor4 %>%
  left_join(base_LIVESTOCK %>% select(Ref, thisYr_fodder), by = "Ref")

count_laborLivestock_4a4a <- laborLivestock_4a4a %>%
  count(labor_migImpactLabor, thisYr_fodder, sort = TRUE)

kable(count_laborLivestock_4a4a)







##LABOR_4A vs. LIVESTOCK_6A---------------------------------------
# Does migration impact labor? vs. Has your herd size changed over the last five years?
#Columns: labor_migImpactLabor/past5yrs_herdsize
count_migImpactLabor5 <- base_LABOR %>%
  select(Ref, labor_migImpactLabor)

laborLivestock_4a6a <- count_migImpactLabor5 %>%
  left_join(base_LIVESTOCK %>% select(Ref, past5yrs_herdsize), by = "Ref")

count_laborLivestock_4a6a <- laborLivestock_4a6a %>%
  count(labor_migImpactLabor, past5yrs_herdsize, sort = TRUE)

kable(count_laborLivestock_4a6a)





##LABOR_4A vs. LIVESTOCK_9A---------------------------------------
#Does migration impact labor? vs. Do you have plans to substantially change the size of your herd?
#Columns: labor_migImpactLabor/nextYr_herdChg
nextYr_herdChg <- base_LIVESTOCK %>%
  select(Ref, nextYr_herdChg, nextYr_what) %>% 
  mutate(
    plans_comparison = case_when(
      nextYr_herdChg == "Yes" & nextYr_what == "Decrease" ~ "Yes: decrease herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Increase" ~ "Yes: increase herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Maintain" ~ "Yes: maintain herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Unsure" ~ "Yes: unsure of change",
      nextYr_herdChg == "No" ~ "No change"
    )
  )

laborLivestock_4a9a <-nextYr_herdChg %>%
  left_join(base_LABOR %>% select(Ref, labor_migImpactLabor), by = "Ref")

count_laborLivestock_4a9a <- laborLivestock_4a9a %>%
  count(labor_migImpactLabor, plans_comparison, sort = TRUE)

kable(count_laborLivestock_4a9a)



##LABOR_4B vs. LIVESTOCK_9A---------------------------------------
#Does migration impact herding practices? vs. Do you have plans to substantially change the size of your herd?
#Columns: labor_migImpactPract/nextYr_herdChg
nextYr_herdChg <- base_LIVESTOCK %>%
  select(Ref, nextYr_herdChg, nextYr_what) %>% 
  mutate(
    plans_comparison = case_when(
      nextYr_herdChg == "Yes" & nextYr_what == "Decrease" ~ "Yes: decrease herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Increase" ~ "Yes: increase herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Maintain" ~ "Yes: maintain herd size",
      nextYr_herdChg == "Yes" & nextYr_what == "Unsure" ~ "Yes: unsure of change",
      nextYr_herdChg == "No" ~ "No change"
    )
  )

laborLivestock_4b9a <-nextYr_herdChg %>%
  left_join(base_LABOR %>% select(Ref, labor_migImpactLabor), by = "Ref")

count_laborLivestock_4b9a <- laborLivestock_4b9a %>%
  count(labor_migImpactLabor, plans_comparison, sort = TRUE)
view(count_laborLivestock_4b9a)



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












