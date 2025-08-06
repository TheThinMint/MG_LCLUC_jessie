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
  mutate(
    herdDist_daily = cut(
      herdMgmt_dailyDist,
      breaks = c(0, 6, 11, 16, 21, Inf),
      labels = c("0–5 km", "6–10 km", "11–15 km", "16–20 km", "21+ km"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(labor_numMigrates, herdDist_daily, sort = TRUE)

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
  left_join(base_LABOR %>% select(Ref, labor_migImpactPract), by = "Ref")

count_laborLivestock_4b9a <- laborLivestock_4b9a %>%
  count(labor_migImpactPract, plans_comparison, sort = TRUE)

kable(count_laborLivestock_4b9a)



##LABOR_5A vs. ALTLIVELIHOODS_1A---------------------------------------
#Do you hire labor? vs. Is someone in the household doing non-herding work?
#Columns: labor_hire/altLife_nonHerdWork
hireLabor <- base_LABOR %>%
  select(Ref, labor_hire)

laborAltLife_5a1a <- hireLabor %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_nonHerdWork), by = "Ref")

count_laborAltLife_5a1a <- laborAltLife_5a1a %>%
  count(labor_hire, altLife_nonHerdWork, sort = TRUE)

kable(count_laborAltLife_5a1a)






##LABOR_5A vs. ALTLIVELIHOODS_3A---------------------------------------
#Do you hire labor? vs. Number of loans taken out per year?
#Columns: labor_hire/altLife_loansPerYr
hireLabor2 <- base_LABOR %>%
  select(Ref, labor_hire)

laborAltLife_5a3a <- hireLabor2 %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_loansPerYr), by = "Ref")

count_laborAltLife_5a3a <- laborAltLife_5a3a %>%
  count(labor_hire, altLife_loansPerYr, sort = TRUE)

kable(count_laborAltLife_5a3a)


##LABOR_5A vs. HERDMGMT_9A---------------------------------------
#Do you hire labor? vs. In the past five years, have you changed your herding management practices?
#Columns: labor_hire/herdMgmt_past5Yrs_mgmtChanges
hireLabor3 <- base_LABOR %>%
  select(Ref, labor_hire)

laborHerdMgmt_5a9a <- hireLabor3 %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref")

count_laborHerdMgmt_5a9a <- laborHerdMgmt_5a9a %>%
  count(labor_hire, herdMgmt_past5Yrs_mgmtChanges, sort = TRUE)

kable(count_laborHerdMgmt_5a9a)




##LABOR_5A vs. HERDMGMT_10A---------------------------------------
#Do you hire labor? vs. Do you have plans to make changes to management practices in the next 5years?
#Columns: labor_hire/herdMgmt_next5Yrs_mgmtChanges
hireLabor4 <- base_LABOR %>%
  select(Ref, labor_hire)

laborHerdMgmt_5a10a <- hireLabor4 %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_next5Yrs_mgmtChanges), by = "Ref")

count_laborHerdMgmt_5a10a <- laborHerdMgmt_5a10a %>%
  count(labor_hire, herdMgmt_next5Yrs_mgmtChanges, sort = TRUE)

kable(count_laborHerdMgmt_5a10a)


##LABOR_5A vs. LIVESTOCK_5A---------------------------------------
#Do you hire labor? vs. Have you noticed any long term shifts in vegetation/forage?
#Columns: labor_hire/vegShifts_yn/vegShifts_quanQual
pastureChg <- base_LIVESTOCK %>% 
  select(Ref, vegShifts_yn, vegShifts_quanQual) %>%
  mutate(
    condition_comparison = case_when(
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Quantity" ~ "Yes: Quantity",
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Quality" ~ "Yes: Quality",
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Both" ~ "Yes: Both",
      vegShifts_yn == "No" ~ "No change",
      TRUE ~ "Unclear"
    ))

laborLivestock_5a5a <- pastureChg %>%
  left_join(base_LABOR %>% select(Ref, labor_hire), by = "Ref")

count_laborLivestock_5a5a <- laborLivestock_5a5a %>%
  count(labor_hire, condition_comparison, sort = TRUE)

kable(count_laborLivestock_5a5a)





##LABOR_5A vs. LIVESTOCK_6A---------------------------------------
#Do you hire labor? vs. Has your herd size changed over the last five years?
#Columns: labor_hire/past5yrs_herdsize
hireLabor5 <- base_LABOR %>%
  select(Ref, labor_hire)

laborLivestock_5a6a <- hireLabor5 %>%
  left_join(base_LIVESTOCK %>% select(Ref, past5yrs_herdsize), by = "Ref")

count_laborLivestock_5a6a <- laborLivestock_5a6a %>%
  count(labor_hire, past5yrs_herdsize, sort = TRUE)

kable(count_laborLivestock_5a6a)




##LABOR_5A vs. LIVESTOCK_9A---------------------------------------
#Do you hire labor? vs. Do you have plans to substantially change the size of your herd? 
#Columns: labor_hire/nextYr_herdChg
nextYr_herdChg2 <- base_LIVESTOCK %>%
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

laborLivestock_5a9a <-nextYr_herdChg2 %>%
  left_join(base_LABOR %>% select(Ref, labor_hire), by = "Ref")

count_laborLivestock_5a9a <- laborLivestock_5a9a %>%
  count(labor_hire, plans_comparison, sort = TRUE)

kable(count_laborLivestock_5a9a)


##ALTLIVELIHOODS_1A vs. LIVESTOCK_2A---------------------------------------
#Is someone in the household doing non-herding work? vs. OVERALL SFU.
#Columns: altLife_nonHerdWork/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
# Step 1: Calculate SFU totals
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
  pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

# Step 2: Compare SFU between years
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

# Step 3: Join with alternative livelihood data
altLifeLivestock_1a2a <- SFU_count %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_nonHerdWork), by = "Ref") %>%
  mutate(altLife_nonHerdWork = str_to_lower(str_trim(altLife_nonHerdWork)))

# Step 4: Group SFU_2023 into bins and count by alt livelihood + SFU group
count_altLifeLivestock_1a2a <- altLifeLivestock_1a2a %>%
  mutate(
    SFU_group = cut(
      SFU_2023,
      breaks = c(0, 200, 400, 600, 800, 1000, Inf),
      labels = c(
        "0–200 SFU", "200–400 SFU", "400–600 SFU",
        "600–800 SFU", "800–1000 SFU", "1000+ SFU"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(altLife_nonHerdWork, SFU_group, sort = TRUE) %>% 
  arrange(desc(n)) 

# Step 5: Display table
kable(count_altLifeLivestock_1a2a)






##ALTLIVELIHOODS_1A vs. LIVESTOCK_6A---------------------------------------
#Is someone in the household doing non-herding work? vs. Has your herd size changed over the last five years?
#Columns: altLife_nonHerdWork/past5yrs_herdsize
nonHerdWork <- base_ALTLIFE %>%
  select(Ref, altLife_nonHerdWork)

altLifeLivestock_1a6a <- nonHerdWork %>%
  left_join(base_LIVESTOCK %>% select(Ref, past5yrs_herdsize), by = "Ref")

count_altLifeLivestock_1a6a <- altLifeLivestock_1a6a %>%
  count(altLife_nonHerdWork, past5yrs_herdsize, sort = TRUE)

kable(count_altLifeLivestock_1a6a)


##ALTLIVELIHOODS_1A vs. LIVESTOCK_9A---------------------------------------
#Is someone in the household doing non-herding work? vs. Do you have plans to substantially change the size of your herd? 
#Columns: altLife_nonHerdWork/nextYr_herdChg
nextYr_herdChg3 <- base_LIVESTOCK %>%
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

altLifeLivestock_1a9a <-nextYr_herdChg3 %>%
  left_join(base_ALTLIFE %>% select(Ref, altLife_nonHerdWork), by = "Ref")

count_altLifeLivestock_1a9a <- altLifeLivestock_1a9a %>%
  count(altLife_nonHerdWork, plans_comparison, sort = TRUE)

kable(count_altLifeLivestock_1a9a)


##ALTLIVELIHOODS_3A vs. HERDMGMT_9A---------------------------------------
#Number of loans taken out per year? vs. In the past five years, have you changed your herding management practices?
#Columns: altLife_loansPerYr/herdMgmt_past5Yrs_mgmtChanges
loansPerYr <- base_ALTLIFE %>%
  select(Ref, altLife_loansPerYr)

altLifeHerdMgmt_3a9a <- loansPerYr %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref")

count_altLifeHerdMgmt_3a9a <- altLifeHerdMgmt_3a9a %>%
  count(herdMgmt_past5Yrs_mgmtChanges, altLife_loansPerYr, sort = TRUE)

kable(count_altLifeHerdMgmt_3a9a)






##ALTLIVELIHOODS_3A vs. LIVESTOCK_3A---------------------------------------
#Number of loans taken out per year? vs. Did you purchase supplemental fodder last year?
#Columns: altLife_loansPerYr/lastYr_fodder
loansPerYr2 <- base_ALTLIFE %>%
  select(Ref, altLife_loansPerYr)

altLifeLivestock_3a3a <- loansPerYr2 %>%
  left_join(base_LIVESTOCK %>% select(Ref, lastYr_fodder), by = "Ref")

count_altLifeLivestock_3a3a <- altLifeLivestock_3a3a %>%
  count(lastYr_fodder, altLife_loansPerYr, sort = TRUE)

kable(count_altLifeLivestock_3a3a)


##ALTLIVELIHOODS_3A vs. LIVESTOCK_4A---------------------------------------
#Number of loans taken out per year? vs. Do you plan to purchase supplemental fodder this year? 
#Columns: altLife_loansPerYr/thisYr_fodder
loansPerYr3 <- base_ALTLIFE %>%
  select(Ref, altLife_loansPerYr)

altLifeLivestock_3a4a <- loansPerYr3 %>%
  left_join(base_LIVESTOCK %>% select(Ref, thisYr_fodder), by = "Ref")

count_altLifeLivestock_3a4a <- altLifeLivestock_3a4a %>%
  count(thisYr_fodder, altLife_loansPerYr, sort = TRUE)

kable(count_altLifeLivestock_3a4a)


##ALTLIVELIHOODS_3A vs. LIVESTOCK_6A---------------------------------------
#Number of loans taken out per year? vs. Has your herd size changed over the last five years?
#Columns: altLife_loansPerYr/past5yrs_herdsize
loansPerYr4 <- base_ALTLIFE %>%
  select(Ref, altLife_loansPerYr)

altLifeLivestock_3a6a <- loansPerYr4 %>%
  left_join(base_LIVESTOCK %>% select(Ref, past5yrs_herdsize), by = "Ref")

count_altLifeLivestock_3a6a <- altLifeLivestock_3a6a %>%
  count(past5yrs_herdsize, altLife_loansPerYr, sort = TRUE)

view(count_altLifeLivestock_3a6a)







##HERDMGMT_1A vs. LIVESTOCK_5A---------------------------------------
#Distance for daily herding, generally vs. Have you noticed any long term shifts in vegetation/forage?
#Columns: herdMgmt_dailyDist/vegShifts_yn/vegShifts_quanQual
pastureChg2 <- base_LIVESTOCK %>% 
  select(Ref, vegShifts_yn, vegShifts_quanQual) %>%
  mutate(
    condition_comparison = case_when(
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Quantity" ~ "Yes: Quantity",
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Quality" ~ "Yes: Quality",
      vegShifts_yn == "Yes" & vegShifts_quanQual == "Both" ~ "Yes: Both",
      vegShifts_yn == "No" ~ "No change",
      TRUE ~ "Unclear"
    ))

herdMgmtLivestock_1a5a <- pastureChg2 %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_dailyDist), by = "Ref")

count_herdMgmtLivestock_1a5a <- herdMgmtLivestock_1a5a %>%
  mutate(
    herdDist_daily = cut(
      herdMgmt_dailyDist,
      breaks = c(0, 6, 11, 16, 21, Inf),
      labels = c("0–5 km", "6–10 km", "11–15 km", "16–20 km", "21+ km"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(condition_comparison, herdDist_daily, sort = TRUE) %>% 
  arrange(desc(n))

kable(count_herdMgmtLivestock_1a5a)


##HERDMGMT_9A vs. LIVESTOCK_2A---------------------------------------
#In the past five years, have you changed your herding management practices? vs.  OVERALL SFU
#Columns: herdMgmt_past5Yrs_mgmtChanges/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
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
  pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

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

herdMgmtLivestock_9a2a <- SFU_count %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref") %>%
  mutate(herdMgmt_past5Yrs_mgmtChanges = str_to_lower(str_trim(herdMgmt_past5Yrs_mgmtChanges)))

count_herdMgmtLivestock_9a2a <- herdMgmtLivestock_9a2a %>%
  mutate(
    SFU_group = cut(
      SFU_2023,
      breaks = c(0, 200, 400, 600, 800, 1000, Inf),
      labels = c(
        "0–200 SFU", "200–400 SFU", "400–600 SFU",
        "600–800 SFU", "800–1000 SFU", "1000+ SFU"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(herdMgmt_past5Yrs_mgmtChanges, SFU_group, sort = TRUE) %>% 
  arrange(desc(n)) 

kable(count_herdMgmtLivestock_9a2a)






##HERDMGMT_9A vs. LIVESTOCK_2D---------------------------------------
#In the past five years, have you changed your herding management practices? vs. SFU delta change
#Columns: herdMgmt_past5Yrs_mgmtChanges/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
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
  pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

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

herdMgmtLivestock_9a2d <- SFU_count %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref") %>%
  mutate(herdMgmt_past5Yrs_mgmtChanges = str_to_lower(str_trim(herdMgmt_past5Yrs_mgmtChanges)))

count_herdMgmtLivestock_9a2d <- herdMgmtLivestock_9a2d %>%
  mutate(
    SFU_delta = cut(
      delta,
      breaks = c(-5410, -5000, -4000, -3000, -2000, -1000, -800, -600, -400, -200, 0, 200, 400, 600, 800, 1000, Inf),
      labels = c(
        "<-5000 SFU", 
        "-5000-4000 SFU", 
        "-4000-3000 SFU", 
        "-3000-2000 SFU", 
        "-2000-1000 SFU", 
        "-1000-800 SFU", 
        "-800-600 SFU", 
        "-600-400 SFU", 
        "-400-200 SFU", 
        "-200-0 SFU", 
        "0–200 SFU", 
        "200–400 SFU", 
        "400–600 SFU",
        "600–800 SFU", 
        "800–1000 SFU", 
        "1000+ SFU"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(herdMgmt_past5Yrs_mgmtChanges, SFU_delta, sort = TRUE) %>% 
  arrange(desc(n)) 

kable(count_herdMgmtLivestock_9a2d)




##HERDMGMT_9A vs. LIVESTOCK_2E---------------------------------------
#In the past five years, have you changed your herding management practices? vs. SFU delta change, simplified
#Columns: herdMgmt_past5Yrs_mgmtChanges/livestock_2023_camel/livestock_2023_cow/livestock_2023_horse/livestock_2023_sheep/livestock_2023_g……and so on
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
  pivot_wider(names_from = year, values_from = SFU, names_prefix = "SFU_")

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

herdMgmtLivestock_9a2e <- SFU_count %>%
  left_join(base_HERDMGMT %>% select(Ref, herdMgmt_past5Yrs_mgmtChanges), by = "Ref") %>%
  mutate(herdMgmt_past5Yrs_mgmtChanges = str_to_lower(str_trim(herdMgmt_past5Yrs_mgmtChanges)))

count_herdMgmtLivestock_9a2e <- herdMgmtLivestock_9a2e %>%
  count(herdMgmt_past5Yrs_mgmtChanges, SFU_comparison, sort = TRUE) %>% 
  arrange(desc(n)) 

kable(count_herdMgmtLivestock_9a2e)





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












