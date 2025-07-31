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
  #Who in the family undertake migrations? vs. Does migration impact labor?
  #Columns: labor_whoMigrates/labor_migImpactLabor
labor_long <- base_LABOR %>%
  select(labor_whoMigrates, labor_migImpactLabor) %>%
  filter(!is.na(labor_whoMigrates)) %>%
  mutate(
    labor_whoMigrates = str_replace(labor_whoMigrates, "Husband. 2 sons", "husband, sons")
  ) %>%
  separate_rows(labor_whoMigrates, sep = ",") %>%
  mutate(
    labor_whoMigrates = str_trim(str_to_lower(labor_whoMigrates)),
    labor_whoMigrates = case_when(
      labor_whoMigrates == "grandparents" ~ "grandparent(s), unspecified",
      labor_whoMigrates == "grandmother" ~ "grandparent(s), unspecified",
      labor_whoMigrates == "child" ~ "child(ren), unspecified",
      labor_whoMigrates == "children" ~ "child(ren), unspecified",
      labor_whoMigrates == "son" ~ "son(s)",
      labor_whoMigrates == "2 sons" ~ "son(s)",
      labor_whoMigrates == "3 sons" ~ "son(s)",
      labor_whoMigrates == "son (student)" ~ "son(s)",
      labor_whoMigrates == "son (when he is available)" ~ "son(s)",
      labor_whoMigrates == "son 2" ~ "son(s)",
      labor_whoMigrates == "olders son comes to help" ~ "son(s)",
      labor_whoMigrates == "sons" ~ "son(s)",
      labor_whoMigrates == "daughter" ~ "daughter(s)",
      labor_whoMigrates == "1 daughter" ~ "daughter(s)",
      labor_whoMigrates == "2 daughters" ~ "daughter(s)",
      labor_whoMigrates == "2 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "3 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "4 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "7 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "older brother" ~ "brother(s)",
      labor_whoMigrates == "older brothers help" ~ "brother(s)",
      labor_whoMigrates == "sibling" ~ "sibling(s), unspecified",
      labor_whoMigrates == "siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "2 younger siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "3 siblings-neighbors" ~ "sibling(s), unspecified",
      labor_whoMigrates == "4 younger siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "younger sibling" ~ "sibling(s), unspecified",
      labor_whoMigrates == "younger siblings 2" ~ "sibling(s), unspecified",
      labor_whoMigrates == "grandchild" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "grandchildren" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "grandchidlren" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "1" ~ "person(s), unspecified",
      labor_whoMigrates == "2" ~ "person(s), unspecified",
      labor_whoMigrates == "3" ~ "person(s), unspecified",
      labor_whoMigrates == "4" ~ "person(s), unspecified",
      labor_whoMigrates == "4 people" ~ "person(s), unspecified",
      labor_whoMigrates == "24" ~ "person(s), unspecified",
      labor_whoMigrates == "3-feb" ~ "person(s), unspecified",
      labor_whoMigrates == "6 people" ~ "person(s), unspecified",
      labor_whoMigrates == "everyone 3" ~ "person(s), unspecified",
      labor_whoMigrates == "everyone" ~ "person(s), unspecified",
      labor_whoMigrates == "neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "3 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "2 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "4 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "neighbors 4" ~ "friend/neighbor(s)",
      labor_whoMigrates == "friends" ~ "friend/neighbor(s)",
      labor_whoMigrates == "daughter-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "son-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "father-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "brother-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "extended family members" ~ "extended family/in-laws",
      labor_whoMigrates == "herder children's families" ~ "extended family/in-laws",
      labor_whoMigrates == "in soum" ~ "extended family/in-laws",
      labor_whoMigrates == "school children" ~ "extended family/in-laws",
      labor_whoMigrates == "wide" ~ "extended family/in-laws",
      labor_whoMigrates == "younger brother and his wife" ~ "extended family/in-laws",
      labor_whoMigrates == "extended families" ~ "extended family/in-laws",
      labor_whoMigrates == "assistant herder" ~ "hired help",
      labor_whoMigrates == "herders 4" ~ "hired help",
      labor_whoMigrates == "50 year old" ~ "other",
      labor_whoMigrates == "ask for help" ~ "other",
      labor_whoMigrates == "ask someone for help" ~ "other",
      labor_whoMigrates == "don't move" ~ "other",
      labor_whoMigrates == "me" ~ "just myself",
      labor_whoMigrates == "0" ~ "just myself",
      labor_whoMigrates == "others" ~ "other",
      TRUE ~ labor_whoMigrates
    ),
    labor_migImpactLabor = str_trim(str_to_lower(labor_migImpactLabor)),
    labor_migImpactLabor = case_when(
      labor_migImpactLabor %in% c("yes", "y", "1", "true") ~ "Yes",
      labor_migImpactLabor %in% c("no", "n", "0", "false") ~ "No",
      is.na(labor_migImpactLabor) ~ NA_character_,
      TRUE ~ str_to_title(labor_migImpactLabor)  # keeps values like "Don't know"
    )
  )

impact_by_member <- labor_long %>%
  group_by(labor_whoMigrates, labor_migImpactLabor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(labor_whoMigrates, desc(labor_migImpactLabor))

print(impact_by_member, n = 200)

count_overall <- labor_long %>%
  count(labor_whoMigrates, name = "total_mentions")

impact_wide <- impact_by_member %>%
  filter(labor_migImpactLabor %in% c("Yes", "No")) %>%
  select(labor_whoMigrates, impact = labor_migImpactLabor, n) %>%
  tidyr::pivot_wider(names_from = impact, values_from = n, values_fill = 0) %>%
  mutate(pct_yes = if_else(Yes + No > 0, Yes / (Yes + No), NA_real_)) %>%
  left_join(count_overall, by = "labor_whoMigrates") %>%
  arrange(desc(pct_yes), desc(total_mentions))

print(impact_wide, n = 200)


##3A vs 4B----------------------------------------------------------------------
  #Who in the family undertake migrations? vs. Does migration impact herding practices?
  #Columns: labor_whoMigrates/labor_migImpactPract
labor_long <- base_LABOR %>%
  select(labor_whoMigrates, labor_migImpactPract) %>%
  filter(!is.na(labor_whoMigrates)) %>%
  mutate(
    labor_whoMigrates = str_replace(labor_whoMigrates, "Husband. 2 sons", "husband, sons")
  ) %>%
  separate_rows(labor_whoMigrates, sep = ",") %>%
  mutate(
    labor_whoMigrates = str_trim(str_to_lower(labor_whoMigrates)),
    labor_whoMigrates = case_when(
      labor_whoMigrates == "grandparents" ~ "grandparent(s), unspecified",
      labor_whoMigrates == "grandmother" ~ "grandparent(s), unspecified",
      labor_whoMigrates == "child" ~ "child(ren), unspecified",
      labor_whoMigrates == "children" ~ "child(ren), unspecified",
      labor_whoMigrates == "son" ~ "son(s)",
      labor_whoMigrates == "2 sons" ~ "son(s)",
      labor_whoMigrates == "3 sons" ~ "son(s)",
      labor_whoMigrates == "son (student)" ~ "son(s)",
      labor_whoMigrates == "son (when he is available)" ~ "son(s)",
      labor_whoMigrates == "son 2" ~ "son(s)",
      labor_whoMigrates == "olders son comes to help" ~ "son(s)",
      labor_whoMigrates == "sons" ~ "son(s)",
      labor_whoMigrates == "daughter" ~ "daughter(s)",
      labor_whoMigrates == "1 daughter" ~ "daughter(s)",
      labor_whoMigrates == "2 daughters" ~ "daughter(s)",
      labor_whoMigrates == "2 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "3 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "4 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "7 children" ~ "child(ren), unspecified",
      labor_whoMigrates == "older brother" ~ "brother(s)",
      labor_whoMigrates == "older brothers help" ~ "brother(s)",
      labor_whoMigrates == "sibling" ~ "sibling(s), unspecified",
      labor_whoMigrates == "siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "2 younger siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "3 siblings-neighbors" ~ "sibling(s), unspecified",
      labor_whoMigrates == "4 younger siblings" ~ "sibling(s), unspecified",
      labor_whoMigrates == "younger sibling" ~ "sibling(s), unspecified",
      labor_whoMigrates == "younger siblings 2" ~ "sibling(s), unspecified",
      labor_whoMigrates == "grandchild" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "grandchildren" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "grandchidlren" ~ "grandchild(ren), unspecified",
      labor_whoMigrates == "1" ~ "person(s), unspecified",
      labor_whoMigrates == "2" ~ "person(s), unspecified",
      labor_whoMigrates == "3" ~ "person(s), unspecified",
      labor_whoMigrates == "4" ~ "person(s), unspecified",
      labor_whoMigrates == "4 people" ~ "person(s), unspecified",
      labor_whoMigrates == "24" ~ "person(s), unspecified",
      labor_whoMigrates == "3-feb" ~ "person(s), unspecified",
      labor_whoMigrates == "6 people" ~ "person(s), unspecified",
      labor_whoMigrates == "everyone 3" ~ "person(s), unspecified",
      labor_whoMigrates == "everyone" ~ "person(s), unspecified",
      labor_whoMigrates == "neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "3 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "2 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "4 neighbors" ~ "friend/neighbor(s)",
      labor_whoMigrates == "neighbors 4" ~ "friend/neighbor(s)",
      labor_whoMigrates == "friends" ~ "friend/neighbor(s)",
      labor_whoMigrates == "daughter-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "son-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "father-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "brother-in-law" ~ "extended family/in-laws",
      labor_whoMigrates == "extended family members" ~ "extended family/in-laws",
      labor_whoMigrates == "herder children's families" ~ "extended family/in-laws",
      labor_whoMigrates == "in soum" ~ "extended family/in-laws",
      labor_whoMigrates == "school children" ~ "extended family/in-laws",
      labor_whoMigrates == "wide" ~ "extended family/in-laws",
      labor_whoMigrates == "younger brother and his wife" ~ "extended family/in-laws",
      labor_whoMigrates == "extended families" ~ "extended family/in-laws",
      labor_whoMigrates == "assistant herder" ~ "hired help",
      labor_whoMigrates == "herders 4" ~ "hired help",
      labor_whoMigrates == "50 year old" ~ "other",
      labor_whoMigrates == "ask for help" ~ "other",
      labor_whoMigrates == "ask someone for help" ~ "other",
      labor_whoMigrates == "don't move" ~ "other",
      labor_whoMigrates == "me" ~ "just myself",
      labor_whoMigrates == "0" ~ "just myself",
      labor_whoMigrates == "others" ~ "other",
      TRUE ~ labor_whoMigrates
    ),
    labor_migImpactPract = str_trim(str_to_lower(labor_migImpactPract)),
    labor_migImpactPract = case_when(
      labor_migImpactPract %in% c("yes", "y", "1", "true") ~ "Yes",
      labor_migImpactPract %in% c("no", "n", "0", "false") ~ "No",
      is.na(labor_migImpactPract) ~ NA_character_,
      TRUE ~ str_to_title(labor_migImpactPract)  # keeps values like "Don't know"
    )
  )

impact_by_member2 <- labor_long %>%
  group_by(labor_whoMigrates, labor_migImpactPract) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(labor_whoMigrates, desc(labor_migImpactPract))

print(impact_by_member, n = 200)

count_overall <- labor_long %>%
  count(labor_whoMigrates, name = "total_mentions")

impact_wide2 <- impact_by_member2 %>%
  filter(labor_migImpactPract %in% c("Yes", "No")) %>%
  select(labor_whoMigrates, impact = labor_migImpactPract, n) %>%
  tidyr::pivot_wider(names_from = impact, values_from = n, values_fill = 0) %>%
  mutate(pct_yes = if_else(Yes + No > 0, Yes / (Yes + No), NA_real_)) %>%
  left_join(count_overall, by = "labor_whoMigrates") %>%
  arrange(desc(pct_yes), desc(total_mentions))

print(impact_wide2, n = 200)


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










































