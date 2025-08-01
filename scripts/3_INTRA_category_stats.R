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
      TRUE ~ str_to_title(labor_migImpactLabor)
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


##3B vs. 4A---------------------------------------------------------------------
#How many family members undertake migrations? vs. Does migration impact labor?
#Columns: labor_numMigrates/labor_migImpactLabor

num_vs_impact1 <- base_LABOR %>%
  select(labor_numMigrates, labor_migImpactLabor) %>%
  mutate(
    labor_numMigrates = readr::parse_number(as.character(labor_numMigrates)),
    labor_migImpactLabor = str_trim(str_to_lower(labor_migImpactLabor)),
    labor_migImpactLabor = case_when(
      labor_migImpactLabor %in% c("yes", "y", "1", "true") ~ "Yes",
      labor_migImpactLabor %in% c("no",  "n", "0", "false") ~ "No",
      is.na(labor_migImpactLabor) ~ NA_character_,
      TRUE ~ str_to_title(labor_migImpactLabor)
    )
  ) %>%
  filter(!is.na(labor_numMigrates))

impact_by_number1 <- num_vs_impact1 %>%
  group_by(labor_numMigrates, labor_migImpactLabor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(labor_numMigrates, desc(labor_migImpactLabor))

print(impact_by_number1, n = 200)


##3B vs. 4B---------------------------------------------------------------------
#How many family members undertake migrations? vs. Does migration impact herding practices?
#Columns: labor_numMigrates/labor_migImpactPract
num_vs_impact2 <- base_LABOR %>%
  select(labor_numMigrates, labor_migImpactPract) %>%
  mutate(
    labor_numMigrates = readr::parse_number(as.character(labor_numMigrates)),
    labor_migImpactPract = str_trim(str_to_lower(labor_migImpactPract)),
    labor_migImpactPract = case_when(
      labor_migImpactPract %in% c("yes", "y", "1", "true") ~ "Yes",
      labor_migImpactPract %in% c("no",  "n", "0", "false") ~ "No",
      is.na(labor_migImpactPract) ~ NA_character_,
      TRUE ~ str_to_title(labor_migImpactPract)
    )
  ) %>%
  filter(!is.na(labor_numMigrates))

impact_by_number2 <- num_vs_impact2 %>%
  group_by(labor_numMigrates, labor_migImpactPract) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(labor_numMigrates, desc(labor_migImpactPract))

print(impact_by_number2, n = 200)



##3A vs. 5A---------------------------------------------------------------------
  #How many people undertake migrations? vs. Do you hire labor?
  #Columns: labor_whoMigrates/labor_hire
num_vs_impact3 <- base_LABOR %>%
  select(labor_numMigrates, labor_hire) %>%
  mutate(
    labor_numMigrates = readr::parse_number(as.character(labor_numMigrates)),
    labor_hire = str_trim(str_to_lower(labor_hire)),
    labor_hire = case_when(
      labor_hire %in% c("yes", "y", "1", "true") ~ "Yes",
      labor_hire %in% c("no",  "n", "0", "false") ~ "No",
      is.na(labor_hire) ~ NA_character_,
      TRUE ~ str_to_title(labor_hire)
    )
  ) %>%
  filter(!is.na(labor_numMigrates))

impact_by_number3 <- num_vs_impact3 %>%
  group_by(labor_numMigrates, labor_hire) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(labor_numMigrates, desc(labor_hire))

print(impact_by_number3, n = 200)







#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

###ALTERNATIVE LIVELIHOODS------------------------------------------------------
#-------------------------------------------------------------------------------
##1A vs. 3A---------------------------------------------------------------------
  #Is someone in the household doing non-herding work?  vs. Number of loans taken out per year?
  #Columns: altLife_nonHerdWork/altLife_loansPerYr
nonHerd_loans <- base_ALTLIFE %>%
  select(altLife_loansPerYr, altLife_nonHerdWork) %>%
  mutate(
    altLife_loansPerYr = readr::parse_number(as.character(altLife_loansPerYr)),
    altLife_nonHerdWork = str_trim(str_to_lower(altLife_nonHerdWork)),
    altLife_nonHerdWork = case_when(
      altLife_nonHerdWork %in% c("yes", "y", "1", "true") ~ "Yes",
      altLife_nonHerdWork %in% c("no",  "n", "0", "false") ~ "No",
      is.na(altLife_nonHerdWork) ~ NA_character_,
      TRUE ~ str_to_title(altLife_nonHerdWork)
    )
  ) %>%
  filter(!is.na(altLife_loansPerYr))

nonHerd_loans <- nonHerd_loans %>%
  group_by(altLife_loansPerYr, altLife_nonHerdWork) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(altLife_loansPerYr, desc(altLife_nonHerdWork))

print(nonHerd_loans, n = 200)


##1A vs. 3C---------------------------------------------------------------------
  #Is someone in the household doing non-herding work?  vs. When do you typically need loans?
  #Columns: altLife_nonHerdWork/altLife_loansWhenNeed

count_loansWhenNeed <- base_ALTLIFE %>%
  select(altLife_loansWhenNeed, altLife_nonHerdWork) %>%
  filter(!is.na(altLife_loansWhenNeed)) %>%
  mutate(
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter and spring", "winter, spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter and Spring", "winter, spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "august- for school year and lunar new year", "autumn, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter-fodder", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winter-fodder", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter- require fodder", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "and Winter", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winter and petrol", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "february", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "February", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "After December", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter and preparation of fodder", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Dec", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "december", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winter-winter for fodder", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winterember", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter Nov-March", "winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winter and spring", "winter, spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "winter and Lunar new year", "winter, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter and Lunar new year", "winter, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Winter and Lunar New Year", "winter, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "spring and autumn", "spring, autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "spring-fodder", "spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Spring- before cashmere collection", "spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Lunar new year-spring", "spring, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "summer and winter", "summer, winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Sep-fodder", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn-fodder", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "prepare fodder in Autumn", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "August", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn- school year", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn- school new year", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Aug", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn-sep", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn-sep", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "October", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "September", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn- student tuition fee payment and buying fodder", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn- school year starts", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "september-school year", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn-school new year", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn-school year starts", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn -sep", "autumn"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn- for school year and lunar new year", "autumn, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn and Lunar New Year", "autumn, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn and during lunar new year", "autumn, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Lunar New Year and Sept", "autumn, lunar new year"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn and spring-lunar new year", "autumn, lunar new year, spring"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autum and winter", "autumn, winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "september and during harsh winter", "autumn, winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "autumn and winter", "autumn, winter"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Lunar New year and need of fodder", "lunar new year"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "If livestock is weak", "depends on needs"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "depends on demands", "depends on needs"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "0", "never"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "None", "never"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "one", "never"),
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "During migration and need of fodder", "during migration, depends on needs"), 
    altLife_loansWhenNeed = str_replace(altLife_loansWhenNeed, "Autumn and during medical treatment", "autumn, during medical treatment")
  ) %>%
  separate_rows(altLife_loansWhenNeed, sep = ",") %>%
  mutate(
    altLife_loansWhenNeed = str_trim(altLife_loansWhenNeed),
    altLife_loansWhenNeed = str_to_lower(altLife_loansWhenNeed),
    altLife_nonHerdWork = str_trim(str_to_lower(altLife_nonHerdWork)),
    altLife_nonHerdWork = case_when(
      altLife_nonHerdWork %in% c("yes", "y", "1", "true") ~ "Yes",
      altLife_nonHerdWork %in% c("no", "n", "0", "false") ~ "No",
    is.na(altLife_nonHerdWork) ~ NA_character_,
  TRUE ~ str_to_title(altLife_nonHerdWork)  # keeps values like "Don't know"
)
)

  
count_loansWhenNeed <- count_loansWhenNeed %>%
  group_by(altLife_loansWhenNeed, altLife_nonHerdWork) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(altLife_loansWhenNeed, desc(altLife_nonHerdWork))

print(count_loansWhenNeed, n = 200)
  
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

###HERD MANAGEMENT--------------------------------------------------------------
#-------------------------------------------------------------------------------
##5B vs. 9A---------------------------------------------------------------------
  # What is the average distance of moves, now vs. 10yrs ago?  vs. Changed management practices (yes/no):
  # Columns: herdMgmt_avgDistMoves/herdMgmt_10yrs_avgMoveDist/herdMgmt_past5Yrs_mgmtChanges
herdMove_changePrac <- base_HERDMGMT %>%
  select(herdMgmt_10yrs_avgMoveDist, herdMgmt_avgDistMoves, herdMgmt_past5Yrs_mgmtChanges) %>%
  mutate(
    dist_10yrs = parse_number(as.character(herdMgmt_10yrs_avgMoveDist)),
    dist_now   = parse_number(as.character(herdMgmt_avgDistMoves)),
    move_diff  = dist_now - dist_10yrs,  # >0 means moved more now
    move_category = case_when(
      is.na(dist_10yrs) | is.na(dist_now) ~ NA_character_,
      move_diff > 0 ~ "moved less 10yrs ago than last year",
      move_diff < 0 ~ "moved more 10yrs ago than last year",
      TRUE ~ "moved an equal amount of distance"
    ),
    practices_chgd = str_trim(str_to_lower(herdMgmt_past5Yrs_mgmtChanges)),
    practices_chgd = case_when(
      practices_chgd %in% c("yes", "y", "1", "true") ~ "Yes",
      practices_chgd %in% c("no",  "n", "0", "false") ~ "No",
      is.na(practices_chgd) ~ NA_character_,
      TRUE ~ str_to_title(practices_chgd)
    )
  ) %>%
  filter(!is.na(move_category))

impact_by_movement <- herdMove_changePrac %>%
  group_by(move_category, practices_chgd) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(move_category, desc(practices_chgd))

print(impact_by_movement, n = 200)


##9A vs. 11A--------------------------------------------------------------------
  # Changed management practices (yes/no):  vs. Are there changes you want to make to your management practices but can’t?
  # Columns: herdMgmt_past5Yrs_mgmtChanges/herdMgmt_whatChanges_cantMake
change_compare <- base_HERDMGMT %>%
  select(herdMgmt_past5Yrs_mgmtChanges, herdMgmt_whatChanges_cantMake) %>%
  mutate(
    made_changes = str_trim(str_to_lower(herdMgmt_past5Yrs_mgmtChanges)),
    made_changes = case_when(
      made_changes %in% c("yes", "y", "1", "true") ~ "Yes",
      made_changes %in% c("no", "n", "0", "false") ~ "No",
      TRUE ~ str_to_title(made_changes)
    ),
    cant_make_changes = str_trim(str_to_lower(herdMgmt_whatChanges_cantMake)),
    cant_make_changes = case_when(
      cant_make_changes %in% c("yes", "y", "1", "true") ~ "Yes",
      cant_make_changes %in% c("no", "n", "0", "false") ~ "No",
      TRUE ~ str_to_title(cant_make_changes)
    )
  ) %>%
  count(made_changes, cant_make_changes, sort = TRUE)
print(change_compare)


##10A vs. 12--------------------------------------------------------------------
  # Planning to change management practices (yes/no):  vs. Condition and degree of pastoral change:
  # Columns: herdMgmt_next5Yrs_mgmtChanges/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg
mgmt_next_vs_condition <- base_HERDMGMT %>%
  mutate(
    yn  = str_to_title(str_trim(herdMgmt_pastureCon_chg_yn)),
    deg = str_to_title(str_trim(herdMgmt_pastureCon_chg_deg)),
    condition_comparison = case_when(
      yn == "Degraded"  & deg == "Slight"      ~ "Slightly Degraded",
      yn == "Degraded"  & deg == "Medium"      ~ "Moderately Degraded",
      yn == "Degraded"  & deg == "Substantial" ~ "Substantially Degraded",
      yn == "Improved"  & deg == "Slight"      ~ "Slightly Improved",
      yn == "Improved"  & deg == "Medium"      ~ "Moderately Improved",
      yn == "Improved"  & deg == "Substantial" ~ "Substantially Improved",
      yn == "No Change"                        ~ "No change",
      TRUE ~ NA_character_
    ),
    next5yrs_changes = str_to_lower(str_trim(herdMgmt_next5Yrs_mgmtChanges)),
    next5yrs_changes = case_when(
      next5yrs_changes %in% c("yes", "y", "1", "true") ~ "Yes",
      next5yrs_changes%in% c("no",  "n", "0", "false") ~ "No",
      is.na(next5yrs_changes) ~ NA_character_,
      TRUE ~ str_to_title(next5yrs_changes)  # keeps "Don't know", etc.
    )
  )
condition_order <- c(
  "Substantially Degraded","Moderately Degraded","Slightly Degraded",
  "No change",
  "Slightly Improved","Moderately Improved","Substantially Improved"
)
mgmt_next_vs_condition <- mgmt_next_vs_condition %>%
  mutate(condition_comparison = factor(condition_comparison, levels = condition_order))

next5_by_condition <- mgmt_next_vs_condition %>%
  filter(!is.na(condition_comparison)) %>%
  group_by(condition_comparison, next5yrs_changes) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(condition_comparison, desc(next5yrs_changes))

print(next5_by_condition, n = 200)


##11A vs. 12--------------------------------------------------------------------
  # 11A. Are there changes you want to make to your management practices but can’t? vs. Condition and degree of pastoral change:
  # Columns: herdMgmt_whatChanges_cantMake/herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg
chgDesire_vs_condition <- base_HERDMGMT %>%
  mutate(
    yn  = str_to_title(str_trim(herdMgmt_pastureCon_chg_yn)),
    deg = str_to_title(str_trim(herdMgmt_pastureCon_chg_deg)),
    condition_comparison = case_when(
      yn == "Degraded"  & deg == "Slight"      ~ "Slightly Degraded",
      yn == "Degraded"  & deg == "Medium"      ~ "Moderately Degraded",
      yn == "Degraded"  & deg == "Substantial" ~ "Substantially Degraded",
      yn == "Improved"  & deg == "Slight"      ~ "Slightly Improved",
      yn == "Improved"  & deg == "Medium"      ~ "Moderately Improved",
      yn == "Improved"  & deg == "Substantial" ~ "Substantially Improved",
      yn == "No Change"                        ~ "No change",
      TRUE ~ NA_character_
    ),
    chgDesire = str_to_lower(str_trim(herdMgmt_whatChanges_cantMake)),
    chgDesire = case_when(
      chgDesire %in% c("yes", "y", "1", "true") ~ "Yes",
      chgDesire%in% c("no",  "n", "0", "false") ~ "No",
      is.na(chgDesire) ~ NA_character_,
      TRUE ~ str_to_title(chgDesire)  # keeps "Don't know", etc.
    )
  )
condition_order <- c(
  "Substantially Degraded","Moderately Degraded","Slightly Degraded",
  "No change",
  "Slightly Improved","Moderately Improved","Substantially Improved"
)
chgDesire_vs_condition <- chgDesire_vs_condition %>%
  mutate(condition_comparison = factor(condition_comparison, levels = condition_order))

chgDesire_vs_condition <- chgDesire_vs_condition %>%
  filter(!is.na(condition_comparison)) %>%
  group_by(condition_comparison, chgDesire) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n), pct = n / total) %>%
  ungroup() %>%
  arrange(condition_comparison, desc(chgDesire))

print(chgDesire_vs_condition, n = 200)





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




























































