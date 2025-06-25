# basic summary stats for initial investigation
# exploring questions WITHIN dataframes created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)



### base_LABOR -----------------------------------------------------------------
skim(base_LABOR)

## Who does the daily moves (labor_whoMovesDaily) (broken up by soum)
count_dailyMoves <- base_LABOR %>%
  select(Soum, labor_whoMovesDaily) %>%
  filter(!is.na(labor_whoMovesDaily)) %>%
  separate_rows(labor_whoMovesDaily, sep = ",") %>%
  mutate(
    labor_whoMovesDaily = str_trim(labor_whoMovesDaily),
    labor_whoMovesDaily = str_to_lower(labor_whoMovesDaily),
    labor_whoMovesDaily = case_when(
      labor_whoMovesDaily == "grandparents" ~ "grandparent, unspecified",
      labor_whoMovesDaily == "child" ~ "child, unspecified",
      labor_whoMovesDaily == "sibling" ~ "sibling, unspecified",
      labor_whoMovesDaily == "grandchild" ~ "grandchild, unspecified",
      TRUE ~ labor_whoMovesDaily
    )
  ) %>%
  count(Soum, labor_whoMovesDaily, sort = TRUE)

count_wide <- count_dailyMoves %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  )

print(count_wide, n = 30)



## Who undertakes migrations (labor_whoMigrates) 
relationship_order <- c("husband", "wife", "son(s)", "daughter(s)", "child(ren), unspecified", "brother(s)", "sibling(s), unspecified", "father",
  "mother", "grandparent(s), unspecified", "grandchild(ren), unspecified", "household head", "extended family/in-laws", "friend/neighbor(s)",
  "person(s), unspecified", "hired help", "just myself", "other", "husband. 2 sons")

count_whoMigrates <- base_LABOR %>%
  select(Soum, labor_whoMigrates) %>%
  filter(!is.na(labor_whoMigrates)) %>%
  mutate(labor_whoMigrates = str_replace(labor_whoMigrates, "Husband. 2 sons", "husband, sons")
  ) %>%
  separate_rows(labor_whoMigrates, sep = ",") %>%
  mutate(
    labor_whoMigrates = str_trim(labor_whoMigrates),
    labor_whoMigrates = str_to_lower(labor_whoMigrates),
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
    )
  ) %>%
  count(Soum, labor_whoMigrates, sort = FALSE) %>%
  mutate(labor_whoMigrates = factor(labor_whoMigrates, levels = relationship_order)) %>%
  arrange(labor_whoMigrates)

count_wide2 <- count_whoMigrates %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  )

print(count_wide2, n = 30)




## How many people undertake migrations (labor_numMigrates)? 
count_numMigrates <- base_LABOR %>%
  select(Soum, labor_numMigrates) %>%
  filter(!is.na(labor_numMigrates)) %>%
  count(Soum, labor_numMigrates, sort = TRUE)

count_wide3 <- count_numMigrates %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  )

print(count_wide3, n = 30)




## Does migration impact labor and/or herding practices? (labor_migImpactLabor/labor_migImpactPract)



## Do you hire labor? If so, for what? (labor_hire/labor_hireDaily/labor_hire_bigMove/labor_hire_forOtor_labor_hire_Other)



##






