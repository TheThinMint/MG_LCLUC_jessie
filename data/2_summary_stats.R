# basic summary stats for initial investigation
# exploring questions WITHIN dataframes created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)



### base_LABOR -----------------------------------------------------------------
skim(base_LABOR)

## Who does the daily moves (labor_whoMovesDaily) (broken up by soum)-----------
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
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

print(count_wide, n = 30)



## Who undertakes herding migrations (labor_whoMigrates)--------------------------------
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
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-1))) %>%  # exclude the first column (labor_numMigrates)
  ungroup()

print(count_wide2, n = 30)




## How many people undertake migrations (labor_numMigrates)?--------------------
count_numMigrates <- base_LABOR %>%
  select(Soum, labor_numMigrates) %>%
  filter(!is.na(labor_numMigrates)) %>%
  count(Soum, labor_numMigrates, sort = TRUE)

count_wide3 <- count_numMigrates %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-1))) %>%  # exclude the first column (labor_numMigrates)
  ungroup()

print(count_wide3, n = 13)



## Does migration impact labor and/or herding practices?------------------------
  # Impact on Labor(labor_migImpactLabor): 
count_migImpactLabor <- base_LABOR %>%
  select(Soum, labor_migImpactLabor) %>%
  count(Soum, labor_migImpactLabor, sort = TRUE)
count_wide4 <- count_migImpactLabor %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide4)

  #Impact on Practices(labor_migImpactPract):
count_migImpactPract <- base_LABOR %>%
  select(Soum, labor_migImpactPract) %>%
  count(Soum, labor_migImpactPract, sort = TRUE)
count_wide5 <- count_migImpactPract %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

print(count_wide5, n = 30)



## Do you hire labor? (labor_hire)----------------------------------------------
count_hireYN <- base_LABOR %>%
  select(Soum, labor_hire) %>%
  count(Soum, labor_hire, sort = TRUE)
count_wide6 <- count_hireYN %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide6)


## If you do hire labor, for what?----------------------------------------------
  # Moving the herds daily (labor_hire_DailyMove):
count_hire_dailyMove <- base_LABOR %>%
  select(Soum, labor_hire_dailyMove) %>%
  count(Soum, labor_hire_dailyMove, sort = TRUE)
count_wide7 <- count_hire_dailyMove %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide7)


  # Moving the herds seasonally (labor_hire_bigMove):
count_hire_bigMove <- base_LABOR %>%
  select(Soum, labor_hire_bigMove) %>%
  count(Soum, labor_hire_bigMove, sort = TRUE)
count_wide8 <- count_hire_bigMove %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide8)


  # Moving the herds for Otor (labor_hire_forOtor):
count_hire_Otor <- base_LABOR %>%
  select(Soum, labor_hire_forOtor) %>%
  count(Soum, labor_hire_forOtor, sort = TRUE)
count_wide9 <- count_hire_Otor %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide9)


  # Hiring for other tasks (labor_hire_Other):
count_hire_OTHER <- base_LABOR %>%
  select(Soum, labor_hire_Other) %>%
  separate_rows(labor_hire_Other, sep = ",") %>%
  mutate(
    labor_hire_Other = str_trim(labor_hire_Other),
    labor_hire_Other = str_to_lower(labor_hire_Other)
    ) %>% 
  count(Soum, labor_hire_Other, sort = TRUE)
count_wide10 <- count_hire_OTHER %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide10, n = 30)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
  
  
  
  
  
  
###base_TENURE------------------------------------------------------------------
skim(base_TENURE)

##CrossTab of different land tenure arrangements: 
cross_tab <- base_TENURE %>%
  mutate(tenure_wintCamp = replace_na(tenure_wintCamp, "No"),
         tenure_wintContract = replace_na(tenure_wintContract, "No"),
         tenure_wintPas = replace_na(tenure_wintCamp, "No"),
         tenure_wintPasContract = replace_na(tenure_wintPasContract, "No"),
         tenure_sameCamp = replace_na(tenure_sameCamp, "No"),
         tenure_sprCamp = replace_na(tenure_sprCamp, "No"),
         tenure_sprCampContract = replace_na(tenure_sprCampContract, "No"),
         tenure_sprPasContract = replace_na(tenure_sprPasContract, "No"),
         )

cross_tab <- xtabs(~ tenure_wintCamp + 
                     tenure_wintContract + 
                     tenure_wintPas + 
                     tenure_wintPasContract + 
                     tenure_sameCamp + 
                     tenure_sprCamp + 
                     tenure_sprCampContract + 
                     tenure_sprPasContract, 
                   data = base_TENURE)
ftable(cross_tab)


##Contingency table of different land tenure arrangements: 
base_contingency <- base_TENURE %>%
  mutate(tenure_wintCamp = replace_na(tenure_wintCamp, "No"),
         tenure_wintContract = replace_na(tenure_wintContract, "No"),
         tenure_wintPas = replace_na(tenure_wintPas, "No"),
         tenure_wintPasContract = replace_na(tenure_wintPasContract, "No"),
         tenure_sameCamp = replace_na(tenure_sameCamp, "No"),
         tenure_sprCamp = replace_na(tenure_sprCamp, "No"),
         tenure_sprCampContract = replace_na(tenure_sprCampContract, "No"),
         tenure_sprPasContract = replace_na(tenure_sprPasContract, "No"),
         )

contingency_table <- base_contingency %>%
  count(tenure_wintCamp, 
        tenure_wintContract, 
        tenure_wintPas, 
        tenure_wintPasContract, 
        tenure_sameCamp, 
        tenure_sprCamp, 
        tenure_sprCampContract, 
        tenure_sprPasContract, 
        sort = TRUE
        )
print(contingency_table)


##Contingency Tables on tenure, by Soum:----------------------------------------
tenure_table_wintCamp <- table(base_contingency$Soum, base_contingency$tenure_wintCamp)
print(tenure_table_wintCamp)

tenure_table_wintContract <- table(base_contingency$Soum, base_contingency$tenure_wintContract)
print(tenure_table_wintContract)

tenure_table_wintPas <- table(base_contingency$Soum, base_contingency$tenure_wintPas)
print(tenure_table_wintPas)

tenure_table_wintPasContract <- table(base_contingency$Soum, base_contingency$tenure_wintPasContract)
print(tenure_table_wintPasContract)

tenure_table_sameCamp <- table(base_contingency$Soum, base_contingency$tenure_sameCamp)
print(tenure_table_sameCamp)

tenure_table_sprCamp <- table(base_contingency$Soum, base_contingency$tenure_sprCamp)
print(tenure_table_sprCamp)

tenure_table_sprCampContract <- table(base_contingency$Soum, base_contingency$tenure_sprCampContract)
print(tenure_table_sprCampContract)

tenure_table_sprPasContract <- table(base_contingency$Soum, base_contingency$tenure_sprPasContract)
print(tenure_table_sprPasContract)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------






###base_ALTLIFE-----------------------------------------------------------------
skim(base_ALTLIFE)

## Is someone in the household doing non-herding work? (altLife_nonHerdWork)----
  #Broken up by Soum
altLife_table_nonHerdWork <- table(base_ALTLIFE$Soum, base_ALTLIFE$altLife_nonHerdWork)
print(altLife_table_nonHerdWork)



  #If so, who is doing non-herding work? (altLife_whoNoHerdWork)
count_whoNoHerdWork <- base_ALTLIFE %>%
  select(Soum, altLife_whoNoHerdwork) %>%
  filter(!is.na(altLife_whoNoHerdwork)) %>%
  separate_rows(altLife_whoNoHerdwork, sep = ",") %>%
  mutate(
    altLife_whoNoHerdwork = str_trim(altLife_whoNoHerdwork),
    altLife_whoNoHerdwork = str_to_lower(altLife_whoNoHerdwork),
    altLife_whoNoHerdwork = case_when(
      altLife_whoNoHerdwork == "son-in-law" ~ "extended family members",
      altLife_whoNoHerdwork == "doctor" ~ "not specified",
      TRUE ~ altLife_whoNoHerdwork
    )
  ) %>% 
  count(Soum, altLife_whoNoHerdwork, sort = TRUE)

count_whoNoHerdWork <- count_whoNoHerdWork %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

print(count_whoNoHerdWork, n = 30)



  #If so, what is that work? (altLife_noHerdWhatWork)
count_noHerdWhatWork <- base_ALTLIFE %>%
  select(Soum, altLife_noHerdWhatWork) %>%
  filter(!is.na(altLife_noHerdWhatWork)) %>%
  separate_rows(altLife_noHerdWhatWork, sep = ",") %>%
  mutate(
    altLife_noHerdWhatWork = str_trim(altLife_noHerdWhatWork),
    altLife_noHerdWhatWork = str_to_lower(altLife_noHerdWhatWork),
    altLife_noHerdWhatWork = case_when(
      altLife_noHerdWhatWork == "bagh governor" ~ "government leadership",
      altLife_noHerdWhatWork == "public service" ~ "government employment",
      altLife_noHerdWhatWork == "employment at soum governor office" ~ "government employment",
      altLife_noHerdWhatWork == "public servant" ~ "government employment",
      altLife_noHerdWhatWork == "soum culture center" ~ "government employment",
      altLife_noHerdWhatWork == "dance instructor at culture center" ~ "government employment",
      altLife_noHerdWhatWork == "soum center" ~ "government employment",
      altLife_noHerdWhatWork == "procurement at the governor office" ~ "government employment",
      altLife_noHerdWhatWork == "social worker in ulaanbaatar hospital" ~ "government employment",
      altLife_noHerdWhatWork == "employment in environmental agency" ~ "government employment",
      altLife_noHerdWhatWork == "mining" ~ "mining and construction",
      altLife_noHerdWhatWork == "employment in mining" ~ "mining and construction",
      altLife_noHerdWhatWork == "part-time employment in mining" ~ "mining and construction",
      altLife_noHerdWhatWork == "drives truck in mining" ~ "mining and construction",
      altLife_noHerdWhatWork == "employment in construction" ~ "mining and construction",
      altLife_noHerdWhatWork == "metal work (sometimes)" ~ "mining and construction",
      altLife_noHerdWhatWork == "plumber" ~ "mining and construction",
      altLife_noHerdWhatWork == "driver" ~ "mining and construction",
      altLife_noHerdWhatWork == "employment in restaurant" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "cook at school kitchen" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "retail business" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "cook at kindergarten" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "cashier at grocery store" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "part-time cashier at supermarket" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "any part-time job" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "running store" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "working at supermarket" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "commercial service" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "work at supermarket" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "private business" ~ "commerce-related and restaurants",
      altLife_noHerdWhatWork == "kindergarten teacher" ~ "education",
      altLife_noHerdWhatWork == "school teacher" ~ "education",
      altLife_noHerdWhatWork == "driving instructor" ~ "education",
      altLife_noHerdWhatWork == "assistant teacher at kindergarten" ~ "education",
      altLife_noHerdWhatWork == "studies in japan" ~ "education",
      altLife_noHerdWhatWork == "teacher at kindergarten" ~ "education",
      altLife_noHerdWhatWork == "subsidy for wool/hide" ~ "agriculture and pastoralist-adjacent",
      altLife_noHerdWhatWork == "working the hide" ~ "agriculture and pastoralist-adjacent",
      altLife_noHerdWhatWork == "crop farming" ~ "agriculture and pastoralist-adjacent",
      altLife_noHerdWhatWork == "dairy store" ~ "agriculture and pastoralist-adjacent",
      altLife_noHerdWhatWork == "sales from dairy products" ~ "agriculture and pastoralist-adjacent",
      altLife_noHerdWhatWork == "employment" ~ "employment unspecified",
      altLife_noHerdWhatWork == "in ulaanbaatar" ~ "employment unspecified",
      altLife_noHerdWhatWork == "pension" ~ "employment unspecified",
      altLife_noHerdWhatWork == "salary" ~ "employment unspecified",
      altLife_noHerdWhatWork == "grandmother" ~ "employment unspecified",
      altLife_noHerdWhatWork == "employed" ~ "employment unspecified",
      altLife_noHerdWhatWork == "knitting" ~ "arts, crafts, and handwork",
      altLife_noHerdWhatWork == "horse archery" ~ "arts, crafts, and handwork",
      altLife_noHerdWhatWork == "film director" ~ "arts, crafts, and handwork",
      altLife_noHerdWhatWork == "tailoring" ~ "arts, crafts, and handwork",
      altLife_noHerdWhatWork == "doctor" ~ "medical/veterinary",
      altLife_noHerdWhatWork == "veterinarian" ~ "medical/veterinary",
      altLife_noHerdWhatWork == "cleaner" ~ "mischellaneous",
      altLife_noHerdWhatWork == "head of the soum disabled community" ~ "mischellaneous",
      altLife_noHerdWhatWork == "security" ~ "mischellaneous",
      TRUE ~ altLife_noHerdWhatWork
    )
  ) %>% 
  count(Soum, altLife_noHerdWhatWork, sort = TRUE)

count_noHerdWhatWork <- count_noHerdWhatWork %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_noHerdWhatWork, n = 60)




## Additional sources of income for the household? (altLife_otherInc)-----------
  #Broken up by Soum
count_otherInc <- base_ALTLIFE %>%
  select(Soum, altLife_otherInc) %>%
  filter(!is.na(altLife_otherInc)) %>%
  separate_rows(altLife_otherInc, sep = ",") %>%
  mutate(
    altLife_otherInc = str_trim(altLife_otherInc),
    altLife_otherInc = str_to_lower(altLife_otherInc),
    altLife_otherInc = case_when(
      altLife_otherInc == "bagh governor" ~ "government leadership",
      TRUE ~ altLife_otherInc
    )
  ) %>% 
  count(Soum, altLife_otherInc, sort = TRUE)

count_otherInc <- count_otherInc %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  # Fill missing values with 0s
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_otherInc, n = 60)




## Typical number of loans per year? (altLife_loansPerYr)-----------------------
altLife_table_loansPerYr <- table(base_ALTLIFE$Soum, base_ALTLIFE$altLife_loansPerYr)
print(altLife_table_loansPerYr)


## Loan Sizes (min, max, mean, median, range) (altLife_loansMin/altLife_loansMax)
  #Broken up by Soum
  #Summaries for separate columns: 
base_loans <- base_ALTLIFE %>%
  select(Soum, altLife_loansMin, altLife_loansMax) %>%
  group_by(Soum) %>%  # Group by Soum before summarizing
  summarise(
    min_minLoan = min(altLife_loansMin, na.rm = TRUE),
    max_minLoan = max(altLife_loansMin, na.rm = TRUE),
    mean_minLoan = mean(altLife_loansMin, na.rm = TRUE),
    median_minLoan = median(altLife_loansMin, na.rm = TRUE),
    min_maxLoan = min(altLife_loansMax, na.rm = TRUE),
    max_maxLoan = max(altLife_loansMax, na.rm = TRUE),
    mean_maxLoan = mean(altLife_loansMax, na.rm = TRUE),
    median_maxLoan = median(altLife_loansMax, na.rm = TRUE)
    )
print(base_loans)





## When do you typically need loans? (altLife_loansWhenNeed)--------------------
  #Broken up by Soum
count_loansWhenNeed <- base_ALTLIFE %>%
  select(Soum, altLife_loansWhenNeed) %>%
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
    altLife_loansWhenNeed = str_to_lower(altLife_loansWhenNeed)
  ) %>% 
  count(Soum, altLife_loansWhenNeed, sort = TRUE)

count_loansWhenNeed <- count_loansWhenNeed %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

print(count_loansWhenNeed, n = 60)
















