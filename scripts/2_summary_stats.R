# basic summary stats for initial investigation
# exploring questions WITHIN dataframes created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)



### base_LABOR -----------------------------------------------------------------
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
    values_fill = 0  
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
    values_fill = 0  
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-1))) %>%  
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
  mutate(Total = sum(c_across(-1))) %>% 
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
    values_fill = 0 
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
    values_fill = 0 
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
    values_fill = 0  
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
    values_fill = 0  
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
    values_fill = 0
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
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wide10, n = 30)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
  
  
  
  
  
  
  
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
  
###base_TENURE------------------------------------------------------------------
## CrossTab of different land tenure arrangements:------------------------- 
cross_tab <- base_TENURE %>%
  mutate(wintCamp = replace_na(wintCamp, "No"),
         wintContract = replace_na(wintContract, "No"),
         wintPas = replace_na(wintCamp, "No"),
         wintPasContract = replace_na(wintPasContract, "No"),
         sameCamp = replace_na(sameCamp, "No"),
         sprCamp = replace_na(sprCamp, "No"),
         sprCampContract = replace_na(sprCampContract, "No"),
         sprPasContract = replace_na(sprPasContract, "No"),
         )

cross_tab <- xtabs(~ wintCamp + 
                     wintContract + 
                     wintPas + 
                     wintPasContract + 
                     sameCamp + 
                     sprCamp + 
                     sprCampContract + 
                     sprPasContract, 
                   data = base_TENURE)
ftable(cross_tab)


##Contingency table of different land tenure arrangements: 
base_contingency <- base_TENURE %>%
  mutate(wintCamp = replace_na(wintCamp, "No"),
         wintContract = replace_na(wintContract, "No"),
         wintPas = replace_na(wintPas, "No"),
         wintPasContract = replace_na(wintPasContract, "No"),
         sameCamp = replace_na(sameCamp, "No"),
         sprCamp = replace_na(sprCamp, "No"),
         sprCampContract = replace_na(sprCampContract, "No"),
         sprPasContract = replace_na(sprPasContract, "No"),
         )

contingency_table <- base_contingency %>%
  count(wintCamp, 
        wintContract, 
        wintPas, 
        wintPasContract, 
        sameCamp, 
        sprCamp, 
        sprCampContract, 
        sprPasContract, 
        sort = TRUE
        )
print(contingency_table)


## Contingency Tables on tenure, by Soum:----------------------------------------
tenure_table_wintCamp <- table(base_contingency$Soum, base_contingency$wintCamp)
print(tenure_table_wintCamp)

tenure_table_wintContract <- table(base_contingency$Soum, base_contingency$wintContract)
print(tenure_table_wintContract)

tenure_table_wintPas <- table(base_contingency$Soum, base_contingency$wintPas)
print(tenure_table_wintPas)

tenure_table_wintPasContract <- table(base_contingency$Soum, base_contingency$wintPasContract)
print(tenure_table_wintPasContract)

tenure_table_sameCamp <- table(base_contingency$Soum, base_contingency$sameCamp)
print(tenure_table_sameCamp)

tenure_table_sprCamp <- table(base_contingency$Soum, base_contingency$sprCamp)
print(tenure_table_sprCamp)

tenure_table_sprCampContract <- table(base_contingency$Soum, base_contingency$sprCampContract)
print(tenure_table_sprCampContract)

tenure_table_sprPasContract <- table(base_contingency$Soum, base_contingency$sprPasContract)
print(tenure_table_sprPasContract)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------






  
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
  
###base_ALTLIFE-----------------------------------------------------------------
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
    values_fill = 0
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
    values_fill = 0
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
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
  
  
  
  
  
  
  
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
  
###base_HERDMGMT----------------------------------------------------------------
#-------------------------------------------------------------------------------
##Distance for daily herding:---------------------------------------------------
  #(herdMgmt_dailyDist):
  #All together:
count_DailyDist <- base_HERDMGMT %>%
  select(herdMgmt_dailyDist) %>%
  filter(!is.na(herdMgmt_dailyDist)) %>%
  count(herdMgmt_dailyDist, sort = TRUE)
print(count_DailyDist)

  #Broken up by Soum:
count_DailyDist <- base_HERDMGMT %>%
  select(Soum, herdMgmt_dailyDist) %>%
  filter(!is.na(herdMgmt_dailyDist)) %>%
  count(Soum, herdMgmt_dailyDist, sort = TRUE)
print(count_herdDailyDist)

count_dist1 <- count_DailyDist %>%
  pivot_wider(names_from = Soum, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  ungroup()
print(count_dist1)


##Distance for daily herding, summer and winter:-------------------------------- 
  #(herdMgmt_sumDailyDist/herdMgmt_wintDailyDist)
  #Basic distances, all together: 
count_sumDailyDist <- base_HERDMGMT %>%
  select(herdMgmt_sumDailyDist) %>%
  filter(!is.na(herdMgmt_sumDailyDist)) %>%
  count(herdMgmt_sumDailyDist, sort = TRUE)
print(count_sumDailyDist)

count_wintDailyDist <- base_HERDMGMT %>%
  select(herdMgmt_wintDailyDist) %>%
  filter(!is.na(herdMgmt_wintDailyDist)) %>%
  count(herdMgmt_wintDailyDist, sort = TRUE)
print(count_wintDailyDist)


  #Basic distances, broken up by Soum: 
    #Summer: 
count_sumDailyDist2 <- base_HERDMGMT %>%
  select(Soum, herdMgmt_sumDailyDist) %>%
  filter(!is.na(herdMgmt_sumDailyDist)) %>%
  count(Soum, herdMgmt_sumDailyDist, sort = TRUE)
print(count_sumDailyDist2)
count_dist2 <- count_sumDailyDist2 %>%
  pivot_wider(names_from = Soum, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  ungroup()
print(count_dist2)

    #Winter:
count_wintDailyDist2 <- base_HERDMGMT %>%
  select(Soum, herdMgmt_wintDailyDist) %>%
  filter(!is.na(herdMgmt_wintDailyDist)) %>%
  count(Soum, herdMgmt_wintDailyDist, sort = TRUE)
print(count_wintDailyDist2)
count_dist3 <- count_wintDailyDist2 %>%
  pivot_wider(names_from = Soum, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  ungroup()
print(count_dist3)


  #Do they travel greater distances in summer vs. winter? 
    #All together: 
count_distComparison <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_sumDailyDist > herdMgmt_wintDailyDist ~ "greater dist in summer",
      herdMgmt_sumDailyDist < herdMgmt_wintDailyDist ~ "greater dist in winter",
      herdMgmt_sumDailyDist == herdMgmt_wintDailyDist ~ "Equal"
    )
  ) %>%
  count(dist_comparison)
print(count_distComparison)

    #Broken up by Soum: 
count_distComparison2 <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_sumDailyDist > herdMgmt_wintDailyDist ~ "greater dist in summer",
      herdMgmt_sumDailyDist < herdMgmt_wintDailyDist ~ "greater dist in winter",
      herdMgmt_sumDailyDist == herdMgmt_wintDailyDist ~ "Equal"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_dist4 <- count_distComparison2 %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_dist4)



##Distance moved this year vs. last year:--------------------------------------- 
  #(herdMgmt_timesMoved_thisYr/herdMgmt_timesMoved_lastYr)
  #Times moved this year, broken up by Soum: 
count_moves_thisYr <- base_HERDMGMT %>%
  select(Soum, herdMgmt_timesMoved_thisYr) %>%
  filter(!is.na(herdMgmt_timesMoved_thisYr)) %>%
  count(Soum, herdMgmt_timesMoved_thisYr, sort = TRUE)
count_moves1 <- count_moves_thisYr %>%
  pivot_wider(names_from = Soum, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  ungroup()
print(count_moves1)

  #Times moved last year, broken up by Soum: 
count_moves_lastYr <- base_HERDMGMT %>%
  select(Soum, herdMgmt_timesMoved_lastYr) %>%
  filter(!is.na(herdMgmt_timesMoved_lastYr)) %>%
  count(Soum, herdMgmt_timesMoved_lastYr, sort = TRUE)
count_moves2 <- count_moves_lastYr %>%
  pivot_wider(names_from = Soum, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  ungroup()
print(count_moves2)


  #Times moved, this yr vs. last year, difference for everyone
count_moves <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_timesMoved_thisYr > herdMgmt_timesMoved_lastYr ~ "moved more this year",
      herdMgmt_timesMoved_lastYr > herdMgmt_timesMoved_thisYr ~ "moved more last year",
      herdMgmt_timesMoved_lastYr == herdMgmt_timesMoved_thisYr ~ "Equal"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_moves <- count_moves %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_moves)


  #One table showing the difference, coalesced together 
herd_moves <- base_HERDMGMT %>%
  mutate(
    move_diff = herdMgmt_timesMoved_thisYr - herdMgmt_timesMoved_lastYr,
    move_description = case_when(
      move_diff > 0 ~ paste("moved", move_diff, "times more this year than last year"),
      move_diff < 0 ~ paste("moved", abs(move_diff), "times fewer this year than last year"),
      move_diff == 0 ~ "moved an equal number of times"
    )
  ) %>%
  select(herdMgmt_timesMoved_lastYr, herdMgmt_timesMoved_thisYr, move_diff, move_description)
move_summary <- herd_moves %>%
  count(move_description)
print(move_summary)

  #The differences, broken up by Soum
    #Bayan
herd_moves_bayan <- herd_moves %>%
  filter(Soum == "Bayan")
move_summary_bayan <- herd_moves_bayan %>%
  count(move_description)
print(move_summary_bayan)

    #Bayantal
herd_moves_bayantal <- herd_moves %>%
  filter(Soum == "Bayantal")
move_summary_bayantal <- herd_moves_bayantal %>%
  count(move_description)
print(move_summary_bayantal)

    #Bayantsagaan
herd_moves_bayantsagaan <- herd_moves %>%
  filter(Soum == "Bayantsagaan")
move_summary_bayantsagaan <- herd_moves_bayantsagaan %>%
  count(move_description)
print(move_summary_bayantsagaan)

    #Buren
herd_moves_buren <- herd_moves %>%
  filter(Soum == "Buren")
move_summary_buren <- herd_moves_buren %>%
  count(move_description)
print(move_summary_buren)

    #Delgerkhaan
herd_moves_delgerkhaan <- herd_moves %>%
  filter(Soum == "Delgerkhaan")
move_summary_delgerkhaan <- herd_moves_delgerkhaan %>%
  count(move_description)
print(move_summary_delgerkhaan)

    #Deren
herd_moves_deren <- herd_moves %>%
  filter(Soum == "Deren")
move_summary_deren <- herd_moves_deren %>%
  count(move_description)
print(move_summary_deren)

    #Erdenedalai
herd_moves_erdenedalai <- herd_moves %>%
  filter(Soum == "Erdenedalai")
move_summary_erdenedalai <- herd_moves_erdenedalai %>%
  count(move_description)
print(move_summary_erdenedalai)

    #Sumber
herd_moves_sumber <- herd_moves %>%
  filter(Soum == "Sumber")
move_summary_sumber <- herd_moves_sumber %>%
  count(move_description)
print(move_summary_sumber)



##What is the average distance of moves, now vs. 10yrs ago?:-------------------- 
  #(herdMgmt_avgDistMoves/herdMgmt_10yrs_avgMoveDist)
herd_move_dif <- base_HERDMGMT %>%
  mutate(
    move_diff = herdMgmt_10yrs_avgMoveDist - herdMgmt_avgDistMoves,
    move_description = case_when(
      move_diff > 0 ~ paste("moved", move_diff, "kilometers more 10yrs ago than last year"),
      move_diff < 0 ~ paste("moved", abs(move_diff), "kilometers less 10yrs ago than last year"),
      move_diff == 0 ~ "moved an equal amount of distance"
    )
  ) %>%
  select(herdMgmt_timesMoved_lastYr, herdMgmt_timesMoved_thisYr, move_diff, move_description)
move_summary <- herd_move_dif %>%
  count(move_description) %>% 
  arrange(desc(n))
print(move_summary)

  #Simplified:
herd_move_difSimp <- base_HERDMGMT %>%
  mutate(
    move_diff = herdMgmt_10yrs_avgMoveDist - herdMgmt_avgDistMoves,
    move_description = case_when(
      move_diff > 0 ~ paste("moved more 10yrs ago than last year"),
      move_diff < 0 ~ paste("moved less 10yrs ago than last year"),
      move_diff == 0 ~ "moved an equal amount of distance"
    )
  ) %>%
  select(herdMgmt_timesMoved_lastYr, herdMgmt_timesMoved_thisYr, move_diff, move_description)
move_summary <- herd_move_difSimp %>%
  count(move_description) %>% 
  arrange(desc(n))
print(move_summary)



##Contingency tables for these columns:-----------------------------------------
  #Big ugly contingency table to start: 
contingency_pasture <- base_HERDMGMT %>%
  count(
    herdMgmt_lastYr_Otor, 
    herdMgmt_thisYr_Otor, 
    herdMgmt_lastYr_wintPast, 
    herdMgmt_thisYr_wintPast, 
    herdMgmt_lastYr_springPast, 
    herdMgmt_thisYr_springPast, 
    herdMgmt_lastYr_DzudPast, 
    herdMgmt_thisYr_DzudPast, 
    sort = TRUE
  ) %>%
  rename(
    lastYr_Otor = herdMgmt_lastYr_Otor,
    thisYr_Otor = herdMgmt_thisYr_Otor,
    lastYr_Winter = herdMgmt_lastYr_wintPast,
    thisYr_Winter = herdMgmt_thisYr_wintPast,
    lastYr_Spring = herdMgmt_lastYr_springPast,
    thisYr_Spring = herdMgmt_thisYr_springPast,
    lastYr_Dzud = herdMgmt_lastYr_DzudPast,
    thisYr_Dzud = herdMgmt_thisYr_DzudPast
  )
print(contingency_pasture)


  #herdMgmt_lastYr_Otor
count_lastYr_Otor <- base_HERDMGMT %>%
  select(Soum, herdMgmt_lastYr_Otor) %>%
  count(Soum, herdMgmt_lastYr_Otor, sort = TRUE)
count_Otor1 <- count_lastYr_Otor %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_Otor1)

  #herdMgmt_thisYr_Otor
count_thisYr_Otor <- base_HERDMGMT %>%
  select(Soum, herdMgmt_thisYr_Otor) %>%
  count(Soum, herdMgmt_thisYr_Otor, sort = TRUE)
count_Otor2 <- count_thisYr_Otor %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_Otor2)

  #OtorCounts: Going this year versus last year, etc.: 
count_OtorComp <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_lastYr_Otor == "No" & herdMgmt_thisYr_Otor == "No" ~ "Did not take Otor either year",
      herdMgmt_lastYr_Otor == "Yes" & herdMgmt_thisYr_Otor == "No" ~ "Took Otor last yr but not this yr",
      herdMgmt_lastYr_Otor == "No" & herdMgmt_thisYr_Otor == "Yes" ~ "Too Otor this yr but not last yr",
      herdMgmt_lastYr_Otor == "Yes" & herdMgmt_thisYr_Otor == "Yes" ~ "Took Otor both years"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_Otor3 <- count_OtorComp %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_Otor3)


  #herdMgmt_thisYr_wintPast
count_thisYr_wintPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_thisYr_wintPast) %>%
  count(Soum, herdMgmt_thisYr_wintPast, sort = TRUE)
count_wintPast1 <- count_thisYr_wintPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wintPast1)


  #herdMgmt_lastYr_wintPast
count_lastYr_wintPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_lastYr_wintPast) %>%
  count(Soum, herdMgmt_lastYr_wintPast, sort = TRUE)
count_wintPast2 <- count_lastYr_wintPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_wintPast2)


  #wintPastCounts: Going this year versus last year, etc.:
count_wintPastComp <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_lastYr_wintPast == "No" & herdMgmt_thisYr_wintPast == "No" ~ "Did not reserve Winter Pasture either year",
      herdMgmt_lastYr_wintPast == "Yes" & herdMgmt_thisYr_wintPast == "No" ~ "Reserved Winter paster last yr but not this yr",
      herdMgmt_lastYr_wintPast == "No" & herdMgmt_thisYr_wintPast == "Yes" ~ "Reserved Winter Pasture this yr but not last yr",
      herdMgmt_lastYr_wintPast == "Yes" & herdMgmt_thisYr_wintPast == "Yes" ~ "Reserved Winter Pasture both yrs"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_wintPast3 <- count_wintPastComp %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_wintPast3)


  #herdMgmt_thisYr_springPast
count_thisYr_sprPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_thisYr_springPast) %>%
  count(Soum, herdMgmt_thisYr_springPast, sort = TRUE)
count_sprPast1 <- count_thisYr_sprPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_sprPast1)


  #herdMgmt_lastYr_springPast
count_lastYr_sprPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_lastYr_springPast) %>%
  count(Soum, herdMgmt_lastYr_springPast, sort = TRUE)
count_sprPast2 <- count_lastYr_sprPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_sprPast2)


  #SprPastCounts: Going this year versus last year, etc.:
count_sprPastComp <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_lastYr_springPast == "No" & herdMgmt_thisYr_springPast == "No" ~ "Did not reserve Spring Pasture either year",
      herdMgmt_lastYr_springPast == "Yes" & herdMgmt_thisYr_springPast == "No" ~ "Reserved Spring paster last yr but not this yr",
      herdMgmt_lastYr_springPast == "No" & herdMgmt_thisYr_springPast == "Yes" ~ "Reserved Spring Pasture this yr but not last yr",
      herdMgmt_lastYr_springPast == "Yes" & herdMgmt_thisYr_springPast == "Yes" ~ "Reserved Spring Pasture both yrs"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_sprPast3 <- count_sprPastComp %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_sprPast3)



  #herdMgmt_thisYr_DzudPast
count_thisYr_dzudPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_thisYr_DzudPast) %>%
  count(Soum, herdMgmt_thisYr_DzudPast, sort = TRUE)
count_dzudPast1 <- count_thisYr_dzudPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_dzudPast1)



  #herdMgmt_lastYr_DzudPast
count_lastYr_dzudPast <- base_HERDMGMT %>%
  select(Soum, herdMgmt_lastYr_DzudPast) %>%
  count(Soum, herdMgmt_lastYr_DzudPast, sort = TRUE)
count_dzudPast2 <- count_lastYr_dzudPast %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_dzudPast2)


  #DzudCounts: Going this year versus last year, etc.:
count_dzudPastComp <- base_HERDMGMT %>%
  mutate(
    dist_comparison = case_when(
      herdMgmt_lastYr_DzudPast == "No" & herdMgmt_thisYr_DzudPast == "No" ~ "Did not reserve Dzud Pasture either year",
      herdMgmt_lastYr_DzudPast == "Yes" & herdMgmt_thisYr_DzudPast == "No" ~ "Reserved Dzud Pasture last yr but not this yr",
      herdMgmt_lastYr_DzudPast == "No" & herdMgmt_thisYr_DzudPast == "Yes" ~ "Reserved Dzud Pasture this yr but not last yr",
      herdMgmt_lastYr_DzudPast == "Yes" & herdMgmt_thisYr_DzudPast == "Yes" ~ "Reserved Dzud Pasture both yrs"
    )
  ) %>%
  group_by(Soum, dist_comparison) %>%
  summarise(count = n(), .groups = "drop")
count_dzudPast3 <- count_dzudPastComp %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0)
print(count_dzudPast3)



## Have you grazed your reserve pastures out of season in the past five years?-- 
  # (herdMgmt_past5Yrs_resPast)
contingency_reserves <- base_HERDMGMT %>%
  count(herdMgmt_past5yrs_resPast, sort = TRUE)
print(contingency_reserves)

  # Broken down by Soum
contingency_reserves_soum <- table(base_HERDMGMT$Soum, base_HERDMGMT$herdMgmt_past5yrs_resPast)
print(contingency_reserves_soum)



## Means of travel 10, 5, and 1 year ago:---------------------------------------
  #(herdMgmt_10yrsAgo_herdTravel/herdMgmt_5yrsAgo_herdTravel/herdMgmt_lastYr_herdTravel)
travel_long_clean <- base_HERDMGMT %>%
  select(Soum,
         travel_10 = herdMgmt_10yrsAgo_herdTravel,
         travel_5 = herdMgmt_5yrsAgo_herdTravel,
         travel_1 = herdMgmt_lastYr_herdTravel) %>%
  pivot_longer(
    cols = starts_with("travel_"),
    names_to = "year",
    values_to = "transport"
  ) %>%
  mutate(
    transport = str_replace_all(transport, "Camel Motorbike", "Camel, Motorbike"),
    transport = str_replace_all(transport, "Walk Horse", "Walk, Horse"),
    transport = str_replace_all(transport, "Walk Car", "Walk, Car"),
    transport = str_to_lower(transport)
  ) %>%
  separate_rows(transport, sep = ",") %>%
  mutate(
    transport = str_trim(transport),
    year = case_when(
      year == "travel_10" ~ "10",
      year == "travel_5" ~ "5",
      year == "travel_1" ~ "1"
    )
  )

count_by_year <- travel_long_clean %>%
  count(Soum, transport, year)
count_side_by_side <- count_by_year %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(combined = paste0(`10`, "-", `5`, "-", `1`)) %>%
  select(Soum, transport, combined)
final_table <- count_side_by_side %>%
  pivot_wider(
    names_from = Soum,
    values_from = combined,
    values_fill = "0-0-0"
  )

total_counts <- count_by_year %>%
  group_by(transport, year) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  mutate(Total = paste0(`10`, "-", `5`, "-", `1`)) %>%
  select(transport, Total)
final_table_with_total <- final_table %>%
  left_join(total_counts, by = "transport")
print(final_table_with_total)



## Daily distance traveled, 10, 5, and 1 year ago:------------------------------
  #(herdMgmt_10yrsAgo_dailyDist/herdMgmt_5yrsAgo_dailyDist/herdMgmt_lastYr_dailyDist)
  #Broken down by Soum
dist_long <- base_HERDMGMT %>%
  select(Soum,
         dist_10 = herdMgmt_10yrsAgo_dailyDist,
         dist_5 = herdMgmt_5yrsAgo_dailyDist,
         dist_1 = herdMgmt_lastYr_dailyDist) %>%
  pivot_longer(
    cols = starts_with("dist_"),
    names_to = "year",
    values_to = "distance"
  ) %>%
  mutate(
    year = case_when(
      year == "dist_10" ~ "10",
      year == "dist_5" ~ "5",
      year == "dist_1" ~ "1"
    )
  ) %>%
  filter(!is.na(distance))  # optional, if there are NAs

count_by_distance <- dist_long %>%
  count(Soum, distance, year)

distance_side_by_side <- count_by_distance %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(combined = paste0(`10`, "-", `5`, "-", `1`)) %>%
  select(Soum, distance, combined)

final_distance_table <- distance_side_by_side %>%
  pivot_wider(
    names_from = Soum,
    values_from = combined,
    values_fill = "0-0-0"
  )

total_distance_counts <- count_by_distance %>%
  group_by(distance, year) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  mutate(Total = paste0(`10`, "-", `5`, "-", `1`)) %>%
  select(distance, Total)
final_distance_with_total <- final_distance_table %>%
  left_join(total_distance_counts, by = "distance")
print(final_distance_with_total, n = 45)






## In the past 5 years, have you changed you management practices?:-------------
  #(herdMgmt_past5Yrs_mgmtChanges)
  #Broken down by Soum
count_mgmtChanges <- base_HERDMGMT %>%
  select(Soum, herdMgmt_past5Yrs_mgmtChanges) %>%
  count(Soum, herdMgmt_past5Yrs_mgmtChanges, sort = TRUE)
count_mgmt1 <- count_mgmtChanges %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_mgmt1)



## What management changes have been made:--------------------------------------
  #(herdMgmt_mgmtChanges_what)
  #Broken down by Soum
count_mgmtChanges <- base_HERDMGMT %>%
  select(Soum, herdMgmt_mgmtChanges_what) %>%
  filter(!is.na(herdMgmt_mgmtChanges_what)) %>%
  separate_rows(herdMgmt_mgmtChanges_what, sep = ",") %>%
  mutate(
    herdMgmt_mgmtChanges_what = str_trim(herdMgmt_mgmtChanges_what),
    herdMgmt_mgmtChanges_what = str_to_lower(herdMgmt_mgmtChanges_what),
    herdMgmt_mgmtChanges_what = case_when(
      herdMgmt_mgmtChanges_what == "reduced the herd size" ~ "reduced the overall herd size",
      herdMgmt_mgmtChanges_what == "reduced the size of herd" ~ "reduced the overall herd size",
      herdMgmt_mgmtChanges_what == "save winter and spring pasture" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "moved to better pasture area" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "changed winter and spring pasture" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "migrate to land with more salt marsh and wild leek" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "follow the livestock during winter" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "stopped migrating to winter camp early" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "herding from aimag" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "use summer pasture freely" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "increased pasture land" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "reserved pasture areas" ~ "reserved/moved to improved pasture",
      herdMgmt_mgmtChanges_what == "changed the breeder livestock (male)" ~ "improved the quality of the livestock",
      herdMgmt_mgmtChanges_what == "improve the quality of the livestock" ~ "improved the quality of the livestock",
      herdMgmt_mgmtChanges_what == "producing less dairy product" ~ "Engaged less in dairy farming",
      herdMgmt_mgmtChanges_what == "dug a new water well" ~ "Improved water supply",
      herdMgmt_mgmtChanges_what == "maintain livestock numbers" ~ "maintain the overall herd size",
      TRUE ~ herdMgmt_mgmtChanges_what
    )
  ) %>%
  count(Soum, herdMgmt_mgmtChanges_what, sort = TRUE)

count_herdChg1 <- count_mgmtChanges %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  arrange(desc(Total))  # <- This line orders by Total in descending order

print(count_herdChg1, n = 30)






## Plans to make changes in the future?------------------------------------------
  #(herdMgmt_next5Yrs_mgmtChanges)
  #Broken down by Soum
count_futureChg <- base_HERDMGMT %>%
  select(Soum, herdMgmt_next5Yrs_mgmtChanges) %>%
  count(Soum, herdMgmt_next5Yrs_mgmtChanges, sort = TRUE)
count_mgmt2 <- count_futureChg %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_mgmt2)



## What changes planning to make in the next 5 years:---------------------------
  #(herdMgmt_futureChg_what)
  #Broken down by Soum
count_futureChanges <- base_HERDMGMT %>%
  select(Soum, herdMgmt_futureChg_what) %>%
  filter(!is.na(herdMgmt_futureChg_what)) %>%
  separate_rows(herdMgmt_futureChg_what, sep = ",") %>%
  mutate(
    herdMgmt_futureChg_what = str_trim(herdMgmt_futureChg_what),
    herdMgmt_futureChg_what = str_to_lower(herdMgmt_futureChg_what),
    herdMgmt_futureChg_what = case_when(
      herdMgmt_futureChg_what == "fence needlegrass" ~ "fence the pasture",
      herdMgmt_futureChg_what == "fence hay-making area" ~ "fence the pasture",
      herdMgmt_futureChg_what == "fence grazing area" ~ "fence the pasture",
      herdMgmt_futureChg_what == "fence the grazing area" ~ "fence the pasture",
      herdMgmt_futureChg_what == "fence needlegrass" ~ "fence the pasture",
      herdMgmt_futureChg_what == "fencing the pasture" ~ "fence the pasture",
      herdMgmt_futureChg_what == "planting pasture grass" ~ "plant pasture grass",
      herdMgmt_futureChg_what == "dig a water well" ~ "improve water supply",
      herdMgmt_futureChg_what == "dig for water" ~ "improve water supply",
      herdMgmt_futureChg_what == "reduced the herd size" ~ "reduce the overall herd size",
      herdMgmt_futureChg_what == "reduce the herd size" ~ "reduce the overall herd size",
      herdMgmt_futureChg_what == "reduce the number of smaller animals" ~ "reduce the overall herd size",
      herdMgmt_futureChg_what == "sell out the lambs" ~ "reduce the overall herd size",
      herdMgmt_futureChg_what == "maintain livestock numbers" ~ "maintain the overall herd size",
      herdMgmt_futureChg_what == "maintain cows only" ~ "maintain the overall herd size",
      herdMgmt_futureChg_what == "maintain the bigger animals" ~ "maintain the overall herd size",
      herdMgmt_futureChg_what == "increase the herd size" ~ "increase the overall herd size",
      herdMgmt_futureChg_what == "develop milk farming" ~ "Engage more in dairy farming",
      herdMgmt_futureChg_what == "make nicer dairy products" ~ "Engage more in dairy farming",
      herdMgmt_futureChg_what == "adopt intensive cow farming" ~ "Engage more in dairy farming",
      herdMgmt_futureChg_what == "adopt intensive animal husbandry" ~ "improve the quality of the livestock",
      herdMgmt_futureChg_what == "changed the breeder livestock (male)" ~ "improve the quality of the livestock",
      herdMgmt_futureChg_what == "focus on one type of animal" ~ "improve the quality of the livestock",
      herdMgmt_futureChg_what == "improve the fattening of animals" ~ "improve the quality of the livestock",
      herdMgmt_futureChg_what == "reserve pasture areas" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "need better pasture as horses are dominant" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "move to a better pasture area" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "change spring/winter camps" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "increase the number of camps" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "find own winter camp" ~ "reserve/move to improved pasture",
      herdMgmt_futureChg_what == "prepare forage" ~ "forage/fodder related",
      herdMgmt_futureChg_what == "run irrigated crop farming" ~ "forage/fodder related",
      TRUE ~ herdMgmt_futureChg_what
    )
  ) %>%
  count(Soum, herdMgmt_futureChg_what, sort = TRUE)

count_herdChg2 <- count_futureChanges %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  arrange(desc(Total))  # <- This line orders by Total in descending order

print(count_herdChg2, n = 40)



## Are there changes you want to make but can't?--------------------------------
  #(herdMgmt_whatChanges_cantMake)
  #Broken down by Soum
count_hopeChg <- base_HERDMGMT %>%
  select(Soum, herdMgmt_whatChanges_cantMake) %>%
  count(Soum, herdMgmt_whatChanges_cantMake, sort = TRUE)
count_mgmt3 <- count_hopeChg %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0 
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()
print(count_mgmt3)






## Limiting factor in not being able to make change:----------------------------
  #(herdMgmt_chg_limitingFactor)
  #Broken down by Soum
count_cantChg <- base_HERDMGMT %>%
  select(Soum, herdMgmt_chg_limitingFactor) %>%
  separate_rows(herdMgmt_chg_limitingFactor, sep = ",") %>%
  mutate(
    herdMgmt_chg_limitingFactor = str_trim(herdMgmt_chg_limitingFactor),
    herdMgmt_chg_limitingFactor = str_to_lower(herdMgmt_chg_limitingFactor),
    herdMgmt_chg_limitingFactor = case_when(
      herdMgmt_chg_limitingFactor == "lacking human capacity" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "lacking of human capacity" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "need a pasture specialist" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "looking after children" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "lack of technical skills" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "lack of solidarity" ~ "lack of human capacity/skills",
      herdMgmt_chg_limitingFactor == "high expenses" ~ "financial difficulty",
      herdMgmt_chg_limitingFactor == "inflation" ~ "financial difficulty",
      herdMgmt_chg_limitingFactor == "economy" ~ "financial difficulty",
      herdMgmt_chg_limitingFactor == "fencing the pasture" ~ "fencing related",
      herdMgmt_chg_limitingFactor == "fence needlegrass" ~ "fencing related",
      herdMgmt_chg_limitingFactor == "fence haymaking area" ~ "fencing related",
      herdMgmt_chg_limitingFactor == "fencing a small area is not effective" ~ "fencing related",
      herdMgmt_chg_limitingFactor == "critiques from other herders around building fences in grazing areas" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "delayed resolution on land issue" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "other herders' livestock grazed" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "protect pasture" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "insufficient legal framework" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "expanding the area is not possible as it will overlap others' land" ~ "legal issues/conflict with other herders",
      herdMgmt_chg_limitingFactor == "insufficient water well" ~ "insufficient water resources",
      herdMgmt_chg_limitingFactor == "establish more water points" ~ "insufficient water resources",
      herdMgmt_chg_limitingFactor == "flooding" ~ "insufficient water resources",
      herdMgmt_chg_limitingFactor == "lack of materials" ~ "insufficient land, equipment, or materials",
      herdMgmt_chg_limitingFactor == "lack of equipment" ~ "insufficient land, equipment, or materials",
      herdMgmt_chg_limitingFactor == "lacking adequate land for activities" ~ "insufficient land, equipment, or materials",
      herdMgmt_chg_limitingFactor == "lack of adequate land for activities" ~ "insufficient land, equipment, or materials",
      herdMgmt_chg_limitingFactor == "no projects/programs" ~ "little attention/resources for herders",
      herdMgmt_chg_limitingFactor == "low attention to herders" ~ "little attention/resources for herders",
      herdMgmt_chg_limitingFactor == "need to plant pasture grass" ~ "degraded pastureland",
      herdMgmt_chg_limitingFactor == "low price of products" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "difficulty in selling animals" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "herd beef cattle breeds" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "improve the herd quality" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "small size of herds" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "animal quality" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "selling the livestock products directly to the manufacturers/not through middlemen" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "environmental pollution and threats to humans/animals imposed by fluorspar mining" ~ "difficulties in selling/breeding livestock",
      herdMgmt_chg_limitingFactor == "have dairy cattle breeds" ~ "difficulties in selling/breeding/managing livestock",
      herdMgmt_chg_limitingFactor == "camp/building renovations" ~ "lack of time",
          TRUE ~ herdMgmt_chg_limitingFactor
    )
  ) %>%
  count(Soum, herdMgmt_chg_limitingFactor, sort = TRUE)

count_cantChg2 <- count_cantChg %>%
  pivot_wider(
    names_from = Soum,
    values_from = n,
    values_fill = 0  
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  arrange(desc(Total))  # <- This line orders by Total in descending order

print(count_cantChg2, n = 50)





## Condition and degree of pastoral change:-------------------------------------
  #(herdMgmt_pastureCon_chg_yn/herdMgmt_pastureCon_chg_deg)
  #Broken down by Soum
count_pastureCon <- base_HERDMGMT %>%
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
  ) %>%
  group_by(Soum, condition_comparison) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Soum, values_from = count, values_fill = 0) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

print(count_pastureCon)











































#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

###base_LIVESTOCK---------------------------------------------------------------
#-------------------------------------------------------------------------------








