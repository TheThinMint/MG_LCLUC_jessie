---
title: LCLUC Later Stage Analyses
author: "Jessie Dearing"
date: "2025-06-26"
output: pdf_document
---

This relies on the combined dfs from 1_data_import. There is not currently any commentary in this document because I'm still running through analyses and chunking them in here as I go. 






```r
setwd("C:/Users/jessi/OneDrive - Cornell University/Documents/MSPhD/AllingtonLab_LCLUC/RStudio_WorkingDirectory")
names(base_) <- gsub("\\.x$", "", names(base_))

str(base_)
```

```
## 'data.frame':	187 obs. of  331 variables:
##  $ start                                                       : chr  "12/25/2023" "12/28/2023" "12/28/2023" "12/25/2023" ...
##  $ end                                                         : chr  "12/25/2023" "12/28/2023" "12/28/2023" "12/25/2023" ...
##  $ featureid                                                   : num  726 726 726 727 727 727 727 727 727 727 ...
##  $ Ref                                                         : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ _2_idInfo_aimag_                                            : chr  "Dundgovi" "Dundgovi" "Dundgovi" "Dundgovi" ...
##  $ _3_idInfo_soum_                                             : chr  "Deren" "Deren" "Deren" "Deren" ...
##  $ _4_idInfo_bagCode_                                          : chr  "Bag 1" "Bag 1" "Bag 1" "Bag 2" ...
##  $ _5_idInfo_cbrm_orgName_                                     : chr  NA NA "???????" NA ...
##  $ _6_idInfo_interviewerName_                                  : chr  "Bilguun" "Bolor" "Tungalag" "Bilguun" ...
##  $ _7_idInfo_interviewDate_                                    : chr  "12/25/2023" "12/25/2023" "12/25/2023" "12/25/2023" ...
##  $ _idInfo_record_crntLoc_                                     : chr  "46.2126606 106.7091468 1260.9000244140625 20.0" "46.2126305 106.7091099 0 0" NA "46.2128266 106.7093315 1261.4000244140625 21.558" ...
##  $ _idInfo_record_crntLoc_lat_                                 : num  46.2 46.2 NA 46.2 46.2 ...
##  $ _idInfo_record_crntLoc_lon_                                 : num  107 107 NA 107 107 ...
##  $ _idInfo_record_crntLoc_alt_                                 : num  1261 0 NA 1261 1272 ...
##  $ _idInfo_record_crntLoc_prec_                                : num  20 0 NA 21.6 10 ...
##  $ _1_HHDems_num_HHmembers_                                    : num  1 5 2 2 1 7 3 5 6 5 ...
##  $ _2_HHDems_hoh_age_                                          : num  64 50 21 53 67 40 69 46 35 43 ...
##  $ _3_HHDems_hoh_sex_                                          : chr  "Female" "Male" "Male" "Male" ...
##  $ _4_HHDems_hoh_education_                                    : chr  "Complete secondary/High school" "Secondary" "Secondary" "Secondary" ...
##  $ _5_HHDems_num_HHmems_sub16_camp_tot_                        : num  0 1 0 0 0 4 2 1 4 1 ...
##  $ _5_a_HHDems_num_HHmems_sub16_soum_                          : num  3 1 0 0 0 2 2 1 2 1 ...
##  $ _5_b_HHDems_num_HHmems_sub16_aimag_                         : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ _5_c_HHDems_num_HHmems_sub16_ub_                            : num  0 0 0 0 0 1 0 0 0 0 ...
##  $ _6_HHDems_num_HHmems_16to30_camp_tot_                       : num  0 2 0 0 0 1 0 2 NA 2 ...
##  $ _6_a_HHDems_num_HHmems_16to30_soum_                         : num  0 0 0 NA 0 0 0 1 0 0 ...
##  $ _6_b_HHDems_num_HHmems_16to30_aimag_                        : num  0 1 0 1 0 0 0 0 0 0 ...
##  $ _6_c_HHDems_num_HHmems_16to30_ub_                           : num  0 1 0 1 0 0 0 0 0 0 ...
##  $ _7_HHDems_num_HHmems_30to60_camp_tot_                       : num  0 1 0 2 0 1 0 1 1 2 ...
##  $ _7_a_HHDems_num_HHmems_30to60_soum_                         : num  0 1 0 0 0 0 0 0 0 2 ...
##  $ _7_b_HHDems_num_HHmems_30to60_aimag_                        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ _7_c_HHDems_num_HHmems_30to60_ub_                           : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ _8_HHDems_num_HHmems_60plus_camp_tot_                       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ _8_a_HHDems_num_HHmems_60plus_soum_                         : num  0 3 1 0 1 0 0 0 0 0 ...
##  $ _8_b_HHDems_num_HHmems_60plus_aimag_                        : num  0 1 0 0 0 0 0 0 0 0 ...
##  $ _8_c_HHDems_num_HHmems_60plus_ub_                           : num  0 1 0 0 0 0 0 0 0 0 ...
##  $ _9_HHDems_num_HH_khotAil_                                   : num  2 2 2 3 2 1 1 1 1 2 ...
##  $ _10_HHDems_num_people_khotAil_                              : num  6 3 3 6 7 3 5 5 6 2 ...
##  $ _11_HHDems_do_HH_stayTogether_                              : chr  "Yes" "Yes" "No" "Yes" ...
##  $ _1_labor_HHmems_movesAnimals_daily_                         : chr  "Son" "Husband, wife" "Husband" "Husband" ...
##  $ _1_labor_HHmems_movesAnimals_daily_female_                  : num  NA 1 NA NA 1 1 1 NA 1 NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_male_                    : num  1 1 1 1 2 2 1 1 1 NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_husband_                 : num  NA 1 1 1 1 1 NA 1 1 NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_wife_                    : num  NA 1 NA NA NA 1 NA NA 1 NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_son_                     : num  1 NA NA NA 1 1 1 NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_daughter_                : num  NA NA NA NA 1 NA NA NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_children_                : num  1 NA NA NA 2 1 2 NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_grandchildren_           : num  NA NA NA NA 2 NA 1 NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_grandparents_            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_siblings_                : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_hiredHelp_               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_neighbors_               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _1_labor_HHmems_movesAnimals_daily_unspecified_             : num  NA NA NA NA NA NA NA NA NA 2 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_                         : chr  "Son, wife" "Husband, wife, son" "Husband, ask for help" "Husband, assistant herder" ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_female_                  : num  1 1 1 NA NA 1 1 1 1 1 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_male_                    : num  1 2 NA 1 NA 1 1 2 1 1 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_husband_                 : num  NA 1 NA 1 NA 1 NA 1 1 1 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_wife_                    : num  1 NA NA NA NA 1 NA 1 1 1 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_son_                     : num  1 1 NA NA NA NA 1 1 NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_daughter_                : num  NA NA NA NA NA NA 1 NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_children_                : num  NA NA NA NA NA NA NA NA NA 3 ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_grandchildren_           : num  NA NA NA NA NA NA 2 NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_grandparents_            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_siblings_                : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_hiredHelp_               : num  NA NA NA 1 NA NA NA NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_neighbors_               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_unspecified_             : num  NA NA NA NA 3 NA NA NA NA NA ...
##  $ _2_labor_HHmems_seasonalMig_toCamp_other_                   : num  NA NA 1 NA NA NA NA NA NA NA ...
##  $ _2_a_labor_HHmems_seasonalMig_toCamp_num_                   : num  2 3 1 2 3 3 5 3 2 5 ...
##  $ _2_b_labor_HHmems_durAutmnWinter_                           : num  1 1 3 1 5 2 2 1 3 2 ...
##  $ _2_c_labor_HHmems_durSpringSummer_                          : num  1 1 2 1 4 1 2 1 3 2 ...
##  $ _3_labor_HHmems_leftCamp_                                   : chr  "Yes" "Yes" "Yes" "Yes" ...
##  $ _3_a_labor_HHmems_leftCamp_reason_                          : chr  "Employment" "School, Employment" "Healthcare services" "Marriage" ...
##  $ _3_a_labor_HHmems_leftCamp_reason_school_                   : num  NA 1 NA NA NA 1 1 NA 1 1 ...
##  $ _3_a_labor_HHmems_leftCamp_reason_kinderg_                  : num  NA NA NA NA NA NA 1 NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_higherEd_                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_employ_                   : num  1 1 NA NA NA NA NA 1 NA 1 ...
##  $ _3_a_labor_HHmems_leftCamp_reason_marriage_                 : num  NA NA NA 1 NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_home_                     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_childWatch_               : num  NA NA NA NA 1 NA 1 NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_cattle_                   : num  NA NA NA NA 1 NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_retire_                   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_healthcare_               : num  NA NA 1 NA NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_family_                   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _3_a_labor_HHmems_leftCamp_reason_military_                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ _3_b_i_labor_HHmems_leftCamp_yr_                            : num  2021 2009 2022 2010 NA ...
##  $ _3_b_ii_labor_HHmems_leftCamp_month_                        : chr  NA NA NA NA ...
##  $ _3_c_labor_HHmems_returnCamp_                               : chr  "To assist with migration to new camp (seasonal)" "Weekly Monthly To assist with migration to new camp (seasonal) During Naadam Other" "Weekly" "During Naadam" ...
##  $ _3_c_labor_HHmems_returnCamp_weekly                         : num  NA 1 1 NA NA 1 NA NA NA 1 ...
##  $ _3_c_labor_HHmems_returnCamp_monthly_                       : num  NA 1 NA NA NA NA NA NA NA NA ...
##  $ _3_c_labor_HHmems_returnCamp_seasonally_                    : num  1 1 NA NA 1 1 NA NA NA NA ...
##  $ _3_c_labor_HHmems_returnCamp_naadam_                        : num  NA 1 NA 1 NA NA 1 1 NA NA ...
##  $ _3_c_labor_HHmems_returnCamp_schoolBreaks_                  : num  NA NA NA NA NA 1 1 NA 1 NA ...
##  $ _3_c_labor_HHmems_returnCamp_other_                         : num  NA 1 NA NA NA NA NA NA NA NA ...
##  $ _3_c_i_labor_other_text_                                    : chr  NA "November /winter meat prep and May sheep and goat shearing/" NA NA ...
##  $ _3_d_labor_permMove_                                        : chr  "Yes" "Yes" "No" "Yes" ...
##  $ _3_e_labor_returnPlan_                                      : chr  "No" "No" "Yes" "Yes" ...
##  $ _4_labor_HHmems_moveReason_                                 : chr  "Employment" "School for children" "Access to services (health care)" "Employment" ...
##  $ _4_labor_HHmems_moveReason_school_                          : num  NA 1 NA NA 1 1 1 NA 1 1 ...
##  $ _4_labor_HHmems_moveReason_university_                      : num  NA NA NA NA NA NA NA NA NA NA ...
##   [list output truncated]
```

```r
base_ <- as.data.frame(base_)
base_ <- base_ %>% 
  mutate(across(Ref, as.factor))
```

# I. LABOR

### 1. Who does the daily moves for herding? 
  Column: (labor_whoMovesDaily) 
  (broken up by soum)

|labor_whoMovesDaily     | Buren| Bayan| Bayantal| Delgerkhaan| Sumber| Deren| Erdenedalai| Bayantsagaan| Total|
|:-----------------------|-----:|-----:|--------:|-----------:|------:|-----:|-----------:|------------:|-----:|
|husband                 |    23|    21|       19|          19|     18|    15|          15|           13|   143|
|child, unspecified      |     5|    11|       10|           1|     15|     5|           6|            2|    55|
|wife                    |    13|    12|        9|          10|     12|     9|          10|            8|    83|
|son                     |     8|     7|        1|           5|      2|     7|          11|            5|    46|
|unspecified             |     5|     0|        2|           4|      4|     8|           8|            9|    40|
|daughter                |     2|     5|        1|           1|      0|     2|           3|            1|    15|
|herder                  |     0|     4|        0|           0|      0|     0|           0|            0|     4|
|grandchild, unspecified |     2|     0|        0|           0|      0|     4|           0|            2|     8|
|neighbor                |     1|     0|        0|           0|      2|     4|           1|            0|     8|
|daughter-in-law         |     3|     0|        1|           0|      1|     2|           1|            3|    11|
|grandparent             |     0|     2|        0|           1|      0|     0|           0|            0|     3|
|assistant herder        |     1|     1|        0|           0|      0|     0|           0|            2|     4|
|friend                  |     0|     0|        1|           0|      0|     0|           0|            0|     1|
|sibling, unspecified    |     0|     0|        1|           0|      0|     0|           0|            0|     1|
|grandmother             |     0|     0|        0|           0|      0|     0|           0|            1|     1|
|grandfather             |     1|     0|        0|           0|      0|     0|           0|            0|     1|
|brother                 |     0|     0|        0|           1|      0|     1|           1|            0|     3|
|hired help              |     0|     0|        0|           1|      0|     0|           0|            0|     1|
|son-in-law              |     0|     0|        0|           0|      0|     1|           0|            0|     1|
|brother-in-law          |     0|     0|        0|           0|      1|     0|           1|            0|     2|
|father-in-law           |     0|     0|        0|           0|      0|     0|           1|            0|     1|

### 2. Who undertakes herding migrations?
  Column: labor_whoMigrates

|labor_whoMigrates            | Bayan| Bayantal| Bayantsagaan| Buren| Delgerkhaan| Deren| Erdenedalai| Sumber| Total|
|:----------------------------|-----:|--------:|------------:|-----:|-----------:|-----:|-----------:|------:|-----:|
|husband                      |    18|       18|            8|    19|          17|    17|          18|     19|   134|
|wife                         |    17|       14|            8|    19|          13|    14|          15|     18|   118|
|son(s)                       |     8|        4|            7|     8|           3|     7|           7|      1|    45|
|daughter(s)                  |     2|        1|            1|     1|           0|     2|           2|      1|    10|
|child(ren), unspecified      |     5|        2|            1|     5|           3|     5|           3|      8|    32|
|brother(s)                   |     0|        1|            0|     0|           1|     0|           1|      0|     3|
|sibling(s), unspecified      |     2|        3|            0|     0|           1|     1|           1|      0|     8|
|father                       |     1|        0|            0|     0|           0|     0|           0|      0|     1|
|mother                       |     3|        0|            1|     0|           0|     1|           0|      0|     5|
|grandparent(s), unspecified  |     1|        0|            1|     1|           1|     0|           0|      0|     4|
|grandchild(ren), unspecified |     0|        0|            1|     1|           0|     2|           0|      0|     4|
|household head               |     2|        0|            0|     0|           0|     0|           1|      1|     4|
|extended family/in-laws      |     1|        0|            2|     7|           1|     3|           4|      4|    22|
|friend/neighbor(s)           |     2|        2|            1|     0|           1|     1|           1|      0|     8|
|person(s), unspecified       |     1|        1|            8|     5|           4|     1|           4|      0|    24|
|hired help                   |     1|        0|            0|     0|           1|     1|           0|      0|     3|
|just myself                  |     0|        0|            0|     0|           0|     0|           1|      1|     2|
|other                        |     1|        1|            0|     0|           0|     1|           2|      0|     5|

### 3. How many people undertake migrations?
  Column: labor_numMigrates

| labor_numMigrates| Bayantsagaan| Buren| Delgerkhaan| Sumber| Bayan| Deren| Erdenedalai| Bayantal| Total|
|-----------------:|------------:|-----:|-----------:|------:|-----:|-----:|-----------:|--------:|-----:|
|                 2|           12|    10|          10|     10|     4|     9|           8|        7|    70|
|                 4|            1|     2|           6|      1|     9|     4|           3|        2|    28|
|                 3|            3|     8|           2|      3|     4|     4|           7|        2|    33|
|                 6|            2|     1|           0|      0|     3|     1|           2|        4|    13|
|                 5|            1|     3|           2|      4|     2|     3|           1|        1|    17|
|                 1|            3|     1|           1|      0|     0|     2|           2|        1|    10|
|                 8|            0|     1|           0|      1|     0|     0|           0|        2|     4|
|                 7|            0|     0|           0|      2|     0|     1|           2|        0|     5|
|                 0|            0|     0|           0|      1|     0|     0|           0|        1|     2|
|                11|            1|     0|           0|      0|     0|     0|           0|        0|     1|
|                 9|            0|     1|           0|      0|     0|     0|           1|        0|     2|
|                10|            0|     0|           1|      0|     0|     0|           1|        0|     2|

### 4.  Does migration impact labor and/or herding practices?
#### A. Impact on labor: 
  Column: labor_migImpactLabor

|labor_migImpactLabor | Buren| Deren| Erdenedalai| Bayan| Sumber| Bayantal| Bayantsagaan| Delgerkhaan| Total|
|:--------------------|-----:|-----:|-----------:|-----:|------:|--------:|------------:|-----------:|-----:|
|Yes                  |    15|     8|          14|     8|     11|       10|           10|           6|    82|
|No                   |     7|    15|          12|    13|      9|        9|            9|          10|    84|
|NA                   |     5|     1|           1|     1|      2|        1|            4|           6|    21|

#### B. Impact on herding practices:  
  Column: labor_migImpactPract

|labor_migImpactPract | Deren| Buren| Erdenedalai| Bayan| Sumber| Bayantsagaan| Bayantal| Delgerkhaan| Total|
|:--------------------|-----:|-----:|-----------:|-----:|------:|------------:|--------:|-----------:|-----:|
|No                   |    16|     8|          14|    13|     13|           11|        9|          10|    94|
|Yes                  |     7|    14|          12|     8|      7|            8|       10|           6|    72|
|NA                   |     1|     5|           1|     1|      2|            4|        1|           6|    21|

### 5. Do you hire labor?  
  Column: labor_hire

|labor_hire | Buren| Erdenedalai| Deren| Sumber| Bayantal| Bayantsagaan| Bayan| Delgerkhaan| Total|
|:----------|-----:|-----------:|-----:|------:|--------:|------------:|-----:|-----------:|-----:|
|No         |    22|          22|    20|     20|       18|           17|    16|          14|   149|
|Yes        |     5|           5|     3|      2|        2|            6|     6|           8|    37|
|NA         |     0|           0|     1|      0|        0|            0|     0|           0|     1|

### 6. If you do hire labor, for what?
#### A. Moving the herds daily:
  Column: labor_hire_DailyMove

|labor_hire_dailyMove | Buren| Erdenedalai| Deren| Sumber| Bayantal| Bayantsagaan| Bayan| Delgerkhaan| Total|
|:--------------------|-----:|-----------:|-----:|------:|--------:|------------:|-----:|-----------:|-----:|
|NA                   |    22|          22|    21|     20|       18|           18|    16|          14|   151|
|Yes                  |     3|           4|     2|      0|        0|            3|     5|           7|    24|
|No                   |     2|           1|     1|      2|        2|            2|     1|           1|    12|

#### B. Moving the herds seasonally:
  Column: labor_hire_bigMove

|labor_hire_bigMove | Buren| Erdenedalai| Deren| Sumber| Bayantal| Bayantsagaan| Bayan| Delgerkhaan| Total|
|:------------------|-----:|-----------:|-----:|------:|--------:|------------:|-----:|-----------:|-----:|
|NA                 |    22|          22|    21|     20|       18|           18|    16|          14|   151|
|Yes                |     2|           5|     3|      1|        2|            5|     5|           7|    30|
|No                 |     3|           0|     0|      1|        0|            0|     1|           1|     6|

#### C. Moving the herds for Otor: 
  Column: labor_hire_forOtor

|labor_hire_forOtor | Buren| Erdenedalai| Deren| Sumber| Bayantal| Bayantsagaan| Bayan| Delgerkhaan| Total|
|:------------------|-----:|-----------:|-----:|------:|--------:|------------:|-----:|-----------:|-----:|
|NA                 |    22|          22|    21|     20|       18|           18|    16|          14|   151|
|No                 |     3|           2|     1|      1|        1|            2|     5|           3|    18|
|Yes                |     2|           3|     2|      1|        1|            3|     1|           5|    18|

#### D. Hiring for other tasks: 
  Column: labor_hire_Other

|labor_hire_Other   | Buren| Erdenedalai| Sumber| Bayantsagaan| Delgerkhaan| Deren| Bayantal| Bayan| Total|
|:------------------|-----:|-----------:|------:|------------:|-----------:|-----:|--------:|-----:|-----:|
|NA                 |    22|          20|     18|           17|          15|    15|       14|    11|   132|
|shearing livestock |     3|           5|      2|            5|           3|     6|        5|     7|    36|
|day laboring       |     1|           0|      0|            0|           1|     3|        0|     2|     7|
|herding            |     0|           0|      0|            2|           1|     0|        1|     1|     5|
|household chores   |     1|           0|      0|            0|           2|     0|        0|     0|     3|
|migration          |     1|           2|      0|            1|           1|     0|        0|     0|     5|
|livestock care     |     0|           1|      2|            0|           0|     1|        0|     1|     5|

--------------------------------------------------------------------------------



# II. TENURE

### 1. Land tenure arrangements: 
#### A. Cross-tab:
  Not the best looking, I'll work on making it more legible. Is easier to look at if it's viewed in the markdown or in the script. 

```
##                                                                                                                                 tenure_sprPasContract No Yes
## tenure_wintCamp tenure_wintContract tenure_wintPas tenure_wintPasContract tenure_sameCamp tenure_sprCamp tenure_sprCampContract                             
## No              No                  No             No                     No              No             No                                            5   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                    Yes                    No              No             No                                            1   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                     Yes            No                     No              No             No                                            8   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            4   1
##                                                                                                          Yes                                           0   0
##                                                    Yes                    No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                 Yes                 No             No                     No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                    Yes                    No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                     Yes            No                     No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           2   0
##                                                    Yes                    No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
## Yes             No                  No             No                     No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                    Yes                    No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                     Yes            No                     No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            6   1
##                                                                                                          Yes                                           1   0
##                                                    Yes                    No              No             No                                            1   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                 Yes                 No             No                     No              No             No                                            1   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            1   0
##                                                                                                          Yes                                           1   0
##                                                    Yes                    No              No             No                                            0   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            0   0
##                                                                                                          Yes                                           0   0
##                                     Yes            No                     No              No             No                                            7   0
##                                                                                                          Yes                                           0   0
##                                                                                           Yes            No                                            8   2
##                                                                                                          Yes                                          45   2
##                                                    Yes                    No              No             No                                            1   0
##                                                                                                          Yes                                           1   0
##                                                                                           Yes            No                                            1   0
##                                                                                                          Yes                                           2   3
```

#### B. Contingency table:
  This one looks better and is depicting the same information.

|tenure_wintCamp |tenure_wintContract |tenure_wintPas |tenure_wintPasContract |tenure_sameCamp |tenure_sprCamp |tenure_sprCampContract |tenure_sprPasContract |  n|
|:---------------|:-------------------|:--------------|:----------------------|:---------------|:--------------|:----------------------|:---------------------|--:|
|Yes             |Yes                 |Yes            |No                     |Yes             |No             |No                     |No                    | 50|
|Yes             |Yes                 |Yes            |No                     |No              |Yes            |Yes                    |No                    | 45|
|No              |No                  |Yes            |No                     |Yes             |No             |No                     |No                    | 10|
|Yes             |No                  |Yes            |No                     |Yes             |No             |No                     |No                    | 10|
|Yes             |Yes                 |Yes            |No                     |No              |Yes            |No                     |No                    |  9|
|No              |No                  |Yes            |No                     |No              |No             |No                     |No                    |  8|
|Yes             |Yes                 |Yes            |No                     |No              |No             |No                     |No                    |  7|
|Yes             |No                  |Yes            |No                     |No              |Yes            |No                     |No                    |  6|
|No              |No                  |No             |No                     |No              |No             |No                     |No                    |  5|
|No              |No                  |Yes            |No                     |No              |Yes            |No                     |No                    |  4|
|No              |Yes                 |Yes            |No                     |No              |Yes            |Yes                    |No                    |  3|
|Yes             |Yes                 |No             |No                     |Yes             |No             |No                     |No                    |  3|
|Yes             |Yes                 |Yes            |Yes                    |No              |Yes            |Yes                    |Yes                   |  3|
|No              |No                  |No             |Yes                    |No              |No             |No                     |No                    |  2|
|Yes             |No                  |Yes            |Yes                    |Yes             |No             |No                     |No                    |  2|
|Yes             |Yes                 |Yes            |No                     |No              |Yes            |No                     |Yes                   |  2|
|Yes             |Yes                 |Yes            |No                     |No              |Yes            |Yes                    |Yes                   |  2|
|Yes             |Yes                 |Yes            |Yes                    |No              |Yes            |Yes                    |No                    |  2|
|No              |No                  |No             |No                     |Yes             |No             |No                     |No                    |  1|
|No              |No                  |Yes            |No                     |No              |Yes            |No                     |Yes                   |  1|
|No              |No                  |Yes            |No                     |No              |Yes            |Yes                    |No                    |  1|
|Yes             |No                  |No             |No                     |Yes             |No             |No                     |No                    |  1|
|Yes             |No                  |Yes            |No                     |No              |Yes            |No                     |Yes                   |  1|
|Yes             |No                  |Yes            |No                     |No              |Yes            |Yes                    |No                    |  1|
|Yes             |No                  |Yes            |Yes                    |No              |No             |No                     |No                    |  1|
|Yes             |Yes                 |No             |No                     |No              |No             |No                     |No                    |  1|
|Yes             |Yes                 |No             |No                     |No              |Yes            |No                     |No                    |  1|
|Yes             |Yes                 |No             |No                     |No              |Yes            |Yes                    |No                    |  1|
|Yes             |Yes                 |Yes            |Yes                    |No              |No             |No                     |No                    |  1|
|Yes             |Yes                 |Yes            |Yes                    |No              |No             |Yes                    |No                    |  1|
|Yes             |Yes                 |Yes            |Yes                    |No              |Yes            |No                     |No                    |  1|
|Yes             |Yes                 |Yes            |Yes                    |Yes             |No             |No                     |No                    |  1|

### 2. Contingency Tables on tenure, by Soum: 
#### A. Winter Camp (y/n): 

|             | No| Yes|
|:------------|--:|---:|
|Bayan        |  1|  21|
|Bayantal     |  5|  15|
|Bayantsagaan |  1|  22|
|Buren        |  6|  21|
|Delgerkhaan  |  3|  19|
|Deren        |  5|  19|
|Erdenedalai  |  8|  19|
|Sumber       |  6|  16|

#### B. Winter Camp Contract (y/n): 

|             | No| Yes|
|:------------|--:|---:|
|Bayan        |  3|  19|
|Bayantal     |  9|  11|
|Bayantsagaan |  1|  22|
|Buren        |  8|  19|
|Delgerkhaan  |  7|  15|
|Deren        |  9|  15|
|Erdenedalai  |  9|  18|
|Sumber       |  8|  14|

#### C. Winter Pasture (y/n):

|             | No| Yes|
|:------------|--:|---:|
|Bayan        |  2|  20|
|Bayantal     |  1|  19|
|Bayantsagaan |  0|  23|
|Buren        |  4|  23|
|Delgerkhaan  |  2|  20|
|Deren        |  2|  22|
|Erdenedalai  |  2|  25|
|Sumber       |  2|  20|

#### D. Winter Paster Contract (y/n): 

|             | No| Yes|
|:------------|--:|---:|
|Bayan        | 21|   1|
|Bayantal     | 19|   1|
|Bayantsagaan | 23|   0|
|Buren        | 24|   3|
|Delgerkhaan  | 19|   3|
|Deren        | 22|   2|
|Erdenedalai  | 25|   2|
|Sumber       | 20|   2|

#### E. Is your winter camp your spring camp (y/n)?  

|             | No| Yes|
|:------------|--:|---:|
|Bayan        | 14|   8|
|Bayantal     | 12|   8|
|Bayantsagaan | 16|   7|
|Buren        | 22|   5|
|Delgerkhaan  | 13|   9|
|Deren        | 13|  11|
|Erdenedalai  | 10|  17|
|Sumber       |  9|  13|

#### F. Spring Camp? (y/n):   

|             | No| Yes|
|:------------|--:|---:|
|Bayan        |  8|  14|
|Bayantal     | 10|  10|
|Bayantsagaan |  9|  14|
|Buren        | 12|  15|
|Delgerkhaan  | 15|   7|
|Deren        | 12|  12|
|Erdenedalai  | 22|   5|
|Sumber       | 16|   6|

#### G. Spring Camp Contract (y/n):

|             | No| Yes|
|:------------|--:|---:|
|Bayan        |  9|  13|
|Bayantal     | 14|   6|
|Bayantsagaan | 11|  12|
|Buren        | 20|   7|
|Delgerkhaan  | 16|   6|
|Deren        | 17|   7|
|Erdenedalai  | 23|   4|
|Sumber       | 18|   4|

#### H. Spring Pasture Contract (y/n): 

|             | No| Yes|
|:------------|--:|---:|
|Bayan        | 21|   1|
|Bayantal     | 18|   2|
|Bayantsagaan | 22|   1|
|Buren        | 24|   3|
|Delgerkhaan  | 22|   0|
|Deren        | 23|   1|
|Erdenedalai  | 26|   1|
|Sumber       | 22|   0|
--------------------------------------------------------------------------------



# III. ALTERNATIVE LIVELIHOODS


### 1. Is someone in the household doing non-herding work?
  Column: altLife_nonHerdWork

```
##               
##                No Yes
##   Bayan        13   9
##   Bayantal     13   7
##   Bayantsagaan 16   7
##   Buren        21   6
##   Delgerkhaan  17   5
##   Deren        16   8
##   Erdenedalai  14  13
##   Sumber       15   6
```

#### A. If so, who is doing the non-herding work?   
  Column: labor_hire

|altLife_whoNoHerdwork   | Erdenedalai| Deren| Bayan| Bayantal| Bayantsagaan| Buren| Delgerkhaan| Sumber| Total|
|:-----------------------|-----------:|-----:|-----:|--------:|------------:|-----:|-----------:|------:|-----:|
|not specified           |           8|     7|     5|        5|            5|     3|           2|      1|    36|
|daughter                |           0|     0|     1|        1|            1|     3|           1|      1|     8|
|wife                    |           1|     2|     1|        1|            0|     0|           2|      1|     8|
|household head          |           1|     0|     1|        0|            1|     0|           0|      0|     3|
|son                     |           1|     0|     1|        1|            1|     0|           0|      1|     5|
|father                  |           1|     0|     0|        0|            0|     0|           0|      0|     1|
|husband                 |           1|     0|     0|        0|            0|     0|           0|      1|     2|
|extended family members |           0|     0|     0|        0|            0|     0|           0|      1|     1|

#### B. Is so, what is the non-herding work?  
  Column: labor_hire

|altLife_noHerdWhatWork               | Buren| Deren| Erdenedalai| Bayantal| Bayantsagaan| Sumber| Bayan| Delgerkhaan| Total|
|:------------------------------------|-----:|-----:|-----------:|--------:|------------:|------:|-----:|-----------:|-----:|
|employment unspecified               |     5|     1|           2|        0|            1|      0|     0|           0|     9|
|commerce-related and restaurants     |     1|     4|           4|        3|            2|      0|     2|           0|    16|
|mining and construction              |     0|     0|           0|        2|            3|      2|     2|           0|     9|
|education                            |     0|     0|           1|        1|            0|      3|     1|           2|     8|
|government employment                |     1|     2|           1|        1|            2|      1|     2|           2|    12|
|agriculture and pastoralist-adjacent |     0|     1|           2|        0|            1|      1|     0|           0|     5|
|government leadership                |     0|     0|           2|        0|            0|      0|     0|           0|     2|
|arts, crafts, and handwork           |     0|     0|           1|        1|            0|      0|     1|           1|     4|
|medical/veterinary                   |     0|     1|           0|        1|            0|      0|     1|           0|     3|
|mischellaneous                       |     0|     0|           1|        0|            1|      1|     0|           0|     3|


### 2. Additional sources of income for the household? Broken up by Soum
  Column: altLife_otherInc

|altLife_otherInc                 | Erdenedalai| Deren| Bayan| Buren| Delgerkhaan| Bayantal| Bayantsagaan| Sumber| Total|
|:--------------------------------|-----------:|-----:|-----:|-----:|-----------:|--------:|------------:|------:|-----:|
|government allowances            |          14|    11|    12|    12|          12|       11|           10|      9|    91|
|pension                          |          13|    13|     8|     9|           7|        4|           10|     10|    74|
|salary                           |           8|     7|    11|     6|           5|        7|            7|      4|    55|
|other                            |           3|     1|     1|     4|           0|        2|            1|      1|    13|
|saving in bank                   |           3|     2|     2|     3|           1|        0|            0|      1|    12|
|crafts                           |           3|     1|     0|     1|           1|        1|            0|      0|     7|
|remits                           |           0|     1|     0|     2|           1|        2|            0|      1|     7|
|pension saving in bank           |           0|     0|     0|     0|           0|        0|            1|      0|     1|
|hourly wage                      |           1|     0|     0|     0|           0|        0|            0|      1|     2|
|herding lsk from other household |           0|     0|     0|     0|           0|        0|            0|      1|     1|

### 3. Loans:

#### A. Number of loans taken out per year?
  Column: altLife_loansPerYr

|             |  0| 0.5|  1|  2|  3|
|:------------|--:|---:|--:|--:|--:|
|Bayan        |  4|   1| 16|  0|  0|
|Bayantal     |  6|   0| 11|  1|  0|
|Bayantsagaan |  2|   0| 17|  4|  0|
|Buren        |  3|   1| 19|  3|  1|
|Delgerkhaan  |  1|   0| 17|  4|  0|
|Deren        |  2|   0| 12|  8|  1|
|Erdenedalai  |  1|   1| 20|  4|  0|
|Sumber       |  4|   0| 16|  2|  0|

#### B. Loan Sizes (min, max, mean, median, range)  
  Column: altLife_loansMin/altLife_loansMax

|Soum         | min_minLoan| max_minLoan| mean_minLoan| median_minLoan| min_maxLoan| max_maxLoan| mean_maxLoan| median_maxLoan|
|:------------|-----------:|-----------:|------------:|--------------:|-----------:|-----------:|------------:|--------------:|
|Bayan        |         3.0|          30|     9.882353|           10.0|         3.0|          30|    15.294118|           14.0|
|Bayantal     |         2.0|          25|    10.000000|            9.0|        10.0|          30|    19.250000|           20.0|
|Bayantsagaan |         0.3|          10|     4.752381|            5.0|         1.0|          30|    10.466667|            6.0|
|Buren        |         1.0|          20|     5.104167|            4.0|         3.0|          20|     9.187500|            8.5|
|Delgerkhaan  |         1.0|          40|     8.619048|            5.0|         2.0|          50|    12.047619|            7.0|
|Deren        |         0.3|          50|     6.823810|            5.0|         0.5|          50|    10.119048|            5.0|
|Erdenedalai  |         0.5|          20|     4.286000|            3.0|         2.0|          50|     8.240000|            5.0|
|Sumber       |         1.0|          30|     6.222222|            3.5|         1.0|          30|     8.472222|            6.5|

#### C. When do you typically need loans?  
  Column: altLife_loansWhenNeed

|altLife_loansWhenNeed    | Buren| Delgerkhaan| Bayan| Bayantsagaan| Deren| Bayantal| Sumber| Erdenedalai| Total|
|:------------------------|-----:|-----------:|-----:|------------:|-----:|--------:|------:|-----------:|-----:|
|winter                   |    18|          16|    14|           14|    14|       13|     13|          10|   112|
|spring                   |     9|           4|     4|            3|     2|        3|      0|           6|    31|
|autumn                   |     7|           8|     6|            4|     8|        7|      8|           8|    56|
|lunar new year           |     6|           3|     5|            5|     6|        2|      5|           2|    34|
|summer                   |     0|           2|     1|            0|     2|        1|      1|           3|    10|
|year round               |     0|           0|     0|            0|     1|        1|      1|           2|     5|
|depends on needs         |     0|           0|     0|            1|     0|        1|      0|           1|     3|
|never                    |     1|           1|     0|            1|     1|        1|      0|           0|     5|
|during medical treatment |     0|           1|     0|            0|     0|        0|      0|           0|     1|
|during migration         |     0|           0|     0|            0|     0|        0|      0|           1|     1|















