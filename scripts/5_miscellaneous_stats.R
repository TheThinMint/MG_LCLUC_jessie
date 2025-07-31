# Mischellaneous Analyses - where else should I put them? 
# All dataframes for comparison created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)
library(tidyverse)


###Contract & Reservation Cross-tabs -------------------------------------------
merged_data <- base_HERDMGMT %>%
  inner_join(base_TENURE, by = "Ref")

contract_cols <- c("wintContract", "wintPasContract", "sprCampContract", "sprPasContract")
reservation_cols <- c(
  "herdMgmt_thisYr_wintPast", "herdMgmt_lastYr_wintPast",
  "herdMgmt_thisYr_springPast", "herdMgmt_lastYr_springPast",
  "herdMgmt_thisYr_DzudPast", "herdMgmt_lastYr_DzudPast")

cross_counts <- expand.grid(
  res_col = reservation_cols,
  res_val = c("Yes", "No"),
  con_col = contract_cols,
  con_val = c("Yes", "No"),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    count = sum(
      merged_data[[res_col]] == res_val & merged_data[[con_col]] == con_val,
      na.rm = TRUE)
  ) %>%
  ungroup()

cross_table <- cross_counts %>%
  mutate(
    res_label = paste0(res_col, ": ", res_val),
    con_label = paste0(con_col, ": ", con_val)
  ) %>%
  select(res_label, con_label, count)

ordered_con_labels <- unlist(lapply(contract_cols, function(col) {
  c(paste0(col, ": No"), paste0(col, ": Yes"))
}))

cross_table_wide <- cross_table %>%
  pivot_wider(
    names_from = con_label,
    values_from = count,
    values_fill = 0
  ) %>%
  select(res_label, all_of(ordered_con_labels))

view(cross_table_wide)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



###Contract Percentages --------------------------------------------------------
cols_to_summarize <- c(
  "wintCamp", "wintContract", "wintPas", "wintPasContract",
  "sprCamp", "sprCampContract", "sprPasContract")

summary_table <- base_TENURE %>%
  select(all_of(cols_to_summarize)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(
    percent = round(100 * count / sum(count), 1)
  ) %>%
  ungroup() %>%
  select(variable, response, percent) %>%   # <- This line drops the count column
  pivot_wider(names_from = response, values_from = percent, values_fill = 0)

kable(summary_table, col.names = c("Variable", "No (%)", "Yes (%)"))
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



###Reservation Percentages --------------------------------------------------------
cols_to_summarize <- c(
  "herdMgmt_thisYr_wintPast", "herdMgmt_lastYr_wintPast", "herdMgmt_thisYr_springPast", "herdMgmt_lastYr_springPast",
  "herdMgmt_thisYr_DzudPast", "herdMgmt_lastYr_DzudPast")

summary_table <- base_HERDMGMT %>%
  select(all_of(cols_to_summarize)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(
    percent = round(100 * count / sum(count), 1)
  ) %>%
  ungroup() %>%
  select(variable, response, percent) %>%   # <- This line drops the count column
  pivot_wider(names_from = response, values_from = percent, values_fill = 0)

# Display without the count column
kable(summary_table, col.names = c("Variable", "No (%)", "Yes (%)"))
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



###Contract Bar Chart --------------------------------------------------------
cols_to_plot <- c("wintCamp", "wintContract", "wintPas", "wintPasContract", "sprCamp", "sprCampContract", "sprPasContract")

plot_data <- base_TENURE %>%
  select(all_of(cols_to_plot)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(
    percent = count / sum(count) * 100
  ) %>%
  ungroup()

ggplot(plot_data, aes(x = variable, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(
    x = NULL,
    y = "Percentage",
    fill = "Response",
    title = "Proportion of Yes/No Responses by Category"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



###Reservation Bar Chart --------------------------------------------------------
cols_to_plot2 <- c(
  "herdMgmt_thisYr_wintPast", "herdMgmt_lastYr_wintPast",
  "herdMgmt_thisYr_springPast", "herdMgmt_lastYr_springPast",
  "herdMgmt_thisYr_DzudPast", "herdMgmt_lastYr_DzudPast"
)

# Short labels (edit as you like)
var_labels <- c(
  herdMgmt_thisYr_wintPast   = "Winter Pasture\nThis Year",
  herdMgmt_lastYr_wintPast   = "Winter Pasture\nLast Year",
  herdMgmt_thisYr_springPast = "Spring Pasture\nThis Year",
  herdMgmt_lastYr_springPast = "Spring Pasture\nLast Year",
  herdMgmt_thisYr_DzudPast   = "Dzud Pasture\nThis Year",
  herdMgmt_lastYr_DzudPast   = "Dzud Pasture\nLast Year"
)

plot_data2 <- base_HERDMGMT %>%
  select(all_of(cols_to_plot2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(
    variable = recode(variable, !!!var_labels),
    # keep a sensible order in the flipped plot
    variable = factor(variable, levels = rev(unique(var_labels)))
  )

ggplot(plot_data2, aes(x = variable, y = percent, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(
    x = NULL, y = "Percentage", fill = "Response",
    title = "Proportion of Yes/No Responses by Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",                 # move legend off the right margin
    legend.box = "vertical",
    legend.margin = margin(0, 0, 0, 0),
    axis.text.y = element_text(size = 10),      # slightly smaller to reduce left margin
    plot.title = element_text(size = 14, face = "bold"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)    # standard margins
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))  # compact legend
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------


###Herding Distance Histograms -------------------------------------------------
df_long <- base_HERDMGMT %>% 
  select(herdMgmt_sumDailyDist, herdMgmt_wintDailyDist) %>%
  pivot_longer(
    cols = everything(),
    names_to = "season",
    values_to = "distance"
  ) %>%
  mutate(
    season = recode(season,
                    "herdMgmt_sumDailyDist" = "Summer",
                    "herdMgmt_wintDailyDist" = "Winter")
  )

medians <- df_long %>%
  group_by(season) %>%
  summarise(med = median(distance, na.rm = TRUE))

ggplot(df_long, aes(x = distance, fill = season)) +
  geom_histogram(color = "black", bins = 30) +
  geom_vline(data = medians, aes(xintercept = med, color = season),
             linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Summer" = "#fc8d62", "Winter" = "#3273a8")) +
  scale_color_manual(values = c("Summer" = "#b2182b", "Winter" = "#5de3f5")) +
  facet_wrap(~season, scales = "free") +
  labs(
    title = "Daily Herding Distance by Season",
    x = "Distance (km)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



###Means of Herding Travel, Plots ----------------------------------------------
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
      year == "travel_10" ~ "10 Years Ago",
      year == "travel_5" ~ "5 Years Ago",
      year == "travel_1" ~ "Last Year"
    )
  )

travel_counts <- travel_long_clean %>%
  count(year, transport)

view(travel_counts)

ggplot(travel_counts,
       aes(x = year, y = n, group = transport, color = transport)) +
  geom_line(aes(), linewidth = 1.2) +
  geom_point(aes(), size = 3,
             position = position_dodge(width = 0.15)) +
  theme_minimal() +
  labs(title = "Reported Herding Travel Modes Over Time",
       x = "Year", y = "Count of Mentions", color = "Travel Mode")
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------




###Distance of Herding Travel, Plots ----------------------------------------------
distance_long <- base_HERDMGMT %>%
  select(Soum,
         dist_10 = herdMgmt_10yrsAgo_dailyDist,
         dist_5 = herdMgmt_5yrsAgo_dailyDist,
         dist_1 = herdMgmt_lastYr_dailyDist) %>%
  pivot_longer(
    cols = starts_with("dist_"),
    names_to = "year",
    values_to = "distance"
  ) %>%
  filter(!is.na(distance)) %>%
  mutate(
    year = case_when(
      year == "dist_10" ~ "10 Years Ago",
      year == "dist_5" ~ "5 Years Ago",
      year == "dist_1" ~ "Last Year"
    ),
    # Step 2: Bin distances into 5 km intervals
    distance_bin = cut(
      distance,
      breaks = seq(0, 100, by = 5),  # from 0 to 100 in 5 km intervals
      include.lowest = TRUE,
      right = TRUE,
      labels = paste(seq(0, 95, 5), seq(5, 100, 5), sep = "-")
    )
  )

distance_counts <- distance_long %>%
  count(year, distance_bin)

ggplot(distance_counts, aes(x = year, y = n, group = distance_bin, color = distance_bin)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Herding Distance Trends Over Time",
    x = "Year",
    y = "Count of Respondents",
    color = "Daily Distance (km)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



