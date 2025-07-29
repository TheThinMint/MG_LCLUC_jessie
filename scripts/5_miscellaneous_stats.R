# Mischellaneous Analyses - where else should I put them? 
# All dataframes for comparison created in "1_data_import.r"

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)


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

# Display without the count column
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

# Step 1: Reshape and calculate proportions
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

# Step 2: Create the stacked bar chart
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
cols_to_plot2 <- c("herdMgmt_thisYr_wintPast", "herdMgmt_lastYr_wintPast", "herdMgmt_thisYr_springPast", "herdMgmt_lastYr_springPast",
  "herdMgmt_thisYr_DzudPast", "herdMgmt_lastYr_DzudPast")

# Step 1: Reshape and calculate proportions
plot_data2 <- base_HERDMGMT %>%
  select(all_of(cols_to_plot2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(
    percent = count / sum(count) * 100
  ) %>%
  ungroup()

# Step 2: Create the stacked bar chart
ggplot(plot_data2, aes(x = variable, y = percent, fill = response)) +
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



