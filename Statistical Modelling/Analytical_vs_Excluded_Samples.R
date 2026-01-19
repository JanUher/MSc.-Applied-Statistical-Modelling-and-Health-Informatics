

library(haven)
library(dplyr)
library(tibble)
library(ggplot2)
library(gt)

# paths 
data_dir <- "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework"
out_dir  <- file.path(data_dir, "Section 3")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# load datasets from stata
complete <- read_dta(file.path(data_dir, "dataset_complete.dta"))
missing  <- read_dta(file.path(data_dir, "dataset_missing.dta"))

# combine into overall sample
overall <- bind_rows(complete %>% mutate(sample = "Analytical"),
                     missing  %>% mutate(sample = "Excluded"))

# define variables which missingness will be assessed  
vars <- c("age", "ses", "pm25", "phq9", "gender", "smoker", "exercise")
vars <- vars[vars %in% names(overall)]  # keep only those that exist

# table to see overall missingness extent
missing_summary <- tibble(Sample = c("Analytical", "Excluded"),
                          N = c(nrow(complete), nrow(missing))) %>%
  mutate(Percent = 100 * N / sum(N),
         Percent_label = paste0(round(Percent, 1), "%"))

# save extent table
write.csv(missing_summary, file.path(out_dir, "missing_data_extent.csv"), row.names = FALSE)

# percent pie chart with labels 
plot_pie_pct <- ggplot(missing_summary, aes(x = "", y = N, fill = Sample)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Percent_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Overall proportion excluded due to missing data",
       subtitle = "Excluded = any missing in required variables (listwise deletion)",
       x = NULL, y = NULL) +
  theme_void() +
  theme(plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA))

ggsave(filename = file.path(out_dir, "missingness_pie_percent.png"),
       plot = plot_pie_pct, width = 6, height = 4, device = "png", bg = "white")


## Missingness by variable -------


# Convert labelled categorical variables to factors (prevents haven_labelled issues)
labelled_to_factor <- intersect(c("gender", "smoker", "borough", "employment"), names(overall))
overall <- overall %>% mutate(across(all_of(labelled_to_factor), ~ haven::as_factor(.x)))

# Treat "NA"/"" as missing for character variables (safe even if already cleaned)
overall_clean <- overall %>% mutate(across(all_of(vars), ~ { if (is.character(.x)) { .x[.x %in% c("NA", "N/A", "")] <- NA }; .x }))

missing_by_var <- tibble(Variable = vars,
                         Missing_n  = sapply(vars, function(v) sum(is.na(overall_clean[[v]]))),
                         Observed_n = sapply(vars, function(v) sum(!is.na(overall_clean[[v]])))) %>%
  mutate(Total_n      = Missing_n + Observed_n,
         Missing_pct  = round(100 * Missing_n / Total_n, 1),
         Observed_pct = round(100 * Observed_n / Total_n, 1)) %>%
  arrange(desc(Missing_pct))

# Save as CSV
write.csv(missing_by_var, file.path(out_dir, "missingness_by_variable.csv"), row.names = FALSE)



## Compare Analytical vs Excluded --------


# Force intended continuous variables to be numeric 
overall_comp <- overall_clean %>%
  mutate(age      = if ("age" %in% names(.)) as.numeric(as.character(age)) else age,
         ses      = if ("ses" %in% names(.)) as.numeric(as.character(ses)) else ses,
         pm25     = if ("pm25" %in% names(.)) as.numeric(as.character(pm25)) else pm25,
         phq9     = if ("phq9" %in% names(.)) as.numeric(as.character(phq9)) else phq9,
         exercise = if ("exercise" %in% names(.)) as.numeric(as.character(exercise)) else exercise)

# Continuous comparison mean (SD) and median (IQR)
comparison_cont <- bind_rows(lapply(intersect(c("age","ses","pm25","phq9","exercise"), vars), function(v) {
  overall_comp %>%
    group_by(sample) %>%
    summarise(n = sum(!is.na(.data[[v]])),
              mean = mean(.data[[v]], na.rm = TRUE),
              sd   = sd(.data[[v]], na.rm = TRUE),
              median = median(.data[[v]], na.rm = TRUE),
              q1 = quantile(.data[[v]], 0.25, na.rm = TRUE),
              q3 = quantile(.data[[v]], 0.75, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Variable = v,
           mean_sd = paste0(round(mean, 2), " (", round(sd, 2), ")"),
           med_iqr = paste0(round(median, 2), " (", round(q1, 2), ", ", round(q3, 2), ")")) %>%
    select(Variable, sample, n, mean_sd, med_iqr)
}))

# Categorical comparison n (%)
comparison_cat <- bind_rows(lapply(intersect(c("gender","smoker"), vars), function(v) {
  overall_comp %>%
    filter(!is.na(.data[[v]])) %>%
    count(sample, .data[[v]], name = "n") %>%
    group_by(sample) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    mutate(Variable = v) %>%
    rename(Level = !!v) %>%
    select(Variable, Level, sample, n, pct)
}))

# Save outputs for section (b)
write.csv(comparison_cont, file.path(out_dir, "B_comparison_continuous.csv"), row.names = FALSE)
write.csv(comparison_cat,  file.path(out_dir, "B_comparison_categorical.csv"), row.names = FALSE)

if (nrow(comparison_cont) > 0) { gtsave(comparison_cont %>% gt() %>% tab_header(title = "Analytical vs Excluded sample comparison (Continuous variables)",
                                                                                subtitle = "n, mean (SD), and median (IQR) by sample"),
                                        filename = file.path(out_dir, "B_comparison_continuous.html")) }

if (nrow(comparison_cat) > 0) { gtsave(comparison_cat %>% gt() %>% tab_header(title = "Analytical vs Excluded sample comparison (Categorical variables)",
                                                                              subtitle = "Counts and percentages by sample"),
                                       filename = file.path(out_dir, "B_comparison_categorical.html")) }
