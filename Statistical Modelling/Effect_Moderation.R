library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(haven)
library(gtsummary)
library(modelsummary)
library(broom)
library(flextable)
library(ggplot2)
library(ggrepel)
library(car)

# load dataset
my_data <- haven::read_dta(
  "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/dataset_complete.dta"
)

# factorise variables so that they are compatible with the analysis ------
my_data$pm25   <- as.numeric(my_data$pm25)
my_data$phq9   <- as.numeric(my_data$phq9)
my_data$borough <- factor(my_data$borough)
my_data$smoker <- trimws(as.character(my_data$smoker))
my_data$smoker[my_data$smoker %in% c("NA", "N/A", "")] <- NA
my_data$smoker <- factor(
  my_data$smoker,
  levels = c("1", "2"),
  labels = c("Non-smoker", "Smoker")
)
my_data$smoker <- relevel(my_data$smoker, ref = "Non-smoker")

# plot a model with adequate effect modification -------
m2 <- lm(phq9 ~ pm25 * smoker + borough, data = my_data)

tab_int <- tidy(m2, conf.int = TRUE)
ft_int  <- flextable(tab_int)

save_as_docx(
  ft_int,
  path = "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/Section 5/model_results_interaction.docx"
)

anova_results <- Anova(m2, type = 3)
anova_results

beta_nonsmoker <- coef(m2)["pm25"]
int_name <- grep("^pm25:smoker", names(coef(m2)), value = TRUE)
beta_smoker <- coef(m2)["pm25"] + coef(m2)[int_name]

beta_nonsmoker
beta_smoker

newdata <- expand.grid(
  pm25 = seq(min(my_data$pm25, na.rm = TRUE),
             max(my_data$pm25, na.rm = TRUE),
             length.out = 100),
  smoker = levels(my_data$smoker),
  borough = levels(my_data$borough)[1]
)

newdata$predicted_phq9 <- predict(m2, newdata)

# plot interaction plot------
plot_int <- ggplot(newdata,
                   aes(x = pm25,
                       y = predicted_phq9,
                       color = smoker)) +
  geom_line(size = 1.1) +
  labs(
    title = "Moderation of PM2.5 Effect on PHQ-9 by Smoking Status",
    x = "PM2.5 (mg/m³/year)",
    y = "Predicted PHQ-9 score",
    color = "Smoking status"
  ) +
  theme_minimal()

ggsave(
  "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/Section 5/interaction_plot.jpg",
  plot = plot_int,
  width = 7,
  height = 5
)
