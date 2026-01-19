
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
library(broom)
library(ggrepel)
library(car) 

# Load the dataset

my_data <- haven::read_dta("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/dataset_complete.dta")

# Convert variables into according types 

my_data$pm25 <- as.numeric(my_data$pm25)
my_data$borough <- factor(my_data$borough)
my_data$phq9 <- as.numeric(my_data$phq9)

str(my_data$pm25)
str(my_data$phq9)

# Apply linear model accordingly to DAG

m1 <- lm(phq9 ~ pm25 + borough, data = my_data)

modelsummary(list("Adjusted (Borough)" = m1), fmt = 2, statistic = "conf.int")

# Put the statistics into a table

tab <- tidy(m1, conf.int = TRUE)
ft <- flextable(tab)
save_as_docx(ft, path = "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/model_results.docx")


# Plot the model with both regression and LOESS line

plot_1 <- my_data |>
  drop_na(pm25, phq9) |>
  ggplot() +
  aes(x = pm25,
      y = phq9) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(
    x = "PM2.5 (mg/m³/year)",
    y = "PHQ-9 depression score",
    title = "                        Relationship between PM2.5 and PHQ-9"
  )

ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_1.jpg", plot = plot_1)  

# Plot fitted values against residuals with LOESS

plot_data <- data.frame(
  fitted_values = fitted(m1),
  residuals     = residuals(m1)
)

# Check for range 

summary(residuals(m1))

# Plot

plot_2 <- plot_data |>
  ggplot(aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, colour = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -10, linetype = "dashed", colour = "black") +
  geom_hline(yintercept =  10, linetype = "dashed", colour = "black") +
  labs(
    title = "Fitted Values vs Residuals (Model Adjusted for Borough)",
    x = "Fitted PHQ-9 values",
    y = "Residuals"
  )

ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_2.jpg", plot = plot_2)  


# Show Normality of the Residuals

res_data <- data.frame(
  resid = resid(m1)
)

# Plot Q-Q graph

plot_3 <- ggplot(res_data, aes(sample = resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(
    title = "Q–Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Residuals"
  )

ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_3.jpg", plot = plot_3)  

# Plot histogram

plot_4 <- res_data |>
  ggplot() +
  aes(x = resid) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 color = "black",
                 fill = "lightpink") +
  geom_density(color = "red")

ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_4.jpg", plot = plot_4)  


# Studentized residuals vs fitted values (outlier check)

plot_data2 <- augment(m1)
plot_data2$.id <- seq_len(nrow(plot_data2))   # create an ID column


plot_5 <- plot_data2 |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.6) +
  geom_label_repel(aes(label = if_else(abs(.std.resid) > 3, as.character(.id), ""))) +
  geom_hline(yintercept = 3,  linetype = "dashed", color = "red") +
  geom_hline(yintercept = -3, linetype = "dashed", color = "red") +
  labs(
    title = "Studentized Residuals vs Fitted Values",
    x = "Fitted PHQ-9 values",
    y = "Studentized residuals"
  )
plot_5
ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_5.jpg",
       plot = plot_5)

# Check for leverage points
# Plot leverage vs studentised residuals

plot_6 <-
  plot_data2 |>
  ggplot(aes(x = .hat, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 3,  linetype = "dashed", color = "red") +
  geom_hline(yintercept = -3, linetype = "dashed", color = "red") +
  geom_label_repel(
    aes(label = if_else(abs(.std.resid) > 3, as.character(.id), ""))
  ) +
  labs(
    title = "Leverage vs. Studentized Residuals",
    x = "Leverage (Hat-Values)",
    y = "Studentized Residuals"
  )

plot_6

ggsave("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/plot_6.jpg",
       plot = plot_6)


# Calculate VIF for pm25

vif_values <- as.data.frame(vif(m1))

vif_values$Predictor <- rownames(vif_values)
rownames(vif_values) <- NULL
vif_values <- vif_values |> select(Predictor, everything())

# Turn into flextable
vif_table <- flextable(vif_values)

save_as_docx(vif_table, path = "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/vif_results.docx")


