library(haven)
library(dplyr)
library(broom)
library(flextable)

# load dataset
d <- read_dta("C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/dataset_complete.dta")

# restrict borough variable to Greenwich only 
d_g <- d %>%
  filter(as_factor(borough) == "Greenwich") %>%
  mutate(
    pm25 = as.numeric(pm25),
    mdd  = as.numeric(haven::zap_labels(mdd))  # ensures 0/1 numeric
  )

# fit logistic regression model

m6 <- glm(mdd ~ pm25, family = binomial(link = "logit"), data = d_g)

# odds ratios
or_tab <- tidy(m6, conf.int = TRUE, exponentiate = TRUE)
or_tab


# save outputs
out_dir <- "C:/Users/xxx/KCL/Introduction to Statistical Modelling/Coursework/Section 6"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(or_tab, file.path(out_dir, "mdd_logistic_OR_table.csv"), row.names = FALSE)

save_as_docx(
  flextable(or_tab),
  path = file.path(out_dir, "mdd_logistic_OR_table.docx")
)


