library(tidyverse)
library(gtsummary)

# load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


## The problem ----------------------------------------------------------------

# we keep writing the same thing with a different variable
summarise(nlsy, mean = mean(income, na.rm = TRUE))
summarise(nlsy, mean = mean(age_bir, na.rm = TRUE))
summarise(nlsy, mean = mean(nsibs, na.rm = TRUE))

# the obvious function DOESN'T work
# run it and read the error -- this is the point of the exercise
summarize_var_bad <- function(data, variable) {
  data |>
    summarise(mean = mean(variable, na.rm = TRUE))
}
summarize_var_bad(nlsy, income)

# dplyr looks for a COLUMN literally named `variable`, doesn't find one, gives up


## The fix: {{ }} -------------------------------------------------------------

summarize_var <- function(data, variable) {
  data |>
    summarise(
      n    = sum(!is.na({{ variable }})),
      mean = mean({{ variable }}, na.rm = TRUE),
      sd   = sd({{ variable }}, na.rm = TRUE)
    )
}

summarize_var(nlsy, income)
summarize_var(nlsy, age_bir)

# it takes data first, so it pipes
nlsy |> summarize_var(nsibs)


## {{ }} works anywhere dplyr does --------------------------------------------

# with .by =
# note the NULL default, so the group argument is optional
summarize_by <- function(data, variable, group = NULL) {
  data |>
    summarise(
      mean = mean({{ variable }}, na.rm = TRUE),
      n    = n(),
      .by  = {{ group }}
    )
}

summarize_by(nlsy, income, sex_cat)
summarize_by(nlsy, income) # still works with no group

# naming the output column after the variable
# note := instead of = when the name is computed
mean_named <- function(data, variable) {
  data |>
    summarise("mean_{{ variable }}" := mean({{ variable }}, na.rm = TRUE))
}

mean_named(nlsy, income)
mean_named(nlsy, age_bir)

# in ggplot
plot_hist <- function(data, variable) {
  ggplot(data, aes(x = {{ variable }})) +
    geom_histogram(bins = 30) +
    theme_minimal()
}

plot_hist(nlsy, income)
plot_hist(nlsy, age_bir)

# in gtsummary -- write the formatting once, change the grouping variable freely
table_by <- function(data, group) {
  data |>
    tbl_summary(
      by = {{ group }},
      include = c(race_eth_cat, eyesight_cat, age_bir)
    ) |>
    add_overall() |>
    bold_labels()
}

table_by(nlsy, sex_cat)
table_by(nlsy, region_cat)


## Where {{ }} does NOT work --------------------------------------------------

# lm() is base R -- it doesn't know anything about tidy evaluation
# run this and read the error
fit_bad <- function(data, predictor) {
  lm(income ~ {{ predictor }}, data = data)
}
fit_bad(nlsy, age_bir)

# instead, pass a string and build the formula with reformulate()
fit_income <- function(data, predictors) {
  lm(reformulate(predictors, response = "income"), data = data)
}

coef(fit_income(nlsy, "age_bir"))

# reformulate() takes a vector, so multivariable models come for free
coef(fit_income(nlsy, c("age_bir", "sex_cat", "race_eth_cat")))


## When the variable is a string, use .data[[ ]] -------------------------------

summarize_string <- function(data, var_name) {
  data |>
    summarise(
      variable = var_name,
      mean     = mean(.data[[var_name]], na.rm = TRUE)
    )
}

summarize_string(nlsy, "income")

# a typo gives you a clear error, which is a nice property
summarize_string(nlsy, "incom")


## Why strings are useful: you can loop over them ------------------------------

# purrr::map() applies a function to each element; list_rbind() stacks results
map(c("income", "age_bir", "nsibs"), \(v) summarize_string(nlsy, v)) |>
  list_rbind()

# same idea with models -- compare with the copy-and-paste version
# in broom-examples.R
map(
  c("age_bir", "sex_cat", "race_eth_cat"),
  \(p) broom::tidy(fit_income(nlsy, p), conf.int = TRUE)
) |>
  list_rbind()


#### Exercises ####

# 1. Write summarize_var() yourself, from scratch, using {{ }}.
#    Test it on income, age_bir, and nsibs.

# 2. Add a `group` argument using .by = {{ group }}, with a default so that
#    the function still works when you don't pass a group.

# 3. Write a function that takes a dataset and a grouping variable and returns
#    a gtsummary table stratified by it. Add at least one formatting function
#    (bold_labels(), add_overall(), add_p(), modify_caption(), ...).
#    Then call it twice with different grouping variables.

# 4. Confirm that the {{ }} version of the lm() function fails, and read the
#    error. Then rewrite it with a string argument and reformulate().

# 5. Switch to your FINAL PROJECT project. Write a function that does something
#    you'd otherwise copy and paste, and use it at least twice with different
#    variables. This satisfies the "write and use a function" objective.
