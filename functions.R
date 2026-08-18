#### PART 1 ####

new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

mean(c(1, 5, 9))
new_mean(c(1, 5, 9))


# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
square <- function(x) {
  squared_val <- x * x
  return(squared_val)
}
# test it out
square(x)
square(53)
53^2 # does this match?


#### PART 2 ####

prop <- function(x, multiplier = 1) {
	n <- length(x)
	mean_val <- multiplier * sum(x) / n
	return(mean_val)
}

raise <- function(x, power) {
	val <- x ^ power
	return(val)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

raise(x = 3, power = 3)

raise <- function(x, power = 2) {
	val <- x ^ power
	return(val)
}

# test
raise(x = 5)
# should give you
5^2

# same
raise(x = 2, power = 4)
raise(power = 4, x = 2)
# different
raise(2, 4)
raise(4, 2)


my_function <- function(x = 10){
	y <- 2*x + 4
	return(y)
}
for (i in 1:100) {
	print(my_function(i))
}

my_function(1)
my_function()


library(tidyverse)
library(gtsummary)
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

new_mean(nlsy$income)

#### PART 3 ####

prop <- function(x, percentage = FALSE) {
	n <- length(x)
	mean_val <- sum(x) / n
	if (percentage) {
		mean_val <- mean_val * 100
	} else {
		# don't actually need this else statement!
		mean_val <- mean_val
	}
	return(mean_val)
}

x <- c(234, 634, 234, 124, 5437, 2354345)

demeaned_x <- x - mean(x)
squared_demeaned_x <- demeaned_x^2
sum_of_squares <- sum(squared_demeaned_x)
n_minus_1 <- length(x) - 1
std_dev <- sqrt(sum_of_squares / n_minus_1)

sd(x)


new_sd <- function(x) {
	demeaned_x <- x - mean(x)
	squared_demeaned_x <- demeaned_x^2
	sum_of_squares <- sum(squared_demeaned_x)
	n_minus_1 <- length(x) - 1
	std_dev <- sqrt(sum_of_squares / n_minus_1)
	return(std_dev)
}
new_sd(c(345, 123, 7564, 345, 123, 4653, 4346, 346, 76))
sd(c(345, 123, 7564, 345, 123, 4653, 4346, 346, 76))


new_sd(2)
sd(2)
