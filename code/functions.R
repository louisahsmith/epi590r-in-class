
new_mean <- function(x) {
	n <- length(x)
	mean_value <- sum(x) / n
	return(mean_value)
}

y <- c(34563, 234, 2352, 7457, 865, 2534)

new_mean(y)
mean(y)


square <- function(z) {
	squared_value <- z*z
	return(squared_value)
}
z <- 5
z*z

square(8)

add_two_numbers <- function(number1, number2) {
	result <- number1 + number2
	return(result)
}

add_two_numbers(number1 = 5, number2 = 26)
add_two_numbers(5, 26)
add_two_numbers(x = 5, y = 26)

add_two_numbers <- function(x, y) {
	result <- x + y
	return(result)
}

add_two_numbers(x = 5, y = 26)

add_two_numbers()

add_two_numbers <- function(x, y) {
	result <- number1 + number2
	return(result)
}

add_two_numbers(5, 26)

add_two_numbers <- function(x, y) {
	result <- x + y
	return(result)
}
add_two_numbers(4, 5) + 8

add_two_numbers <- function(x, y) {
	result <- x + y
	sentence <- paste("The result is", result)
	return(sentence)
}
add_two_numbers(4, 5)

raise <- function(x, power = 2) {
	result <- x^power
	return(result)
}

raise(x = 2, power = 4)
# should give you
2^4
raise(x = 5, power = 3)

raise(x = 5)
# should give you
5^2

new_mean <- function(x) {
	n <- length(x)
	mean_value <- sum(x) / n
	return(mean_value)
}

x <- c(3, 345, 356, NA)
x
na.omit(x)
na.rm <- FALSE
sd(x, na.rm = FALSE)
var(x, na.rm = TRUE)


na.rm <- TRUE
if (na.rm) {
	print("TRUE")
}

std_dev <- function(x, na.rm = TRUE) {
	# if na.rm is TRUE, remove NA
	# if na.rm is FALSE, don't remove NA
	if (na.rm) {
		x <- na.omit(x)
	}
	denominator <- length(x) - 1
	if (denominator <= 0) {
		return(NA)
	} else {
		mean_x <- mean(x)
		differences <- x - mean_x
		squared_differences <- differences^2
		numerator <- sum(squared_differences)
		standard_dev <- sqrt(numerator / denominator)
		return(standard_dev)
	}
}


x <- c(345, 234,234, 345634, 346, 234, NA)
std_dev(x, na.rm = TRUE)
sd(x, na.rm = TRUE)
std_dev(4)

std_dev <- function(x) {
	# if (length(x) <= 1) {
	# 	return(NA)
	# }


	denominator <- length(x) - 1
  mean_x <- mean(x)
  differences <- x - mean_x
  squared_differences <- differences^2
  # numerator <- sum((x - mean_x)^2)
  numerator <- sum(squared_differences)
  standard_dev <- sqrt(numerator / denominator)

	if (denominator <= 0) {
		return(NA)
	} else {
		return(standard_dev)
	}
}
