new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}


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
