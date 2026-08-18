# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
x*x
square <- function(x) {
	squared_val <- x*x
	return(squared_val)
	}
# test it out
square(x)
square(53)
53^2 # does this match?

#Raising to any power
raise <- function(x, power) {
		value_raised <- x^power
		return(value_raised)
	}

# test with
raise(x = 2, power = 4)
# should give you
2^4

#Default to squaring
raise_default <- function(x, arg2 = 2) {
	value_squared <- x^2
	return(value_squared)
}
# test
raise_default(x = 5)
# should give you
5^2



x <- c(234, 634, 234, 124, 5437, 2354345)
demeaned_x <- x - mean(x)
squared_demeaned_x <- demeaned_x^2
sumofsquares <- sum(squared_demeaned_x)
n_minus_1 <- length(x) - 1
std_dev <- sqrt(sumofsquares/n_minus_1)



std_dev <- function(x) {
	demeaned_x <- x - mean(x)
	squared_demeaned_x <- demeaned_x^2
	sumofsquares <- sum(squared_demeaned_x)
	n_minus_1 <- length(x) - 1
	std_dev <- sqrt(sumofsquares/n_minus_1)
	if(n_minus_1 + 1 <= 1){
		std_dev <- "NA"
	}
	return(std_dev)
	}
std_dev(x)
sd(x)
std_dev(2)

x <- c(3, 6, NA)

new_sd <- function(x, na.rm = TRUE) {
	if (na.rm) {
		# remove NAs
		new_x <- na.omit(x)
	} else {
		# don't remove NAs
		new_x <- x
	} # will have a new value of x if NAs are removed, same value if not
	if (length(new_x) <= 1) {
		return_val <- NA
	} else {
		# calculate the standard deviation using new x and save as return_val
		demeaned_x <- new_x - mean(new_x)
		squared_demeaned_x <- demeaned_x^2
		sum_of_squares <- sum(squared_demeaned_x)
		n_minus_1 <- length(new_x) - 1
		return_val <- sqrt(sum_of_squares / n_minus_1)
	}
	return(return_val)
}
new_sd(nlsy$income)
sd(nlsy$income)
sd(nlsy$income, na.rm = TRUE)
new_sd(c(1, NA, NA))
new_sd(c(1))

new_sd <- function(x) {
	sd(x, na.rm = TRUE)
}


#Exercises
# 1. Write summarize_var_new() yourself, which returns the median, 25% percentile,
#    and 75% percentile of a variable using {{ }}.
#    Test it on income, age_bir, and nsibs.

summarize_var_new <- function(data, variable) {
	summarize(data,
						median = median({{ variable }}, na.rm = TRUE),
						pctl_25 = quantile({{ variable }}, p = .25, na.rm = TRUE), #{p25}
						pctl_75 = quantile({{ variable }}, p = .75, na.rm = TRUE)
	)
}

# test on a few variables
summarize_var_new(nlsy, income)
summarize_var_new(nlsy, age_bir)
summarize_var_new(nlsy, nsibs)

# 2. Add a `group` argument using .by = {{ group }}, with a default so that
#    the function still works when you don't pass a group.

summarize_var_new <- function(data, variable, group = NULL) {
	summarize(data,
						median = median({{ variable }}, na.rm = TRUE),
						pctl_25 = quantile({{ variable }}, p = .25, na.rm = TRUE), #{p25}
						pctl_75 = quantile({{ variable }}, p = .75, na.rm = TRUE),
						.by = {{ group }}
	)
}

summarize_var_new(nlsy, income, region_cat)
summarize_var_new(nlsy, income) # should still work

# 3. Write a function summarize_two_vars() that takes a dataset and two variables and
#    returns their correlation and covariance. Use {{ }} to pass the variables. Test it
#    on income and age_bir, and on income and nsibs.

# 4. Write a function that takes a dataset and a grouping variable and returns
#    a gtsummary table stratified by it. Add at least one formatting function
#    (bold_labels(), add_overall(), add_p(), modify_caption(), ...).
#    Then call it twice with different grouping variables.

table_by <- function(data, group) {

}

table_by(nlsy, sex_cat)
table_by(nlsy, region_cat)
