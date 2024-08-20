# start out with a number to test
x <- 5
# you'll want your function to return this number
x^2

#creating my own square function
square <- function(x) {
	thesquare <- x^2
	return(thesquare)
}
# test it out
square(x)
square(53)
53^2 # does this match?

#creating my own proportion function
prop <- function(y, multiplier = 1) {
	n <- length(y)
	prop_val <- sum(y) / n
	multiplied_val <- multiplier * prop_val
	return(multiplied_val)
}

y <- (c(0,0,1,1,1))
prop(y)

#creating my own power function
new_power <- function(x, npower = 2) {
	power_value <- x^npower
	return(power_value)
}

new_power(x, npower = 4)
new_power(x)

#working on logistic functions
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

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)

new_table_function <- function(model, tidy_fun = broom.helpers::tidy_with_broom_or_parameters()= TRUE) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		)
	)
}

new_table_function(poisson_model)
new_table_function(logistic_model)
new_table_function(logbinomial_model)
