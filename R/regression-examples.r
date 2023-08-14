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


# Univariate regression

#lm = linear models

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)

#logistic regression

tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**No interaction**", "**Interaction**"))


#Exercise

tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(sex_cat, nsibs, starts_with("sleep"), income),
	method = glm,	method.args = list(family = poisson()),
	exponentiate = TRUE)

tbl_uvregression(
	nlsy,
	y = nsibs,
	include = c(nsibs, region_cat, race_eth_cat, eyesight_cat),
	method = glm,	method.args = list(family = poisson()),
	exponentiate = TRUE)

poisson_model <- glm(nsibs ~ eyesight_cat + region_cat + race_eth_cat,
										 data = nlsy, family = poisson())

tbl_regression(poisson_model, exponentiate = TRUE,
label = list(
eyesight_cat ~ "Eyesight",
region_cat ~ "Region",
race_eth_cat ~ "Race/ethnicity"
))

logistic_model2 <- glm(glasses ~ eyesight_cat + sex_cat, data=nlsy, family = binomial(link = "log"))

tbl_regression(logistic_model2, exponentiate = TRUE,
							 label = list(
							 	eyesight_cat ~ "Eyesight",
							 	sex_cat ~ "Sex"
							 ))

poisson_model2 <- glm(glasses ~ eyesight_cat + sex_cat, data=nlsy, family = poisson(link = "log"))

tbl_regression(poisson_model2, exponentiate = TRUE,
							 label = list(
							 	eyesight_cat ~ "Eyesight",
							 	sex_cat ~ "Sex"
							 ))
tbl_eyes_bino <- tbl_regression(logistic_model2, exponentiate = TRUE,
																label = list(
																	eyesight_cat ~ "Eyesight",
																	sex_cat ~ "Sex"
																))

tbl_merge(list(tbl_eyes_poisson, tbl_eyes_bino), tab_spanner = c("**poisson**", "**binomial**"))



