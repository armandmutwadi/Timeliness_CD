# File: cox_regression_analysis.R
# Description: This script performs Cox regression analysis on 
#vaccination data from 2021, 2022, and 2023.

#clear the workspace
rm(list = ls())

#set the working directory
wdir<-setwd("C:/Users/HP/OneDrive/ECV Paper writing/Base_ECV/Base des données ECVs")

# Load necessary libraries
library(readxl)
library(dplyr)
library(DescTools)
library(tidyr)
library(purrr)
library(gtsummary)
library(writexl)
library(coxme)
library(survival)
library(broom.mixed)
library(readr)
library(tidyverse)

#import all 3 datasets
bd2021vf<-read_excel("bd2021_vf.xlsx")
bd2022vf<-read_excel("bd2022_vf.xlsx")
bd2023vf<-read_excel("bd2023_vf.xlsx")

############ COX REGRESSION #################################################

#2021

# Create a survival object
surv_obj2021 <- Surv(time = bd2021vf$age_var_m, event = bd2021vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2021 <- coxme(
	surv_obj2021 ~ wealth+nutritional_status +children_u5_cat+knowledge_cat+
		child_civil_registration + age_mother_cat + education_mother + religion +
		area_residency + using_telephone + paid_vaccination +television+
		(1 | province/healthzone),
	data = bd2021vf
)
summary(coxme_model2021)

#2022

# Create a survival object
surv_obj2022 <- Surv(time = bd2022vf$age_var_m, event = bd2022vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2022 <- coxme(
	surv_obj2022 ~ wealth+nutritional_status +children_u5_cat+knowledge_cat+
		child_civil_registration + age_mother_cat + education_mother + religion +
		marital_status + area_residency + using_telephone + paid_vaccination +television+
		(1 | province/healthzone),
	data = bd2022vf
)
summary(coxme_model2022)

#2023

# Create a survival object
surv_obj2023 <- Surv(time = bd2023vf$age_var_m, event = bd2023vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2023 <- coxme(
	surv_obj2023 ~ wealth+child_civil_registration + age_mother_cat + education_mother + religion +children_u5_cat+knowledge_cat+
		area_residency + using_telephone + paid_vaccination +television+
		(1 | province/healthzone),
	data = bd2023vf
)
summary(coxme_model2023)

#######################################################################

####### COMPARING STANDARD TO MIXED EFFECT COX-MODELS #################

library(survival)
library(coxme)

# Fit both models
#2021
cox_fixed2021 <- coxph(surv_obj2021 ~ nutritional_status + child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	using_telephone + paid_vaccination, 
											 data = bd2021vf)

cox_mixed2021 <- coxme(surv_obj2021 ~ nutritional_status + child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	using_telephone + paid_vaccination + 
											 	(1 | province/healthzone), 
											 data = bd2021vf)

#2022
cox_fixed2022 <- coxph(surv_obj2022 ~ nutritional_status + child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	marital_status + using_telephone + paid_vaccination, 
											 data = bd2022vf)

cox_mixed2022 <- coxme(surv_obj2022 ~ nutritional_status + child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	marital_status + using_telephone + paid_vaccination + 
											 	(1 | province/healthzone), 
											 data = bd2022vf)

#2023
cox_fixed2023 <- coxph(surv_obj2023 ~ child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	using_telephone + paid_vaccination, 
											 data = bd2023vf)

cox_mixed2023 <- coxme(surv_obj2023 ~ child_civil_registration + age_mother_cat + 
											 	education_mother + religion + area_residency + 
											 	using_telephone + paid_vaccination + 
											 	(1 | province/healthzone), 
											 data = bd2023vf)

# Likelihood Ratio Test
model_fiting2021<-anova(cox_fixed2021, cox_mixed2021)
model_fiting2022<-anova(cox_fixed2022, cox_mixed2022)
model_fiting2023<-anova(cox_fixed2023, cox_mixed2023)

model_fiting_All<-list(model_fiting2021,model_fiting2022,model_fiting2023)

write_xlsx(model_fiting_All, "model_fiting_All.xlsx")


#EXPORTING THE RESULTS IN EXCEL FORMAT

extract_coxme_table <- function(model, year) {
	# Extract fixed effects and standard errors
	beta <- fixef(model)
	se <- sqrt(diag(vcov(model)))
	
	# Compute z-scores and p-values
	z <- beta / se
	p <- 2 * (1 - pnorm(abs(z)))  # two-sided
	
	# Assemble the results
	data.frame(
		term = names(beta),
		estimate = beta,
		std.error = se,
		z = z,
		p.value = p,
		Year = year
	) |>
		mutate(
			HR = exp(estimate),
			CI_lower = exp(estimate - 1.96 * std.error),
			CI_upper = exp(estimate + 1.96 * std.error)
		)
}


results2021 <- extract_coxme_table(coxme_model2021, 2021)
results2022 <- extract_coxme_table(coxme_model2022, 2022)
results2023 <- extract_coxme_table(coxme_model2023, 2023)

# Combine into one data frame
coxme_results_all <- bind_rows(results2021, results2022, results2023)
coxme_results_all

# Select and format key columns
formatted_results <- coxme_results_all %>%
	mutate(
		HR_CI = paste0(round(HR, 2), " (", round(CI_lower, 2), "–", round(CI_upper, 2), ")"),
		p_display = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
	) %>%
	select(Year, term, HR_CI, p_display) %>%
	rename(
		Covariate = term,
		`Hazard Ratio (95% CI)` = HR_CI,
		`p-value` = p_display
	)
# Write to Excel
write_xlsx(formatted_results, "coxme_results_with_p_values.xlsx")
