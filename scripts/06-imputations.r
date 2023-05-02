# loading packages
library(tidyverse)
library(lmerTest)
library(lme4)
library(mice)
library(miceadds)
library(broom.mixed) #needed for pooling data
library(miceadds)
library(psych)
library(reshape2)
library(furrr)
library(modelsummary)


df <- read.csv("./data/cleaned_recoded_tender_country_data.csv", header = TRUE)

cols <- c("contract_type", "contracting_authority_type", "contracting_authority_activity", 
"government_procurement_agreement", "is_framework_agreement", "is_dynamic", "is_open",
"inno_procurement", "geo")

df_recoded <- df |> 
  select(!c("title", "cpv", "description", "buyer_name", 
  "buyer_address", "buyer_town")) |>
  mutate(across(all_of(cols), ~ ifelse(.x == "", NA, .x))) |>
  mutate(across(all_of(cols), as.factor)) |>
  mutate(country = as.factor(country),
         year = as.factor(year),
         is_service = as.factor(ifelse(contract_type == "Services", "Yes", 
         ifelse(contract_type == "Supplies", "No", NA))),
         is_saas = as.factor(ifelse(saas == "Yes", 1, 0)),
         year = as.factor(year),
         contracting_authority_type = relevel(contracting_authority_type, ref = "Other"),
         contracting_authority_activity = relevel(contracting_authority_activity, ref = "Other"),
         inno_procurement = case_when(inno_procurement == "Strong performer" ~ "Good performer", 
         .default = inno_procurement),
         inno_procurement = relevel(as.factor(inno_procurement), ref = "Low performer"),
         geo = relevel(geo, ref = "South"))
         
summary(df_recoded)
glimpse(df_recoded)



## Step 00: Standardizing 
## Standardisation of continuous variables for the later model.
## For imputations, non-standardized variables are used.

standardise_two_std <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE))
}


###############################################################
## Step 0: Imputations
## Note: Data for population and gdp was standardized before imputations,
## as they were on a much different scale than the other variables.
## They are not imputed in the model.
## value_eur had to be standardised as it was on a much different scale. Given it is a level-1
## variable, it will be scaled based on cluster mean. 

# imputations based on the syntax from example 2.2 in 
# Grund, S., Lüdtke, O., & Robitzsch, A. (2018). Multiple Imputation of Missing 
# Data for Multilevel Models: Simulations and Recommendations. 
# Organizational Research Methods, 21(1), 111–149. https://doi.org/10.1177/1094428117703686


not_analyzed <- c("id", "contract_type", "saas", "is_dynamic", "dii_low")

df_recoded_short <- df_recoded |>
  mutate(across(.cols = c("gdp", "population"), standardise_two_std)) |>  # standardize just these two variables
  group_by(country, year) |>
  mutate(value_eur = standardise_two_std(value_eur)) |> # cluster mean scaling for value_eur
  ungroup() |>
  select(!all_of(not_analyzed)) |>
  mutate(country = as.integer(factor(country)), # necessary to run impute
         year = as.integer(year)) |>
  relocate(c("dii_high", "desi_total"), .after = last_col()) # move these columns to the end


summary(df_recoded_short)
glimpse(df_recoded_short)

###################

df_recoded_short <- df_recoded_short |>
  mutate(
  value_eur.IND.gdp = NA, # adding passive imputation 
  gdp.pop = NA)           # columns for interactions

##################

impMethod <- character(ncol(df_recoded_short))    # creates empty vector for
names(impMethod) <- colnames(df_recoded_short)    # imputations methods

# Define method for each variable
impMethod["contracting_authority_type"] <- "2l.polyreg"   # two-level polyreg or pmm
impMethod["contracting_authority_activity"] <- "2l.polyreg" # two-level polyreg or pmm
impMethod["government_procurement_agreement"] <- "2l.bin"
impMethod["is_framework_agreement"] <- "2l.bin"
impMethod["is_open"] <- "2l.bin"
impMethod["value_eur"] <- "2l.pmm"
impMethod["buss_gerd"] <- "2lonly.pmm"
impMethod["dii_very_high"] <- "2lonly.pmm"
impMethod["dii_very_low"] <- "2lonly.pmm"
impMethod["gov_gerd"] <- "2lonly.pmm"
impMethod["gov_personnel"] <- "2lonly.pmm"
impMethod["ict_employment"] <- "2lonly.pmm"
impMethod["ict_percent"] <- "2lonly.pmm"
impMethod["ict_value_added"] <- "2lonly.pmm"
impMethod["venture"] <- "2lonly.pmm"
impMethod["pct_employment_gov"] <- "2lonly.pmm"
impMethod["pct_gov_procurement"] <- "2lonly.pmm"
impMethod["is_service"] <- "2l.bin"

impMethod["dii_high"] <- "2lonly.pmm" ### added
impMethod["desi_total"] <- "2lonly.pmm" ### added


###################
impMethod["value_eur.IND.gdp"] <- "~I(value_eur.IND * gdp)" ## used in predMatrix for level-1 vars (1)
impMethod["gdp.pop"] <- "~ I(gdp * population)" ## used in predMatrix for level-2 vars (1)
######################

impMethod #check

# Set up predictor matrix
predMatrix  <- matrix(0, ncol(df_recoded_short), ncol(df_recoded_short))     # create empty predictor
rownames(predMatrix) <- colnames(predMatrix) <- colnames(df_recoded_short)   # matrix


# Define predictors for each variable
# -2 = cluster variable
# 1 = overall effect
# 3 = overall + group effect

predMatrix["contracting_authority_type", ] <- c(rep(-2, 2), 0, rep(3, 5), rep(1, 18), rep(3, 2), rep(1, 2), 1, 1)
predMatrix["contracting_authority_activity", ] <- c(rep(-2, 2), 3, 0, 3, 3, 3, 3, rep(1, 18), rep(3, 2), rep(1, 2), 1, 1)
predMatrix["government_procurement_agreement", ] <- c(rep(-2, 2), 3, 3, 0, 3, 3, 3, rep(1, 18), rep(3, 2), rep(1, 2), 1, 1)
predMatrix["is_framework_agreement", ] <- c(rep(-2, 2), 3, 3, 3, 0, 3, 3, rep(1, 18), rep(3, 2), rep(1, 2), 1, 1)

predMatrix["is_open", ] <- c(rep(-2, 2), 3, 3, 3, 3, 0, 3, rep(1, 18), rep(3, 2), rep(1, 2), 1, 1)
predMatrix["value_eur", ] <- c(rep(-2, 2), 3, 3, 3, 3, 3, 0, rep(1, 18), rep(3, 2), rep(1, 2), 0, 1)

predMatrix["buss_gerd", ] <- c(rep(-2, 2), rep(1, 6), 0, rep(1, 17), rep(1, 2), rep(1, 2), 1, 1)

predMatrix["dii_very_high", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 1), 0, rep(1, 16), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["dii_very_low", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 2), 0, rep(1, 15), rep(1, 2), rep(1, 2), 1, 1)

predMatrix["gov_gerd", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 4), 0, rep(1, 13), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["gov_personnel", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 5), 0, rep(1, 12), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["ict_employment", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 6), 0, rep(1, 11), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["ict_percent", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 7), 0, rep(1, 10), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["ict_value_added", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 8), 0, rep(1, 9), rep(1, 2), rep(1, 2), 1, 1)

predMatrix["venture", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 10), 0, rep(1, 7), rep(1, 2), rep(1, 2), 1, 1)

predMatrix["pct_employment_gov", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 16), 0, rep(1, 1), rep(1, 2), rep(1, 2), 1, 1)
predMatrix["pct_gov_procurement", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 17), 0, rep(1, 2),rep(1, 2), 1, 1)

predMatrix["is_service", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 18), 0, 3, rep(1, 2), 1, 1)

predMatrix["dii_high", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 18), rep(1, 2), 0, 1, 1, 1)
#predMatrix["dii_low", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 18), rep(1, 2), 1, 0, 1, 1)
predMatrix["desi_total", ] <- c(rep(-2, 2), rep(1, 6), rep(1, 18), rep(1, 2), 1, 0, 1, 1)



# save predMatrix as csv to check
write.csv(predMatrix, file = "./outputs/predMatrix_2304.csv")

no_cores <- availableCores() - 1

start <- Sys.time() 
imp_par <- mice::futuremice(data=df_recoded_short, maxit=20, m=14, imputationMethod=impMethod,
predictorMatrix=predMatrix, parallelseed = 1505, n.core = no_cores)
end <- Sys.time() - start
print(end)


# Saving imputed data

setwd("./outputs")
save(imp_par, file = "imp_par_2304.rda")
#rm(imp_par)
setwd("..")

load("./outputs/imputed/imp_par_2304.rda", verbose = TRUE)

class(imp_par)


## Descriptives for imputed data, grouped by imputation

imp <- mice::complete(imp_par, "long")

columns_imputed <- c("buss_gerd", "dii_high", "gov_gerd", "gov_personnel",
"ict_employment", "ict_percent", "ict_value_added", "venture", "position_economy",
 "position_efficiency", "position_market", "pct_employment_gov", "pct_gov_procurement",
 "desi_total", "value_eur")

descriptive_numeric_imp <- imp |> group_by(.imp) |> 
summarise(across(
  .cols = all_of(columns_imputed),
  .fns = list(mean = ~ mean(.), sd = ~ sd(.)),
  .names = "{col}_{fn}"
  ))

descriptive_numeric_imp

x |> group_by(.imp) |>
count(contracting_authority_type) |>
mutate(pct = n / sum(n) * 100,
pct = round(pct, 1)) |>
ungroup() |>
reshape2::dcast(contracting_authority_type ~ .imp, value.var = "pct")



#check convergence

# half
plot(imp_par, contracting_authority_activity + contracting_authority_type +
buss_gerd + is_service + desi_total + dii_very_high + dii_high +
is_framework_agreement + gov_gerd +
ict_employment + ict_percent ~ .it | .ms,
layout = c(2, 11))

# other half
plot(imp_par, ict_value_added + 
is_open + pct_gov_procurement + pct_employment_gov +
gov_personnel + value_eur  ~ .it | .ms,
layout = c(2, 6))

# all (for thesis)
plot(imp_par, contracting_authority_activity + contracting_authority_type +
buss_gerd + is_service + desi_total + dii_very_high + dii_high +
is_framework_agreement + gov_gerd +
ict_employment + ict_percent +
ict_value_added + is_open + pct_gov_procurement + pct_employment_gov +
gov_personnel + value_eur ~ .it | .ms,
layout = c(2, 17))



mice::stripplot(imp_par, contracting_authority_activity~.imp, pch=20, cex=2)

mice::stripplot(imp_par, "value_eur")

mice::densityplot(imp_par)

plot(imp_par)
mice::densityplot(imp_par, ~value_eur)
ggsave("./outputs/figures/imp_value_eur_density2.png", bg = "white", dpi = 300)


############################################
## Final data prep
############################################

# Creating a list of imputed data frames
imp_list <- mice::complete(imp_par, "all")


imp_list_recoded <- purrr::map(imp_list, ~ . |> mutate(
  contracting_authority_type = relevel(contracting_authority_type, 
  ref = "Ministry or any other national or federal authority"),
  dii_high = dii_very_high + dii_high))

# Standardisation
# (not for gdp, pop, value_eur; these were standardised for the imputation)

columns_to_standardise <- c("buss_gerd", "dii_high", "gov_gerd", "gov_personnel",
"ict_employment", "ict_percent", "ict_value_added", "venture", "position_economy",
 "position_efficiency", "position_market", "pct_employment_gov", "pct_gov_procurement",
 "desi_total")

imp_list_std <- purrr::map(imp_list_recoded, ~ . |> mutate(across(all_of(columns_to_standardise), 
standardise_two_std)))


purrr::map(imp_list_std, ~ summary(.))
purrr::map(imp_list_std, ~ nrow(.))

#save(imp_list_std, file = "./outputs/imputed/imp_list_std_2404.rda")
load("./outputs/imputed/imp_list_std_2404.rda")


