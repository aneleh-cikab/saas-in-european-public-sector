# loading packages
library(tidyverse)
library(lme4)
library(mice)
library(miceadds)
library(broom.mixed) #needed for pooling data
library(psych)
library(furrr)
library(modelsummary)
library(performance)

# function for pooling params

pooled_params <- function(model, model_name = "") {

  # Rsq

  model_rsq <- purrr::map(model, performance::r2_nakagawa)

  model_rsq_conditional <- list()
  model_rsq_marginal <- list()

  for (i in seq(1, 14)) {

    value <- unlist(model_rsq[i])

    model_rsq_conditional <- append(model_rsq_conditional, as.numeric(value[1]))
    model_rsq_marginal <- append(model_rsq_marginal, as.numeric(value[2]))

  }

  model_rsq_conditional_pooled <- mean(unlist(model_rsq_conditional))
  model_rsq_marginal_pooled <- mean(unlist(model_rsq_marginal))

  # Other fit measures
  model_AIC_pooled <- mean(unlist(purrr::map(model, AIC)))
  model_BIC_pooled <- mean(unlist(purrr::map(model, BIC)))

  # ICC

  model_icc <- purrr::map(model, performance::icc)
  model_icc_adj <- list()

  for (i in seq(1, 14)) {

    value <- unlist(model_icc[i])

    model_icc_adj <- append(model_icc_adj, as.numeric(value[1]))

  }

  model_icc_adj_pool <- mean(unlist(model_icc_adj))

  pooled_values <- tibble(model = model_name, icc = model_icc_adj_pool, 
  rsq_conditional = model_rsq_conditional_pooled, 
  rsq_marginal = model_rsq_marginal_pooled, AIC = model_AIC_pooled, 
  BIC = model_BIC_pooled)

  return(pooled_values)

}



### Loading imputed data
load("./outputs/imputed/imp_list_std_2404.rda")
purrr::map(imp_list_std, ~ nrow(.))

### Load saved models

load("./outputs/models/model0.rda")
load("./outputs/models/model1_2404.rda")
load("./outputs/models/model2_2404.rda")
#load("./outputs/models/model3_2404.rda") # model 3 with random slopes with singular fit
load("./outputs/models/model3_2604.rda")



#######################################
# MODELS
#######################################


#######################################
## STEP 1: BUILDING AN EMPTY MODEL


model0 <- glmer(is_saas ~ (1 | country) + (1 | year), family="binomial", data = df_recoded)

#file.create("./outputs/models/runtimes.txt")
write(paste0("Empty model run for: ", end, " mins."), file="./outputs/models/runtimes.txt", append=TRUE)

#setwd("./outputs/models")
#save(model0, file = "model0.rda")
#setwd("..")
#rm(model0)
#load("./outputs/models/model0.rda")


modelsummary(model0)

# for icc formula see Eq 6
icc_country <- model0@theta[1]^2 / (model0@theta[1]^2 + (3.14159^2/3))
icc_time <- model0@theta[2]^2 / (model0@theta[2]^2 + (3.14159^2/3))
icc_country
icc_time


#######################################
## MODEL 1: MODEL WITH JUST CONTROL VARIABLES


model_1 <- function(data){
  glmer(is_saas ~ is_service + government_procurement_agreement + is_framework_agreement + 
  is_open + geo + pct_gov_procurement + (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)
}

no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

start <- Sys.time()
set.seed(1505)
model1 <- furrr::future_map(imp_list_std, model_1, 
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end

# save model

#save(model1, file = "./outputs/models/model1_2404.rda")
#load("./outputs/models/model1_2404.rda")

summary(pool(model1))
str(model1[[1]])


#ranef(model1[[1]])
#coef(model1[[1]])


#######################################
## MODEL 2: NO CROSS-LEVEL INTERACTION AND NO RANDOM SLOPES


model_2 <- function(data){
  glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + geo + pct_gov_procurement +
  contracting_authority_type + contracting_authority_activity + value_eur +
  gdp + population + buss_gerd + dii_high +
  gov_gerd + gov_personnel + ict_employment + ict_percent + ict_value_added +
  inno_procurement +  position_economy + position_efficiency +
  position_market + pct_employment_gov +  + desi_total +
  gdp:population + (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)
}

no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

start <- Sys.time()
set.seed(1505)
model2 <- furrr::future_map(imp_list_std, model_2, 
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end

# save model

#save(model2, file = "./outputs/models/model2_2404.rda")
#load("./outputs/models/model2_2404.rda")

summary(pool(model2))


#######################################
## MODEL 3: CROSS-LEVEL INTERACTION AND RANDOM SLOPES

model_3 <- function(data){
  glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + geo + pct_gov_procurement +
  contracting_authority_type + contracting_authority_activity + value_eur +
  gdp + population + buss_gerd + dii_high +
  gov_gerd + gov_personnel + ict_employment + ict_percent + ict_value_added +
  inno_procurement +  position_economy + position_efficiency +
  position_market + pct_employment_gov +  + desi_total +
  gdp:population + value_eur:gdp +
  (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)
}

no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

start <- Sys.time()
set.seed(1505)
model3 <- furrr::future_map(imp_list_std, model_3, 
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end

# save model

#save(model3, file = "./outputs/models/model3_2604.rda")
#load("./outputs/models/model3_2404.rda")

summary(pool(model3))

# Is model fit singular (for model with random slopes for tender value)

#load("./outputs/models/model3_2404.rda")
#purrr::map(model3, performance::check_singularity)



###### MODEL SUMMARY

modelsummary::modelsummary(list("Model 0" = model0,
"Model 1" = pool(model1), "Model 2" = pool(model2),
"Model 3" = pool(model3)), exponentiate = TRUE,
stars = TRUE, estimate = "{estimate}{stars} [{conf.low}, {conf.high}]", 
fmt = 1,
statistic = NULL,
output = "./outputs/models/all_models_2604.docx")


modelsummary::modelsummary(list(
"Model 3" = pool(model3)), exponentiate = TRUE,
stars = TRUE, estimate = "{estimate}{stars} [{conf.low}, {conf.high}]", 
fmt = 1,
statistic = NULL)


pooled_values_model1 <- pooled_params(model1, "model1")
pooled_values_model2 <- pooled_params(model2, "model2")
pooled_values_model3 <- pooled_params(model3, "model3")

pooled_values_all <- bind_rows(pooled_values_model1, pooled_values_model2, pooled_values_model3)
pooled_values_all

write_csv(pooled_values_all, "./outputs/models/pooled_values_all_2604.csv")


#######################################
###### PLOT INTERACTION
###### Adjusted from: https://stackoverflow.com/questions/75377948/getting-an-interaction-plot-from-a-pooled-lme-model-with-mids-object

library(marginaleffects)

fit_predict <- function(data) {

    mod <- glmer(is_saas ~ is_service + government_procurement_agreement + 
    is_framework_agreement + is_open + geo + pct_gov_procurement +
    contracting_authority_type + contracting_authority_activity + value_eur +
    gdp + population + buss_gerd + dii_high +
    gov_gerd + gov_personnel + ict_employment + ict_percent + ict_value_added +
    inno_procurement +  position_economy + position_efficiency +
    position_market + pct_employment_gov +  + desi_total +
    gdp:population + value_eur:gdp +
    (1 | country) + (1 | year), family = "binomial",
    nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)

    out <- marginaleffects::plot_predictions(mod, condition = list("gdp", "population"), draw = FALSE)
    
    # `mice` requires a unique row identifier called "term"
    out$term <- out$rowid 
    class(out) <- c("custom", class(out))
    return(out)

}

# `tidy.custom()` is needed by `mice` to combine datasets, but the output of fit() also has
# the right structure and column names, so we can just return the input
tidy.custom <- function(x, ...) return(x)

# Fit on each imputation
no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)
start <- Sys.time()
set.seed(1505)
mod_interaction <- furrr::future_map(imp_list_std, fit_predict,
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end


# Pool
mod_pool <- pool(mod_interaction)$pooled
#mod_pool <- pool(mod_interaction)


# Merge back some of the covariates
datplot <- data.frame(mod_pool, mod_interaction[[1]][, c("gdp", "population")])



# save as csv
# write_csv(datplot, "./outputs/models/datplot_interaction_2604.csv")

datplot <- read_csv("./outputs/models/datplot_interaction_2604.csv")

library(directlabels)

datplot <- datplot |>
mutate(population2 = factor(population, 
labels = c("-1.5SD", "-1SD", "Mean", "+1SD", "+1.5SD")),
line = factor(population, 
labels = c("longdash", "dashed", "solid", "dotted", "dashdot")))

glimpse(datplot)

# Plot
interaction_plot <- ggplot(datplot, aes(gdp, estimate, color = population2)) + 
  geom_line(aes(linetype = line), linewidth = 0.8) +
  theme_minimal() +
  scale_color_manual(
    name = "Population size",
    labels = c("-1.5SD", "-1SD", "Mean", "+1SD", "+1.5SD"),
    values = c("#999999", "#999999", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(
    name = "Population size",
    labels = c("-1.5SD", "-1SD", "Mean", "+1SD", "+1.5SD"),
    values = datplot$line) +
  scale_x_continuous(limits = c(-0.8, 2.3),
  breaks = c(-0.5, 0, 0.5, 1, 1.5),
  labels = c("-0.5SD", "M", "+0.5SD", "+1SD", "+1.5SD")) +
  labs(x = "GDP", y = "Predicted probability of SaaS adoption (loglikelihood)") +
  theme(text = element_text(size = 15)) +
  geom_dl(aes(label = population2), method = "last.points")
  
ggsave(plot=interaction_plot, filename="./outputs/figures/interaction_plot.png", bg = "white")

interaction_plot

###########################################
# OR for interaction

imp_list_std_interaction <- purrr::map(imp_list_std, ~ . |> mutate(population_p2sd = population + 2 * sd(population),
population_m2sd = population - 2 * sd(population)))


model_3_p2sd <- function(data){
  glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + geo + pct_gov_procurement +
  contracting_authority_type + contracting_authority_activity + value_eur +
  gdp+ population_p2sd  + buss_gerd + dii_high +
  gov_gerd + gov_personnel + ict_employment + ict_percent + ict_value_added +
  inno_procurement +  position_economy + position_efficiency +
  position_market + pct_employment_gov +  + desi_total +
  gdp:population_p2sd + value_eur:gdp +
  (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)
}

no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)
start <- Sys.time()
set.seed(1505)
modelint_p2sd <- furrr::future_map(imp_list_std_interaction, model_3_p2sd, 
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end

save(modelint_p2sd, file = "./outputs/models/modelint_p2sd.rda")


model_3_m2sd <- function(data){
  glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + geo + pct_gov_procurement +
  contracting_authority_type + contracting_authority_activity + value_eur +
  gdp + population_m2sd + buss_gerd + dii_high +
  gov_gerd + gov_personnel + ict_employment + ict_percent + ict_value_added +
  inno_procurement +  position_economy + position_efficiency +
  position_market + pct_employment_gov +  + desi_total +
  gdp:population_m2sd + value_eur:gdp +
  (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), data = data)
}


no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)
start <- Sys.time()
set.seed(1505)
modelint_m2sd <- furrr::future_map(imp_list_std_interaction, model_3_m2sd, 
.options = furrr_options(seed = 1505), .progress = TRUE)
end <- Sys.time() - start
end

save(modelint_m2sd, file = "./outputs/models/modelint_m2sd.rda")


load("./outputs/models/modelint_p2sd.rda")
load("./outputs/models/modelint_m2sd.rda")

modelsummary::modelsummary(list("Model +2SD" = pool(modelint_p2sd),
"Model -2SD" = pool(modelint_m2sd)), exponentiate = TRUE,
stars = TRUE, estimate = "{estimate}{stars} [{conf.low}, {conf.high}]", 
fmt = 1,
statistic = NULL,
output = "./outputs/models/interaction_3004.docx")


###########################################
######## MODEL CHECK: ALL FIT
# Code adapted from: http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/

model_all_fit <- glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + pct_gov_procurement +
  ict_percent + ict_employment + value_eur +
  gov_gerd + gov_personnel +
  contracting_authority_type + contracting_authority_activity + 
  gdp + population + 
  inno_procurement + position_market + gdp:population +
  (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, data = imp_list_std[[1]])

#save(model_all_fit, file = "./outputs/models/model_all_fit.rda")
load("./outputs/models/model_all_fit.rda")

allFit(show.meth.tab=TRUE)

fit_all <- allFit(model_all_fit)
fit_all_summary <- summary(fit_all)

fit_all_summary$which.OK
fit_all_summary$llik

fit_all_lliks <- sort(sapply(fit_all,logLik))
fit_all_lliks

tidy(fit_all_lliks) |>
  ggplot(aes(names, x)) + geom_point() +
  coord_flip() +
  theme_minimal() +
  ylab("Log-Likelihood") +
  labs(title = "The Log-Likelihoods of different optimizers")

ggsave("./outputs/figures/fit_all_lliks.png", dpi = 300, bg = "white")


lapply(fit_all, tidy) |>
  map2_df(.,
          names(fit_all),
          ~mutate(.x, optimizer = .y)) |>
  mutate(optimizer = case_when(optimizer == "nloptwrap.NLOPT_LN_NELDERMEAD" ~ "nlopt: Nelder-Mead",
                                optimizer == "nloptwrap.NLOPT_LN_BOBYQA" ~ "nlopt: BOBYQA",
                                optimizer == "nlminbwrap" ~ "nlminbwrap",
                                optimizer == "Nelder_Mead" ~ "Nelder-Mead",
                                optimizer == "bobyqa" ~ "BOBYQA")) |>
  mutate(term = case_when(str_detect(term, "activityDe") ~ "Authority Activity: \nDefence",
  str_detect(term, "activityEco") ~ "Authority Activity: \nEconomic and financial affairs",
  str_detect(term, "activityEdu") ~ "Authority Activity: \nEducation",
  str_detect(term, "activityEnv") ~ "Authority Activity: \nEnvironment",
  str_detect(term, "activityGen") ~ "Authority Activity: \nGeneral public services",
  str_detect(term, "activityHea") ~ "Authority Activity: \nHealth",
  str_detect(term, "activityHou") ~ "Authority Activity: \nHousing and community amenities",
  str_detect(term, "activityPub") ~ "Authority Activity: \nPublic order and safety",
  str_detect(term, "activitySoc") ~ "Authority Activity: \nSocial protection",
  str_detect(term, "typeBody") ~ "Authority type: \nBody governed by public law",
  str_detect(term, "typeNational") ~ "Authority type: \nNational or federal agency/office",
  str_detect(term, "typeOther") ~ "Authority type: \nOther",
  str_detect(term, "typeRegional or local agency/office") ~ "Authority type: \nRegional or local agency/office",
  str_detect(term, "typeRegional or local authority") ~ "Authority type: \nRegional or local authority",
  str_detect(term, "activityEle") ~ "Authority type: \nElectricity",
  str_detect(term, "gdp:population") ~ "GDP:Population",
  str_detect(term, "gdp") ~ "GDP",
  str_detect(term, "population") ~ "Population",
  str_detect(term, "gov_gerd") ~ "Government R&D",
  str_detect(term, "gov_personnel") ~ "R&D personnel",
  str_detect(term, "procurement_agreement") ~ "GPA",
  str_detect(term, "ict_empl") ~ "ICT employment",
  str_detect(term, "ict_percent") ~ "ICT GDP contribution",
  str_detect(term, "framework_agreement") ~ "Framework agreement",
  str_detect(term, "procurementGood") ~ "Innovation procurement: \nGood performer",
  str_detect(term, "procurementModerate") ~ "Innovation procurement: \nModerate performer",
  str_detect(term, "procurementModest") ~ "Innovation procurement: \nModest performer",
  str_detect(term, "is_open") ~ "Open procedure",
  str_detect(term, "is_service") ~ "Contract type",
  str_detect(term, "position_market") ~ "Position on the\n free market",
  str_detect(term, "value_eur") ~ "Tender value",
  str_detect(term, "pct_gov_procurement") ~ "Procurement size",
  .default = term
  )) |>
  filter(term != "sd__(Intercept)" & !is.na(optimizer)) |>
  ggplot(aes(x = exp(estimate), y = optimizer, xmin = exp(estimate - (1.96*std.error)),
  xmax = exp(estimate + (1.96*std.error)))) + geom_point() + geom_errorbarh() +
  #coord_flip() +
  #scale_x_continuous(limits = c(0, 6)) +
  facet_wrap(~term, scales = "free_x", ncol=4) +
  geom_vline(xintercept = 1, linetype="dashed") +
  theme_minimal() +
  xlab("Odds ratio (OR)") +
  ylab("") +
  labs(title = "Odds ratio (OR) of tender beeing SaaS for variables in the model estimated with different optimizers")

# save plot

ggsave("./outputs/figures/all_fit.png", dpi = 300, bg = "white", width=4000, height=5000, units = "px")



# NAGQ testing with 2 cluster variables (nagq can't be > 1)

model_nagq0 <- glmer(is_saas ~ is_service + government_procurement_agreement + 
  is_framework_agreement + is_open + pct_gov_procurement +
  ict_percent + ict_employment + value_eur +
  gov_gerd + gov_personnel +
  contracting_authority_type + contracting_authority_activity + 
  gdp + population + 
  inno_procurement + position_market + gdp:population +
  (1 | country) + (1 | year), family = "binomial",
  nAGQ = 0, control = glmerControl(optimizer = "nloptwrap",
  optCtrl=list(maxfun=100000)), data = imp_list_std[[1]])

model_nagq1 <- update(model_nagq0, nAGQ = 1)

summary(model_nagq0)
summary(model_nagq1)

#save(model_nagq0, file = "./outputs/models/model_nagq0.rda")
#save(model_nagq1, file = "./outputs/models/model_nagq1.rda")

load("./outputs/models/model_nagq0.rda")
load("./outputs/models/model_nagq1.rda")

tidy_model_nagq0 <- tidy(model_nagq0) |>
mutate(model = "nAGQ = 0",
  term = case_when(str_detect(term, "activityDe") ~ "Authority Activity: \nDefence",
  str_detect(term, "activityEco") ~ "Authority Activity: \nEconomic and financial affairs",
  str_detect(term, "activityEdu") ~ "Authority Activity: \nEducation",
  str_detect(term, "activityEnv") ~ "Authority Activity: \nEnvironment",
  str_detect(term, "activityGen") ~ "Authority Activity: \nGeneral public services",
  str_detect(term, "activityHea") ~ "Authority Activity: \nHealth",
  str_detect(term, "activityHou") ~ "Authority Activity: \nHousing and community amenities",
  str_detect(term, "activityPub") ~ "Authority Activity: \nPublic order and safety",
  str_detect(term, "activitySoc") ~ "Authority Activity: \nSocial protection",
  str_detect(term, "typeBody") ~ "Authority type: \nBody governed by public law",
  str_detect(term, "typeNational") ~ "Authority type: \nNational or federal agency/office",
  str_detect(term, "typeOther") ~ "Authority type: \nOther",
  str_detect(term, "typeRegional or local agency/office") ~ "Authority type: \nRegional or local agency/office",
  str_detect(term, "typeRegional or local authority") ~ "Authority type: \nRegional or local authority",
  str_detect(term, "activityEle") ~ "Authority type: \nElectricity",
  str_detect(term, "gdp:population") ~ "GDP:Population",
  str_detect(term, "gdp") ~ "GDP",
  str_detect(term, "population") ~ "Population",
  str_detect(term, "gov_gerd") ~ "Government R&D",
  str_detect(term, "gov_personnel") ~ "R&D personnel",
  str_detect(term, "procurement_agreement") ~ "GPA",
  str_detect(term, "ict_empl") ~ "ICT employment",
  str_detect(term, "ict_percent") ~ "ICT GDP contribution",
  str_detect(term, "framework_agreement") ~ "Framework agreement",
  str_detect(term, "procurementGood") ~ "Innovation procurement: \nGood performer",
  str_detect(term, "procurementModerate") ~ "Innovation procurement: \nModerate performer",
  str_detect(term, "procurementModest") ~ "Innovation procurement: \nModest performer",
  str_detect(term, "is_open") ~ "Open procedure",
  str_detect(term, "is_service") ~ "Contract type",
  str_detect(term, "position_market") ~ "Position on the\n free market",
  str_detect(term, "value_eur") ~ "Tender value",
  str_detect(term, "pct_gov_procurement") ~ "Procurement size",
  .default = term
))

tidy_model_nagq1 <- tidy(model_nagq1) |>
mutate(model = "nAGQ = 1",
  term = case_when(str_detect(term, "activityDe") ~ "Authority Activity: \nDefence",
  str_detect(term, "activityEco") ~ "Authority Activity: \nEconomic and financial affairs",
  str_detect(term, "activityEdu") ~ "Authority Activity: \nEducation",
  str_detect(term, "activityEnv") ~ "Authority Activity: \nEnvironment",
  str_detect(term, "activityGen") ~ "Authority Activity: \nGeneral public services",
  str_detect(term, "activityHea") ~ "Authority Activity: \nHealth",
  str_detect(term, "activityHou") ~ "Authority Activity: \nHousing and community amenities",
  str_detect(term, "activityPub") ~ "Authority Activity: \nPublic order and safety",
  str_detect(term, "activitySoc") ~ "Authority Activity: \nSocial protection",
  str_detect(term, "typeBody") ~ "Authority type: \nBody governed by public law",
  str_detect(term, "typeNational") ~ "Authority type: \nNational or federal agency/office",
  str_detect(term, "typeOther") ~ "Authority type: \nOther",
  str_detect(term, "typeRegional or local agency/office") ~ "Authority type: \nRegional or local agency/office",
  str_detect(term, "typeRegional or local authority") ~ "Authority type: \nRegional or local authority",
  str_detect(term, "activityEle") ~ "Authority type: \nElectricity",
  str_detect(term, "gdp:population") ~ "GDP:Population",
  str_detect(term, "gdp") ~ "GDP",
  str_detect(term, "population") ~ "Population",
  str_detect(term, "gov_gerd") ~ "Government R&D",
  str_detect(term, "gov_personnel") ~ "R&D personnel",
  str_detect(term, "procurement_agreement") ~ "GPA",
  str_detect(term, "ict_empl") ~ "ICT employment",
  str_detect(term, "ict_percent") ~ "ICT GDP contribution",
  str_detect(term, "framework_agreement") ~ "Framework agreement",
  str_detect(term, "procurementGood") ~ "Innovation procurement: \nGood performer",
  str_detect(term, "procurementModerate") ~ "Innovation procurement: \nModerate performer",
  str_detect(term, "procurementModest") ~ "Innovation procurement: \nModest performer",
  str_detect(term, "is_open") ~ "Open procedure",
  str_detect(term, "is_service") ~ "Contract type",
  str_detect(term, "position_market") ~ "Position on the\n free market",
  str_detect(term, "value_eur") ~ "Tender value",
  str_detect(term, "pct_gov_procurement") ~ "Procurement size",
  .default = term
))

bind_rows(tidy_model_nagq0, tidy_model_nagq1) |>
  filter(term != "sd__(Intercept)") |>
  ggplot(aes(x = exp(estimate), y = term, xmin = exp(estimate - (1.96*std.error)),
  xmax = exp(estimate + (1.96*std.error)))) + geom_point() + geom_errorbar() +
  scale_y_discrete(limits=rev) +
  #coord_flip() +
  scale_x_continuous(limits = c(0, 6)) +
  facet_wrap(~model, scales = "free_y", ncol=2) +
  geom_vline(xintercept = 1, linetype="dashed") +
  theme_minimal() +
  xlab("Odds ratio (OR)") +
  ylab("") +
  labs(title = "Odds ratio (OR) of tender beeing SaaS for variables in the model estimated with varying nAGQ")

# save plot

ggsave("./outputs/figures/nagq.png", dpi = 300, bg = "white", width=3000, height=5000, units = "px")


# LogLikelihoods are the same for nagq = 0 and nagq = 1
model_nagq0_loglik <- logLik(model_nagq0)
model_nagq1_loglik <- logLik(model_nagq1)
model_nagq0_loglik
model_nagq1_loglik
