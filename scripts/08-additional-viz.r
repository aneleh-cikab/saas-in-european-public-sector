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
library(ggeffects)
library(gridExtra)
library(ggtext)

### Loading UNSTANDARDISED imputed data
load("./outputs/imputed/imp_par_2304.rda", verbose = TRUE)
imp_list <- mice::complete(imp_par, "all")


### Loading STANDARDISED imputed data
load("./outputs/imputed/imp_list_std_2404.rda")
purrr::map(imp_list_std, ~ nrow(.))

### Loading final model (model 3)
load("./outputs/models/model3_2604.rda")

### Loading row data (just for value in EUR)
df <- read.csv("./data/cleaned_recoded_tender_country_data.csv", header = TRUE)


#######################################
#### DESCRIPTIVE PLOTS
#######################################

######### Waffle

library(waffle)

# counts

counts <- table(df$saas)
counts <- counts[c("Yes", "No")]


# Convert counts to percentage
total <- sum(counts)
percentages <- round((counts / total) * 100)

# Adjust waffle parameters based on percentages
total_squares <- 100
values <- as.numeric(percentages)

subtitle_waffle <- paste0("<span style = 'color:black;'>Only </span><span style='color:tomato; font-weight:bold;'>3.4% </span>out of almost 300,000 tenders <br> were for SaaS",
    "\n")

# Adjusting the rows for a rectangular shape; for instance, 5 rows with 20 boxes per row
plot <- waffle(values, rows=10, size=0.5, colors=c("tomato", "grey" )) + 
labs(title = "Percent of SaaS tenders", subtitle = subtitle_waffle, x = "", y = "") +
theme(plot.title = element_text(size = 20),
plot.subtitle = element_markdown(size = 15),
plot.title.position = "plot",
legend.position = "none")

# Save the plot with a transparent background
ggsave(filename="waffle_plot_percent.png", plot=plot, width=5, height=5, dpi=300, bg="white")


######### Bar plot

# Data processing
df_years <- df %>%
  group_by(year, saas) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = saas, values_from = n, values_fill = 0) %>%
  mutate(saas_share = Yes / (Yes + No),
         saas_pct_rounded = round(saas_share * 100, 1))

df_years

subtitle_years <- paste0("<span style = 'color:black;'>The percent of SaaS tenders is slowly increasing: from
 </span><span style='color:tomato; font-weight:bold;'>1.4% <br> </span>in 2014 to <span style='color:tomato; font-weight:bold;'>4.6% </span>in 2022",
    "\n")

# Plot
ggplot(df_years, aes(x = year, y = saas_share)) +
  geom_line(color = "grey") +
  geom_point(shape = 21, color = "grey", fill = "tomato", size = 8) +
  #geom_hline(yintercept = 0.034, linetype = "dashed", color = "tomato") + # Added this line
  theme_minimal() +
  labs(title = "SaaS tenders over time", 
  subtitle = subtitle_years, x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  limits = c(0, 0.30), breaks = seq(0, 0.3, by = 0.05)) +
  scale_x_continuous(breaks = unique(df_years$year)) +
  theme(
    plot.title = element_text(size = 20),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 15),
    axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 15),
    axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 15),
    legend.position = "none"
  )

ggsave("saas_over_time.png", width = 7, height = 5, dpi = 300, bg = "white")

######### Heatmap


df_saas <- df %>%
  group_by(country, year, saas) %>%
  summarise(n = n(), .groups = 'drop') %>%
  spread(saas, n, fill = 0) %>%
  mutate(saas_share = Yes / (Yes + No),
         saas_pct_rounded = round(saas_share * 100, 1))

# Sort countries by the saas_pct_rounded for the year 2022
sorted_countries <- df_saas %>%
  filter(year == 2022) %>%
  arrange(saas_pct_rounded) %>%
  pull(country)

df_saas$country <- factor(df_saas$country, levels = sorted_countries)

subtitle = paste0(
       "<span style = 'color:black;'>Percent of SaaS tenders varies greatly between countries: </span><span style='color:tomato;'>Finland, the Netherlands, Italy, </span>and <br><span style='color:tomato;'>Ireland</span>
       had over <span style='color:tomato;'>10% </span> of SaaS tenders in 2022.
       These countries are closely followed by <br><span style='color:tomato;'>Norway </span>and <span style='color:tomato;'>Denmark</span>.
       The UK had <span style='color:tomato;'>7.6% </span>of SaaS tenders in 2021."
    )

# Generate heatmap
ggplot(df_saas, aes(x = as.factor(year), y = country, fill = saas_pct_rounded)) +
  geom_tile(colour="white", linewidth = 0.25) +
  scale_fill_gradient(name = "SaaS %", low = "grey90", high = "tomato", limits = c(0, 16)) +
  geom_text(aes(label = sprintf("%.1f", saas_pct_rounded)), size = 4, vjust = 0.5) +
  labs(title = "Percent of SaaS tenders per country (2014-2022)",
       subtitle = subtitle,
       x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    #plot.subtitle = element_text(size = 15),
    plot.subtitle = element_markdown(size = 15),
    plot.title.position = "plot",
    axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 15),
    axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 15),
    legend.position = "none"
  ) 

ggsave("heatmap.png", width = 10, height = 10, dpi = 300, bg = "white")

#######################################
##### MODEL 3 LOG ODDS
#######################################

# Create a named vector of desired labels
# Create a named vector of desired labels
desired_labels <- c(
    "is_serviceYes" = "Contract type: Service",
    "government_procurement_agreementYes" = "Government Procurement Agreement: Yes",
    "is_framework_agreementYes" = "Framework Agreement: Yes",
    "is_openYes" = "Open Procedure: Yes",
    "geoEast" = "Region: East",
    "geoNorth" = "Region: North",
    "geoWest" = "Region: West",
    "pct_gov_procurement" = "Procurement size",
    "contracting_authority_typeOther" = "Authority Type: Other",
    "contracting_authority_typeBody governed by public law" = "Authority Type: Body Governed by Public Law",
    "contracting_authority_typeNational or federal agency/office" = "Authority Type: National/Federal Agency/Office",
    "contracting_authority_typeRegional or local agency/office" = "Authority Type: Regional/Local Agency/Office",
    "contracting_authority_typeRegional or local authority" = "Authority Type: Regional/Local Authority",
    "contracting_authority_activityDefence" = "Authority Activity: Defence",
    "contracting_authority_activityEconomic and financial affairs" = "Authority Activity: Economic/Financial Affairs",
    "contracting_authority_activityEducation" = "Authority Activity: Education",
    "contracting_authority_activityElectricity" = "Authority Activity: Electricity",
    "contracting_authority_activityEnvironment" = "Authority Activity: Environment",
    "contracting_authority_activityGeneral public services" = "Authority Activity: General Public Services",
    "contracting_authority_activityHealth" = "Authority Activity: Health",
    "contracting_authority_activityHousing and community amenities" = "Authority Activity: Housing/Community Amenities",
    "contracting_authority_activityPublic order and safety" = "Authority Activity: Public Order/Safety",        
    "contracting_authority_activitySocial protection" = "Authority Activity: Social Protection",
    "value_eur" = "Tender value (EUR)",
    "gdp" = "GDP",
    "population" = "Population",
    "buss_gerd" = "Business R&D",
    "dii_high" = "Digital Intensity Intex (DII)",
    "gov_gerd" = "Government R&D",
    "gov_personnel" = "R&D Personnel",
    "ict_employment" = "ICT Employment",
    "ict_percent" = "GDP contribution",
    "ict_value_added" = "Added value",
    "inno_procurementGood performer" = "Innovation Procurement: Good Performer",
    "inno_procurementModerate performer" = "Innovation Procurement: Moderate Performer",
    "inno_procurementModest performer" = "Innovation Procurement: Modest Performer",
    "position_economy" = "Position on technology",
    "position_efficiency" = "Position on efficiency",
    "position_market" = "Position on the free market",
    "pct_employment_gov" = "Public sector size",
    "desi_total" = "Digital Economy and Society Index (DESI)",
    "gdp:population" = "GDP vs Population",
    "value_eur:gdp" = "Value (EUR) vs GDP"
)

# Specify the order you want the terms to appear
term_order <- c(
# Control
"is_serviceYes", "is_framework_agreementYes", "is_openYes", "government_procurement_agreementYes", "pct_gov_procurement", "geoEast", "geoNorth", "geoWest",
# Tech
"dii_high", "ict_percent", "ict_employment", "ict_value_added", "buss_gerd", "value_eur",
# Org
"gov_gerd", "gov_personnel", "pct_employment_gov",
"contracting_authority_typeBody governed by public law", "contracting_authority_typeNational or federal agency/office", "contracting_authority_typeRegional or local agency/office", "contracting_authority_typeRegional or local authority", "contracting_authority_typeOther", 
"contracting_authority_activityDefence", "contracting_authority_activityEconomic and financial affairs", "contracting_authority_activityEducation", "contracting_authority_activityElectricity", "contracting_authority_activityEnvironment", 
"contracting_authority_activityGeneral public services", "contracting_authority_activityHealth", "contracting_authority_activityHousing and community amenities", "contracting_authority_activityPublic order and safety", 
"contracting_authority_activitySocial protection", 
# Env
"gdp", "population", "inno_procurementGood performer", "inno_procurementModerate performer", "inno_procurementModest performer", "position_economy", "position_efficiency", "position_market", "desi_total", "gdp:population", "value_eur:gdp"
    #... continue for all terms
)

# create a vector for colors
# vars in term_order from 1:8 are control vars, 9:14 tech, 15:30 org, 31:41 env


# Extracting values
ci_model <- broom.mixed::tidy(pool(model3), conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(conf_int = paste0("(",round(conf.low,3),";",round(conf.high,3), ")"),
  labels = factor(desired_labels[term], levels = desired_labels[term_order], ordered = TRUE)) # Reordering the terms based on term_order
# Map the color based on the terms
ci_model$color <- ifelse(ci_model$term %in% term_order[1:8], "#1f77b4", # Control
                 ifelse(ci_model$term %in% term_order[9:14], "#ff7f0e", # Tech
                 ifelse(ci_model$term %in% term_order[15:30], "#2ca02c", "#d62728"))) # Org and Env

ci_model

ci_model_8 <- broom.mixed::tidy(pool(model3), conf.int = TRUE, conf.level = 0.8) %>%
  dplyr::filter(term != "(Intercept)")

# FE estimates
png(filename="outputs/figures/logodds.png", height=1200, width=800, res=100) # Adjust the resolution with 'res' as needed

title <- "Fixed-effects estimates of tender- and country-level variables on <br>the odds of SaaS tenders"

subtitle <- "<span style = 'color:black;'>From </span><span style='color:#1f77b4;'>control variables</span>, contract type, tenders covered by framework agreements and government procurement agreement, <br> as well as government procurement
size were related to the odds of tender being SaaS. From <span style='color:#d62728;'>technological factors</span>,<br>
 significant predictors were the ICT employment, tender value, and the ICT GDP contribution (marginally). Several <br><span style='color:#ff7f0e;'>organisational factors</span> were significant predictors: Government R&D, R&D Personnel, 
authority type, and authority activity.<br> Finally, innovation procurement performance, position of the the free market, and the interaction of GDP and population size <br>were significant <span style='color:#2ca02c;'>environmental factors</span>."


# Plot
ggplot(ci_model, aes(x = estimate, y = reorder(labels, dplyr::desc(labels)), color = color)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  geom_text(aes(label = round(estimate, 2)), position = position_nudge(y = 0.43), size = 3) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "black", size = 0.2) +
  geom_segment(aes(y = labels, yend = labels, x = conf.low, xend = conf.high), size = 0.4) + 
  geom_segment(aes(y = labels, yend = labels, x = ci_model_8$conf.low, xend = ci_model_8$conf.high), size = 0.8) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) +  
  theme_minimal() +
  theme(legend.position = "none") +  
  labs(y ="", x= "Log odds", title = title, subtitle = subtitle) + 
  theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm"), # t,r,b,l
 plot.title = element_markdown(size = 15),
  plot.subtitle = element_markdown(size = 10),
  plot.title.position = "plot") 


dev.off()




#######################################
##### MODEL 3 PREDICTED PROBABILITIES
#######################################

###########################################################



# Get data in a loop

# Create a list of all variables

#Titles
titles_tech <- c("Percentage of the ICT sector in GDP", "Percentage of the ICT personnel in total employment", "Tender value")
titles_org <- c("Government R&D expenditure", "Government ICT personnel", "Contracting authority type",
"Contracting authority activity")
titles_envir <- c("Innovation procurement", "Market position", "GDP x Population size")

all_titles <- c(titles_tech, titles_org, titles_envir)
all_titles

' Names'
significant_vars_technological <- c("ict_percent", "ict_employment", "value_eur")
significant_vars_organisational <- c("gov_gerd", "gov_personnel", "contracting_authority_type", 
"contracting_authority_activity")
significant_vars_envir <- c("inno_procurement", "position_market") # +GDP.POP interaction

all_vars <- c(significant_vars_technological, significant_vars_organisational, significant_vars_envir)

predictions_list <- list()

no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

for (i in 1:length(all_vars)){
    # list of predicted values for each imputed dataset
    pred_list <- furrr::future_map(model3, ~ ggpredict(.x, terms = all_vars[i]))
    # pool predictions
    pooled_predictions <- ggeffects::pool_predictions(pred_list)
    plot_data <- as.data.frame(pooled_predictions)
    predictions_list[[i]] <- plot_data
}

save(predictions_list, file = "./outputs/figures/predictions_list.RData")
load("./outputs/figures/predictions_list.RData")

###################

# FUnctions for plotting

create_base_plot <- function(data) {
    library(ggplot2)
    library(scales)
  
    p <- ggplot(data, aes(x = x, y = predicted)) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "tomato", alpha = 0.3) +
        theme_minimal() +
        labs(x = "", y = "", title = "") +
        coord_cartesian(ylim=c(0,NA)) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        # Increase title size
        theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 8))
  
    return(p)
}

unscale <- function(x, average_mean, average_sd) {
  return((x - average_mean) / (2 * average_sd))
}

# Mean and sd for ticks

# average mean and sd for ticks

average_mean <- numeric(length(all_vars))
names(average_mean) <- all_vars

average_sd <- numeric(length(all_vars))
names(average_sd) <- all_vars

for (i in all_vars){
    average_mean[i] <- mean(purrr::map_dbl(imp_list, ~ mean(.x[[i]], na.rm = TRUE)))
    average_sd[i] <- mean(purrr::map_dbl(imp_list, ~ sd(.x[[i]], na.rm = TRUE)))

    # value eur was standardized prior imputations, raw values are needed
    if (i == "value_eur") {
        average_mean[i] <- mean(df$value_eur, na.rm = TRUE)
        average_sd[i] <- sd(df$value_eur, na.rm = TRUE)
        }
}

average_mean
average_sd

######## ICT percent

p1 <- create_base_plot(predictions_list[[1]]) +
    geom_line() +
    labs(title = all_titles[1], x = "% of GDP") +
    scale_x_continuous(breaks = unscale(c(0, 3, 6, 9), 
                        average_mean = average_mean["ict_percent"],
                        average_sd = average_sd["ict_percent"]),
    labels = c(0, 3, 6, 9))

ggsave("./outputs/figures/p1.png", p1, width = 3, height = 3, dpi = 300, bg = "white")


######## ICT employment

p2 <- create_base_plot(predictions_list[[2]]) +
    geom_line() +
    labs(title = all_titles[2], x = "% of total employment") +
    scale_x_continuous(breaks = unscale(c(0, 2, 4, 6), 
                        average_mean = average_mean["ict_employment"],
                        average_sd = average_sd["ict_employment"]),
    labels = c(0, 2, 4, 6))

ggsave("./outputs/figures/p2.png", p2, width = 3, height = 3, dpi = 300, bg = "white")

######## Value eur

predictions_list[[3]][1:4,]

p3 <- create_base_plot(predictions_list[[3]][1:3, ]) +
    geom_line() +
    labs(title = all_titles[3], x = "Value in million EUR") +
    scale_x_continuous(breaks = unscale(c(60000000),
                        average_mean = average_mean["value_eur"],
                        average_sd = average_sd["value_eur"]),
    labels = c(60))

ggsave("./outputs/figures/p3.png", p3, width = 3, height = 3, dpi = 300, bg = "white")


######## Gov R&D

p4 <- create_base_plot(predictions_list[[4]]) +
    geom_line() +
    labs(title = all_titles[4], x = "% of GPD") +
    scale_x_continuous(breaks = unscale(c(0, 0.2, 0.4), 
                        average_mean = average_mean["gov_gerd"],
                        average_sd = average_sd["gov_gerd"]),
    labels = c(0, 0.2, 0.4))

ggsave("./outputs/figures/p4.png", p4, width = 3, height = 3, dpi = 300, bg = "white")


######## Gov personnel

p5 <- create_base_plot(predictions_list[[5]]) +
    geom_line() +
    labs(title = all_titles[5], x = "% of total employment") +
    scale_x_continuous(breaks = unscale(c(0, 0.3, 0.6), 
                        average_mean = average_mean["gov_personnel"],
                        average_sd = average_sd["gov_personnel"]),
    labels = c(0, 0.3, 0.6))

ggsave("./outputs/figures/p5.png", p5, width = 3, height = 3, dpi = 300, bg = "white")


######## Authority type

# Example custom labels (you can replace this with your own labels)
original_levels <- unique(predictions_list[[6]]$x)
custom_labels <- c("Ministry or other national/regional authority", 
"Other", "Body governed by public law", "National or federal agency/office",
"Regional or local agency/office", "Regional or local authority") # this is just an example, adjust it accordingly
custom_labels_wrapped <- stringr::str_wrap(custom_labels, 10)

## Order levels by the median of 'predicted'
ordered_levels <- predictions_list[[6]] %>%
  group_by(x) %>%
  summarize(median_pred = median(predicted)) %>%
  arrange(median_pred) %>%
  pull(x)

# Convert 'x' to a factor with ordered levels
predictions_list[[6]]$x <- factor(predictions_list[[6]]$x, levels = ordered_levels)

# Create custom labels that match the order of ordered_levels
labels_ordered <- custom_labels[match(ordered_levels, original_levels)]
labels_mapping <- setNames(stringr::str_wrap(labels_ordered, 20), ordered_levels)

p6 <- create_base_plot(predictions_list[[6]]) +
  geom_point(color = "black", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "black") +
  labs(title = all_titles[6], x = "") +
  scale_x_discrete(labels = labels_mapping) + coord_flip() +
          theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 7),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 7))

ggsave("./outputs/figures/p6.png", p6, width = 3, height = 3, dpi = 300, bg = "white")


# Contracting authority activity

## Order levels by the median of 'predicted'

ordered_levels <- predictions_list[[7]] %>%
  group_by(x) %>%
  summarize(median_pred = median(predicted)) %>%
  arrange(median_pred) %>%
  pull(x)

# Convert 'x' to a factor with ordered levels

predictions_list[[7]]$x <- factor(predictions_list[[7]]$x, levels = ordered_levels)

labels_mapping <- stringr::str_wrap(ordered_levels, 20)

p7 <- create_base_plot(predictions_list[[7]]) +
  geom_point(color = "black", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "black") +
  labs(title = all_titles[7], x = "") +
  scale_x_discrete(labels = labels_mapping) + coord_flip() +
  scale_x_discrete(labels = labels_mapping) + coord_flip() +
          theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 7),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 6))


ggsave("./outputs/figures/p7.png", p7, width = 3, height = 3, dpi = 300, bg = "white")


# Innovation procurement

# 1. Define your custom order
# Assuming you have a vector 'custom_order' that specifies the order:
custom_order <- c("Low performer", "Modest performer", "Moderate performer", "Good performer")  # Replace this with the actual order you want

# 2. Convert 'x' to a factor with the custom order
predictions_list[[8]]$x <- factor(predictions_list[[8]]$x, levels = custom_order)

p8 <- create_base_plot(predictions_list[[8]]) +
  geom_point(color = "black", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "black") +
  labs(title = all_titles[8], x = "") +
  scale_x_discrete(labels = str_wrap(custom_order, 10)) +
          theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 8))


ggsave("./outputs/figures/p8.png", p8, width = 3, height = 3, dpi = 300, bg = "white")


# Market position

p9 <- create_base_plot(predictions_list[[9]]) +
    geom_line() +
    labs(title = all_titles[9], x = "Parliament position (0 = Average)")

ggsave("./outputs/figures/p9.png", p9, width = 3, height = 3, dpi = 300, bg = "white")



# Interaction GDP x Population size

term <- c("gdp", "population [-0.5, 0, 0.5]") # +GDP.POP interaction
title_term <- c("GDP x Population size")


no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

pred_list_int <- furrr::future_map(model3, ~ ggpredict(.x, terms = term))
pooled_predictions_int <- ggeffects::pool_predictions(pred_list_int)

# Extract the data from pooled predictions
plot_data_int <- as.data.frame(pooled_predictions_int)
plot_data_int

average_mean_gdp <- mean(df$gdp, na.rm = TRUE)
average_sd_gdp <- sd(df$gdp, na.rm = TRUE)

average_mean_pop <- mean(df$population, na.rm = TRUE)
average_sd_pop <- sd(df$population, na.rm = TRUE)

# print the mean and sd

average_mean_gdp
average_sd_gdp

average_mean_pop
average_sd_pop

library(directlabels)


plot_int <- ggplot(plot_data_int, aes(x = x, y = predicted, group = factor(group))) +
    geom_line(aes(color = factor(group))) +  # Plot lines for each group
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(group)), alpha = 0.3) +  # Shaded region for confidence intervals
    theme_minimal() +
    labs(x = "GDP (in thousands of EUR)", y = "", title = title_term) +
    coord_cartesian(ylim=c(0,NA)) +
    scale_x_continuous(breaks = unscale(c(0, 30000, 60000), 
                        average_mean = average_mean_gdp,
                        average_sd = average_sd_gdp),
    labels = c(0, 30, 60)) +
    # rename the legend
     # rename the legend
    scale_color_discrete(name = "Population size", 
                         breaks = c(-0.5, 0, 0.5), 
                         labels = c("Small (~5 Million)", "Average (~40 Million)", "Large (~70 Million)")) +
    scale_fill_discrete(name = "Population size", 
                        breaks = c(-0.5, 0, 0.5), 
                        labels = c("Small (~5 Million)", "Average (~40 Million)", "Large (~70 Million)")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    # add direct labels
    # Increase title size
        theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 8),
        legend.title = element_text(size = 5),     # Adjust legend title size
        legend.text = element_text(size = 5),      # Adjust text size
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),  # Adjust the width of the legend key
    legend.key.height = unit(0.3, "cm"))

ggsave("./outputs/figures/p10.png", plot_int, width = 3, height = 3, dpi = 300, bg = "white")


unscale(5000000, average_mean = average_mean_pop, average_sd = average_sd_pop)
unscale(70000000, average_mean = average_mean_pop, average_sd = average_sd_pop)

library(gridExtra)
library(ggplot2)

library(grid)


grid_plot <- arrangeGrob(p1, p2, p3, nullGrob(),
                         p4, p5, p6, p7,
                         p8, p9, plot_int, nullGrob(),
                         ncol = 4)

ggsave(filename = "./outputs/figures/my_combined_plot.png", plot = grid_plot, width = 10, height = 8)




