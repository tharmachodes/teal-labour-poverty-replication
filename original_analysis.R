# Teal Paper Replication - Original Analysis
# This script reproduces the main regression table and Figure 2 from Teal et al. (2000)
# Author: Research Assistant
# Date: 2025-12-28

# Load required libraries
library(tidyverse)
library(ggplot2)
library(broom)

# Set seed for reproducibility
set.seed(12345)

# Load the household survey data
data <- read.csv("data/household_survey.csv", header = TRUE)

# Data cleaning and variable creation as per Teal paper
# Log wage already present as log_wage
# Poverty status indicator (1 = below poverty line, 0 = above)
# Create additional control variables: household size, landholding
# Region dummies
data <- data %>%
  mutate(region_factor = as.factor(region),
         credit_access = as.factor(credit_access),
         school_attendance = as.numeric(school_attendance))

# Regression specification: poverty_status ~ log_wage + household_size + landholding + region
model <- glm(poverty_status ~ log_wage + household_size + landholding + region_factor,
             family = binomial(link = "logit"),
             data = data)

# Display regression results
summary(model)

# Create tidy table of coefficients
coef_table <- tidy(model, conf.int = TRUE)
print(coef_table)

# Save regression table to CSV for later use
write.csv(coef_table, "output/regression_table.csv", row.names = FALSE)

# Figure 2: Predicted probability of poverty vs. log wage
# Generate predicted probabilities over a range of log wages
pred_data <- expand.grid(log_wage = seq(min(data$log_wage), max(data$log_wage), length.out = 100),
                         household_size = mean(data$household_size),
                         landholding = mean(data$landholding),
                         region_factor = factor(1, levels = levels(data$region_factor)))

pred_data$pred_prob <- predict(model, newdata = pred_data, type = "response")

# Plot
fig2 <- ggplot(pred_data, aes(x = log_wage, y = pred_prob)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "Predicted Probability of Poverty vs. Log Wage",
       x = "Log of Agricultural Wage Rate",
       y = "Predicted Probability of Being Poor") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save figure
ggsave("output/figure2.png", fig2, width = 8, height = 6)

# Print confirmation
cat("Original analysis completed. Regression table saved to output/regression_table.csv, figure saved to output/figure2.png.\n")