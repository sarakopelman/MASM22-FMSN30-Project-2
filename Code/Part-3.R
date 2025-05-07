library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
data <- read_excel("Data/carotene.xlsx")
summary(data)


# 3(a). -----------------------------------------------------------------------
# Fit a multiple logistic model using all 11 x-variables, including calories, 
# and calculate the VIF-values. Did the change of model from a linear regression
# in Project 1.3(c) to a logistic regression, affect the VIF? Why not? In all 
# further analyses, exclude the variable with the multicollinearity issues.

# From Project_1
# ---------------------------------------------------------------------------------------------------
# Turn the categorical variables into factor variables and pick suitable reference categories for them

data <- data %>%
  mutate(smokstat = factor(smokstat,
                           levels = c(1, 2, 3),
                           labels = c("Never", "Former", "Current")))

data <- data %>%
  mutate(sex = factor(sex,
                      levels = c(1, 2),
                      labels = c("Male", "Female")))
# Set baseline
data$sex <- relevel(data$sex, ref = "Female")

data <- data %>%
  mutate(vituse = factor(vituse,
                         levels = c(1, 2, 3),
                         labels = c("Often", "Rarely", "Never")))

# Set baseline
data$vituse <- relevel(data$vituse, ref = "Never")

# Linear model with all variables
Full_Linear_Model <- lm(log(betaplasma) ~ bmi + age + calories + 
                          fat + cholesterol + fiber + alcohol + betadiet + 
                          smokstat + sex + vituse, data = data)

# From part 1. 
# --------------------------
data |> mutate(
  lowplasma_01 = as.numeric(betaplasma < 225.5),
  lowplasma_hl = factor(lowplasma_01,
                        levels = c(0, 1),
                        labels = c("high", "low"))) -> data  

Full_Logistic_Model <- glm(lowplasma_01 ~ bmi + age + calories + 
                               fat + cholesterol + fiber + alcohol + betadiet + 
                               smokstat + sex + vituse, family = "binomial", data = data)

vif(Full_Linear_Model)
vif(Full_Logistic_Model) 
# Compared with the values from Project 1 - they have changed slightly, with a 
# variation of the third decimal. They still showcase the same pattern, i.e. 
# the same problematic (multicollinearity) variables - calories (worst) and fat. 

# Henceforth we remove the most problematic variable (calories) from the model! 
# And include all pair-wise interaction terms - for 10 variables that is (10 2) = 45 
# interaction variables, and a total (with main) 45 + 10 = 55 variables in the model. 
# We can simplify this by squaring the variables, OBS! Actually there is more  
# because we have categorical variables (from the model data we can see it's 
# actually 77)! 
Logistic_Model <- glm(lowplasma_01 ~ (bmi + age + fat + cholesterol + fiber + 
                      alcohol + betadiet + smokstat + sex + vituse)^2,
                      family = "binomial", data = data)






