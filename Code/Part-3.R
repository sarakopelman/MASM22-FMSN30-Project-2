library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
data <- read_excel("Data/carotene.xlsx")
summary(data)


# 3(a). -----------------------------------------------------------------------
# Task:
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

# From Project 2 Part 1. 
data |> mutate(
  lowplasma_01 = as.numeric(betaplasma < 225.5),
  lowplasma_hl = factor(lowplasma_01,
                        levels = c(0, 1),
                        labels = c("high", "low"))) -> data  
# The full logistic model - ALL available variables! 
Full_Logistic_Model <- glm(lowplasma_01 ~ bmi + age + calories + 
                               fat + cholesterol + fiber + alcohol + betadiet + 
                               smokstat + sex + vituse, family = "binomial", data = data)

#VIF-tests- compare linear and logistic models! 
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


# Task: 
# Perform one Backwards elimination, starting with the full model, and one 
# Forward selection, starting with the null model. Both versions should have the
# null model as lower scope and the full model as upper scope, and use BIC as 
# selection criterion.

Null_Model <- glm(lowplasma_01 ~ 1, family = "binomial", data = data)

#Backward Elimination, starts from the full model 
Backwards_Elimination_Model <- step(Logistic_Model,
     direction = "backward",
     scope = list(lower = formula(Null_Model),
                  upper = formula(Logistic_Model)),
     k = log(nobs(Null_Model)))

#Forward Selection, start from the null model 
Forward_Selection_Model <- step(Null_Model,
     direction = "forward",
     scope = list(lower = formula(Null_Model),
                  upper = formula(Logistic_Model)),
     k = log(nobs(Null_Model)))


# Task: 
# Then, use each of the resulting models as starting model in a Stepwise 
# regression, again with null and full models as scope, and BIC as criterion

Stepwise_From_Backward <- step(Backwards_Elimination_Model,
                               direction = "both",
                               scope = list(lower = formula(Null_Model),
                                            upper = formula(Logistic_Model)),
                               k = log(nobs(Backwards_Elimination_Model)))

Stepwise_From_Forward <- step(Forward_Selection_Model,
                              direction = "both",
                              scope = list(lower = formula(Null_Model),
                                           upper = formula(Logistic_Model)),
                              k = log(nobs(Forward_Selection_Model)))


# Task: 
# If any of the resulting Stepwise models contain categorical variables, 
# determine, using a suitable test, if you can reduce the number of categories 
# from three to two in any of those variables

Stepwise_From_Backward$coefficients
Stepwise_From_Forward$coefficients

# We notice -> that the Stepwise_From_Forward model - has two categorical variables, 
# vituseOften and vituseRarely. But that is the only two. So we check if we can 
# reduce further by combing the two factors into one! 
#  ---> Likelihood ratio test! 

# First we want to create a new, two-category vituse - where Rarely and Often 
# has collapsed to "Used" (for having used vitamins), we don't mess with the 
# original data so I create a new.. 
new_data <- data

# Transform three levels to two, but assigning vituse category "Never" into "Never" 
# and if the category is not "Never" it's assigned into "Used". 
new_data$vituse_reduced <- ifelse(new_data$vituse == "Never", "Never", "Used")
new_data$vituse_reduced <- factor(new_data$vituse_reduced)
# See that a new 2-factor variable (vituse_reduced) has indeed been created in
# the list of variables! 
glimpse(new_data)

#FORTSÄTT HÄRIFRÅN SARA!! 
#DET ÄR BARA ATT JÄMFÖRA MODELLERNA PÅ SAMMA VIS SOM DU GJORDE LIKELIHOOD TESTET 
#I UPPGIFT 1(E) - men nu paus! :D






