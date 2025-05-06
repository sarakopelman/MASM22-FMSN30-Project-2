library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
data <- read_excel("Data/carotene.xlsx")
summary(data)

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
                         labels = c("Often", "Rarely", "No")))

# Set baseline
data$vituse <- relevel(data$vituse, ref = "No")

# Linear model with all variables
Full_Linear_Model <- lm(log(betaplasma) ~ bmi + age + calories + 
                          fat + cholesterol + fiber + alcohol + betadiet + 
                          smokstat + sex + vituse, data = data)


# ---------------------------------------------------------------------------------------------
# 1(a). We will now focus on whether the concentration of plasma β-carotene is low, or not, and
# model the probability of having a low plasma β-carotene concentration as a function of the
# background and/or dietary variables. In order to do this we need a suitable cut-off value. We
# will use one that has been identified as suitable for separating between high or low risk of
# developing certain medical conditions.

# The cut-off value has been reported as 0.42 μmol/l (micromoles per litre). Unfortunately, our
# data is from 1989 when concentrations where expressed as ng/ml (nanograms per millilitre)1

# Read up on Mole (unit) on Wikipedia, especially under Molar mass and Molar concentration.
# Then find the molar mass (or molecular weight) for β-carotene expressed as g/mol. Use the
# molar mass to translate the cut-off value 0.42 μmol/l into a ng/ml. Sanity check: the correct
# answer, a, lies in the neighborhood of the upper quartile of the plasma β-carotene values in
# the dataset.



# Molar mass = 536.9 g/mol
# Source: https://pubchem.ncbi.nlm.nih.gov/compound/Beta-Carotene

# Molar mass * molar concentration = [g/mol] * [mol/l] = [g / l]
# 536.9 * 0.42 * e-6 = 0,000225498 g/l = 225 498 ng/l = 225,5 ng/ml

# Sanity check: Compare with upper quantile value from the data - should be in
# the same region: 
#       3rd Qu.: 230.0  
#       => Ok! 










#1(b). ---------------------------------------------------------------------------------------------
# Create a new variable, lowplasma_01, that is 1 when betaplasma < a ng/ml, and 0
# otherwise. In order to make it easier to colour code the observations according to low or
# high plasma β-carotene, as well as producing confusion matrices, it will be convenient to also
# create a separate factor version of this variable:


data |> mutate(
  lowplasma_01 = as.numeric(betaplasma < 225.5),
  lowplasma_hl = factor(lowplasma_01,
                        levels = c(0, 1),
                        labels = c("high", "low"))) -> data  

#Testplot endast:
#ggplot(data, aes(x = vituse, y = lowplasma_01)) +
#  geom_point() +
#  xlab("Vitamin usage") +
#  ylab("High or Low Betaplasma") +
#  labs(title = "High BetaPlasma (=0) or Low Betaplasma (=1) vs Vitamin Usage") 

# Present the number of observations that have "low" and "high" plasma-β
table(data$lowplasma_01)
# 0   1 
# 80 235 

#Present the proportion with "low" concentration, as a percentage.
# 235/315 = 0.746 = 74.6%




#1(c). -----------------------------------------------------------------------------
# Start by examining the relationship between low plasma β-carotene and vitamin 
# use by counting the number of observations for all combinations, using count, 
# and then transform the result into a wide format with pivot_wider(), resulting
# in a 3 × 2-table:

# Calculate the probabilities and the corresponding odds for having low plasma β-carotene
# for each of the three vitamin use categories, and add them to the table (mutate()). Also
# calculate the odds ratios for each of the categories, compared to a suitable reference category,
# and add them to the table, Table.1(c)


data |> count(vituse, lowplasma_hl) |>
  pivot_wider(id_cols = vituse,
              names_from = lowplasma_hl,
              values_from = n) |>
  mutate(p_low = low / (low + high )) |>
  mutate(odds_low = p_low / (1 - p_low)) |>
  mutate(odds_ratio = odds_low / odds_low[vituse == "No"] )

# SaraS kommentar: 
# Eftersom |> används (istället för ->) sparas detta bara 
# tillfälligt i minnet och inte i datan, syns alltså ej om man kör glimpse(data)-
# Notera också att jag valde "No"-kategoerin som referens för odds ratio (OR), 
# dvs. ingen vitaminanvändning är referens
# p_low ovanför är förkortning av probability_low





# 1(d). ------------------------------------------------------------------------------------------------

# Fit a logistic regression model, Model.1(d), for lowplasma_01 or lowplasma_hl 
# with vituse as explanatory variable. Present the β-estimates, the e^β -estimates 
# and their profile likelihood based 95 % confidence intervals. Identify these estimates with the corresponding
# values in Table.1(c), and express the odds and the probabilities for each of the categories as
# functions of the β-parameters.

Logistic_Model_Vituse <- glm(lowplasma_01 ~ vituse, family = "binomial", data = data)


# Use the regression model to calculate the linear predictor, the odds, and the probability of
# having a low plasma β-carotene concentration, together with their respective 95 % confidence intervals, for each of the three vitamin use categories. Compare the result with the
# probabilities in Table.1(c).

summary(Logistic_Model_Vituse)
coef(Logistic_Model_Vituse) 



# ------------------------------------------------------------------------------------------------




