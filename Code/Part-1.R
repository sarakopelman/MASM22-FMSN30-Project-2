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
                         labels = c("Often", "Rarely", "Never")))

# Set baseline
data$vituse <- relevel(data$vituse, ref = "Never")

# Linear model with all variables
Full_Linear_Model <- lm(log(betaplasma) ~ bmi + age + calories + 
                          fat + cholesterol + fiber + alcohol + betadiet + 
                          smokstat + sex + vituse, data = data)


# 1(a). ---------------------------------------------------------------------------------------------
# We will now focus on whether the concentration of plasma β-carotene is low, or not, and
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
  mutate(odds_ratio = odds_low / odds_low[vituse == "Never"] )

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

summary(Logistic_Model_Vituse)

cbind(beta = coef(Logistic_Model_Vituse),
      expbeta = exp(Logistic_Model_Vituse$coefficients),
      exp(confint(Logistic_Model_Vituse))) |>
  round(digits = 4)

# The full model in vector form:
# p_i = e^(x_i*β)/(1 + e^(x_i*β))

#This is the full model:
# p_i = e^(β_0 + x_1i*β_1 + x_2i*β_2)/(1 + e^(β_0 + x_1i*β_1 + x_2i*β_2))

#From Table.1(d): 
# e^(β_0) = 7.54 = Odds for reference category, Never
# e^(β_0 + β_1) = 7.54 * 0.219 = Odds for category 'Often' 
# e^(β_0 + β_2) = 7.54 * 0.3853 = Odds for category 'Rarely' 

Odds_Never <- exp(Logistic_Model_Vituse$coefficients["(Intercept)"])
Odds_Often <- exp(Logistic_Model_Vituse$coefficients["(Intercept)"] + Logistic_Model_Vituse$coefficients["vituseOften"])
Odds_Rarely <- exp(Logistic_Model_Vituse$coefficients["(Intercept)"] + Logistic_Model_Vituse$coefficients["vituseRarely"])

#Probabilities 
# p = odds / (1 + odds)
p_Never <- Odds_Never / (1 + Odds_Never)
p_Often <- Odds_Often / (1 + Odds_Often)
p_Rarely <- Odds_Rarely / (1 + Odds_Rarely)

#Tabulated!
table_1d <- cbind(
  OddsRatio = c(Odds_Never, Odds_Often, Odds_Rarely),
  Probabilities = c(p_Never, p_Often, p_Rarely)) |>
  round(digits = 4)

rownames(table_1d) <- c("Never", "Often", "Rarely")
table_1d


# Use the regression model to calculate the linear predictor, the odds, and the probability of
# having a low plasma β-carotene concentration, together with their respective 95 % confidence intervals, for each of the three vitamin use categories. Compare the result with the
# probabilities in Table.1(c).


# Intervals for Beta
# Intervals for Odds and Odds ratio (e^β)
cbind(beta_hat = coef(Logistic_Model_Vituse),
      CI_beta_hat = confint(Logistic_Model_Vituse),
      odds_ratio = exp(coef(Logistic_Model_Vituse)),
      CI_odds_ratio = exp(confint(Logistic_Model_Vituse))) |>
  round(digits = 4)

# Linear predictor and probabilities
data |> mutate(phat = predict(Logistic_Model_Vituse, type = "response")) -> data_pred

data_pred <- cbind(data_pred,
                   logit = predict(Logistic_Model_Vituse, se.fit = TRUE))
data_pred |> mutate(logit.residual.scale = NULL) -> data_pred
glimpse(data_pred)

#Confidence interval for the linear predictor, i.e. the log-odds (betas)
lambda <- qnorm(1 - 0.05/2)
data_pred |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> data_pred

#Confidence interval for the odds, i.e. e^betas
data_pred |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> data_pred

#Confidence interval for the probabilities, i.e. p = odds / ( 1 + odds )
data_pred |> mutate(
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> data_pred
glimpse(data_pred)

#Tabulated
cbind(log_odds = data_pred$logit.fit,
      Lower = data_pred$logit.lwr,
      Upper = data_pred$logit.upr) |>
  round(digits = 4)


# 1(e). ------------------------------------------------------------------------------------------------
# For Model.1(d), use a suitable test to determine whether there are any significant differences
# between the vitamin use categories in the model. Report what type of test you use, the null
# hypothesis H0, the test statistic, the asymptotic distribution of the test statistic when H0 is
# true, the P-value and the conclusion. Explain why you choose that type of test and comment
# on the result

# We want to test whether the null hypothesis, H0: β_1 = β_2 = 0
# I.e. if they differ from the reference category - if not we can not conclude
# that vitamin usage impacts the model. Since we only have vitamin usage as one
# categorical variable in the model we perform a global likelihood ratio test. 

#Save variables
model_sum <- summary(Logistic_Model_Vituse)
null_deviance <- model_sum$null.deviance
df_null <- model_sum$df.null
deviance <- model_sum$deviance
df_residual <- model_sum$df.residual

#Calculate differences
D_diff <- null_deviance - deviance
df_diff <- df_null - df_residual
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

#I create a table and remove the vertical title using rownames, just to clean it up! 
table_1e <- cbind(D_diff, df_diff, chi2_alpha, Pvalue)
rownames(table_1e) <- c(" ")
table_1e

#Conclusion, we reject H_0! One or both of categories have significant impact 
# on the probability of low betaplasma. 


