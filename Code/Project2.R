library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(pROC)
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
#Part 2. 
keep <- c("data", "Full_Linear_Model", "keep")
rm(list=setdiff(ls(),keep))
# 2(a). Plot lowplasma_01 against bmi and add a moving average. Does the probability of having
# low plasma beta-carotene increase or decrease with BMI? Does this agree with what you
# found in Project 1?

ggplot(data, aes(bmi, lowplasma_01)) +
  geom_point() +
  xlab("Bmi") +
  ylab("Low Plasma") +
  geom_smooth() +
  labs(title = "Low Plasma (=1) or Not low Plasma (=0) vs bmi") 


# Fit a logistic regression for lowplasma_01 (or lowplasma_hl) as a function of BMI. This
# will be referred to as Model.2(a). Present the β-estimates, the eβ
# -estimates and their profile
# likelihood based 95 % confidence intervals.
# Add the predicted probabilities, and a 95 % confidence interval, to the plot and comment on
# the result.

Logistic_model_bmi <- glm(lowplasma_01 ~ bmi, family = "binomial", data = data)
summary(Logistic_model_bmi)
bhat <- Logistic_model_bmi$coefficients
ci.beta <- confint(Logistic_model_bmi)
#Coeffiencts
cbind(b = bhat, ci.beta, `exp(b)` = exp(bhat), exp(ci.beta)) |> round(digits = 2)
#OR
# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(bhat)` = or, ci.or) |> round(digits = 2)

#Predicted probabilities
data |> mutate(phat = predict(Logistic_model_bmi, type = "response")) -> log_bmi_pred

predplot <- ggplot(log_bmi_pred, aes(bmi, lowplasma_01)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("bmi") +
  ylab("Low plasma") +
  labs(title = "Low Plasma (=1) or Not low Plasma (=0) vs bmi",
       caption = "red = fitted line, blue dashed = moving average")

log_bmi_pred <- cbind(
  log_bmi_pred,
  logit = predict(Logistic_model_bmi, se.fit = TRUE))
glimpse(log_bmi_pred)

log_bmi_pred |> mutate(logit.residual.scale = NULL) -> log_bmi_pred

lambda <- qnorm(1 - 0.05/2)
log_bmi_pred |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> log_bmi_pred
glimpse(log_bmi_pred)

log_bmi_pred |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> log_bmi_pred
glimpse(log_bmi_pred)

log_bmi_pred |> mutate(
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> log_bmi_pred
glimpse(log_bmi_pred)

predplot +   geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2, data = log_bmi_pred) +   labs(title = "Low Plasma (=1) or Not low Plasma (=0) vs bmi",
                                                                                                     caption = "red = fitted line with 95% confidence interval, blue dashed = moving average")


bmichange<- c(1, -1, -10)


relchange <- exp(bhat[2] * bmichange) - 1


ci.or.change <- exp(ci.beta[2, ] %*% t(bmichange)) - 1


changeresults <- cbind(Change = bmichange, RelativeChange = relchange, 
                       CI_Lower = ci.or.change[1, ], CI_Upper = ci.or.change[2, ])

round(changeresults, digits = 2)

#2b
#Wald-test
model_bmi_sum <- summary(Logistic_model_bmi)
b1 <- model_bmi_sum$coefficients[2, "Estimate"]
se.b1 <- model_bmi_sum$coefficients[2, "Std. Error"]
z.b1 <- model_bmi_sum$coefficients[2, "z value"]
p.b1 <- 2 * (1 - pnorm(abs(z.b1)))

#LR-test
anova(Logistic_model_bmi)

#2c)
bmi_model_infl <- influence(Logistic_model_bmi)
glimpse(bmi_model_infl)
bmi_pred <- cbind(data,
                  xbeta = predict(Logistic_model_bmi),
                  v = bmi_model_infl$hat)
glimpse(bmi_pred)

#plotting leverage
pplus1_bmi <- length(Logistic_model_bmi$coefficients)
n <- nobs(Logistic_model_bmi)

leverageplot <- ggplot(bmi_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(2*pplus1_bmi/n)) +
  facet_wrap(~lowplasma_01)

bmi_pred %>% slice_max(v, n=8)

linmodel <- lm(lowplasma_01 ~ bmi, data)
lin_leverage <- hatvalues(linmodel)
bmi_pred$lin_leverage <- lin_leverage
leverageplot <- ggplot(bmi_pred, aes(x = xbeta)) +
  geom_point(aes(y = v), color = "blue") +   # Logistic regression leverage
  geom_point(aes(y = lin_leverage), color = "red") +  # Linear regression leverage
  geom_hline(yintercept = c(2 * pplus1_bmi / n), linetype = "dashed") +  # Reference line
  facet_wrap(~lowplasma_01) +
  labs(y = "Leverage", x = "Linear Predictor (xbeta)",
       title = "Leverage Values: Logistic (blue) vs Linear (red)",
       subtitle = "Dashed line indicates threshold for high leverage: 2(p+1)/n",
       caption = "1 = Low beta-plasma, 0 = High beta-plasma")




leverageplot


#2d
bmi_pred |> mutate(devresid = bmi_model_infl$dev.res,
                   stddevresid = devresid/sqrt(1-v)) -> bmi_pred
glimpse(bmi_pred)

ggplot(bmi_pred, aes(x = xbeta,
                     y = stddevresid,
                     color = as.factor(lowplasma_01))) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3),
             linetype = c("dotted", "dashed", "solid", "dashed", "dotted"),
             linewidth = 1) +
  labs(title = "Standardised deviance residuals vs linear predictor",
       x = "xb", y = "devstd",
       color = "Y") +
  labs(caption = "1 = Low beta-plasma, 0 = High beta-plasma")


#2e
bmi_pred <- mutate(bmi_pred,
                   Dcook = cooks.distance(Logistic_model_bmi),
                   high_resid = abs (stddevresid) > 2)



ggplot(bmi_pred, aes(x = bmi, y = Dcook)) +
  geom_point(aes(color = factor(lowplasma_01))) +
  geom_point(data = filter(bmi_pred, v > 0.045), 
             aes(x = bmi, y = Dcook, color = "High leverage (v > 0.045)"),
             size = 3) +
  geom_point(data = filter(bmi_pred, high_resid == TRUE), 
             aes(x = bmi, y = Dcook, color = "High residuals (>±2)"),
             size = 3, shape = 21, fill = "orange", stroke = 1.2) +
  geom_hline(yintercept = 4/n, linewidth = 1, linetype = "dashed") +
  facet_wrap(~lowplasma_01) +
  labs(title = "Cook's Distance vs BMI",
       subtitle = "Dashed line indicates threshold for high influence: 4/n",
       x = "BMI", y = "Cook's Distance", color = "Highlight",
       caption = "0 = Not low beta-plasma, 1 = Low beta-plasma") +
  scale_color_manual(values = c("0" = "blue", "1" = "red", 
                                "High leverage (v > 0.045)" = "green", 
                                "High residuals (>±2)" = "orange")) +
  theme(legend.position = "top")




ggplot(data, aes(bmi, lowplasma_01)) +
  geom_point(aes(color = factor(lowplasma_01))) +
  geom_point(data = filter(bmi_pred, abs(stddevresid) > 2), 
             aes(x = bmi, y = lowplasma_01, color = "High residuals (>±2)"),
             size = 3, shape = 21, fill = "orange", stroke = 1.2) +
  geom_smooth() +
  xlab("BMI") +
  ylab("High Plasma") +
  labs(title = "Low Plasma (=1) or Not low Plasma (=0) vs BMI", color = "Highlight") +
  scale_color_manual(values = c("High residuals (>±2)" = "orange")) +
  theme(legend.position = "top")

#Part 3
rm(list=setdiff(ls(),keep))

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
#NOTICE! This is the same model as Forward_Selection, i.e. they conclude the same
#model : Forward_Selection_Model = Stepwise_From_Forward

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

#Thus we create a new model, based on this two-factor collapsed version (reduced):
Stepwise_From_Forward_Reduced <- glm(lowplasma_01 ~ betadiet + bmi + vituse_reduced 
                                     + age + betadiet*bmi, family = "binomial", 
                                     data = new_data)
Stepwise_From_Forward_Reduced$coefficients

#Save variables
sum_1 <- summary(Stepwise_From_Forward)
sum_2 <- summary(Stepwise_From_Forward_Reduced)
deviance_1 <- sum_1$deviance
deviance_2 <- sum_2$deviance
df_1 <- sum_1$df.residual
df_2 <- sum_2$df.residual

#Calculate differences
D_diff <- deviance_2 - deviance_1
df_diff <- df_2 - df_1
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

#I create a table and remove the vertical title using rownames, just to clean it up! 
table_1e <- cbind(D_diff, df_diff, chi2_alpha, Pvalue)
rownames(table_1e) <- c(" ")
table_1e

# Task: 
# Report the name of the test, the null hypothesis, the value of the test statistic,
# its distribution when H0 is true, the P-value and the conclusion.

# 1. Name of test: Partial likelihood ratio test 
# 2. Null Hypothesis, H_0: Beta_often = Beta_Rarely. In words, that the model with 
#    one dummy variable Beta_Used is enough. 
# 3. The value of the test statistic is D = 0.34999 ~ 0.35
# 4. The distribution is Chi-squared
# 5. The P-value is P = 0.55411 ~ 0.55
# 6. The conclusion is to not reject H_0 since Pvalue > 0.05. I.e. we cannot conclude
#    that the full model is significantly better than the reduced. 

#Task:
# Present a table, Table.3(b), with the estimated β-parameters in each of the 
# different models (you will end up with at least one and at most six different models). 
# Use one row for each variable that is present in at least one of the models, 
# and one column of estimates for each of the models.

# Models
# ----------------
#Full_Logistic_Model #All variables, including calories
Logistic_Model #All variables, excluding calories, but including all interaction terms
Null_Model #No variables 
Backwards_Elimination_Model
#Forward_Selection_Model
Stepwise_From_Backward #Backwards elimination from the full model 
Stepwise_From_Forward # Forward selection from the null model
Stepwise_From_Forward_Reduced # Forward selection from the null model without the 


# Hint: 
# You can create one dataframe for each model and then join them into one dataframe:
#df1 <- data.frame(variable = names(Full_Logistic_Model$coefficients),
#                    b_model1 = Full_Logistic_Model$coefficients, row.names = NULL)
df1 <- data.frame(variable = names(Logistic_Model$coefficients),
                  b_model1 = Logistic_Model$coefficients, row.names = NULL)
df2 <- data.frame(variable = names(Null_Model$coefficients),
                  b_model2 = Null_Model$coefficients, row.names = NULL)
df3 <- data.frame(variable = names(Backwards_Elimination_Model$coefficients),
                  b_model3 = Backwards_Elimination_Model$coefficients, row.names = NULL)
df4 <- data.frame(variable = names(Stepwise_From_Forward$coefficients),
                  b_model4 = Stepwise_From_Forward$coefficients, row.names = NULL)
df5 <- data.frame(variable = names(Stepwise_From_Backward$coefficients),
                  b_model5 = Stepwise_From_Backward$coefficients, row.names = NULL)
df6 <- data.frame(variable = names(Stepwise_From_Forward_Reduced$coefficients),
                  b_model6 = Stepwise_From_Forward_Reduced$coefficients, row.names = NULL)
All_Models <- full_join(df2, df3) |> full_join(df4) |> full_join(df5) |> full_join(df6)

#Writes a table:
table_3b <- kable(All_Models, caption = "Table 3(b): Estimated β-parameters in each of the six models")
table_3b
#Exported to excel:
#write.csv(All_Models, "Table_3b_ny.csv", row.names = FALSE)


#---------------------- TO-DO: --------------------------------------
# Task:
# Comment on any interesting differences between the models.
# -------------------------------------------------------------------



# 3(c). ------------------------------------------------------------------------
# Calculate McFadden’s adjusted pseudo R2, AIC and BIC for all models from Table.3(b), 
# and indicate which model is best, according to each of these criteria.

aic <- AIC(Logistic_Model, Null_Model, Backwards_Elimination_Model, 
           Stepwise_From_Forward, Stepwise_From_Backward, Stepwise_From_Forward_Reduced)
bic <- BIC(Logistic_Model, Null_Model, Backwards_Elimination_Model, 
           Stepwise_From_Forward, Stepwise_From_Backward, Stepwise_From_Forward_Reduced)

#Create dataframe for the AIC- and BIC
collect.AICetc <- data.frame(aic, bic)

#Remove unnecessary df.1 column
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc

#Calculate Psuedo R
collect.AICetc |> mutate(
  loglik =  c(logLik(Logistic_Model)[1],
              logLik(Null_Model)[1],
              logLik(Backwards_Elimination_Model)[1],
              logLik(Stepwise_From_Forward)[1],
              logLik(Stepwise_From_Backward)[1],
              logLik(Stepwise_From_Forward_Reduced)[1])) -> collect.AICetc

#Calculate McFadden's adjusted psuedo R2

#Save loglikelihood for null model:
lnL0 <- logLik(Null_Model)[1]

collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc

#Show result
collect.AICetc

# ------------------------------------------------------------------
#TODO: INDICATE WHICH IS THE BEST MODEL BASED ON THE RESULT ^^^^^
# ------------------------------------------------------------------





# 3(d). ------------------------------------------------------------------------
# Calculate the standardized deviance residuals for the model with the best AIC 
# and the model with the best BIC, from 3(c). 



# Model with the best (lowest) AIC: Stepwise_From_Backward (AIC = 298.1199)
# Model with the best (lowest) BIC: Stepwise_From_Forward_Reduced (BIC = 322-5014)

# Standardized residuals for respective model 
res_Stepwise_From_Backward <- rstandard(Stepwise_From_Backward, type = "deviance")
res_Stepwise_From_Forward_Reduced <- rstandard(Stepwise_From_Forward_Reduced, type = "deviance")

lp_Stepwise_From_Backward <- predict(Stepwise_From_Backward, type = "link")
lp_Stepwise_From_Forward_Reduced <- predict(Stepwise_From_Forward_Reduced, type = "link")


# Task:
# Plot the standardised deviance residuals, with suitable reference lines, 
# against the linear predictors for each of the two models. Also make QQ-plots 
# for the residuals. Discuss which of the models has the best behaved residuals.

#UPDATED: Plot 1 has been updated with colour, see further below
# Plot 1: Residuals vs. Linear Predictor
par(mfrow = c(2, 2))  # 2x2 plot layout

plot(lp_Stepwise_From_Backward, res_Stepwise_From_Backward,
     main = "Model 5: Residuals vs. Linear Predictor",
     xlab = "Linear Predictor", ylab = "Standardized Deviance Residuals")
abline(h = 0, col = "red", lty = 2)

plot(lp_Stepwise_From_Forward_Reduced, res_Stepwise_From_Forward_Reduced,
     main = "Model 6: Residuals vs. Linear Predictor",
     xlab = "Linear Predictor", ylab = "Standardized Deviance Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 2: QQ-plots
qqnorm(res_Stepwise_From_Backward, main = "Model 5: QQ-Plot")
qqline(res_Stepwise_From_Backward, col = "blue")

qqnorm(res_Stepwise_From_Forward_Reduced, main = "Model 6: QQ-Plot")
qqline(res_Stepwise_From_Forward_Reduced, col = "blue")




#UPDATED: Second try for plot 1 in order to get color-coded 

#Plot 1:
Model_5_infl <- influence(Stepwise_From_Backward)
Model_6_infl <- influence(Stepwise_From_Forward_Reduced)

data_pred <- cbind(data,
                   xbeta5 = predict(Stepwise_From_Backward),
                   xbeta6 = predict(Stepwise_From_Forward_Reduced),
                   v5 = Model_5_infl$hat,
                   v6 = Model_6_infl$hat)


data_pred |> mutate(devresid5 = Model_5_infl$dev.res,
                    stddevresid5 = devresid5/sqrt(1 - v5)) -> data_pred
data_pred |> mutate(devresid6 = Model_6_infl$dev.res,
                    stddevresid6 = devresid6/sqrt(1 - v6)) -> data_pred

ggplot(data_pred, aes(x = xbeta5, 
                      y = stddevresid5, 
                      color = as.factor(lowplasma_01))) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = c("dotted", "dashed", "solid", "dashed", "dotted"),
             linewidth = 1) +
  labs(title = "Standardised deviance residuals vs linear predictor",
       x = "Linear predictor, xb", y = "Standardized deviance residuals, devstd",
       color = "Y")

ggplot(data_pred, aes(x = xbeta6, 
                      y = stddevresid6, 
                      color = as.factor(lowplasma_01))) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = c("dotted", "dashed", "solid", "dashed", "dotted"),
             linewidth = 1) +
  labs(title = "Standardised deviance residuals vs linear predictor",
       x = "Linear predictor, xb", y = "Standardized deviance residuals, devstd",
       color = "Y")

#Part4
keep <- c("data","Stepwise_From_Backward","Stepwise_From_Forward_Reduced")
rm(list=setdiff(ls(),keep))

#4a
model_0_glm <- glm(lowplasma_hl ~ 1, family = "binomial", data = data)
data |> mutate(pred_for = predict(Stepwise_From_Backward, type = "response"),
               pred_back = predict(Stepwise_From_Forward_Reduced, type = "response"),
               yhat_for = factor(pred_for > 0.5,
                                 levels = c(FALSE, TRUE),
                                 labels = c("high", "low")),
               yhat_back = factor(pred_back > 0.5,
                                  levels = c(FALSE, TRUE),
                                  labels = c("high", "low")),
               p_0 = predict(model_0_glm, type = "response")) -> low_plasma_pred

cm_back <- confusionMatrix(
  data = low_plasma_pred$yhat_back,
  reference = low_plasma_pred$lowplasma_hl,
  positive = "low"
)

cm_for <- confusionMatrix(
  data = low_plasma_pred$yhat_for,
  reference = low_plasma_pred$lowplasma_hl,
  positive = "low"
)

cm_back
cm_for

#4b

roc_0 <- roc(lowplasma_hl ~ p_0, data = low_plasma_pred, levels = c("high", "low"))
roc_back <- roc(lowplasma_hl ~ pred_back, data = low_plasma_pred, levels = c("high", "low"))
roc_for <- roc(lowplasma_hl ~ pred_for, data = low_plasma_pred, levels = c("high", "low"))

ggroc(list(null = roc_0, A = roc_back, B = roc_for)) +
  coord_fixed() +
  labs(title = "ROC-curves for models and the null model")

auc(roc_back)
ci(roc_back)
auc(roc_for)
ci(roc_for)
roc.test(roc_back,roc_for)


cutback <- coords(roc_back,"best",best.method="closest.topleft")$threshold
cutfor <- coords(roc_for,"best",best.method="closest.topleft")$threshold

cutback
cutfor
data |> mutate(pred_for = predict(Stepwise_From_Backward, type = "response"),
               pred_back = predict(Stepwise_From_Forward_Reduced, type = "response"),
               yhat_for = factor(pred_for > cutfor,
                                 levels = c(FALSE, TRUE),
                                 labels = c("high", "low")),
               yhat_back = factor(pred_back > cutback,
                                  levels = c(FALSE, TRUE),
                                  labels = c("high", "low"))) -> low_plasma_pred
cm_back2 <- confusionMatrix(
  data = low_plasma_pred$yhat_back,
  reference = low_plasma_pred$lowplasma_hl,
  positive = "low"
)

cm_for2 <- confusionMatrix(
  data = low_plasma_pred$yhat_for,
  reference = low_plasma_pred$lowplasma_hl,
  positive = "low"
)

cm_back2
cm_for2

Logistic_model <- Stepwise_From_Forward_Reduced
summary(Logistic_model)
bhat <- Logistic_model$coefficients
ci.beta <- confint(Logistic_model)
#Coeffiencts
cbind(b = bhat, ci.beta, `exp(b)` = exp(bhat), exp(ci.beta)) |> round(digits = 2)
#OR
# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(bhat)` = or, ci.or) |> round(digits = 2)

