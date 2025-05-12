library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
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

# ------------------------------------------------------------------------------
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


