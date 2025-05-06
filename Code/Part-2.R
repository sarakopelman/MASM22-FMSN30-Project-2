#Part 2. 
source("Code/Part-1.R")
# 2(a). Plot lowplasma_01 against bmi and add a moving average. Does the probability of having
# low plasma beta-carotene increase or decrease with BMI? Does this agree with what you
# found in Project 1?

ggplot(data, aes(vituse, lowplasma_01)) +
  geom_point() +
  xlab("") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") 


# Fit a logistic regression for lowplasma_01 (or lowplasma_hl) as a function of BMI. This
# will be referred to as Model.2(a). Present the β-estimates, the eβ
# -estimates and their profile
# likelihood based 95 % confidence intervals.
# Add the predicted probabilities, and a 95 % confidence interval, to the plot and comment on
# the result.
# Describe how, according to the model, the odds of having low plasma β-carotene changes
# when BMI is
# (i) increased by 1 unit,
# (ii) decreased by 1 unit,
# (iii) decreased by 10 units.
# Also calculate 95 % confidence intervals for these odds ratios.
