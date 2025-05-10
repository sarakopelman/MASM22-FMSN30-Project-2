#Part 2. 
source("Code/Part-1.R")
keep <- c("data")
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

