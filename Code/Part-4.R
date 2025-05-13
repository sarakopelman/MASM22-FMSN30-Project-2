#Part 4. 
library(caret)
library(pROC)
source("Code/Part-3.R")
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
