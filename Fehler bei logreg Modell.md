# Fehler bei logreg Modell

## Prompt 1

> #3.2.3)///logreg (explorativ)///
> ##LOGRANK TEST##
> list_var <- c("endpoint_reached_ja_1")  # Dependent variables
> confunder_list <- c("HyperfiltrationMedian_1_ja",
+                     "RRsyst_completed_mean",
+                     "highest_measured_hba1c_naisna_completed_mean",
+                     "BMI_completed_mean",
+                     "UN_completed_mean",
+                     "Diabetesdauer_a_",
+                     "Geschlecht_1_m", "Alter_a_einschluss",
+                     "ProteinJa_1"
+                     )  # Independent variables
> logreg_fun <- function(datensatz, list_var, confunder_list) {
+ 
+   
+   for (i in seq_along(list_var)) {
+     variable <- list_var[[i]]  # Correct extraction of variable name
+     
+     # **0-model (intercept-only)**
+     model0 <- glm(as.formula(paste(variable, "~ 1")), data = datensatz, family = binomial())
+     summary0 <- summary(model0)
+     
+     # **Full model (with confounders)**
+     model1_formula <- paste(variable, "~", paste(confunder_list, collapse = " + "))
+     model1 <- glm(as.formula(model1_formula), data = datensatz, family = binomial())
+     summary1 <- summary(model1)
+     
+     # **Odds Ratio (OR) Calculation**
+     oR <- exp(cbind(OR = coefficients(model1), confint(model1)))  # Correct calculation
+     
+     # **Omnibus test (likelihood ratio test)**
+     modelchi <- model1$null.deviance - model1$deviance
+     chifd <- model1$df.null - model1$df.residual
+     chisqp <- pchisq(modelchi, chifd, lower.tail = FALSE)  # Corrected p-value calculation
+     
+     # **Store results in a structured list**
+     logreg_results[[variable]] <- list(
+       model0 = summary0,
+       model1 = summary1,
+       omnibus_test = list(
+         chi_square = modelchi,
+         df = chifd,
+         p_value = chisqp
+       ),
+       oR = oR  # ✅ Correct placement of the OR calculation
+     )
+   }
+   
+   return(logreg_results)
+ }
> datensatz= datensatz
> logreg_results <- logreg_fun(datensatz, list_var, confunder_list)
Fehler in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  Kontraste können nur auf Faktoren mit 2 oder mehr Stufen angewendet werden
