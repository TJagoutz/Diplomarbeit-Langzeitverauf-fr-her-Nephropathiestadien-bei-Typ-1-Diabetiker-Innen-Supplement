# Alterskohortenanalyse Überlebensunterschiede

## Prompt 1

ja bitte 
ich gebe dir die variablenstruktur miener bisheigen survival+cox funktion, damit du die bennunnungen kennst
da_daten <- subset_minusCKD

list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
 # merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen = merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen
)

list_var <- list(
  "Hyperfiltration150_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "survival_time_age",
  "NE_surv_all_a_tim",
  "survival_time_endpoint"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "Hyperfiltration150_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_a_",
  "Geschlecht_1_m",
  "Alter_a_einschluss",
  "ProteinJa_1"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration120_1_ja=Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_a_" = "Diabetesdauer",
    "Geschlecht_1_m" = "Geschlecht, 1=M",
    "Alter_a_einschluss" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja"
  )

## Prompt 2

wenn das klappt wär es super, ich gebe dier die gesamte funktion, bitte hebe alle änderungen hervor:
#3.3)SEKUNDÄRER ENDPUNKT
##SURVIVAL ANALYSE##
da_daten <- subset_minusCKD

list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
 # merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen = merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen
)

list_var <- list(
  "Hyperfiltration150_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "survival_time_age",
  "NE_surv_all_a_tim",
  "survival_time_endpoint"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "Hyperfiltration150_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_a_",
  "Geschlecht_1_m",
  "Alter_a_einschluss",
  "ProteinJa_1"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration120_1_ja=Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_a_" = "Diabetesdauer",
    "Geschlecht_1_m" = "Geschlecht, 1=M",
    "Alter_a_einschluss" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja"
  )
  
  
  for (dataset_name in names(list_daten)) {
    da_daten <- list_daten[[dataset_name]]
    
    #print(paste("🔍 Checking dataset:", dataset_name))
    #print(names(da_daten))  # Show dataset structure
    
    for (var in list_var) {
      if (!(var %in% names(da_daten))) {
        warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
        next
      }
      
      if (!is.factor(da_daten[[var]])) {
        da_daten[[var]] <- as.factor(da_daten[[var]])
        message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
      }
      
      for (i in seq_along(list_surv_time)) {
        time_var <- unlist(list_surv_time)[i]# ✅ Convert list to vector
        event_var <- unlist(list_endpoint)[i]  # ✅ Convert list to vector
        
        # 🚨 **Debugging: Ensure `time_var` exists before proceeding** 🚨
        #print(paste("🔄 Checking time_var:", time_var, "| event_var:", event_var))
        #print(ls())  # ✅ Check all variables in scope
        
        stopifnot(!is.null(time_var), !is.null(event_var))  # ✅ Catch NULL values early
        
        if (!(time_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        }
        if (!(event_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        }
        
        #print(paste("🚀 Processing:", dataset_name, time_var, event_var, var))
        
        required_cols <- c(time_var, event_var, var, unlist(list_confounder))
        missing_cols <- setdiff(required_cols, names(da_daten))
        
        if (length(missing_cols) > 0) {
          warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
          next
        }
        
        #print("✅ Data Preview (Before Filtering):")
        #print(head(da_daten[ , required_cols, drop = FALSE]))
        #print(paste("TESTTIM", da_daten[[time_var]],"TESTTIMENDE"))
        
        #da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
        #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))

        #print("🚀 DEBUG: Checking input before Kaplan-Meier")
        #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
        #print("✅ Column Names in Dataset:")
        #print(names(da_daten))
        #print("✅ First 6 Rows:")
        #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
        #if (!(time_var %in% names(da_daten))) {
        #  stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        #}
        #if (!(event_var %in% names(da_daten))) {
        #  stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        #}
        #if (!(var %in% names(da_daten))) {
        #stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
        #}
        #print(paste("Type of time_var:", class(da_daten[[time_var]])))
        #print(paste("Type of event_var:", class(da_daten[[event_var]])))
        #if (!is.factor(da_daten[[var]])) {
        #  print(paste("⚠️ Converting", var, "to factor"))}
        
                # ---- 1️⃣ Kaplan-Meier Plot (UNFILTERED DATA) ----
        
         suppressWarnings({
          km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var, "==1) ~", var)
          )
          
          #print("🚀 DEBUG: Kaplan-Meier Formula")
          #print(km_formula)  # Check if the formula is correctly generated
          
          km_fit <- tryCatch(
            surv_fit(km_formula, data = da_daten),  
            error = function(e) {
              print('ERRORRRRRRRRRR!!!!!!!!!!')
              warning(paste("❌ Kaplan-Meier fit failed:", e$message))
              return(NULL)
            }
          )
          
          
          if (!is.null(km_fit)) {
            print("✅ Kaplan-Meier Model Summary:")
            #print(summary(km_fit))  # Ensure km_fit is not NULL
          } else {
            print("❌ Kaplan-Meier model failed")
          }
        })
        
        # ---- 2️⃣ Log-Rank Test (auf denselben Daten wie KM) ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(da_daten[[time_var]]), 
                        as.numeric(da_daten[[event_var]])) ~ da_daten[[var]]),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
        p_val_logrank <- if (!is.null(logrank_test)) {
          signif(1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1), 3)
        } else {
          NA
        }                       
         if (!is.null(km_fit)) {
           print("🚀 DEBUG: Plotting Kaplan-Meier")
           
           
           km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
               conf.int = TRUE,
               pval =  if (!is.na(p_val_logrank)) paste0("Logrank p = ", p_val_logrank) else FALSE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               #ncensor.plot = TRUE,
               #risk.table = TRUE,  #optional
               cumevents = TRUE,  #optional
               cumevents.height = 0.2,  #optional
               ggtheme = theme_bw()
             ),
             error = function(e) {
               warning(paste("❌ Kaplan-Meier plot failed:", e$message))
               return(NULL)
             }
           )
           
           if (is.null(km_plot)) {
             print("❌ ggplot konnte nicht erstellt werden.")
           } else {
             print("✅ ggplot erfolgreich erstellt.")
           }
         } else {
           print("❌ km_fit ist NULL, daher kein Plot möglich.")
         }
        
        #print("📊 Kaplan-Meier Summary:")
        #if (!is.null(km_fit)) print(summary(km_fit))
        #print("Kaplan-Meier Summary done")
    
        # ---- 2️⃣ Filter Data for Cox Models (REMOVE NA) ----
        daten_filtered <- da_daten[complete.cases(da_daten[ , required_cols]), ]
        
        #print("✅ Data Preview (After Filtering):")
        #print(head(daten_filtered))
        #print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        #logrank_test <- tryCatch(
          #survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                       # as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
         # error = function(e) {
            #warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            #return(NULL)
        #  }
       # )
        
        # ---- 4️⃣ Unadjusted Cox Model ---
        if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        unadjusted_cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var)
        )
        
        unadjusted_cox_model <- tryCatch(
          coxph(unadjusted_cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        #unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
        
        # ---- 5️⃣ Adjusted Cox Model ----
        if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
        )
        
        confounders <- setdiff(unlist(list_confounder), var)
        
        
        adjusted_cox_model <- tryCatch(
          coxph(cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
            message("💥 Error message: ", e$message)
            message("📊 Dataset size: ", nrow(daten_filtered))
            message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
            return(NULL)
          }
        )
        
        #adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
        
        # ---- 6️⃣ Cox Forest Plot ----
        #print(("GGFOREST TEST"))
        #print(names(adjusted_cox_model$coefficients))
        #print(names(daten_filtered))
        #print(setdiff(list_confounder, names(daten_filtered)))
        #print("GGFORTES TEST ENDE")
        
        #mutate daten in logarythmische form um extreme values zu minimieren
       #daten_filtered <- daten_filtered %>%
       #mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  
        
        if (!is.null(adjusted_cox_model)) {
          cox_summary <- summary(adjusted_cox_model)
          hr_values <- exp(cox_summary$coefficients[, "coef"])
          ci_lower <- exp(cox_summary$conf.int[, "lower .95"])
          ci_upper <- exp(cox_summary$conf.int[, "upper .95"])
          
          if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
            warning("⚠️ Skipping forest plot due to extreme HR values")
            forest_plot <- NULL
          } else {
            # 🏷️ Add labels to data before plotting
            model_data <- model.frame(adjusted_cox_model)
            labelled::var_label(model_data) <- label_map[names(model_data)]
            
            adjusted_cox_model$model <- model_data
            # 📊 Create forest plot using forestmodel
            forest_plot <- tryCatch(
              forest_model(adjusted_cox_model),
              error = function(e) {
                warning("⚠️ forest_model() failed for ", dataset_name, " - ", var, " with error: ", e$message)
                return(NULL)
              }
            )
          }
        } else {
          forest_plot <- NULL
        }
        
        # ---- 7️⃣ Store Results ----
        result_key <- paste(dataset_name, var, event_var, sep = "_")
        
        results[[result_key]] <- list(
          kaplan_meier_plot = km_plot,
          logrank_test = logrank_test,
          unadjusted_cox_model = summary(unadjusted_cox_model),
          unadjusted_cox_model_raw = unadjusted_cox_model,
          adjusted_cox_model = summary(adjusted_cox_model),
          adjusted_cox_model_raw = adjusted_cox_model,                  
          cox_forest_plot = forest_plot
        )
        
        print(paste("✅ Successfully stored results for:", result_key))
      }
    }
  }
  
  print(paste("Namen der Ergebnisse:", names(results)))
  return(results)
}


results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)



# ## Results Abrufen sekundärer Endpunkt  ---------------------------------


# Access Kaplan-Meier Plot
print(results[["subset_minusCKD_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1"]]$kaplan_meier_plot)

# Access Log-Rank Test
print(results[["subset_minsuCKD_Hyperfiltration140_1_ja_tot_1"]]$logrank_test)

# Access Unadjusted Cox Model
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$unadjusted_cox_model)

# Access Adjusted Cox Model
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$adjusted_cox_model)

# Access Cox Forest Plot
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$cox_forest_plot)

## Prompt 3

also reicht es wenn ich den Altersgruppen block ind er Analyse und beim results spericher reinkjopiere?

## Prompt 4

# Altersgruppen (Terzile) bilden
da_daten$Altersgruppe <- cut(
  da_daten$Alter_a_einschluss,
  breaks = quantile(da_daten$Alter_a_einschluss, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("jung", "mittel", "alt")
)
was bedeuten die labels hier?

## Prompt 5

aber die gruppen muss ich mit dieser funktion schon vorher definieren oder übersehe ich da etwas ind der großen Funktion?

## Prompt 6

ja dann bitte irgendwie ausgeben welche Jahrgänge in welcher gruppe sind

## Prompt 7

kannst du mir den ganzen codeblock in der gesamtheit noch einmal ausgeben bitte

## Prompt 8

bitte die ganze analysefuinktion nocheinmal

## Prompt 9

wie rufe ich den agegroup table ab?

## Prompt 10

Fehler in make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1") : 
  Argument "list_daten" fehlt (ohne Standardwert)

## Prompt 11

beide Versionen geben:> make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1")
Fehler in make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1") : 
  ❌ Dataset nicht in list_daten gefunden

## Prompt 12

Fehler in make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1") : 
  ❌ Altersgruppe nicht im Datensatz gefunden

## Prompt 13

> make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1")
Fehler in make_agegroup_table(results, "da_daten_HyperfiltrationMedian_1_ja_endpoint_reached_ja_1") : 
  Argument "list_daten" fehlt (ohne Standardwert)

kann es sein dass wir uns im kreis drehen?

## Prompt 14

aber was ich gerne testen würde ist ob die abhängigkeit den Endpunkt zu erreichen von der Hyperfiltration in den Altersgruppen unterschiedlich stark ist

## Prompt 15

list_var <- c("endpoint_reached_ja_1",
              "tot_1")  # Dependent variables
Hyperfiltration list <- c("HyperfiltrationMedian_1_ja", "Hyperfiltration120_1_ja","Hyperfiltration130_1_ja","Hyperfiltration140_1_ja","Hyperfiltration150_1_ja")
confunder List (
                    "RRsyst_completed_mean",
                    "highest_measured_hba1c_naisna_completed_mean",
                    "BMI_completed_mean",
                    "UN_completed_mean",
                    "Diabetesdauer_a_",
                    "Geschlecht_1_m", 
                    "Alter_a_einschluss",
                    "ProteinJa_1")
logreg_fun <- function(datensatz, list_var, confunder_list) {

  
  for (i in seq_along(list_var)) {
    variable <- list_var[[i]]  # Correct extraction of variable name
    
    # **0-model (intercept-only)**
    model0 <- glm(as.formula(paste(variable, "~ 1")), data = datensatz, family = binomial())
    summary0 <- summary(model0)
    
    # **Full model (with confounders)**
    model1_formula <- paste(variable, "~", paste(confunder_list, collapse = " + "))
    model1 <- glm(as.formula(model1_formula), data = datensatz, family = binomial())
    summary1 <- summary(model1)
    
    # **Odds Ratio (OR) Calculation**
    oR <- exp(cbind(OR = coefficients(model1), confint(model1)))  # Correct calculation
    
    # **Omnibus test (likelihood ratio test)**
    modelchi <- model1$null.deviance - model1$deviance
    chifd <- model1$df.null - model1$df.residual
    chisqp <- pchisq(modelchi, chifd, lower.tail = FALSE)  # Corrected p-value calculation
    
    # **Store results in a structured list**
    logreg_results[[variable]] <- list(
      model0 = summary0,
      model1 = summary1,
      omnibus_test = list(
        chi_square = modelchi,
        df = chifd,
        p_value = chisqp
      ),
      oR = oR  # ✅ Correct placement of the OR calculation
    )
  }
  
  return(logreg_results)
}
das ist meine bisherige Funktion für adusted logreg analysen, kannst du mir etwas ähnliches mit interaktionsterm bauen )und sollte man die anderen confunder berücksichtigen?(ev. auich bei der cox analyse?)

## Prompt 16

🔹 1. Sollen Confounder berücksichtigt werden?

👉 Ja, unbedingt – sowohl in der Logistik- als auch in der Cox-Analyse.
Denn:

Wenn du nur Altersgruppe * Hyperfiltration modellierst, riskierst du, dass der beobachtete Interaktionseffekt durch Confounder verzerrt ist (z. B. Blutdruck, Diabetesdauer etc.).

Mit Adjustierung prüfst du, ob die Interaktion über die Confounder hinaus bestehen bleibt.

eine cox Funktion haben wir ja schon in meine surv analyse funktion eingebaut:
  ### 3️⃣ Altersgruppen-Analysen
        if ("Altersgruppe" %in% names(daten_filtered)) {
          chi_table <- table(daten_filtered$Altersgruppe, daten_filtered[[event_var]])
          chi_test <- tryCatch(chisq.test(chi_table), error = function(e) NULL)
          
          # Ereignisse pro Altersgruppe
          events <- tapply(daten_filtered[[event_var]], daten_filtered$Altersgruppe, sum, na.rm = TRUE)
          agegroup_info$ereignisse <- as.numeric(events[agegroup_info$Gruppe])
          
          logit_model <- tryCatch(
            glm(as.formula(paste(event_var, "~ Altersgruppe *", var)), data = daten_filtered, family = binomial),
            error = function(e) NULL
          )
          
          cox_interaction <- tryCatch(
            coxph(as.formula(paste("Surv(", time_var, ",", event_var, ") ~ Altersgruppe *", var)), data = daten_filtered),
            error = function(e) NULL
          )
        } else {
          chi_test <- NULL; logit_model <- NULL; cox_interaction <- NULL
        }
aber die confunder werden heir nicht berücksichtigt (oben stet noch list_confounder <- list(
  "HyperfiltrationMedian_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_a_",
  "Geschlecht_1_m",
  "Alter_a_einschluss",
  "ProteinJa_1"
))

## Prompt 17

logreg_interaction_fun <- function(datensatz, list_var, hyperfiltration_list, confounder_list) {
  
  results <- list()
  
  # sicherstellen, dass Altersgruppe als Faktor vorliegt
  if (!"Altersgruppe" %in% names(datensatz)) {
    stop("❌ Altersgruppe nicht im Datensatz gefunden. Bitte vorher definieren.")
  }
  datensatz$Altersgruppe <- as.factor(datensatz$Altersgruppe)
die altersgruppen sind noch nicht definiert, bitte mach das wie in der obigen funktino inkl feedback über das alter der jeweiligen gruppe

## Prompt 18

Fehler in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  Kontraste können nur auf Faktoren mit 2 oder mehr Stufen angewendet werden

## Prompt 19

kannst du mir genauer zeigen wie ich das bugfixxen kann

## Prompt 20

kannst du mir genauer zeigen wie ich das bugfixxen kann:Fehler in contrasts<-(*tmp*, value = contr.funs[1 + isOF[nn]]) : Kontraste können nur auf Faktoren mit 2 oder mehr Stufen angewendet werden

kannst du mir genauer zeigen wie ich das bugfixxen kann

## Prompt 21

kannst du mir dei ganze funktion mit dem fix ausgeben,
vor der funktion definiere ich:

list_var <- c("endpoint_reached_ja_1",
            "tot_1")  # Dependent variables

confunder_list <- c("RRsyst_completed_mean",
                    "highest_measured_hba1c_naisna_completed_mean",
                    "BMI_completed_mean",
                    "UN_completed_mean",
                    "Diabetesdauer_a_",
                    "Geschlecht_1_m", 
                    "Alter_a_einschluss",
                    "ProteinJa_1"
)  # Independent variables

hyperfiltration_list = c("HyperfiltrationMedian_1_ja","Hyperfiltration120_1_ja","Hyperfiltration130_1_ja","Hyperfiltration140_1_ja", "Hyperfiltration150_1_ja")

## Prompt 22

aber es hat mit dem gleichen datensatz schon funktioniert, ich wollte dann die variablen ändern (auf hyperfiltration 120) hab das aber rücgängig gemacht, als es begonnen hat NULL auszugebenm

## Prompt 23

Gibt es eine gute möglichkeit die Ergebnisse solche  Interaktionen/interaktionstests zu visualisieren
