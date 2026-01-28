# Fehlerbehebung Cox-Analyse

## Prompt 1

I have this:
list_daten <- list(
  da_daten = da_daten
  #,subset_minusCKD = subset_minusCKD 
)

list_var <- list(
  "Hyperfiltration130_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "Alter_beitotwenntot_a",#_rounded
  "NE_surv_all_a_tim",
  "time_to_endpoint_a_tim"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "Hyperfiltration130_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_totwenntot_a_tim",
  "Geschlecht_1_m...29",
  "Alter_beitotwenntot_a",
  "ProteinJa_1",
  "NBZ_completed_mean",
  "ppBZ_completed_mean"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
    "Geschlecht_1_m...29" = "Geschlecht, 1=M",
    "Alter_beitotwenntot_a" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja",
    "NBZ_completed_mean" = "Nüchternblutzucker",
    "ppBZ_completed_mean" = "postprandialer Blutzucker"
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
        
       # da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
        #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))

        #print("🚀 DEBUG: Checking input before Kaplan-Meier")
        #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
        #print("✅ Column Names in Dataset:")
        #print(names(da_daten))
        #print("✅ First 6 Rows:")
        #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
        #if (!(time_var %in% names(da_daten))) {
          #stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        #}
        #if (!(event_var %in% names(da_daten))) {
         # stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        #}
        #if (!(var %in% names(da_daten))) {
        #  stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
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
        
                                
         if (!is.null(km_fit)) {
           #print("🚀 DEBUG: Plotting Kaplan-Meier")
           
           km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
               conf.int = TRUE,
               pval = TRUE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               ncensor.plot = TRUE,
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
        print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                        as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
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
          cox_forest_plot = forest_plot, 
        )
        
        print(paste("✅ Successfully stored results for:", result_key))
      }
    }
  }
  
  print(paste("Namen der Ergebnisse:", names(results)))
  return(results)
}


results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)

# Access Kaplan-Meier Plot
print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$kaplan_meier_plot)

# Access Log-Rank Test
print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$logrank_test)

# Access Unadjusted Cox Model
print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$unadjusted_cox_model)

# Access Adjusted Cox Model
print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$adjusted_cox_model)

# Access Cox Forest Plot
print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$cox_forest_plot)



#OTHER ENDPOINT 

# Access Kaplan-Meier Plot
print(results[["da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"]]$kaplan_meier_plot)

# Access Log-Rank Test
print(results[["da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"]]$logrank_test)

# Access Unadjusted Cox Model
print(results[["da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"]]$unadjusted_cox_model)

# Access Adjusted Cox Model
print(results[["da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"]]$adjusted_cox_model)

# Access Cox Forest Plot
print(results[["da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"]]$cox_forest_plot)
 and it worked untill now but now i get this:
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten



[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
Fehler in list(kaplan_meier_plot = km_plot, logrank_test = logrank_test,  : 
  argument 8 is empty
Zusätzlich: Warnmeldungen:
1: In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite

However Hyperfiltration 130 does exist

## Prompt 2

thats my forest plot for the end point dying, 
+           forest_plot <- NULL
+         }
+         
+         # ---- 7️⃣ Store Results ----
+         result_key <- paste(dataset_name, var, event_var, sep = "_")
+         
+         results[[result_key]] <- list(
+           kaplan_meier_plot = km_plot,
+           logrank_test = logrank_test,
+           unadjusted_cox_model = summary(unadjusted_cox_model),
+           unadjusted_cox_model_raw = unadjusted_cox_model,
+           adjusted_cox_model = summary(adjusted_cox_model),
+           adjusted_cox_model_raw = adjusted_cox_model,                  
+           cox_forest_plot = forest_plot, 
+         )
+         
+         print(paste("✅ Successfully stored results for:", result_key))
+       }
+     }
+   }
+   
+   print(paste("Namen der Ergebnisse:", names(results)))
+   return(results)
+ }
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten

[1] "ERRORRRRRRRRRR!!!!!!!!!!"
[1] "❌ Kaplan-Meier model failed"
[1] "❌ km_fit ist NULL, daher kein Plot möglich."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
💥 Error message: konnte Funktion "coxph" nicht finden
📊 Dataset size: 296
📉 Events: 79
Fehler in analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  : 
  Objekt 'km_plot' nicht gefunden
Zusätzlich: Warnmeldungen:
1: In value[[3L]](cond) :
  ⚠️ Log-Rank Test failed for da_daten - Hyperfiltration130_1_ja with error: konnte Funktion "survdiff" nicht finden
2: In value[[3L]](cond) :
  ⚠️ Unadjusted Cox Model failed for da_daten - Hyperfiltration130_1_ja with error: konnte Funktion "coxph" nicht finden
3: In value[[3L]](cond) :
  ⚠️ Adjusted Cox Model failed for da_daten - Hyperfiltration130_1_ja
>   #1.2)LIBRARY
> library(usethis)
Warnmeldung:
Paket ‘usethis’ wurde unter R Version 4.4.3 erstellt 
> library(readxl)
Warnmeldung:
Paket ‘readxl’ wurde unter R Version 4.4.3 erstellt 
> library(lubridate)

Attache Paket: ‘lubridate’

Die folgenden Objekte sind maskiert von ‘package:base’:

    date, intersect, setdiff, union

Warnmeldung:
Paket ‘lubridate’ wurde unter R Version 4.4.3 erstellt 
> library(tibble)
> library(psych)
Warnmeldung:
Paket ‘psych’ wurde unter R Version 4.4.3 erstellt 
> library(tidyverse)
── Attaching core tidyverse packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr   1.1.4     ✔ readr   2.1.5
✔ forcats 1.0.0     ✔ stringr 1.5.1
✔ ggplot2 3.5.1     ✔ tidyr   1.3.1
✔ purrr   1.0.2     
── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ ggplot2::%+%()   masks psych::%+%()
✖ ggplot2::alpha() masks psych::alpha()
✖ dplyr::filter()  masks stats::filter()
✖ dplyr::lag()     masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
Warnmeldungen:
1: Paket ‘tidyverse’ wurde unter R Version 4.4.2 erstellt 
2: Paket ‘tidyr’ wurde unter R Version 4.4.2 erstellt 
> library(ggplot2)
> library(devtools)
> library(dplyr)
> library(rcompanion)

Attache Paket: ‘rcompanion’

Das folgende Objekt ist maskiert ‘package:psych’:

    phi

Warnmeldung:
Paket ‘rcompanion’ wurde unter R Version 4.4.3 erstellt 
> library(survival)
Warnmeldung:
Paket ‘survival’ wurde unter R Version 4.4.2 erstellt 
> library(tidyr)
> library(survminer)
Lade nötiges Paket: ggpubr

Attache Paket: ‘survminer’

Das folgende Objekt ist maskiert ‘package:survival’:

    myeloma

Warnmeldungen:
1: Paket ‘survminer’ wurde unter R Version 4.4.3 erstellt 
2: Paket ‘ggpubr’ wurde unter R Version 4.4.3 erstellt 
> library(openxlsx2)

Attache Paket: ‘openxlsx2’

Das folgende Objekt ist maskiert ‘package:readxl’:

    read_xlsx

Warnmeldung:
Paket ‘openxlsx2’ wurde unter R Version 4.4.3 erstellt 
> library(forestmodel)
Warnmeldung:
Paket ‘forestmodel’ wurde unter R Version 4.4.3 erstellt 
> library(labelled)
Warnmeldung:
Paket ‘labelled’ wurde unter R Version 4.4.3 erstellt 
> library(broom)
> library(flextable)

Attache Paket: ‘flextable’

Die folgenden Objekte sind maskiert von ‘package:ggpubr’:

    border, font, rotate

Das folgende Objekt ist maskiert ‘package:purrr’:

    compose

Warnmeldung:
Paket ‘flextable’ wurde unter R Version 4.4.3 erstellt 
> library(officer)

Attache Paket: ‘officer’

Das folgende Objekt ist maskiert ‘package:openxlsx2’:

    read_xlsx

Das folgende Objekt ist maskiert ‘package:readxl’:

    read_xlsx

Warnmeldung:
Paket ‘officer’ wurde unter R Version 4.4.3 erstellt 
> library(purrr)
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten

[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
Fehler in list(kaplan_meier_plot = km_plot, logrank_test = logrank_test,  : 
  argument 8 is empty
Zusätzlich: Warnmeldungen:
1: In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
> # Access Kaplan-Meier Plot
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$kaplan_meier_plot)
> # Access Kaplan-Meier Plot
> print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$kaplan_meier_plot)
NULL
> # Access Log-Rank Test
> print(results[["tot_1"]]$logrank_test)
NULL
> # Access Log-Rank Test
> print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$logrank_test)
NULL
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten

[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
Fehler in list(kaplan_meier_plot = km_plot, logrank_test = logrank_test,  : 
  argument 8 is empty
Zusätzlich: Warnmeldungen:
1: In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
> list_var <- list(
+   "Hyperfiltration130_1_ja"
+ )
> #list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
> list_surv_time <- list(
+   "Alter_beitotwenntot_a",#_rounded
+   "NE_surv_all_a_tim",
+   "time_to_endpoint_a_tim"
+ )
> list_endpoint <- list(
+   "tot_1",
+   "NE_1_ja",
+   "endpoint_reached_ja_1"
+ )
> list_confounder <- list(
+   "Hyperfiltration130_1_ja",
+   "RRsyst_completed_mean",
+   "highest_measured_hba1c_naisna_completed_mean",
+   "BMI_completed_mean",
+   "UN_completed_mean",
+   "Diabetesdauer_totwenntot_a_tim",
+   "Geschlecht_1_m...29",
+   "Alter_beitotwenntot_a",
+   "ProteinJa_1",
+   "NBZ_completed_mean",
+   "ppBZ_completed_mean"
+ )
> #survival_analysing, commands used for debbuging left inside as comments for easy reuse
> analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
+   
+   results <- list()
+   
+   label_map <- c(
+     "Hyperfiltration140_1_ja" = "Hyperfiltration",
+     "Hyperfiltration130_1_ja"= "Hyperfiltration",
+     "Hyperfiltration150_1_ja"= "Hyperfiltration",
+     "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
+     "RRsyst_completed_mean" = "systolischer Blutdruck",
+     "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
+     "BMI_completed_mean" = "BMI",
+     "UN_completed_mean" = "BUN",
+     "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
+     "Geschlecht_1_m...29" = "Geschlecht, 1=M",
+     "Alter_beitotwenntot_a" = "Alter",
+     "ProteinJa_1" = "Proteinurie, 1=ja",
+     "NBZ_completed_mean" = "Nüchternblutzucker",
+     "ppBZ_completed_mean" = "postprandialer Blutzucker"
+   )
+   
+   
+   for (dataset_name in names(list_daten)) {
+     da_daten <- list_daten[[dataset_name]]
+     
+     #print(paste("🔍 Checking dataset:", dataset_name))
+     #print(names(da_daten))  # Show dataset structure
+     
+     for (var in list_var) {
+       if (!(var %in% names(da_daten))) {
+         warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
+         next
+       }
+       
+       if (!is.factor(da_daten[[var]])) {
+         da_daten[[var]] <- as.factor(da_daten[[var]])
+         message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
+       }
+       
+       for (i in seq_along(list_surv_time)) {
+         time_var <- unlist(list_surv_time)[i]# ✅ Convert list to vector
+         event_var <- unlist(list_endpoint)[i]  # ✅ Convert list to vector
+         
+         # 🚨 **Debugging: Ensure `time_var` exists before proceeding** 🚨
+         #print(paste("🔄 Checking time_var:", time_var, "| event_var:", event_var))
+         #print(ls())  # ✅ Check all variables in scope
+         
+         stopifnot(!is.null(time_var), !is.null(event_var))  # ✅ Catch NULL values early
+         
+         if (!(time_var %in% names(da_daten))) {
+           stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
+         }
+         if (!(event_var %in% names(da_daten))) {
+           stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
+         }
+         
+         #print(paste("🚀 Processing:", dataset_name, time_var, event_var, var))
+         
+         required_cols <- c(time_var, event_var, var, unlist(list_confounder))
+         missing_cols <- setdiff(required_cols, names(da_daten))
+         
+         if (length(missing_cols) > 0) {
+           warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
+           next
+         }
+         
+         #print("✅ Data Preview (Before Filtering):")
+         #print(head(da_daten[ , required_cols, drop = FALSE]))
+         #print(paste("TESTTIM", da_daten[[time_var]],"TESTTIMENDE"))
+         
+        # da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
+         #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))
+ 
+         #print("🚀 DEBUG: Checking input before Kaplan-Meier")
+         #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
+         #print("✅ Column Names in Dataset:")
+         #print(names(da_daten))
+         #print("✅ First 6 Rows:")
+         #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
+         #if (!(time_var %in% names(da_daten))) {
+           #stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
+         #}
+         #if (!(event_var %in% names(da_daten))) {
+          # stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
+         #}
+         #if (!(var %in% names(da_daten))) {
+         #  stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
+         #}
+         #print(paste("Type of time_var:", class(da_daten[[time_var]])))
+         #print(paste("Type of event_var:", class(da_daten[[event_var]])))
+         #if (!is.factor(da_daten[[var]])) {
+         #  print(paste("⚠️ Converting", var, "to factor"))}
+         
+                 # ---- 1️⃣ Kaplan-Meier Plot (UNFILTERED DATA) ----
+         
+          suppressWarnings({
+           km_formula <- as.formula(
+             paste("Surv(", time_var, ",", event_var, "==1) ~", var)
+           )
+           
+           #print("🚀 DEBUG: Kaplan-Meier Formula")
+           #print(km_formula)  # Check if the formula is correctly generated
+           
+           km_fit <- tryCatch(
+             surv_fit(km_formula, data = da_daten),  
+             error = function(e) {
+               print('ERRORRRRRRRRRR!!!!!!!!!!')
+               warning(paste("❌ Kaplan-Meier fit failed:", e$message))
+               return(NULL)
+             }
+           )
+           
+           
+           if (!is.null(km_fit)) {
+             print("✅ Kaplan-Meier Model Summary:")
+             #print(summary(km_fit))  # Ensure km_fit is not NULL
+           } else {
+             print("❌ Kaplan-Meier model failed")
+           }
+         })
+         
+                                 
+          if (!is.null(km_fit)) {
+            #print("🚀 DEBUG: Plotting Kaplan-Meier")
+            
+            km_plot <- tryCatch(
+              ggsurvplot(
+                km_fit,
+                data = da_daten,
+                censor.shape = "|", censor.size = 4,
+                title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
+                conf.int = TRUE,
+                pval = TRUE,
+                xlab = "Time in years",
+                break.time.by = 5,
+                surv.median.line = "hv",
+                ncensor.plot = TRUE,
+                #risk.table = TRUE,  #optional
+                cumevents = TRUE,  #optional
+                cumevents.height = 0.2,  #optional
+                ggtheme = theme_bw()
+              ),
+              error = function(e) {
+                warning(paste("❌ Kaplan-Meier plot failed:", e$message))
+                return(NULL)
+              }
+            )
+            
+            if (is.null(km_plot)) {
+              print("❌ ggplot konnte nicht erstellt werden.")
+            } else {
+              print("✅ ggplot erfolgreich erstellt.")
+            }
+          } else {
+            print("❌ km_fit ist NULL, daher kein Plot möglich.")
+          }
+         
+         #print("📊 Kaplan-Meier Summary:")
+         #if (!is.null(km_fit)) print(summary(km_fit))
+         #print("Kaplan-Meier Summary done")
+     
+         # ---- 2️⃣ Filter Data for Cox Models (REMOVE NA) ----
+         daten_filtered <- da_daten[complete.cases(da_daten[ , required_cols]), ]
+         
+         #print("✅ Data Preview (After Filtering):")
+         #print(head(daten_filtered))
+         print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
+         
+         # ---- 3️⃣ Log-Rank Test ----
+         logrank_test <- tryCatch(
+           survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
+                         as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
+           error = function(e) {
+             warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
+             return(NULL)
+           }
+         )
+         
+         # ---- 4️⃣ Unadjusted Cox Model ---
+         if (var %in% c(time_var, event_var)) {
+           warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
+           next  # Skip this iteration
+         }
+         unadjusted_cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var)
+         )
+         
+         unadjusted_cox_model <- tryCatch(
+           coxph(unadjusted_cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
+             return(NULL)
+           }
+         )
+         #unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
+         
+         # ---- 5️⃣ Adjusted Cox Model ----
+         if (var %in% c(time_var, event_var)) {
+           warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
+           next  # Skip this iteration
+         }
+         cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
+         )
+         
+         adjusted_cox_model <- tryCatch(
+           coxph(cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
+             message("💥 Error message: ", e$message)
+             message("📊 Dataset size: ", nrow(daten_filtered))
+             message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
+             return(NULL)
+           }
+         )
+         
+         #adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
+         
+         # ---- 6️⃣ Cox Forest Plot ----
+         #print(("GGFOREST TEST"))
+         #print(names(adjusted_cox_model$coefficients))
+         #print(names(daten_filtered))
+         #print(setdiff(list_confounder, names(daten_filtered)))
+         #print("GGFORTES TEST ENDE")
+         
+         #mutate daten in logarythmische form um extreme values zu minimieren
+        #daten_filtered <- daten_filtered %>%
+        #mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  
+         
+         if (!is.null(adjusted_cox_model)) {
+           cox_summary <- summary(adjusted_cox_model)
+           hr_values <- exp(cox_summary$coefficients[, "coef"])
+           ci_lower <- exp(cox_summary$conf.int[, "lower .95"])
+           ci_upper <- exp(cox_summary$conf.int[, "upper .95"])
+           
+           if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
+             warning("⚠️ Skipping forest plot due to extreme HR values")
+             forest_plot <- NULL
+           } else {
+             # 🏷️ Add labels to data before plotting
+             model_data <- model.frame(adjusted_cox_model)
+             labelled::var_label(model_data) <- label_map[names(model_data)]
+             
+             adjusted_cox_model$model <- model_data
+             # 📊 Create forest plot using forestmodel
+             forest_plot <- tryCatch(
+               forest_model(adjusted_cox_model),
+               error = function(e) {
+                 warning("⚠️ forest_model() failed for ", dataset_name, " - ", var, " with error: ", e$message)
+                 return(NULL)
+               }
+             )
+           }
+         } else {
+           forest_plot <- NULL
+         }
+         
+         # ---- 7️⃣ Store Results ----
+         result_key <- paste(dataset_name, var, event_var, sep = "_")
+         
+         results[[result_key]] <- list(
+           kaplan_meier_plot = km_plot,
+           logrank_test = logrank_test,
+           unadjusted_cox_model = summary(unadjusted_cox_model),
+           unadjusted_cox_model_raw = unadjusted_cox_model,
+           adjusted_cox_model = summary(adjusted_cox_model),
+           adjusted_cox_model_raw = adjusted_cox_model,                  
+           cox_forest_plot = forest_plot, 
+         )
+         
+         print(paste("✅ Successfully stored results for:", result_key))
+       }
+     }
+   }
+   
+   print(paste("Namen der Ergebnisse:", names(results)))
+   return(results)
+ }
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten

[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
Fehler in list(kaplan_meier_plot = km_plot, logrank_test = logrank_test,  : 
  argument 8 is empty
Zusätzlich: Warnmeldungen:
1: In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
> print(da_daten$Hyperfiltration130_1_ja)
  [1] "0" "1" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "1" "0" "1"
 [50] "1" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "1" "0" "0" "0" "0" "0" "0" "0" "1" "1" "1" "0" "1" "1" "0" "1" "1" "1" "0"
 [99] "0" "0" "1" "1" "0" "0" "0" "1" "0" "0" "0" "0" "1" "0" "1" "1" "0" "1" "0" "1" "1" "1" "1" "1" "0" "1" "0" "0" "0" "1" "1" "1" "1" "1" "1" "0" "1" "1" "0" "1" "1" "1" "0" "0" "1" "0" "0" "0" "1"
[148] "1" "1" "1" "1" "1" "0" "1" "0" "1" "0" "0" "0" "0" "1" "0" "1" "1" "1" "1" "0" "0" "1" "0" "1" "0" "1" "0" "1" "0" "1" "1" "1" "1" "1" "1" "0" "1" "1" "0" "1" "1" "0" "0" "0" "1" "0" "0" "1" "0"
[197] "1" "1" "0" "0" "1" "1" "0" "1" "0" "1" "0" "1" "0" "1" "1" "1" "0" "0" "0" "1" "1" "0" "1" "0" "0" "1" "0" "1" "1" "1" "1" "0" "0" "1" "1" "1" "0" "1" "1" "0" "0" "1" "1" "1" "1" "1" "1" "0" "1"
[246] "1" "1" "1" "0" "0" "0" "0" "1" "0" "0" "1" "1" "1" "1" "1" "0" "1" "1" "1" "0" "1" "1" "1" "1" "1" "1" "0" "1" "1" "1" "1" "1" "1" "1" "0" "1" "0" "1" "1" "1" "1" "0" "0" "1" "1" "1" "1" "1" "1"
[295] "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "1" "0" "1" "1" "1" "1"
> #survival_analysing, commands used for debbuging left inside as comments for easy reuse
> analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
+   
+   results <- list()
+   
+   label_map <- c(
+     "Hyperfiltration140_1_ja" = "Hyperfiltration",
+     "Hyperfiltration130_1_ja"= "Hyperfiltration",
+     "Hyperfiltration150_1_ja"= "Hyperfiltration",
+     "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
+     "RRsyst_completed_mean" = "systolischer Blutdruck",
+     "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
+     "BMI_completed_mean" = "BMI",
+     "UN_completed_mean" = "BUN",
+     "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
+     "Geschlecht_1_m...29" = "Geschlecht, 1=M",
+     "Alter_beitotwenntot_a" = "Alter",
+     "ProteinJa_1" = "Proteinurie, 1=ja",
+     "NBZ_completed_mean" = "Nüchternblutzucker",
+     "ppBZ_completed_mean" = "postprandialer Blutzucker"
+   )
+   
+   
+   for (dataset_name in names(list_daten)) {
+     da_daten <- list_daten[[dataset_name]]
+     
+     #print(paste("🔍 Checking dataset:", dataset_name))
+     #print(names(da_daten))  # Show dataset structure
+     
+     for (var in list_var) {
+       if (!(var %in% names(da_daten))) {
+         warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
+         next
+       }
+       
+       if (!is.factor(da_daten[[var]])) {
+         da_daten[[var]] <- as.factor(da_daten[[var]])
+         message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
+       }
+       
+       for (i in seq_along(list_surv_time)) {
+         time_var <- unlist(list_surv_time)[i]# ✅ Convert list to vector
+         event_var <- unlist(list_endpoint)[i]  # ✅ Convert list to vector
+         
+         # 🚨 **Debugging: Ensure `time_var` exists before proceeding** 🚨
+         #print(paste("🔄 Checking time_var:", time_var, "| event_var:", event_var))
+         #print(ls())  # ✅ Check all variables in scope
+         
+         stopifnot(!is.null(time_var), !is.null(event_var))  # ✅ Catch NULL values early
+         
+         if (!(time_var %in% names(da_daten))) {
+           stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
+         }
+         if (!(event_var %in% names(da_daten))) {
+           stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
+         }
+         
+         #print(paste("🚀 Processing:", dataset_name, time_var, event_var, var))
+         
+         required_cols <- c(time_var, event_var, var, unlist(list_confounder))
+         missing_cols <- setdiff(required_cols, names(da_daten))
+         
+         if (length(missing_cols) > 0) {
+           warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
+           next
+         }
+         
+         #print("✅ Data Preview (Before Filtering):")
+         #print(head(da_daten[ , required_cols, drop = FALSE]))
+         #print(paste("TESTTIM", da_daten[[time_var]],"TESTTIMENDE"))
+         
+        # da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
+         #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))
+ 
+         #print("🚀 DEBUG: Checking input before Kaplan-Meier")
+         #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
+         #print("✅ Column Names in Dataset:")
+         #print(names(da_daten))
+         #print("✅ First 6 Rows:")
+         #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
+         #if (!(time_var %in% names(da_daten))) {
+           #stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
+         #}
+         #if (!(event_var %in% names(da_daten))) {
+          # stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
+         #}
+         #if (!(var %in% names(da_daten))) {
+         #  stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
+         #}
+         #print(paste("Type of time_var:", class(da_daten[[time_var]])))
+         #print(paste("Type of event_var:", class(da_daten[[event_var]])))
+         #if (!is.factor(da_daten[[var]])) {
+         #  print(paste("⚠️ Converting", var, "to factor"))}
+         
+                 # ---- 1️⃣ Kaplan-Meier Plot (UNFILTERED DATA) ----
+         
+          suppressWarnings({
+           km_formula <- as.formula(
+             paste("Surv(", time_var, ",", event_var, "==1) ~", var)
+           )
+           
+           #print("🚀 DEBUG: Kaplan-Meier Formula")
+           #print(km_formula)  # Check if the formula is correctly generated
+           
+           km_fit <- tryCatch(
+             surv_fit(km_formula, data = da_daten),  
+             error = function(e) {
+               print('ERRORRRRRRRRRR!!!!!!!!!!')
+               warning(paste("❌ Kaplan-Meier fit failed:", e$message))
+               return(NULL)
+             }
+           )
+           
+           
+           if (!is.null(km_fit)) {
+             print("✅ Kaplan-Meier Model Summary:")
+             #print(summary(km_fit))  # Ensure km_fit is not NULL
+           } else {
+             print("❌ Kaplan-Meier model failed")
+           }
+         })
+         
+                                 
+          if (!is.null(km_fit)) {
+            #print("🚀 DEBUG: Plotting Kaplan-Meier")
+            
+            km_plot <- tryCatch(
+              ggsurvplot(
+                km_fit,
+                data = da_daten,
+                censor.shape = "|", censor.size = 4,
+                title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
+                conf.int = TRUE,
+                pval = TRUE,
+                xlab = "Time in years",
+                break.time.by = 5,
+                surv.median.line = "hv",
+                ncensor.plot = TRUE,
+                #risk.table = TRUE,  #optional
+                cumevents = TRUE,  #optional
+                cumevents.height = 0.2,  #optional
+                ggtheme = theme_bw()
+              ),
+              error = function(e) {
+                warning(paste("❌ Kaplan-Meier plot failed:", e$message))
+                return(NULL)
+              }
+            )
+            
+            if (is.null(km_plot)) {
+              print("❌ ggplot konnte nicht erstellt werden.")
+            } else {
+              print("✅ ggplot erfolgreich erstellt.")
+            }
+          } else {
+            print("❌ km_fit ist NULL, daher kein Plot möglich.")
+          }
+         
+         #print("📊 Kaplan-Meier Summary:")
+         #if (!is.null(km_fit)) print(summary(km_fit))
+         #print("Kaplan-Meier Summary done")
+     
+         # ---- 2️⃣ Filter Data for Cox Models (REMOVE NA) ----
+         daten_filtered <- da_daten[complete.cases(da_daten[ , required_cols]), ]
+         
+         #print("✅ Data Preview (After Filtering):")
+         #print(head(daten_filtered))
+         print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
+         
+         # ---- 3️⃣ Log-Rank Test ----
+         logrank_test <- tryCatch(
+           survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
+                         as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
+           error = function(e) {
+             warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
+             return(NULL)
+           }
+         )
+         
+         # ---- 4️⃣ Unadjusted Cox Model ---
+         if (var %in% c(time_var, event_var)) {
+           warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
+           next  # Skip this iteration
+         }
+         unadjusted_cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var)
+         )
+         
+         unadjusted_cox_model <- tryCatch(
+           coxph(unadjusted_cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
+             return(NULL)
+           }
+         )
+         #unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
+         
+         # ---- 5️⃣ Adjusted Cox Model ----
+         if (var %in% c(time_var, event_var)) {
+           warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
+           next  # Skip this iteration
+         }
+         cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
+         )
+         
+         confounders <- setdiff(unlist(list_confounder), var)
+         
+         
+         adjusted_cox_model <- tryCatch(
+           coxph(cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
+             message("💥 Error message: ", e$message)
+             message("📊 Dataset size: ", nrow(daten_filtered))
+             message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
+             return(NULL)
+           }
+         )
+         
+         #adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
+         
+         # ---- 6️⃣ Cox Forest Plot ----
+         #print(("GGFOREST TEST"))
+         #print(names(adjusted_cox_model$coefficients))
+         #print(names(daten_filtered))
+         #print(setdiff(list_confounder, names(daten_filtered)))
+         #print("GGFORTES TEST ENDE")
+         
+         #mutate daten in logarythmische form um extreme values zu minimieren
+        #daten_filtered <- daten_filtered %>%
+        #mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  
+         
+         if (!is.null(adjusted_cox_model)) {
+           cox_summary <- summary(adjusted_cox_model)
+           hr_values <- exp(cox_summary$coefficients[, "coef"])
+           ci_lower <- exp(cox_summary$conf.int[, "lower .95"])
+           ci_upper <- exp(cox_summary$conf.int[, "upper .95"])
+           
+           if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
+             warning("⚠️ Skipping forest plot due to extreme HR values")
+             forest_plot <- NULL
+           } else {
+             # 🏷️ Add labels to data before plotting
+             model_data <- model.frame(adjusted_cox_model)
+             labelled::var_label(model_data) <- label_map[names(model_data)]
+             
+             adjusted_cox_model$model <- model_data
+             # 📊 Create forest plot using forestmodel
+             forest_plot <- tryCatch(
+               forest_model(adjusted_cox_model),
+               error = function(e) {
+                 warning("⚠️ forest_model() failed for ", dataset_name, " - ", var, " with error: ", e$message)
+                 return(NULL)
+               }
+             )
+           }
+         } else {
+           forest_plot <- NULL
+         }
+         
+         # ---- 7️⃣ Store Results ----
+         result_key <- paste(dataset_name, var, event_var, sep = "_")
+         
+         results[[result_key]] <- list(
+           kaplan_meier_plot = km_plot,
+           logrank_test = logrank_test,
+           unadjusted_cox_model = summary(unadjusted_cox_model),
+           unadjusted_cox_model_raw = unadjusted_cox_model,
+           adjusted_cox_model = summary(adjusted_cox_model),
+           adjusted_cox_model_raw = adjusted_cox_model,                  
+           cox_forest_plot = forest_plot
+         )
+         
+         print(paste("✅ Successfully stored results for:", result_key))
+       }
+     }
+   }
+   
+   print(paste("Namen der Ergebnisse:", names(results)))
+   return(results)
+ }
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting Hyperfiltration130_1_ja to factor in da_daten

[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
[1] "✅ Successfully stored results for: da_daten_Hyperfiltration130_1_ja_tot_1"
[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
|=================================================================================================================                                                                 | 64% ~1 s remaining     [1] "✅ Successfully stored results for: da_daten_Hyperfiltration130_1_ja_NE_1_ja"
[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 296"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
[1] "✅ Successfully stored results for: da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"
✅ Converting Hyperfiltration130_1_ja to factor in subset_minusCKD

[1] "Namen der Ergebnisse: da_daten_Hyperfiltration130_1_ja_tot_1"                 "Namen der Ergebnisse: da_daten_Hyperfiltration130_1_ja_NE_1_ja"              
[3] "Namen der Ergebnisse: da_daten_Hyperfiltration130_1_ja_endpoint_reached_ja_1"
Warnmeldungen:
1: In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
4: In .add_surv_median(p, fit, type = surv.median.line, fun = fun,  :
  Median survival not reached.
5: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, NBZ_completed_mean, ppBZ_completed_mean
6: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, NBZ_completed_mean, ppBZ_completed_mean
7: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, NBZ_completed_mean, ppBZ_completed_mean
> # Access Kaplan-Meier Plot
> print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$kaplan_meier_plot)
> # Access Log-Rank Test
> print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$logrank_test)
Call:
survdiff(formula = Surv(as.numeric(get(time_var, daten_filtered)), 
    as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered))

                             N Observed Expected (O-E)^2/E (O-E)^2/V
get(var, daten_filtered)=0 143       61     35.8      17.7      32.4
get(var, daten_filtered)=1 153       18     43.2      14.7      32.4

 Chisq= 32.4  on 1 degrees of freedom, p= 1e-08 
> # Access Unadjusted Cox Model
> print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$unadjusted_cox_model)
Call:
coxph(formula = unadjusted_cox_formula, data = daten_filtered)

  n= 296, number of events= 79 

                            coef exp(coef) se(coef)     z Pr(>|z|)    
Hyperfiltration130_1_ja1 -1.4086    0.2445   0.2683 -5.25 1.52e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                         exp(coef) exp(-coef) lower .95 upper .95
Hyperfiltration130_1_ja1    0.2445       4.09    0.1445    0.4137

Concordance= 0.658  (se = 0.026 )
Likelihood ratio test= 33.48  on 1 df,   p=7e-09
Wald test            = 27.56  on 1 df,   p=2e-07
Score (logrank) test = 32.41  on 1 df,   p=1e-08

> # Access Adjusted Cox Model
> print(results[["da_daten_Hyperfiltration130_1_ja_tot_1"]]$adjusted_cox_model)
Call:
coxph(formula = cox_formula, data = daten_filtered)

  n= 296, number of events= 79 

                                                   coef  exp(coef)   se(coef)      z Pr(>|z|)    
Hyperfiltration130_1_ja1                     -3.910e-01  6.764e-01  6.596e-01 -0.593   0.5533    
RRsyst_completed_mean                         3.734e-02  1.038e+00  1.790e-02  2.086   0.0370 *  
highest_measured_hba1c_naisna_completed_mean -1.244e-01  8.830e-01  1.988e-01 -0.626   0.5314    
BMI_completed_mean                            4.846e-02  1.050e+00  8.161e-02  0.594   0.5527    
UN_completed_mean                             3.415e-02  1.035e+00  4.484e-02  0.761   0.4464    
Diabetesdauer_totwenntot_a_tim               -8.977e-02  9.141e-01  3.852e-02 -2.330   0.0198 *  
Geschlecht_1_m...29                           1.618e-01  1.176e+00  5.635e-01  0.287   0.7741    
Alter_beitotwenntot_a                        -1.547e+01  1.919e-07  2.788e+00 -5.547 2.91e-08 ***
ProteinJa_1                                  -3.007e-02  9.704e-01  5.713e-01 -0.053   0.9580    
NBZ_completed_mean                            1.606e-03  1.002e+00  4.470e-03  0.359   0.7194    
ppBZ_completed_mean                          -2.013e-03  9.980e-01  5.016e-03 -0.401   0.6881    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                                             exp(coef) exp(-coef) lower .95 upper .95
Hyperfiltration130_1_ja1                     6.764e-01  1.479e+00 1.857e-01 2.464e+00
RRsyst_completed_mean                        1.038e+00  9.633e-01 1.002e+00 1.075e+00
highest_measured_hba1c_naisna_completed_mean 8.830e-01  1.133e+00 5.980e-01 1.304e+00
BMI_completed_mean                           1.050e+00  9.527e-01 8.945e-01 1.232e+00
UN_completed_mean                            1.035e+00  9.664e-01 9.477e-01 1.130e+00
Diabetesdauer_totwenntot_a_tim               9.141e-01  1.094e+00 8.477e-01 9.858e-01
Geschlecht_1_m...29                          1.176e+00  8.506e-01 3.896e-01 3.548e+00
Alter_beitotwenntot_a                        1.919e-07  5.211e+06 8.121e-10 4.535e-05
ProteinJa_1                                  9.704e-01  1.031e+00 3.167e-01 2.973e+00
NBZ_completed_mean                           1.002e+00  9.984e-01 9.929e-01 1.010e+00
ppBZ_completed_mean                          9.980e-01  1.002e+00 9.882e-01 1.008e+00
thats the corresponding result, it seams there be an issue with the variable age

## Prompt 3

mabye my initial approach was wrong please help me think:
I have Alter Beitotwenntot as time_variable being measured frombirth from birth. This is a corresponding variable for the endpoint tot in my survival analysis (to know when the death occured)
at the same time i want to know weather age is a predictor of death (which it should be) and therefore include it as covariate in my regression model

## Prompt 4

Ich versuche die entsprechenden spalten in excel zuerstellen, =WENN(CG2-((F2-E2)/365,25)<40;CG2-((F2-E2)/365,25);0) diese angabe wird aber nicht verarbeitet

## Prompt 5

how can i get my column names in r

## Prompt 6

Fehler in analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  : 
  ❌ ERROR: Missing time variable:  survival_time_age in da_daten

I created a new column survival_time_age, it is prestnt in my da_daten:
$ study_begin_to_death_D                                  <dbl> 4828, -30497, 9280, 1954, 736, 7205, 4735, -30497, 2303, -30497, 12094, 10126, 8446, NA, 3580, 6111, 3197, 844, -30497, -30497, 2143, -304…
$ survival_time_age                                       <dbl> 13.227397, 0.000000, 25.424658, 5.353425, 2.016438, 19.739726, 12.972603, 0.000000, 6.309589, 0.000000, 33.134247, 27.742466, 23.139726, N…
$ zwischenschritt                                         <dbl> 1.702943, 40.000000, 23.682409, 3.644079, 2.015058, 1.949350, 12.572211, 3.950719, 3.041752, 40.000000, 5.900068, 27.537303, 23.123888, NA…
$ survival_time_endpoint                                  <dbl> 1.702943, 0.000000, 23.682409, 3.644079, 2.015058, 1.949350, 12.572211, 3.950719, 3.041752, 0.000000, 5.900068, 27.537303, 23.123888, NA, …
and i did redifine my list daten 
but i get that warning

## Prompt 7

Warnmeldungen:
1: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
2: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
3: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
what does this mean?

## Prompt 8

list_daten <- list(
  da_daten = da_daten
  #,subset_minusCKD = subset_minusCKD 
)

list_var <- list(
  "Hyperfiltration130_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "survival_time_age",#_rounded
  "NE_surv_all_a_tim",
  "survival_time_endpoint"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "Hyperfiltration130_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_totwenntot_a_tim",
  "Geschlecht_1_m...29",
  "Alter_beitotwenntot_a",
  "ProteinJa_1",
  "NBZ_completed_mean",
  "ppBZ_completed_mean"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
    "Geschlecht_1_m...29" = "Geschlecht, 1=M",
    "Alter_beitotwenntot_a" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja",
    "NBZ_completed_mean" = "Nüchternblutzucker",
    "ppBZ_completed_mean" = "postprandialer Blutzucker"
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
        
       # da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
        #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))

        #print("🚀 DEBUG: Checking input before Kaplan-Meier")
        #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
        #print("✅ Column Names in Dataset:")
        #print(names(da_daten))
        #print("✅ First 6 Rows:")
        #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
        #if (!(time_var %in% names(da_daten))) {
          #stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        #}
        #if (!(event_var %in% names(da_daten))) {
         # stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        #}
        #if (!(var %in% names(da_daten))) {
        #  stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
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
        
                                
         if (!is.null(km_fit)) {
           #print("🚀 DEBUG: Plotting Kaplan-Meier")
           
           km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
               conf.int = TRUE,
               pval = TRUE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               ncensor.plot = TRUE,
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
        print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                        as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
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
thats the corresponding cde

## Prompt 9

I run this code and get

list_daten <- list(
  da_daten = da_daten
  #,subset_minusCKD = subset_minusCKD 
)

list_var <- list(
  "Hyperfiltration130_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "survival_time_age",#_rounded
  "NE_surv_all_a_tim",
  "survival_time_endpoint"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "Hyperfiltration130_1_ja",
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN_completed_mean",
  "Diabetesdauer_totwenntot_a_tim",
  "Geschlecht_1_m...29",
  "Alter_beitotwenntot_a",
  "ProteinJa_1",
  "NBZ_completed_mean",
  "ppBZ_completed_mean"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
    "Geschlecht_1_m...29" = "Geschlecht, 1=M",
    "Alter_beitotwenntot_a" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja",
    "NBZ_completed_mean" = "Nüchternblutzucker",
    "ppBZ_completed_mean" = "postprandialer Blutzucker"
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
        
       # da_daten <- da_daten[complete.cases(da_daten[, c(time_var, event_var, var)]), ]
        #print(paste("✅ Rows after removing NAs:", nrow(da_daten)))

        #print("🚀 DEBUG: Checking input before Kaplan-Meier")
        #print(paste("Dataset:", dataset_name, "| Time:", time_var, "| Event:", event_var, "| Stratify By:", var))
        #print("✅ Column Names in Dataset:")
        #print(names(da_daten))
        #print("✅ First 6 Rows:")
        #print(head(da_daten[, c(time_var, event_var, var), drop = FALSE]))
        #if (!(time_var %in% names(da_daten))) {
          #stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        #}
        #if (!(event_var %in% names(da_daten))) {
         # stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        #}
        #if (!(var %in% names(da_daten))) {
        #  stop(paste("❌ ERROR: Missing stratification variable:", var, "in", dataset_name))
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
        
                                
         if (!is.null(km_fit)) {
           #print("🚀 DEBUG: Plotting Kaplan-Meier")
           
           km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
               conf.int = TRUE,
               pval = TRUE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               ncensor.plot = TRUE,
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
        print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                        as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
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


Warnmeldungen:
1: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
2: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
3: In geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
  All aesthetics have length 1, but the data has 2 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
what does this mean?

## Prompt 10

ich habe die Werte für alle ohne eingetretenen endpunkt auf 0 gesetzt und sie nicht truncirert (da sie noch leben) macht das probleme?

## Prompt 11

that worked like a charm ;)
one last question (for now) 
  km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
               conf.int = TRUE,
               pval = TRUE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               ncensor.plot = TRUE,
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
thats the code snippet for my kaplan maier plot 
and untill now it did depict the numbers of censoring, however that box is now wmpty
