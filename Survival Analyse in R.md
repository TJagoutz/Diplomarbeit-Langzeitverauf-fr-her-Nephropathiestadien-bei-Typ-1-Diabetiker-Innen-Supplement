# Survival Analyse in R

## Prompt 1

Ich habe als teil meiner statistischenAnalöyse diesen code in R #3.3)SEKUNDÄRER ENDPUNKT
##SURVIVAL ANALYSE##
da_daten <- subset_minusCKD

list_daten <- list(
  subset_minusCKD = subset_minusCKD
)
 # merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen = merged_outliers__Diabetesdauer_a__highest_measured_hba1c_naisna_Kreatininclearance_endogen


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
  "UN",
  "Diabetesdauer_a_",
  "Geschlecht_1_m",
  "Alter_a_einschluss",
  "ProteinJa_1"
)

  
#survival_analysing, commands used for debbuging left inside as comments for easy reuse
analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  label_map <- c(
    "Hyperfiltration120_1_ja" = "Hyperfiltration",
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

nun möchte ich im Rahmen explorativer Analysen noch survival Analysen mit anderen Variablen machen, (Hba1c und Proteinurie) 
die population soll jeweils anhand des medians in 2 Gruppen getzeilt werden und diese im hinblick auf die Endpunkte (tot und endpunbkt erreicht) analysiert werden. Dafür möchte ich einen Chisquare test zum erreichen des endpunktes und eine survival analyse (ähnlich wie im obigen code (mit der jgleichen funktion für die kaplan amier kurve)) sowie einen logrank test für die dauer bis zum erreichen des endpunktes.
 cox modelle brauche ich hier nicht mehr, daher auch keine forrest plots. 

ich möchte wie im code oben, die variablen, datensätze und endpunkte variabel festlegen köännen, um im bedarfsfall eine variable hinzuzufügen

## Prompt 2

oaky danke 1) würde ich doch gerne eine HR/OR angeben, dafür brauche ich doch die unadjustierten modelle oder? und 2. ist der HbA1c nicht mehr signifikant, das würde ich gerne in einem adjustierten modell (mit den im code oben genutzten Variablen) überprüfen

## Prompt 3

ist das eine komplett neue funktion, oder ersetze ich miene funktion dami?

## Prompt 4

ja aber die funktion, die du mir vorher ausgegeben hat (results_median) kann ich mit der ersetzen?

## Prompt 5

macht es sinn in den adjustierten modellen jeweils die variable, nach der die gruppen getzeilt sind einzubeziehen oder versucht das statistische probleme?

## Prompt 6

list_confounder <- c(
  "RRsyst_completed_mean",
  "highest_measured_hba1c_naisna_completed_mean",
  "BMI_completed_mean",
  "UN",
  "Diabetesdauer_a_",
  "Geschlecht_1_m",
  "Alter_a_einschluss",
  "ProteinJa_1"
)

das ist miene confunder liste,

## Prompt 7

Fehler in analyze_survival_median_hr(list_daten, list_vars, list_surv_time,  : 
  Objekt 'confounders_used' nicht gefunden

## Prompt 8

danke, kannst du bitte in den output jweeils noche einbauen, dass geprintet wird wie viele Fälle in der jeweiligen analyse eingeschlossen sind (da ja nas entfernt werden)

## Prompt 9

print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_tot_1"]]$km_plot)
print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_tot_1"]]$logrank)
print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_tot_1"]]$chi_res)

print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_endpoint_reached_ja_1"]]$km_plot)
print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_endpoint_reached_ja_1"]]$logrank)
print(results_hr[["subset_minusCKD_highest_measured_hba1c_naisna_completed_mean_endpoint_reached_ja_1"]]$chi_res)



print(results_hr[["subset_minusCKD_ProteinJa_1_tot_1"]]$km_plot)
print(results_hr[["subset_minusCKD_ProteinJa_1_tot_1"]]$logrank)
print(results_hr[["subset_minusCKD_ProteinJa_1_tot_1"]]$chi_res)

print(results_hr[["subset_minusCKD_ProteinJa_1_endpoint_reached_ja_1"]]$km_plot)
print(results_hr[["subset_minusCKD_ProteinJa_1_endpoint_reached_ja_1"]]$logrank)
print(results_hr[["subset_minusCKD_ProteinJa_1_endpoint_reached_ja_1"]]$chi_res)

ich habe diese ausgabeblöcke noch von vorhin, kannst du mir die bitte für alle resulttypen vervollständigen, jeweilis nach variable und outcome getrennt wie hier

## Prompt 10

nein, bitte so wie ich es angegeben habe, einfach am ende die result arten vervollständgen um mir tipperei abzunehmen, ich möchte sie einzeln aufrufen können

## Prompt 11

library(survival)
library(survminer)
library(dplyr)

analyze_survival_median_hr <- function(list_daten,
                                       list_vars,
                                       list_surv_time,
                                       list_endpoint,
                                       list_confounder,
                                       event_is_1 = TRUE,
                                       label_map = NULL,
                                       min_group_n = 10) {

  list_surv_time <- unlist(list_surv_time)
  list_endpoint  <- unlist(list_endpoint)
  stopifnot(length(list_surv_time) == length(list_endpoint))

  results <- list()

  for (dataset_name in names(list_daten)) {
    da <- list_daten[[dataset_name]]
    if (!is.data.frame(da)) next

    for (var in list_vars) {
      if (!(var %in% names(da))) {
        warning(paste("⚠️ Variable", var, "nicht in", dataset_name, "— übersprungen."))
        next
      }

      # --- Gruppenvariable erstellen (Median-Split bei numerisch, sonst Faktor übernehmen)
      grp_var <- paste0(var, "_grp")
      da_local <- da
      if (is.numeric(da_local[[var]])) {
        med <- median(da_local[[var]], na.rm = TRUE)
        if (!is.finite(med)) {
          warning(paste("⚠️", dataset_name, "|", var, ": kein endlicher Median — übersprungen."))
          next
        }
        da_local[[grp_var]] <- ifelse(da_local[[var]] <= med,
                                      paste0("≤Median (", signif(med, 3), ")"),
                                      paste0(">Median (",  signif(med, 3), ")"))
      } else {
        da_local[[grp_var]] <- as.factor(da_local[[var]])
      }
      da_local[[grp_var]] <- factor(da_local[[grp_var]])

      for (i in seq_along(list_surv_time)) {
        time_var  <- list_surv_time[i]
        event_var <- list_endpoint[i]

        # Pflichtspalten prüfen
        present_cols <- intersect(c(time_var, event_var, grp_var, list_confounder), names(da_local))
        missing_cols <- setdiff(c(time_var, event_var, grp_var), names(da_local))
        if (length(missing_cols) > 0) {
          warning(paste("⚠️ Fehlende Pflichtspalten in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
          next
        }

        # --- Zählungen vorbereiten
        n_total <- nrow(da_local)

        # vollständige Fälle (bezüglich der verfügbaren Variablen)
        complete_df <- da_local[complete.cases(da_local[, present_cols, drop = FALSE]), , drop = FALSE]
        n_complete  <- nrow(complete_df)
        n_removed_na <- n_total - n_complete
        if (n_complete == 0) {
          warning(paste("⚠️", dataset_name, "|", var, "|", event_var, ": keine vollständigen Fälle — übersprungen."))
          next
        }

        # Event binär kodieren
        complete_df[[event_var]] <- if (event_is_1) as.integer(complete_df[[event_var]] == 1) else as.integer(complete_df[[event_var]] != 0)

        # Zu kleine Gruppen entfernen
        grp_sizes_before <- table(complete_df[[grp_var]])
        df <- complete_df
        small_grps <- names(grp_sizes_before[grp_sizes_before < min_group_n])
        if (length(small_grps) > 0) {
          message("ℹ️ ", dataset_name, " | ", var, " | ", event_var,
                  ": entferne zu kleine Gruppen (<", min_group_n, "): ",
                  paste(small_grps, collapse = ", "))
          df <- df[!(df[[grp_var]] %in% small_grps), , drop = FALSE]
          df[[grp_var]] <- droplevels(df[[grp_var]])
        }

        # Final verfügbare Stichprobe
        n_final <- nrow(df)
        if (n_final == 0 || nlevels(df[[grp_var]]) < 2L) {
          warning(paste("⚠️", dataset_name, "|", var, ": zu wenig Daten/Groups für KM — übersprungen."))
          next
        }
        events_final <- sum(df[[event_var]] == 1, na.rm = TRUE)
        group_sizes_final <- table(df[[grp_var]])
        group_sizes_str <- paste(paste(names(group_sizes_final), as.integer(group_sizes_final), sep = "="), collapse = ", ")

        # --- Transparente Ausgabe
        message(
          "📊 Einschluss | ", dataset_name, " | ", var, " | ", event_var, " → ",
          "n_gesamt=", n_total,
          " | vollständig=", n_complete, " (entfernt wg. NA=", n_removed_na, ")",
          " | final n=", n_final, ", Ereignisse=", events_final,
          " | Gruppen: ", group_sizes_str
        )

        # --- Chi-Quadrat: Endpunkt (ja/nein) vs. Gruppe
        chi_tab <- table(df[[grp_var]], df[[event_var]])
        chi_test <- tryCatch(chisq.test(chi_tab, correct = FALSE), error = function(e) NULL)

        # --- Unadjustiertes Cox-Modell
        unadj_formula <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ `", grp_var, "`"))
        unadj_model <- tryCatch(coxph(unadj_formula, data = df), error = function(e) NULL)

        # --- Confounder-Bereinigung + Adjustiertes Cox-Modell
        confounders_input <- unique(list_confounder)
        confounders_used <- setdiff(confounders_input, var)
        confounders_used <- intersect(confounders_used, names(df))

        removed_confounders <- setdiff(confounders_input, confounders_used)
        if (length(removed_confounders) > 0) {
          message("⚠️ Confounder bereinigt für ", dataset_name, " | ", var, " | ", event_var,
                  " → entfernt: ", paste(removed_confounders, collapse = ", "))
        }

        if (length(confounders_used) > 0) {
          adj_formula <- as.formula(paste0(
            "Surv(", time_var, ", ", event_var, ") ~ `", grp_var, "` + ",
            paste(confounders_used, collapse = " + ")
          ))
        } else {
          message("ℹ️ ", dataset_name, " | ", var, " | ", event_var,
                  ": keine Confounder nach Bereinigung übrig → univariates Modell (nur Gruppenvariable).")
          adj_formula <- as.formula(paste0(
            "Surv(", time_var, ", ", event_var, ") ~ `", grp_var, "`"
          ))
        }
        adj_model <- tryCatch(coxph(adj_formula, data = df), error = function(e) NULL)

        # --- Kaplan–Meier + Log-Rank
        surv_formula <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ `", grp_var, "`"))
        km_fit <- tryCatch(surv_fit(surv_formula, data = df), error = function(e) NULL)
        logrank <- tryCatch(survdiff(surv_formula, data = df), error = function(e) NULL)
        p_logrank <- if (!is.null(logrank)) signif(1 - pchisq(logrank$chisq, length(logrank$n) - 1), 3) else NA

        km_plot <- tryCatch(
          ggsurvplot(
            km_fit,
            data = df,
            censor.shape = "|", censor.size = 4,
            title = paste0("Kaplan–Meier: ", dataset_name, " | ", var, " ~ ", event_var),
            conf.int = TRUE,
            pval = if (!is.na(p_logrank)) paste0("Log-Rank p = ", p_logrank) else FALSE,
            xlab = "Zeit",
            ggtheme = theme_bw()
          ),
          error = function(e) NULL
        )

        # --- HR/CI/p extrahieren
        get_hr <- function(model) {
          if (is.null(model)) return(NULL)
          s <- summary(model)
          hr <- exp(s$coef[, "coef"])
          ci_low <- exp(s$conf.int[, "lower .95"])
          ci_up  <- exp(s$conf.int[, "upper .95"])
          p <- signif(s$coef[, "Pr(>|z|)"], 3)
          data.frame(Variable = rownames(s$coef), HR = hr, CI_low = ci_low, CI_up = ci_up, p = p, row.names = NULL)
        }
        hr_unadj <- get_hr(unadj_model)
        hr_adj   <- get_hr(adj_model)

        key <- paste(dataset_name, var, event_var, sep = "_")
        results[[key]] <- list(
          dataset = dataset_name,
          variable = var,
          endpoint = event_var,
          group_var = grp_var,
          # Einschluss-Stats:
          n_total = n_total,
          n_complete = n_complete,
          n_removed_na = n_removed_na,
          n_final = n_final,
          events_final = events_final,
          group_sizes_final = group_sizes_final,
          # Tests/Modelle:
          chi_table = chi_tab,
          chi_test = chi_test,
          km_fit = km_fit,
          km_plot = km_plot,
          logrank = logrank,
          hr_unadjusted = hr_unadj,
          hr_adjusted = hr_adj,
          confounders_used = confounders_used
        )

        message("✅ Fertig: ", key)
      }
    }
  }

  message("📦 Analysen fertig: ", length(results), " Resultate.")
  return(results)
}


warum kommt bei dieser version ein anderes ergebnis beim logrank test und dem kaplan mamier kurven heraus als zuvor?

## Prompt 12

ja ich denke dass der filter nur für die cox funktionen mehr sinn acht oder? bitte korrrigeiere mich wenn das anders ist

## Prompt 13

okay 19 würde ich gerne die namen der Gruppen in der Kaplan maier kurve noch insfern apassen, dass sie 1) kürzer sind, und 2. bei proteinurie nicht median steht (da es nicht der median ist) also eventuell einfach gruppe 1/2 oder >Median_1=0 bzw. ist 1 und Proteinurie_1=0 bzw. 1
Außerdem hätte ich den abschnitt mit der kumulöativen Ereigniszahl unter dem plot gernei wieder dabei.
Es genügt wenn du mir die anpassunggen für KM Plort ausgibst, ich füge es dann ein

## Prompt 14

Fehler: gridtext has encountered a tag that isn't supported yet: <blockquote>
Only a very limited number of tags are currently supported.

## Prompt 15

gib mir bitte den ganzen code erneut

## Prompt 16

print(count(subset_minusCKD$ProteinJa_1==1))

## Prompt 17

if (da_local[[var]]="highest_measured_hba1c_naisna_completed_mean")) {
          medianhba1c <- median(da_local[[var]])
          lable1 <- paste0("HbA1c > Median (", medianhba1c, ")")
          lable2 <- paste0("Hba1c ≤ Median (", medianhba1c,")")
        }
          else {
            
            lable1 <- "Proteinurie_ja"
            lable2 <- "Proteinurie_nein"
        }
wird das funktionieren?

## Prompt 18

Fehler in if (da_local[[var]] == "highest_measured_hba1c_naisna_completed_mean") { : 
  Bedingung hat Länge > 1

## Prompt 19

Fehler in if (da_local[[var]] == "highest_measured_hba1c_naisna_completed_mean") { : 
  Bedingung hat Länge > 1
