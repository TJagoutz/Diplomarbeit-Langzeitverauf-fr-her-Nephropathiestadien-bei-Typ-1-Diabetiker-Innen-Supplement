# Survivalanalyse mit Logrank Test

## Prompt 1

i have this to export a results table for explorative analyses so i dont need to wrrite redundent paragraphs:
datasets <- list("da_daten" = da_daten)
variables <- c("hyperfiltration_egfr")
surv_times <- list("Überlebenszeit" = "Alter_beitotwenntot_a",#_rounded
                   "Zeit bis Endpunkt"="time_to_endpoint_a_tim")
endpoints <- list("Verstorben"="tot_1",
                  "Kombinierter Endpunkt" = "endpoint_reached_ja_1")
confounders <- c("Hyperfiltration140_1_ja",
                 "RRsyst_completed_mean",
                 "highest_measured_hba1c_naisna_completed_mean",
                 "BMI_completed_mean",
                 "UN_completed_mean",
                 "Diabetesdauer_totwenntot_a_tim",
                 "Geschlecht_1_m...29", "Alter_beitotwenntot_a",
                 "ProteinJa_1", "NBZ_completed_mean",
                 "ppBZ_completed_mean")

create_survival_summary_direct <- function(datasets, variables, surv_times, endpoints, confounders, label_map = NULL) {
  library(survival)
  library(dplyr)
  
  summary_df <- data.frame()
  
  for (dataset_name in names(datasets)) {
    df <- datasets[[dataset_name]]
    
    for (var in variables) {
      if (!(var %in% names(df))) next
      df[[var]] <- as.factor(df[[var]])
      
      for (i in seq_along(surv_times)) {
        time_var <- surv_times[[i]]
        event_var <- endpoints[[i]]
        
        if (!(time_var %in% names(df)) || !(event_var %in% names(df))) next
        
        # Filter complete cases
        complete_vars <- c(time_var, event_var, var, confounders)
        df_complete <- df[complete.cases(df[, complete_vars]), ]
        
        # Skip if empty
        if (nrow(df_complete) < 10) next
        
        # Logrank Test
        logrank <- tryCatch({
          survdiff(Surv(df_complete[[time_var]], df_complete[[event_var]]) ~ df_complete[[var]])
        }, error = function(e) NULL)
        p_logrank <- if (!is.null(logrank)) round(1 - pchisq(logrank$chisq, df = 1), 3) else NA
        
        # Event table
        tab <- tryCatch({
          table(df_complete[[var]], df_complete[[event_var]])
        }, error = function(e) matrix(NA, 2, 2))
        n_str <- paste0(colSums(tab, na.rm = TRUE), collapse = " / ")
        event_str <- paste0(tab[, 2], collapse = " / ")
        
        # Unadjusted Cox
        unadj_fit <- tryCatch({
          coxph(as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", var)), data = df_complete)
        }, error = function(e) NULL)
        
        # Adjusted Cox
        adj_formula <- paste0("Surv(", time_var, ",", event_var, ") ~ ", var, "+", paste(confounders, collapse = "+"))
        adj_fit <- tryCatch({
          coxph(as.formula(adj_formula), data = df_complete)
        }, error = function(e) NULL)
        
        if (is.null(unadj_fit) || is.null(adj_fit)) next
        
        # Extract estimates
        unadj_coef <- summary(unadj_fit)$coefficients
        unadj_conf <- summary(unadj_fit)$conf.int
        hr_unadj <- sprintf("%.2f [%.2f–%.2f]", unadj_conf[1, "exp(coef)"], unadj_conf[1, "lower .95"], unadj_conf[1, "upper .95"])
        p_unadj <- signif(unadj_coef[1, "Pr(>|z|)"], 3)
        
        adj_coef <- summary(adj_fit)$coefficients
        adj_conf <- summary(adj_fit)$conf.int
        var_row <- grep(var, rownames(adj_coef), fixed = TRUE)
        if (length(var_row) == 0) next
        hr_adj <- sprintf("%.2f [%.2f–%.2f]", adj_conf[var_row, "exp(coef)"], adj_conf[var_row, "lower .95"], adj_conf[var_row, "upper .95"])
        p_adj <- signif(adj_coef[var_row, "Pr(>|z|)"], 3)
        
        # Apply labels
        display_var <- if (!is.null(label_map) && var %in% names(label_map)) label_map[[var]] else var
        display_endpoint <- names(endpoints)[i] %||% event_var
        
        # Append row
        summary_df <- rbind(summary_df, data.frame(
          Dataset = dataset_name,
          Variable = display_var,
          Endpoint = display_endpoint,
          `N (0 / 1)` = n_str,
          `Events (0 / 1)` = event_str,
          `Logrank p` = p_logrank,
          `Unadjusted HR [95% CI]` = hr_unadj,
          `Unadj. p` = p_unadj,
          `Adjusted HR [95% CI]` = hr_adj,
          `Adj. p` = p_adj,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(summary_df)
} 

#Use example:
 label_map <- c("hyperfiltration_egfr" = "Hyperfiltration(eGFR)",
                "Endpoint" = "Endpunkt",
                "da_daten" = "Gesampopulation",
                "N..0...1" = "n 0/1",
                "Events..0...1" = "Events 0/1"
                
)

 
 summary <- create_survival_summary_direct(datasets, variables, surv_times, endpoints, confounders, label_map)
 flextable::flextable(summary)

it dows work but i'd like to make a few adjustments 
1) add asterisks to significant p values 
2) create a seperate table for the adjusted model that also includes the confunders 
3) create a piece of code that allows exporting the table to word 
4)adjust some column names in the table (given inside lable_map but that doesnt work on column names)

## Prompt 2

Error in `mutate()`:
ℹ In argument: `stars = get_stars(p_val)`.
Caused by error in `if (is.na(p)) ...`:
! Bedingung hat Länge > 1
Run `rlang::last_trace()` to see where the error occurred.
Warnmeldungen:
1: In coxph(as.formula(adj_formula), data = cc) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinit
i get this, my approach before was to only use the summaries of my working analysis tool given here :
list_daten <- list(
  da_daten = da_daten
  #,subset_minusCKD = subset_minusCKD 
)

list_var <- list(
  "hyperfiltration_egfr"
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
  #"Hyperfiltration140_1_ja",
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
print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$kaplan_meier_plot)

# Access Log-Rank Test
print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$logrank_test)

# Access Unadjusted Cox Model
print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$unadjusted_cox_model)

# Access Adjusted Cox Model
print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$adjusted_cox_model)

# Access Cox Forest Plot
print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$cox_forest_plot)
to create the table 
would that approach work too?

## Prompt 3

# 2) Extract Adjusted Results (all terms)
>  adj_df <- imap_dfr(results, ~ {
+    sa <- .x$adjusted_cox_model
+    # use broom to get all coefficients + conf.int
+    tidy(sa, exponentiate = TRUE, conf.int = TRUE) %>%
+      mutate(
+        Dataset  = .y %>% str_split("_") %>% pluck(1),
+        Endpoint = .y %>% str_split("_") %>% pluck(3)
+      ) %>%
+      select(Dataset, Endpoint, Variable = term,
+             HR = estimate, CI_lo = conf.low, CI_hi = conf.high,
+             p.value)
+  })
Error in `map2()`:
ℹ In index: 1.
ℹ With name: da_daten_hyperfiltration_egfr_tot_1.
Caused by error:
! No tidy method for objects of class summary.coxph
Run `rlang::last_trace()` to see where the error occurred.

## Prompt 4

im affraid my raw models are not usable as there seems to be a problem with how they are stored

## Prompt 5

Error in `df[, req_cols]`:
! Can't subset columns with `req_cols`.
✖ `req_cols` must be logical, numeric, or character, not a list.

## Prompt 6

Error in `transmute()`:
ℹ In argument: `Endpoint`.
Caused by error:
! Objekt 'Endpoint' nicht gefunden

## Prompt 7

Error in `transmute()`:
ℹ In argument: `Endpoint`.
Caused by error:
! Objekt 'Endpoint' nicht gefunden
Run `rlang::last_trace()` to see where the error occurred.
Warnmeldungen:
1: In coxph(as.formula(adj_formula), data = d0) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
>
