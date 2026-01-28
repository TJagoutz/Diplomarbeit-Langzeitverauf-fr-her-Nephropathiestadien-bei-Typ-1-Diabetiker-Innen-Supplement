# Normality and Statistical Tests

## Prompt 1

list_var <- c("endpoint_reached_ja_1")  # Dependent variables
confunder_list <- c("RRsyst", "highest_measured_hba1c_naisna", "BMI", "UN", "Diabetesdauer_totwenntot_a_tim","Geschlecht_1_m...29", "Alter_beitotwenntot_a", "ProteinJa_1", "NBZ", "ppBZ"  )  # Independent variables
logreg_fun <- function(datensatz, list_var, confunder_list) {
  
  logreg_results <- list()
  
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

logreg_results <- logreg_fun(datensatz, list_var, confunder_list)
# **Access results for a specific variable**
print(logreg_results[["tot_1"]])
print(logreg_results[["endpoint_reached_1_ja"]])


I have this, when using tot_1 as dependend variable i get a proper result, however when unsing endpoint_reached_ja_1 (which exists and is used just above for a chi squared) i get NULL when trying to print the results

## Prompt 2

can you give me a line of code that simply gives back the number of NA for a variable

## Prompt 3

exp(cbind(OR = coef(model_unadjusted), confint(model_unadjusted)))what does this do?

## Prompt 4

concerning your forest plot answer from before, could you please generate examples how forrest polots with those different packages could look like

## Prompt 5

please an aexample for thesee options: You want to show...	Use...	Notes
Crude vs adjusted OR for one variable	ggplot2 or base R	Simple, clean
ORs for many predictors	forestplot	Classic clinical-style table
Very publication-ready plots	ggplot2 + ggpubr	Clean themes

## Prompt 6

for my forest plot sadly the variable name reaches into the other information, can you give me a hint how to change the code accordingly (best would be to add a name/name correspondece list in the begining to change the variable name in the plot to more normal sounding names

## Prompt 7

if (!is.null(adjusted_cox_model)) {
          cox_summary <- summary(adjusted_cox_model)
          hr_values <- exp(cox_summary$coefficients[, "coef"]) # Extract Hazard Ratios
          ci_lower <- exp(cox_summary$conf.int[, "lower .95"])  # Lower 95% CI
          ci_upper <- exp(cox_summary$conf.int[, "upper .95"])  # Upper 95% CI
     
          
          # Check for extreme values
          if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
            warning("⚠️ Skipping forest plot due to extreme HR values")
            forest_plot <- NULL
          } else {
            forest_plot <- tryCatch(
              ggforest(adjusted_cox_model, data =  model.frame(adjusted_cox_model),
                       main = paste0("Cox Forest Plot: ", dataset_name, " | ", var, " ~ ", event_var)),
              error = function(e) {
                warning("⚠️ Forest plot failed for ", dataset_name, " - ", var, " with error: ", e$message)
                return(NULL)
              }
            )
          }
        } else {
          forest_plot <- NULL
        }
thats my forest plot function, where exactly do i make the adjustments?

## Prompt 8

when i change  data= model.frame to date=model_data i get NULL instead of a plot

## Prompt 9

# 1️⃣ Define this at the beginning of your script or function
label_map <- c(
  "Hyperfiltration140_1_ja1" = "Hyperfiltration",
  "RRsyst_completed_mean" = "Systolic Blood Pressure",
  "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
  "BMI_completed_mean" = "BMI",
  "UN_completed_mean" = "BUN",
  "Diabetesdauer_totwenntot_a_tim" = "Diabetes Duration",
  "Geschlecht_1_m...29" = "Male Sex",
  "Alter_beitotwenntot_a" = "Age",
  "ProteinJa_1" = "Proteinuria",
  "NBZ_completed_mean" = "Fasting Glucose",
  "ppBZ_completed_mean" = "Postprandial Glucose"
)

# 2️⃣ Insert this in your forest plot section before ggforest()
if (!is.null(adjusted_cox_model)) {
  cox_summary <- summary(adjusted_cox_model)
  hr_values <- exp(cox_summary$coefficients[, "coef"]) # Extract Hazard Ratios
  ci_lower <- exp(cox_summary$conf.int[, "lower .95"])  # Lower 95% CI
  ci_upper <- exp(cox_summary$conf.int[, "upper .95"])  # Upper 95% CI

  if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
    warning("⚠️ Skipping forest plot due to extreme HR values")
    forest_plot <- NULL
  } else {
    # ✅ Rename variables in model.frame() just for the plot
    model_data <- model.frame(adjusted_cox_model)
    names(model_data) <- ifelse(names(model_data) %in% names(label_map),
                                label_map[names(model_data)],
                                names(model_data))

    forest_plot <- tryCatch(
      ggforest(adjusted_cox_model, data = model_data,
               main = paste0("Cox Forest Plot: ", dataset_name, " | ", var, " ~ ", event_var)),
      error = function(e) {
        warning("⚠️ Forest plot failed for ", dataset_name, " - ", var, " with error: ", e$message)
        return(NULL)
      }
    )
  }
} else {
  forest_plot <- NULL
}

i ment this returns NULL

## Prompt 10

am i right that now nothing in the function changed, we simply introduced model data, and do nothing with it?

## Prompt 11

so there is no way to change the lables in ggforest?

## Prompt 12

can i change lables and ami in r afterwards?

## Prompt 13

what about this approach?
library(forestmodel)
library(survival)
labelled::var_label(colon) <- list(
  sex = "Sex", #this variable is a numeric -> label works
  rx = "RX", # factor -> label doesn't work!
  adhere = "ADHERE" # numeric -> label works...
)

model <- coxph(Surv(time, status) ~ sex + rx + adhere, data = colon)

print(forest_model(model)) 
#> Warning in recalculate_width_panels(panel_positions, mapped_text =
#> mapped_text, : Unable to resize forest panel to be smaller than its heading;
#> consider a smaller text size

## Prompt 14

can you adjust my code accordingly

## Prompt 15

ehler in var_label(model_data) <- label_map[names(model_data)] : 
  konnte Funktion "var_label<-" nicht finden

## Prompt 16

# Access Cox Forest Plot
> print(results[["da_daten_Hyperfiltration140_1_ja_endpoint_reached_ja_1"]]$cox_forest_plot)
NULL

## Prompt 17

In value[[3L]](cond) :
  ⚠️ forest_model() failed for da_daten - Hyperfiltration140_1_ja with error: unbenutztes Argument (data = model_data)

i did get this warning, i guess the problem is that somehow there is an issue with passing on model.frame(adjusted_cox_model) to model_data, 
could it work like  var_label(model.frame(adjusted_cox_model)) <- label_map[names(model_data)] without model_data or did i understand something wrong

## Prompt 18

it did generate a plot now, however the lables are unchanged and the scaling of the HR needs to include smaller steps

## Prompt 19

⚠️ forest_model() failed for da_daten - Hyperfiltration140_1_ja with error: konnte Funktion "forest_model_theme" nicht finden

## Prompt 20

forest_model() failed for da_daten - Hyperfiltration140_1_ja with error: 'forest_model_theme' ist kein exportiertes Objekt aus 'namespace:forestmodel'
>

## Prompt 21

can i change the version of forestmodel im using?

## Prompt 22

dank ich habe bereits einen forest plot mit folgendem code:

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
allerdings inst die x achse etwas eigenartig skaliert, sodass auf der linken seite 0.5 als nächste index folgt während rechts bei gleichem Abstand 2 ist das eine normale form der x achse bei forest plots oder sollte ich das anpassen, sodass gleiche zahlensprünge von gleichen abständen begleitet sind?

## Prompt 23

for my explorative Analysis I am doing survuval analysis and unadjusted as well as adjusted cox proportional hazard models with a forest plot for the latter,
Sadly the unadjusted cox model as well as the forest plot do not work due to extreme HR values 
the code i have ist this:
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
            warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
        
        # ---- 6️⃣ Cox Forest Plot ----
        #print(("GGFOREST TEST"))
        #print(names(adjusted_cox_model$coefficients))
        #print(names(daten_filtered))
        #print(setdiff(list_confounder, names(daten_filtered)))
        #print("GGFORTES TEST ENDE")
        
        #mutate daten in logarythmische form um extreme values zu minimieren
       daten_filtered <- daten_filtered %>%
          mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  
        
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

## Prompt 24

if (any(ci_upper > 1e5)) {
  warning("⚠️ Some CIs are extremely wide. Plotting anyway with capped axes.")
  ci_upper <- pmin(ci_upper, 100)
}

id like to use this part where do i have to insert it, and can i then skip the mutate part?

## Prompt 25

error = function(e) {
  warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
  message("💥 Error message: ", e$message)
  message("📊 Dataset size: ", nrow(daten_filtered))
  message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
  return(NULL)
} where do i add this?

## Prompt 26

daten_filtered <- daten_filtered %>%
          mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  do i still need this? ( i could comment it to make it available if needed)

## Prompt 27

[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 295"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
[1] "✅ Successfully stored results for: da_daten_hyperfiltration_egfr_tot_1"
[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 295"
[1] "✅ Successfully stored results for: da_daten_hyperfiltration_egfr_NE_1_ja"
[1] "✅ Kaplan-Meier Model Summary:"
[1] "✅ ggplot erfolgreich erstellt."
[1] "📊 Rows before filtering: 324 | After filtering: 295"
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
New names:
• `Geschlecht_1_m...29` -> `Geschlecht_1_m`
[1] "✅ Successfully stored results for: da_daten_hyperfiltration_egfr_endpoint_reached_ja_1"
[1] "Namen der Ergebnisse: da_daten_hyperfiltration_egfr_tot_1"                 "Namen der Ergebnisse: da_daten_hyperfiltration_egfr_NE_1_ja"              
[3] "Namen der Ergebnisse: da_daten_hyperfiltration_egfr_endpoint_reached_ja_1"
Warnmeldungen:
1: In .add_surv_median(p, fit, type = surv.median.line, fun = fun,  :
  Median survival not reached.
2: In .add_surv_median(p, fit, type = surv.median.line, fun = fun,  :
  Median survival not reached.
3: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Skipping forest plot due to extreme HR values
4: In .add_surv_median(p, fit, type = surv.median.line, fun = fun,  :
  Median survival not reached.
> # Access Adjusted Cox Model
> print(results[["da_daten_hyperfiltration_egfr_ja_endpoint_reached_ja_1"]]$adjusted_cox_model)
NULL
> # Access Cox Forest Plot
> print(results[["da_daten_hyperfiltration_egfr_ja_endpoint_reached_ja_1"]]$cox_forest_plot)
NULL 
thats my output

## Prompt 28

table_summary <- create_survival_summary(results, variables = "hyperfiltration_egfr")
Fehler: No tidy method for objects of class summary.coxph

## Prompt 29

will it not brak my original function?

## Prompt 30

adjusted_cox_summary <- summary(adjusted_cox_model)  so this i delete?

## Prompt 31

results[[result_key]] <- list(
          kaplan_meier_plot = km_plot,
          logrank_test = logrank_test,
          unadjusted_cox_model = summary(unadjusted_cox_summary),
          unadjusted_cox_model_raw = unadjusted_cox_model,
          adjusted_cox_model = summary(adjusted_cox_summary),
          adjusted_cox_model_raw = adjusted_cox_model,                  
          cox_forest_plot = forest_plot, 
is it possible like that

## Prompt 32

I now have this function

## Prompt 33

# 🧠 Create summary table from results list
create_survival_summary <- function(results, variables = NULL) {
  summary_df <- data.frame()
  
  for (res_name in names(results)) {
    if (!is.null(variables) && !any(stringr::str_detect(res_name, variables))) {
      next  # skip if not matching requested variable
    }
    
    res <- results[[res_name]]
    
    if (is.null(res$adjusted_cox_model) || is.null(res$unadjusted_cox_model)) next
    
    # Extract name info
    parts <- strsplit(res_name, "_")[[1]]
    endpoint <- tail(parts, 1)
    var <- parts[length(parts) - 1]
    
    # Logrank p
    p_logrank <- tryCatch({
      round(1 - pchisq(res$logrank_test$chisq, df = 1), 3)
    }, error = function(e) NA)
    
    # Get N and event counts per group
    tab <- tryCatch({
      table(get(var, res$adjusted_cox_model$model), get(endpoint, res$adjusted_cox_model$model))
    }, error = function(e) matrix(NA, 2, 2))
    
    n_str <- paste0(colSums(tab, na.rm = TRUE), collapse = " / ")
    event_str <- paste0(tab[, 2], collapse = " / ")
    
    # Unadjusted
    unadj <- broom::tidy(res$unadjusted_cox_model_raw, exponentiate = TRUE, conf.int = TRUE)
    hr_unadj <- sprintf("%.2f [%.2f–%.2f]", unadj$estimate[1], unadj$conf.low[1], unadj$conf.high[1])
    p_unadj <- signif(unadj$p.value[1], 3)
    
    # Adjusted
    adj <- broom::tidy(res$adjusted_cox_model, exponentiate = TRUE, conf.int = TRUE)
    row_match <- which(grepl(var, adj$term))
    if (length(row_match) == 0) next
    hr_adj <- sprintf("%.2f [%.2f–%.2f]", adj$estimate[row_match], adj$conf.low[row_match], adj$conf.high[row_match])
    p_adj <- signif(adj$p.value[row_match], 3)
    
    summary_df <- rbind(summary_df, data.frame(
      Variable = var,
      Endpoint = endpoint,
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
  
  return(summary_df)
}
# 👇 Optional: filter for just one variable (e.g. "hyperfiltration")
table_summary <- create_survival_summary(results, variables = "hyperfiltration_egfr")

# View table
print(table_summary)

# Optional: export to Word (if you have flextable/officer installed)
# install.packages("flextable")
library(flextable)
flextable(table_summary)


And this function

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
            error = function(e) {
              warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
              message("💥 Error message: ", e$message)
              message("📊 Dataset size: ", nrow(daten_filtered))
              message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
              return(NULL)
            }
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

I get this

> table_summary <- create_survival_summary(results, variables = "hyperfiltration_egfr")
Fehler in signif(unadj$p.value[1], 3) : 
  Nicht-numerisches Argument für mathematische Funktion
Zusätzlich: Warnmeldungen:
1: Unknown or uninitialised column: `estimate`. 
2: Unknown or uninitialised column: `conf.low`. 
3: Unknown or uninitialised column: `conf.high`. 
4: Unknown or uninitialised column: `p.value`. 
>

## Prompt 34

You should also do the same kind of check for the adjusted model (right after the adj <- broom::tidy(...) part).
 can you write this for me pls

## Prompt 35

#save explorative analysis as table to word 


# 📦 Required packages

# 🧠 Create summary table from results list
create_survival_summary <- function(results, variables = NULL) {
  summary_df <- data.frame()
  
  for (res_name in names(results)) {
    if (!is.null(variables) && !any(stringr::str_detect(res_name, variables))) {
      next  # skip if not matching requested variable
    }
    
    res <- results[[res_name]]
    
    if (is.null(res$adjusted_cox_model) || is.null(res$unadjusted_cox_model)) next
    
    # Extract name info
    parts <- strsplit(res_name, "_")[[1]]
    endpoint <- tail(parts, 1)
    var <- parts[length(parts) - 1]
    
    # Logrank p
    p_logrank <- tryCatch({
      round(1 - pchisq(res$logrank_test$chisq, df = 1), 3)
    }, error = function(e) NA)
    
    # Get N and event counts per group
    tab <- tryCatch({
      table(get(var, res$adjusted_cox_model$model), get(endpoint, res$adjusted_cox_model$model))
    }, error = function(e) matrix(NA, 2, 2))
    
    n_str <- paste0(colSums(tab, na.rm = TRUE), collapse = " / ")
    event_str <- paste0(tab[, 2], collapse = " / ")
    
    # Unadjusted
    # Unadjusted
    unadj <- tryCatch(
      broom::tidy(res$unadjusted_cox_model_raw, exponentiate = TRUE, conf.int = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(unadj) || nrow(unadj) == 0 || anyNA(unadj[1, c("estimate", "conf.low", "conf.high", "p.value")])) {
      next  # Skip this result if no valid unadjusted model output
    }
    
    hr_unadj <- sprintf("%.2f [%.2f–%.2f]", unadj$estimate[1], unadj$conf.low[1], unadj$conf.high[1])
    p_unadj <- signif(unadj$p.value[1], 3)
    
    
    # Adjusted
    adj <- tryCatch(
      broom::tidy(res$adjusted_cox_model, exponentiate = TRUE, conf.int = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(adj)) {
      message("⚠️ Skipping due to error in adjusted model tidy(): ", res_name)
      next
    }
    
    row_match <- which(grepl(var, adj$term))
    if (length(row_match) == 0 ||
        anyNA(adj[row_match, c("estimate", "conf.low", "conf.high", "p.value")])) {
      message("⚠️ Skipping due to missing or invalid adjusted results: ", res_name)
      next
    }
    
    hr_adj <- sprintf("%.2f [%.2f–%.2f]",
                      adj$estimate[row_match],
                      adj$conf.low[row_match],
                      adj$conf.high[row_match])
    p_adj <- signif(adj$p.value[row_match], 3)
    
    
    summary_df <- rbind(summary_df, data.frame(
      Variable = var,
      Endpoint = endpoint,
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
  
  return(summary_df)
}
# 👇 Optional: filter for just one variable (e.g. "hyperfiltration")
table_summary <- create_survival_summary(results, variables = "hyperfiltration_egfr")

# View table
print(table_summary)

# Optional: export to Word (if you have flextable/officer installed)
# install.packages("flextable")
library(flextable)
flextable(table_summary)

I have this now and get a df with 0 columns and 0 rows
please check my survival analysis function again and see that everything is accessed correctly

## Prompt 36

can you give me the updated function?

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

just for context, thats how i call my results

## Prompt 37

its a 0x0 table

## Prompt 38

please just give me the updated model

## Prompt 39

📦 Checking result: da_daten_hyperfiltration_egfr_tot_1
❌ Skipping da_daten_hyperfiltration_egfr_tot_1: missing raw model objects
📦 Checking result: da_daten_hyperfiltration_egfr_NE_1_ja
❌ Skipping da_daten_hyperfiltration_egfr_NE_1_ja: missing raw model objects
📦 Checking result: da_daten_hyperfiltration_egfr_endpoint_reached_ja
ans skipping again

wouldnt it be easier to create the data in this function again

## Prompt 40

yes please, i want to be able to adujust datasets and variables idaely (mby byx creating a list of each and running through loops?

## Prompt 41

1: In coxph(as.formula(adj_formula), data = df) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite

## Prompt 42

yeah please give me an updaetd version

## Prompt 43

datasets <- list("da_daten" = da_daten)
variables <- c("hyperfiltration_egfr")
surv_times <- list("Überlebenszeit" = "Alter_beitotwenntot_a",#_rounded
                   "Zeit bis Endpunkt"="time_to_endpoint_a_tim")
endpoints <- list("tot_event" = "tot_1",
                  "endpoint_event" = "endpoint_reached_ja_1")
confounders <- c("Hyperfiltration140_1_ja",
                    "RRsyst_completed_mean",
                    "highest_measured_hba1c_naisna_completed_mean",
                    "BMI_completed_mean",
                    "UN_completed_mean",
                    "Diabetesdauer_totwenntot_a_tim",
                    "Geschlecht_1_m...29", "Alter_beitotwenntot_a",
                    "ProteinJa_1", "NBZ_completed_mean",
                    "ppBZ_completed_mean")

is that a viable way to use the lsits?

## Prompt 44

1: In coxph(as.formula(adj_formula), data = df, control = coxph.control(iter.max = 50)) :
  a variable appears on both the left and right sides of the formula
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Ran out of iterations and did not converge
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  one or more coefficients may be infinite
4: Unknown or uninitialised column: `term`.

## Prompt 45

lets try something different first, i might come back here 
but id do have a surviavl analysis with a solid output
can you wirite a function to save those outputs as cvs and then we'll create the table in colab

## Prompt 46

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
            error = function(e) {
              warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
              message("💥 Error message: ", e$message)
              message("📊 Dataset size: ", nrow(daten_filtered))
              message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
              return(NULL)
            }
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

please adjust eht cvs function for the results (non graph results) of this function

## Prompt 47

where  is the path?

## Prompt 48

No data to export. Check your model outputs or variable filter.

## Prompt 49

1)yes
2)same results
3) it does, my names(result) is e-g- da_daten_hyperfiltration_egfr_endpoint_reached_ja_1, and my filter is variables = "hyperfiltration_egfr"
4) gives a 0x0 table

## Prompt 50

it does not include hyperrfiltratiohn_egfr as confounder

## Prompt 51

is there a way to export the cvs files from the outputs i am getting in my original function without broom

## Prompt 52

yes

## Prompt 53

there seems to be a problem with how i save my cox moled_raw as names(...model_raw) put s out NULL 
does this last aproach utilize my raw models?

## Prompt 54

okay how do i execute the latest cvs sving function=

## Prompt 55

names(results[["your_result_name"]]$adjusted_cox_model_raw)
this gives back NULL
 Im saving the model like
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
            error = function(e) {
              warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var)
              message("💥 Error message: ", e$message)
              message("📊 Dataset size: ", nrow(daten_filtered))
              message("📉 Events: ", sum(daten_filtered[[event_var]] == 1, na.rm = TRUE))
              return(NULL)
            }
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

## Prompt 56

stilöl NULL

## Prompt 57

but why do i need the original if i just want to save the summary in a cvs?

## Prompt 58

no i want you to make a function where i can store my results and filter by variable/outcime using  this lists
datasets <- list("da_daten" = da_daten)
variables <- c("hyperfiltration_egfr")
surv_times <- list("Überlebenszeit" = "Alter_beitotwenntot_a",#_rounded
                   "Zeit bis Endpunkt"="time_to_endpoint_a_tim")
endpoints <- list("tot_event" = "tot_1",
                  "endpoint_event" = "endpoint_reached_ja_1")
confounders <- c("Hyperfiltration140_1_ja",
                    "RRsyst_completed_mean",
                    "highest_measured_hba1c_naisna_completed_mean",
                    "BMI_completed_mean",
                    "UN_completed_mean",
                    "Diabetesdauer_totwenntot_a_tim",
                    "Geschlecht_1_m...29", "Alter_beitotwenntot_a",
                    "ProteinJa_1", "NBZ_completed_mean",
                    "ppBZ_completed_mean")


table_summary <- create_survival_summary_direct(
  datasets = datasets,
  variables = variables,
  surv_times = surv_times,
  endpoints = endpoints,
  confounders = confounders
)

# View it
print(table_summary)
flextable::flextable(table_summary)
# View table
print(table_summary)

# Optional: export to Word (if you have flextable/officer installed)

flextable(table_summary)

## Prompt 59

ja bitte schreibe eine funktion

## Prompt 60

okay please chat gpt provide me with an R function that uses my summaries to provide me with a nice and publication fit table in the format you sugested above and can be adjusted by variable name and outcome

## Prompt 61

also please make it so that i can adjust the names insitde the table (as my variable names are a bit clunky at times)

## Prompt 62

very nice thanks
please add asteriscs to marc siginificant p values 
and provide me with a way to export it to word

## Prompt 63

yes please thats what i was asking
