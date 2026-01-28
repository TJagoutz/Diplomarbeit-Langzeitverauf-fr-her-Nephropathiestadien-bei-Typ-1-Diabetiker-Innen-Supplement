# Outlier Entfernen mit NAs

## Prompt 1

Hallo ich habe für eine statistische Datenverarbeitung einige Zeilen code, die Outlier entfernen sollen. Allerdings sollen die Fälle die NA Werte für die betreffende Variable haben nicht entfernt werden, ist das hier der Fall? #zum erstellen eines datensatzes, mit elimination von outliern aus mehreren variablen
remove_mult_outliers <- function(datensatz, spalten_subset) {
  merged_subset <- datensatz
  df_name <- "merged_outliers_" #+ df_original_name
  for (spalte in spalten_subset) {
    myfunc_spal_exist(spalte, datensatz)
    
    mean_val <- mean(datensatz[[spalte]], na.rm = TRUE)
    sd_val <- sd(datensatz[[spalte]], na.rm = TRUE)
    cutoff_up <- mean_val + 2 * sd_val
    cutoff_low <- mean_val - 2 * sd_val
    
    merged_subset <- subset(merged_subset, merged_subset[[spalte]] < cutoff_up & merged_subset[[spalte]] > cutoff_low)
  
    df_name <- paste(df_name, "_", spalte, sep = "")
  }
  print(df_name)
  assign(df_name, merged_subset, envir = .GlobalEnv)
}



remove_mult_outliers(datensatz, spalten_subset)

## Prompt 2

bei 2 bekomme ich > grep("Alter_a_einschluss", names(subset_minusCKD), value = TRUE)
[1] "Alter_a_einschluss"         "Alter_a_einschluss_rounded"
> grep("Geschlecht_1_m", names(subset_minusCKD), value = TRUE)
[1] "Geschlecht_1_m"     "Geschlecht_1_m_2_w"

## Prompt 3

Warnmeldungen:
1: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
2: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
3: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
4: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
5: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
6: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
> # Access Kaplan-Meier Plot
> print(results[["subset_minusCKD_HyperfiltrationMedian_1_ja_tot_1"]]$kaplan_meier_plot)
NULL
>  

es scheint das probleim zu sein, dass er die spalten nciht findet

## Prompt 4

nein die spalten sind in allen datensätzen identisch;
@ funktion ändern, ich möchte nicht die variablen übergehen, die ich habe, ich möchte, dass sie korrekt übergeben werden

## Prompt 5

bleibt bei dem code der access zu results gleich? weil: int(results[["subset_minusCKD_HyperfiltrationMedian_1_ja_tot_1"]]$kaplan_meier_plot)
NULL
> # Access Log-Rank Test
> print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$logrank_test)
NULL
> # Access Unadjusted Cox Model
> print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$unadjusted_cox_model)
NULL
> # Access Adjusted Cox Model
> print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$adjusted_cox_model)
NULL

## Prompt 6

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
+     "Diabetesdauer_a_" = "Diabetesdauer",
+     "Geschlecht_1_m" = "Geschlecht, 1=M",
+     "Alter_a_einschluss" = "Alter",
+     "ProteinJa_1" = "Proteinurie, 1=ja"
+   )
+   
+   for (dataset_name in names(list_daten)) {
+     datensatz <- list_daten[[dataset_name]]
+     names(datensatz) <- trimws(names(datensatz))  # sicherstellen, dass keine Leerzeichen Probleme machen
+     
+     for (var in list_var) {
+       if (!(var %in% names(datensatz))) {
+         warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
+         next
+       }
+       
+       if (!is.factor(datensatz[[var]])) {
+         datensatz[[var]] <- as.factor(datensatz[[var]])
+         message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
+       }
+       
+       for (i in seq_along(list_surv_time)) {
+         time_var <- unlist(list_surv_time)[i]
+         event_var <- unlist(list_endpoint)[i]
+         
+         required_cols <- c(time_var, event_var, var, unlist(list_confounder))
+         available_cols <- names(datensatz)
+         
+         # Robust: ignoriere Groß-/Kleinschreibung, Trim Whitespace
+         missing_cols <- required_cols[!tolower(required_cols) %in% tolower(available_cols)]
+         
+         if (length(missing_cols) > 0) {
+           warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
+           next
+         }
+         
+         # ---- Kaplan-Meier ----
+         km_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, "==1) ~", var)
+         )
+         
+         km_fit <- tryCatch(
+           surv_fit(km_formula, data = datensatz),  
+           error = function(e) {
+             warning(paste("❌ Kaplan-Meier fit failed:", e$message))
+             return(NULL)
+           }
+         )
+         
+         if (!is.null(km_fit)) {
+           km_plot <- tryCatch(
+             ggsurvplot(
+               km_fit,
+               data = datensatz,
+               censor.shape = "|", censor.size = 4,
+               title = paste0("Kaplan-Meier: ", dataset_name, " | ", var, " ~ ", event_var),
+               conf.int = TRUE,
+               pval = TRUE,
+               xlab = "Time in years",
+               break.time.by = 5,
+               surv.median.line = "hv",
+               cumevents = TRUE,
+               cumevents.height = 0.2,
+               ggtheme = theme_bw()
+             ),
+             error = function(e) {
+               warning(paste("❌ Kaplan-Meier plot failed:", e$message))
+               return(NULL)
+             }
+           )
+         } else {
+           km_plot <- NULL
+         }
+         
+         # ---- Daten filtern für Cox ----
+         daten_filtered <- datensatz[complete.cases(datensatz[ , required_cols]), ]
+         print(paste("📊 Rows before filtering:", nrow(datensatz), "| After filtering:", nrow(daten_filtered)))
+         
+         # ---- Logrank-Test ----
+         logrank_test <- tryCatch(
+           survdiff(Surv(daten_filtered[[time_var]], daten_filtered[[event_var]]) ~ daten_filtered[[var]]),
+           error = function(e) {
+             warning(paste("⚠️ Log-Rank Test failed:", e$message))
+             return(NULL)
+           }
+         )
+         
+         # ---- Unadjusted Cox ----
+         unadjusted_cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var)
+         )
+         
+         unadjusted_cox_model <- tryCatch(
+           coxph(unadjusted_cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning(paste("⚠️ Unadjusted Cox Model failed:", e$message))
+             return(NULL)
+           }
+         )
+         
+         # ---- Adjusted Cox ----
+         cox_formula <- as.formula(
+           paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(unlist(list_confounder), collapse = " + "))
+         )
+         
+         adjusted_cox_model <- tryCatch(
+           coxph(cox_formula, data = daten_filtered),
+           error = function(e) {
+             warning(paste("⚠️ Adjusted Cox Model failed:", e$message))
+             return(NULL)
+           }
+         )
+         
+         # ---- Forest Plot ----
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
+             model_data <- model.frame(adjusted_cox_model)
+             labelled::var_label(model_data) <- label_map[names(model_data)]
+             adjusted_cox_model$model <- model_data
+             
+             forest_plot <- tryCatch(
+               forest_model(adjusted_cox_model),
+               error = function(e) {
+                 warning("⚠️ forest_model() failed:", e$message)
+                 return(NULL)
+               }
+             )
+           }
+         } else {
+           forest_plot <- NULL
+         }
+         
+         # ---- Ergebnisse speichern ----
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
+   print(paste("📦 Ergebnisse:", names(results)))
+   return(results)
+ }
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
✅ Converting HyperfiltrationMedian_1_ja to factor in da_daten

✅ Converting HyperfiltrationMedian_1_ja to factor in subset_minusCKD

[1] "📦 Ergebnisse: "
Warnmeldungen:
1: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
2: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
3: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
4: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
5: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
6: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss

## Prompt 7

[1] "🔍 Prüfe required_cols: survival_time_age, tot_1, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
[1] "🔍 Prüfe required_cols: NE_surv_all_a_tim, NE_1_ja, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
[1] "🔍 Prüfe required_cols: survival_time_endpoint, endpoint_reached_ja_1, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
✅ Converting HyperfiltrationMedian_1_ja to factor in subset_minusCKD

[1] "🔍 Prüfe required_cols: survival_time_age, tot_1, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
[1] "🔍 Prüfe required_cols: NE_surv_all_a_tim, NE_1_ja, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
[1] "🔍 Prüfe required_cols: survival_time_endpoint, endpoint_reached_ja_1, HyperfiltrationMedian_1_ja, Hyperfiltration140_1_ja, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, BMI_completed_mean, UN_completed_mean, Diabetesdauer_a_, Geschlecht_1_m, Alter_a_einschluss, ProteinJa_1"
[1] "🧾 Verfügbare Spalten: ID, Platzhalter...2, Platzhalter...3, Platzhalter...4, geboren, untersucht, gestorben, OEDTRID, Sterbedatum, Sterbediagnose, LetzteBekannteTherapie, DatumLetzteTherapie, ZentrumLetzteTherapie, Beginn Nierenersatztherapie, Webokra zuletzt, Check Web, ...17, Alter_d_tim, Alter_a_tim, Alter_beitot_d, Alter_beitotwenntot_a, Kind_1, tot_1, DD_survival, survival, NE_1_ja, DD_NEsurvival, NEsurvival, Geschlecht_1_m...29, TX, TX_1_ja, TX_Datum, ...33, erst_NE_dat, erst_NE, NE_surv_d_tim, NE_surv_a_tim, Diabetesdauer_a_, Diabetesdauer_totwenntot_a_tim, Diabetesdauer_alslebend_a_tim, Manifestationsalter_a_, Alter_ber, Injektionsfrequenz, RRsyst, RRdiast, Groesse__m_, Gewicht__kg_, BMI, HbA1c...49, HbA1c...50, HbA1c...51, NBZ, ppBZ, Kreatinin_serum, UN, Kreatininclearance_endogen, KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_, Clearance_MDRD2, Geschlecht_1_m...59, Clear_80_1, ÖDGclearquart, RHC__ml_min_, GF__ml_, FF, Albumin_Harn__0_negativ__1_positiv_, Albumin_quantitativ, Mikroalbuminurie, OB_0_Mia1_maA2_fehlt3, MikroJa_1, ProteinJa_1, hypertonie_ja_1, ...72, Niere_1_OK_2_MiA_3_PU_4_NB, Hyperfiltration140_1_ja, Hyperfiltration130_1_ja, Hyperfiltration150_1_ja, HyperfiltrationMedian_1_ja, HyperfiltrationMedianohneausreißer_1_ja, endpoint_reached_ja_1, Höherer HbA1c AW/AX, highest_measured_hba1c, highest_measured_hba1c_naisna, survivaltime_a_tim, NE_surv_all_a_tim, time_to_endpoint_a_tim, ...86, NOTIZEN, ...88, ...89, ...90, ...91, egfr_ckdepi2009_tim, min(scr/K)oder 1, alpha, max(scr/K)oder1, ´-1,012 wenn frau, Geschlechtsfaktor, ...98, hyperfiltration_egfr, ...100, study_begin_to_death_D, survival_time_age, survival_time_endpoint, aktuelles Wissem lebend, aktuelles Wissen Lebend Zahelcode, geboren_clean, gestorben_clean, Sterbedatum_clean, DatumLetzteTherapie_clean, Beginn Nierenersatztherapie_clean, Webokra zuletzt_clean, TX_Datum_clean, erst_NE_dat_clean, Alter_beitotwenntot_a_rounded, Diabetesdauer_totwenntot_a_tim_rounded, NE_surv_a_tim_rounded, RRsyst_completed_mean, highest_measured_hba1c_naisna_completed_mean, ppBZ_completed_mean, NBZ_completed_mean, BMI_completed_mean, UN_completed_mean"
[1] "📦 Ergebnisse: "
Warnmeldungen:
1: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
2: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
3: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in da_daten : Geschlecht_1_m, Alter_a_einschluss
4: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
5: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss
6: In analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,  :
  ⚠️ Missing required columns in subset_minusCKD : Geschlecht_1_m, Alter_a_einschluss

## Prompt 8

> for (data in list_daten) {
+   print(colnames(data))
+ }
  [1] "ID"                                                     
  [2] "Platzhalter...2"                                        
  [3] "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                        
  [5] "geboren"                                                
  [6] "untersucht"                                             
  [7] "gestorben"                                              
  [8] "OEDTRID"                                                
  [9] "Sterbedatum"                                            
 [10] "Sterbediagnose"                                         
 [11] "LetzteBekannteTherapie"                                 
 [12] "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                  
 [14] "Beginn Nierenersatztherapie"                            
 [15] "Webokra zuletzt"                                        
 [16] "Check Web"                                              
 [17] "...17"                                                  
 [18] "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                            
 [20] "Alter_beitot_d"                                         
 [21] "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                 
 [23] "tot_1"                                                  
 [24] "DD_survival"                                            
 [25] "survival"                                               
 [26] "NE_1_ja"                                                
 [27] "DD_NEsurvival"                                          
 [28] "NEsurvival"                                             
 [29] "Geschlecht_1_m...29"                                    
 [30] "TX"                                                     
 [31] "TX_1_ja"                                                
 [32] "TX_Datum"                                               
 [33] "...33"                                                  
 [34] "erst_NE_dat"                                            
 [35] "erst_NE"                                                
 [36] "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                          
 [38] "Diabetesdauer_a_"                                       
 [39] "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                          
 [41] "Manifestationsalter_a_"                                 
 [42] "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                     
 [44] "RRsyst"                                                 
 [45] "RRdiast"                                                
 [46] "Groesse__m_"                                            
 [47] "Gewicht__kg_"                                           
 [48] "BMI"                                                    
 [49] "HbA1c...49"                                             
 [50] "HbA1c...50"                                             
 [51] "HbA1c...51"                                             
 [52] "NBZ"                                                    
 [53] "ppBZ"                                                   
 [54] "Kreatinin_serum"                                        
 [55] "UN"                                                     
 [56] "Kreatininclearance_endogen"                             
 [57] "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                        
 [59] "Geschlecht_1_m...59"                                    
 [60] "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                          
 [62] "RHC__ml_min_"                                           
 [63] "GF__ml_"                                                
 [64] "FF"                                                     
 [65] "Albumin_Harn__0_negativ__1_positiv_"                    
 [66] "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                       
 [68] "OB_0_Mia1_maA2_fehlt3"                                  
 [69] "MikroJa_1"                                              
 [70] "ProteinJa_1"                                            
 [71] "hypertonie_ja_1"                                        
 [72] "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                             
 [74] "Hyperfiltration140_1_ja"                                
 [75] "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                
 [77] "HyperfiltrationMedian_1_ja"                             
 [78] "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                  
 [80] "Höherer HbA1c AW/AX"                                    
 [81] "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                          
 [83] "survivaltime_a_tim"                                     
 [84] "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                 
 [86] "...86"                                                  
 [87] "NOTIZEN"                                                
 [88] "...88"                                                  
 [89] "...89"                                                  
 [90] "...90"                                                  
 [91] "...91"                                                  
 [92] "egfr_ckdepi2009_tim"                                    
 [93] "min(scr/K)oder 1"                                       
 [94] "alpha"                                                  
 [95] "max(scr/K)oder1"                                        
 [96] "´-1,012 wenn frau"                                      
 [97] "Geschlechtsfaktor"                                      
 [98] "...98"                                                  
 [99] "hyperfiltration_egfr"                                   
[100] "...100"                                                 
[101] "study_begin_to_death_D"                                 
[102] "survival_time_age"                                      
[103] "survival_time_endpoint"                                 
[104] "aktuelles Wissem lebend"                                
[105] "aktuelles Wissen Lebend Zahelcode"                      
[106] "geboren_clean"                                          
[107] "gestorben_clean"                                        
[108] "Sterbedatum_clean"                                      
[109] "DatumLetzteTherapie_clean"                              
[110] "Beginn Nierenersatztherapie_clean"                      
[111] "Webokra zuletzt_clean"                                  
[112] "TX_Datum_clean"                                         
[113] "erst_NE_dat_clean"                                      
[114] "Alter_beitotwenntot_a_rounded"                          
[115] "Diabetesdauer_totwenntot_a_tim_rounded"                 
[116] "NE_surv_a_tim_rounded"                                  
[117] "RRsyst_completed_mean"                                  
[118] "highest_measured_hba1c_naisna_completed_mean"           
[119] "ppBZ_completed_mean"                                    
[120] "NBZ_completed_mean"                                     
[121] "BMI_completed_mean"                                     
[122] "UN_completed_mean"                                      
  [1] "ID"                                                     
  [2] "Platzhalter...2"                                        
  [3] "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                        
  [5] "geboren"                                                
  [6] "untersucht"                                             
  [7] "gestorben"                                              
  [8] "OEDTRID"                                                
  [9] "Sterbedatum"                                            
 [10] "Sterbediagnose"                                         
 [11] "LetzteBekannteTherapie"                                 
 [12] "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                  
 [14] "Beginn Nierenersatztherapie"                            
 [15] "Webokra zuletzt"                                        
 [16] "Check Web"                                              
 [17] "...17"                                                  
 [18] "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                            
 [20] "Alter_beitot_d"                                         
 [21] "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                 
 [23] "tot_1"                                                  
 [24] "DD_survival"                                            
 [25] "survival"                                               
 [26] "NE_1_ja"                                                
 [27] "DD_NEsurvival"                                          
 [28] "NEsurvival"                                             
 [29] "Geschlecht_1_m...29"                                    
 [30] "TX"                                                     
 [31] "TX_1_ja"                                                
 [32] "TX_Datum"                                               
 [33] "...33"                                                  
 [34] "erst_NE_dat"                                            
 [35] "erst_NE"                                                
 [36] "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                          
 [38] "Diabetesdauer_a_"                                       
 [39] "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                          
 [41] "Manifestationsalter_a_"                                 
 [42] "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                     
 [44] "RRsyst"                                                 
 [45] "RRdiast"                                                
 [46] "Groesse__m_"                                            
 [47] "Gewicht__kg_"                                           
 [48] "BMI"                                                    
 [49] "HbA1c...49"                                             
 [50] "HbA1c...50"                                             
 [51] "HbA1c...51"                                             
 [52] "NBZ"                                                    
 [53] "ppBZ"                                                   
 [54] "Kreatinin_serum"                                        
 [55] "UN"                                                     
 [56] "Kreatininclearance_endogen"                             
 [57] "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                        
 [59] "Geschlecht_1_m...59"                                    
 [60] "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                          
 [62] "RHC__ml_min_"                                           
 [63] "GF__ml_"                                                
 [64] "FF"                                                     
 [65] "Albumin_Harn__0_negativ__1_positiv_"                    
 [66] "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                       
 [68] "OB_0_Mia1_maA2_fehlt3"                                  
 [69] "MikroJa_1"                                              
 [70] "ProteinJa_1"                                            
 [71] "hypertonie_ja_1"                                        
 [72] "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                             
 [74] "Hyperfiltration140_1_ja"                                
 [75] "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                
 [77] "HyperfiltrationMedian_1_ja"                             
 [78] "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                  
 [80] "Höherer HbA1c AW/AX"                                    
 [81] "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                          
 [83] "survivaltime_a_tim"                                     
 [84] "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                 
 [86] "...86"                                                  
 [87] "NOTIZEN"                                                
 [88] "...88"                                                  
 [89] "...89"                                                  
 [90] "...90"                                                  
 [91] "...91"                                                  
 [92] "egfr_ckdepi2009_tim"                                    
 [93] "min(scr/K)oder 1"                                       
 [94] "alpha"                                                  
 [95] "max(scr/K)oder1"                                        
 [96] "´-1,012 wenn frau"                                      
 [97] "Geschlechtsfaktor"                                      
 [98] "...98"                                                  
 [99] "hyperfiltration_egfr"                                   
[100] "...100"                                                 
[101] "study_begin_to_death_D"                                 
[102] "survival_time_age"                                      
[103] "survival_time_endpoint"                                 
[104] "aktuelles Wissem lebend"                                
[105] "aktuelles Wissen Lebend Zahelcode"                      
[106] "geboren_clean"                                          
[107] "gestorben_clean"                                        
[108] "Sterbedatum_clean"                                      
[109] "DatumLetzteTherapie_clean"                              
[110] "Beginn Nierenersatztherapie_clean"                      
[111] "Webokra zuletzt_clean"                                  
[112] "TX_Datum_clean"                                         
[113] "erst_NE_dat_clean"                                      
[114] "Alter_beitotwenntot_a_rounded"                          
[115] "Diabetesdauer_totwenntot_a_tim_rounded"                 
[116] "NE_surv_a_tim_rounded"                                  
[117] "RRsyst_completed_mean"                                  
[118] "highest_measured_hba1c_naisna_completed_mean"           
[119] "ppBZ_completed_mean"                                    
[120] "NBZ_completed_mean"                                     
[121] "BMI_completed_mean"                                     
[122] "UN_completed_mean"
