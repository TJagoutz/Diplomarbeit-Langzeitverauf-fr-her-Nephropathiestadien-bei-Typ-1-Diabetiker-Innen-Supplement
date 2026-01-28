# R Funktion für Tests

## Prompt 1

hallo, für meinen Datensatz möchte ich fehlende metrische werte mit dem Mittelwert der vorhanden werte füllen. eine entsprechende Funktion habe ich bereits.  
Aber um zu testen, ob das ein valider Ansatz ist möchte ich schaue, ob sich die Fälle mit vorhanden werten in gewissen merkmalen signifikant von den Fällen mit fehlenden Merkmalen unterscheiden. 
Könntest du mir bitte eine R Funktion schreiben, in der zunächst der genutzte datensatz, die Variable nach der die Gruppen eingeteilt werden (NA und Wert vorhanden) und die Variablen auf die getestet wird definiert werden kann, und dann ein entsprechende terst durchgeführt wird

## Prompt 2

ich brauche eigentlich nur metrische variable, aber es wäre toll wenn  sie auch kennzahlen (mean/median und SD returnen könnte)

## Prompt 3

bitte mit wilcoxon optioni

## Prompt 4

kansnt du mir den unterschied visualisierne

## Prompt 5

muss ich data unf die variablen zuvor definieren?

## Prompt 6

kannst du  mir bitte die variable definition schrieben, 
 die grup variablen sind :
"RRsyst",
  "highest_measured_hba1c_naisna",
  "BMI",
  "UN"

die test vairbablen sind 
Alter_a_einschluss und 
Kreatininclearance_endogen

## Prompt 7

ich habe noch das gefunden, macht das etwas ähnliches?compare_missing_summary <- function(data, variable_list, compare_vars) {
  results <- list()
  
  for (target_var in variable_list) {
    for (comp_var in compare_vars) {
      missing_group <- data %>% filter(is.na(.data[[target_var]]))
      nonmissing_group <- data %>% filter(!is.na(.data[[target_var]]))
      
      # Beachte: Nur wenn beide Gruppen genug Daten haben
      if (nrow(missing_group) >= 3 & nrow(nonmissing_group) >= 3) {
        tryCatch({
          test_result <- wilcox.test(missing_group[[comp_var]], nonmissing_group[[comp_var]])
          results[[length(results) + 1]] <- data.frame(
            target_variable = target_var,
            compared_parameter = comp_var,
            p_value = test_result$p.value,
            median_missing = median(missing_group[[comp_var]], na.rm = TRUE),
            median_nonmissing = median(nonmissing_group[[comp_var]], na.rm = TRUE),
            n_missing = sum(!is.na(missing_group[[comp_var]])),
            n_nonmissing = sum(!is.na(nonmissing_group[[comp_var]]))
          )
        }, error = function(e) {})
      }
    }
  }
  
  # ⚠️ Wichtig: Gib KEINE Liste zurück – sondern binde alles zu einem Dataframe!
  bind_rows(results)
}
library(dplyr)
variable_list <- list("RRsyst", "highest_measured_hba1c_naisna", "ppBZ", "NBZ", "BMI", "UN")

# Parameter zum Vergleich
compare_vars <- list("Kreatininclearance_endogen", "Alter_a_einschluss_rounded", 
                     "Manifestationsalter_a_", "Diabetesdauer_a__rounded",
                     "NE_surv_a_tim_rounded", "BMI", "RRsyst")

# Vergleich starten und Ergebnis in df speichern
df <- compare_missing_summary(da_daten, variable_list, compare_vars)

df_assessed <- df %>%
  mutate(
    p_value_numeric = as.numeric(p_value),
    difference_median =  abs(median_missing - median_nonmissing) / median_nonmissing,
    interpretierbar = as.numeric(n_missing) >= 5,
    vertretbar = case_when(
      is.na(p_value_numeric) ~ FALSE,
      interpretierbar == FALSE ~ FALSE,
      p_value_numeric > 0.05 & difference_median < 2 ~ TRUE,  # Schwelle für "praktisch relevant" anpassbar
      TRUE ~ FALSE
    ),
    kommentar = case_when(
      is.na(p_value_numeric) ~ "kein Test möglich",
      interpretierbar == FALSE ~ "zu wenige Fälle mit Missing",
      vertretbar ~ "Imputation mit Median ist vertretbar",
      TRUE ~ "Unterschied signifikant – besser keine einfache Imputation"
    )
  )


# Tabellenansicht in RStudio
View(df_assessed)

# Nach problematischen Variablen filtern
df_assessed %>% filter(vertretbar == FALSE)

# Optional: Export als Excel oder Word
write.csv(df_assessed, "beurteilung_imputation.csv", row.names = FALSE)


library(ggplot2)

ggplot(df_assessed, aes(x = target_variable, y = compared_parameter, fill = p_value_numeric)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.05, na.value = "grey80") +
  labs(title = "p-Werte für Unterschiede bei fehlenden Werten", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Prompt 8

bau sie bitte so um, dass für den Mittelwert der gleiche Test (sinngemäß) gemacht wird wie für den median und dann ebenfalls eine empfehlung ausgegeben wird.
Ausßerdem bräuchte ich dann eine zweite heatmap für mittelwerte(in blau gehalten)

## Prompt 9

variable_list <- c("RRsyst", "highest_measured_hba1c_naisna", "BMI", "UN")

for (i in variable_list) {
  print(mean(variable))
  print(median(variable))
}

## Prompt 10

compare_missing_summary_dual <- function(data, variable_list, compare_vars) {
  results <- list()
  
  for (target_var in variable_list) {
    for (comp_var in compare_vars) {
      missing_group <- data %>% filter(is.na(.data[[target_var]]))
      nonmissing_group <- data %>% filter(!is.na(.data[[target_var]]))
      
      if (nrow(missing_group) >= 3 & nrow(nonmissing_group) >= 3) {
        tryCatch({
          # Median-Test (Wilcoxon)
          wilcox_res <- wilcox.test(missing_group[[comp_var]], nonmissing_group[[comp_var]])
          
          # Mean-Test (t-Test)
          t_res <- t.test(missing_group[[comp_var]], nonmissing_group[[comp_var]])
          
          results[[length(results) + 1]] <- data.frame(
            target_variable = target_var,
            compared_parameter = comp_var,
            # p-Werte
            p_value_median = wilcox_res$p.value,
            p_value_mean   = t_res$p.value,
            # Kennzahlen
            median_missing = median(missing_group[[comp_var]], na.rm = TRUE),
            median_nonmissing = median(nonmissing_group[[comp_var]], na.rm = TRUE),
            mean_missing   = mean(missing_group[[comp_var]], na.rm = TRUE),
            mean_nonmissing= mean(nonmissing_group[[comp_var]], na.rm = TRUE),
            n_missing = sum(!is.na(missing_group[[comp_var]])),
            n_nonmissing = sum(!is.na(nonmissing_group[[comp_var]]))
          )
        }, error = function(e) {})
      }
    }
  }
  
  df <- bind_rows(results)
  
  # Einschätzung hinzufügen
  df <- df %>%
    mutate(
      diff_median = abs(median_missing - median_nonmissing) / median_nonmissing,
      diff_mean   = abs(mean_missing - mean_nonmissing) / mean_nonmissing,
      interpretierbar = n_missing >= 5,
      empfehlung_median = case_when(
        !interpretierbar ~ "zu wenige Fälle",
        p_value_median > 0.05 & diff_median < 0.2 ~ "Median-Imputation vertretbar",
        TRUE ~ "Median-Imputation fragwürdig"
      ),
      empfehlung_mean = case_when(
        !interpretierbar ~ "zu wenige Fälle",
        p_value_mean > 0.05 & diff_mean < 0.2 ~ "Mean-Imputation vertretbar",
        TRUE ~ "Mean-Imputation fragwürdig"
      )
    )
  
  return(df)
}


variable_list <- c("RRsyst", "highest_measured_hba1c_naisna", "BMI", "UN")
compare_vars <- c("Alter_a_einschluss", "Kreatininclearance_endogen")

df_res <- compare_missing_summary_dual(da_daten, variable_list, compare_vars)
View(df_res)

bei diesem bekomme ich nur Ergebnisse für RRsys, abenr niccht die anderen variablen in der liste

## Prompt 11

okay ich bekomme das, aber ich habe gerade manuell anchgeschaut und es git mehr NA fälle als 0 für diese Variablen

## Prompt 12

es klappt, nur bei UN ist nur 1 Foll missing, sodass ich keine ergebnisse bekomme

## Prompt 13

ggplot(df_res, aes(x = target_variable, y = compared_parameter, fill = p_value_median)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.05, na.value = "grey80") +
  labs(title = "p-Werte (Wilcoxon, Median)", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(df_res, aes(x = target_variable, y = compared_parameter, fill = p_value_mean)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey80") +
  labs(title = "p-Werte (t-Test, Mean)", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Ich habe zur visualisierung dieses hier, ansich ganzu gut aber ich möchte beide lieber ind iesem format:
ggplot(df_assessed, aes(x = target_variable, y = compared_parameter, fill = p_value_numeric)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.05, na.value = "grey80") +
  labs(title = "p-Werte für Unterschiede bei fehlenden Werten", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Prompt 14

Du:
ggplot(df_res, aes(x = target_variable, y = compared_parameter, fill = p_value_median)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.05, na.value = "grey80") +
  labs(title = "p-Werte (Wilcoxon, Median)", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(df_res, aes(x = target_variable, y = compared_parameter, fill = p_value_mean)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey80") +
  labs(title = "p-Werte (t-Test, Mean)", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Ich habe zur visualisierung dieses hier, ansich ganzu gut aber ich möchte beide lieber ind iesem format:
ggplot(df_assessed, aes(x = target_variable, y = compared_parameter, fill = p_value_numeric)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.05, na.value = "grey80") +
  labs(title = "p-Werte für Unterschiede bei fehlenden Werten", fill = "p-Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Prompt 15

wi kann ich die plots anzeigen?

## Prompt 16

ich habe diesen Datensatz df_res und möchte die differenz der mediane angeben, kannst du mir eine entspürechende funktion schreiben

## Prompt 17

ich führe mit folgenden funktionene chi2 und logistische regressionsanalysen mi adjustierten und nicht adjustierten Modell, sowie einen omnibus test durch: 
list_daten <- list(
  subset_minusCKD = subset_minusCKD, # Now accessed by name
  daten_ursp = daten_ursp
)

list_var <- list("Hyperfiltration140_1_ja",
                 "Hyperfiltration130_1_ja",
                 "Hyperfiltration150_1_ja",
                 "HyperfiltrationMedian_1_ja",
                 "Hyperfiltration120_1_ja"
  
)

chisquare_test_fun <- function(list_daten, list_var) {
  
  # Initialize lists to store results
  list_chisquare_results <- list()
  list_xtabs <- list()
  
  for (i in seq_along(list_daten)) {
    datensatz <- list_daten[[i]]
    
    # Get dataset name, fallback to "Dataset_X" if unnamed
    dataset_name <- ifelse(!is.null(names(list_daten)[i]) && names(list_daten)[i] != "", 
                           names(list_daten)[i], paste0("Dataset_", i))
    
    # Initialize nested lists for this dataset
    list_chisquare_results[[dataset_name]] <- list()
    list_xtabs[[dataset_name]] <- list()
    
    for (var in list_var) {
      # Ensure the variable exists in the dataset
      if (!(var %in% names(datensatz))) {
        warning(paste("Variable", var, "not found in", dataset_name, "Skipping..."))
        next
      }
      
      # Create contingency table
      table <- xtabs(~ datensatz[[var]] + datensatz[["endpoint_reached_ja_1"]])
      print(i)
      print(var)
      print(table)
      print(sum(table))
      print("NEXT")
      # Perform Chi-Square test
      chisq_res <- chisq.test(datensatz[[var]], datensatz[["endpoint_reached_ja_1"]])
      
      # Store results in lists
      list_chisquare_results[[dataset_name]][[var]] <- chisq_res
      list_xtabs[[dataset_name]][[var]] <- table
    }
  }
  
  return(list(xtabs = list_xtabs, chisq_results = list_chisquare_results))
}

# Example usage
chisq_results <- chisquare_test_fun(list_daten, list_var)

# **Access results**
print(chisq_results$chisq_results[["daten_ursp"]][["Hyperfiltration120_1_ja"]])   # Chi-square test for Hyperfiltration140_1_ja
print(chisq_results$chisq_results[["subset_minusCKD"]][["Hyperfiltration120_1_ja"]])    # Contingency table for the subset

colSums(is.na(datensatz))

#3.2.3)///logreg (explorativ)///
##LOGRANK TEST##
list_var <- c("endpoint_reached_ja_1")  # Dependent variables
confunder_list <- c("Hyperfiltration120_1_ja",
                    "RRsyst_completed_mean",
                    "highest_measured_hba1c_naisna_completed_mean",
                    "BMI_completed_mean",
                    "UN_completed_mean",
                    "Diabetesdauer_a_",
                    "Geschlecht_1_m", 
                    "Alter_a_einschluss",
                    "ProteinJa_1"
                    )  # Independent variables
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

datensatz= subset_minusCKD
logreg_results <- logreg_fun(datensatz, list_var, confunder_list)

# **Access results for a specific variable**
print(logreg_results[["tot_1"]])
print(logreg_results[["endpoint_reached_ja_1"]])
#print p values example
print(logreg_results[["endpoint_reached_ja_1"]][["omnibus_test"]][["p_value"]])
#print OR value and confidence intervalls
print(logreg_results[["endpoint_reached_ja_1"]][["oR"]])  

model_unadjusted <- glm(endpoint_reached_ja_1 ~ HyperfiltrationMedian_1_ja, 
                        family = binomial(), data = datensatz)

summary(model_unadjusted)
exp(cbind(OR = coef(model_unadjusted), confint(model_unadjusted)))


ich bekomme viele ergebnisse, brauche aber im wesentlichen folgendes: 
120:
Chi2 nicht signifikant X-squared = 0.11506, df = 1, p-value = 0.7345
Logrank:
Nicht adj: nicht signifikant 
Adj: nicht signifikant Hyperfiltration, signifikant für: 
HBA1c OR: 1.44 p= 0.023
Diabetesdauer OR:1.07 p= 0.029
Proteinurie OR: 2.50 p=0.040

Omnibus:
$omnibus_test$chi_square
[1] 42.35969
$omnibus_test$df
[1] 9
$omnibus_test$p_value
[1] 2.824506e-06
für jede variable aus list_var
kannst du mir eine funktion schreiben, die die funktionen für jede dieser variablen aufruft und mir dann das gewünschte output gibt? (beim adjustierten modell brauche ich OR und P für alle signifikanten Parameter, bei nicht signifikaten reicht "nicht signifikatn" und der P wert

## Prompt 18

danke aber musst du alles neu schreiben? die funktionen sind ja schon da, kannst du die nicht einfach ein einer loop aufruffen und dann die ergebnisse wie gewünscht managen?

## Prompt 19

atensatz[[var]]  0  1
               0 98  0
               1  0 88
[1] 186
[1] "NEXT"
[1] 2
[1] "endpoint_reached_ja_1"
                datensatz[["endpoint_reached_ja_1"]]
datensatz[[var]]   0   1
               0 158   0
               1   0 170
[1] 328atensatz[[var]]  0  1
               0 98  0
               1  0 88
[1] 186
[1] "NEXT"
[1] 2
[1] "endpoint_reached_ja_1"
                datensatz[["endpoint_reached_ja_1"]]
datensatz[[var]]   0   1
               0 158   0
               1   0 170
[1] 328
[1] "NEXT"
Waiting for profiling to be done...
Fehler in coef_unadj[2, "Pr(>|z|)"] : Indizierung außerhalb der Grenzen
Zusätzlich: Warnmeldungen:
1: In model.matrix.default(mt, mf, contrasts) :
  Antwortvariable erschien auf der rechten Seite und wurde verworfen
2: In model.matrix.default(mt, mf, contrasts) :
  Problem mit dem Term 1 in model.matrix: keine Spalten zugewiesen
> # Beispiel: Ergebnisse für "daten_ursp", Variable "Hyperfiltration120_1_ja"
> res_summary[["daten_ursp"]][["Hyperfiltration120_1_ja"]]
Fehler: Objekt 'res_summary' nicht gefunden 
[1] "NEXT"
Waiting for profiling to be done...
Fehler in coef_unadj[2, "Pr(>|z|)"] : Indizierung außerhalb der Grenzen
Zusätzlich: Warnmeldungen:
1: In model.matrix.default(mt, mf, contrasts) :
  Antwortvariable erschien auf der rechten Seite und wurde verworfen
2: In model.matrix.default(mt, mf, contrasts) :
  Problem mit dem Term 1 in model.matrix: keine Spalten zugewiesen
> # Beispiel: Ergebnisse für "daten_ursp", Variable "Hyperfiltration120_1_ja"
> res_summary[["daten_ursp"]][["Hyperfiltration120_1_ja"]]
Fehler: Objekt 'res_summary' nicht gefunden

## Prompt 20

gibt es eine ellegante möglichkeit aus eine se tabellarische darstellung der wichtigen statistischen kennzahlen aus dem bestehenden cod e(s.o.) zu erzeiugen?

## Prompt 21

kanns du mir daraus noch ienen hübschen word table bauen bitte

## Prompt 22

die tabelle ist nicht so gut, eventuell x und y achse flippen, oder hast du eine andere IDee?

## Prompt 23

bitte option B

## Prompt 24

return(list(tests = ft_tests, preds = ft_preds))
+ }
> # ---- Anwendung ----
> # res$table = deine Ergebnistabelle
> two_tables <- make_two_tables(res$table, "Cutoff_Analysen_Tables.docx")
Error in `select()`:
! Can't select columns that don't exist.
✖ Column `Chi2_stat` doesn't exist.
Run `rlang::last_trace()` to see where the error occurred.
>

## Prompt 25

names(res$table)
 [1] "cutoff_var"       "chi2_stat"        "chi2_df"          "chi2_p"           "unadj_p"          "HbA1c_OR"        
 [7] "HbA1c_p"          "Diabetesdauer_OR" "Diabetesdauer_p"  "Proteinurie_OR"   "Proteinurie_p"    "omnibus_p"    names(res$table)
 [1] "cutoff_var"       "chi2_stat"        "chi2_df"          "chi2_p"           "unadj_p"          "HbA1c_OR"        
 [7] "HbA1c_p"          "Diabetesdauer_OR" "Diabetesdauer_p"  "Proteinurie_OR"   "Proteinurie_p"    "omnibus_p"

## Prompt 26

names(res$table) [1] "cutoff_var" "chi2_stat" "chi2_df" "chi2_p" "unadj_p" "HbA1c_OR" [7] "HbA1c_p" "Diabetesdauer_OR" "Diabetesdauer_p" "Proteinurie_OR" "Proteinurie_p" "omnibus_p" names(res$table)

## Prompt 27

Fehler in `colnames<-`(`*tmp*`, value = .names) : 
  Versuch die 'colnames' für ein Objekt mit weniger als zwei Dimensionen zu setzen

## Prompt 28

Fehler in `colnames<-`(`*tmp*`, value = .names) : 
  Versuch die 'colnames' für ein Objekt mit weniger als zwei Dimensionen zu setzen

## Prompt 29

Fehler in `colnames<-`(`*tmp*`, value = .names) : 
  Versuch die 'colnames' für ein Objekt mit weniger als zwei Dimensionen zu setzen

## Prompt 30

klappt nicht, fangen wir nocheinmal von vorne an, vergiss für diesen prompt die letzzten 10 nachrichten
das ist mien code: 
# ===== Abhängigkeiten =====
library(dplyr)
library(glue)
library(purrr)

# ===== kleine Helfer =====

# finde den passenden Term-Namen im Modell (z.B. "var" ODER "var1")
.find_term <- function(model, var){
  terms <- setdiff(names(coef(model)), "(Intercept)")
  hit   <- terms[grep(paste0("^", var, "($|[[:punct:]]|[0-9])"), terms)]
  if (length(hit) == 0) hit <- terms[grep(var, terms, fixed = TRUE)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# sichere OR, CI, p als Wald-Schätzer; NA bei Problemen (Separation etc.)
safe_or <- function(model, term){
  cf <- tryCatch(summary(model)$coef, error = function(e) NULL)
  if (is.null(cf) || is.na(term) || !(term %in% rownames(cf))) {
    return(c(OR = NA_real_, lcl = NA_real_, ucl = NA_real_, p = NA_real_))
  }
  beta <- coef(model)[term]
  se   <- cf[term, "Std. Error"]
  if (is.na(beta) || is.na(se) || !is.finite(se)) {
    return(c(OR = NA_real_, lcl = NA_real_, ucl = NA_real_, p = NA_real_))
  }
  pval <- cf[term, "Pr(>|z|)"]
  c(OR = unname(exp(beta)),
    lcl = unname(exp(beta - 1.96*se)),
    ucl = unname(exp(beta + 1.96*se)),
    p = unname(pval))
}

# hübsches runden
fr <- function(x, k = 3){
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = k))
}
fsp <- function(p){
  ifelse(is.na(p), "NA",
         ifelse(p < 0.001, formatC(p, format="e", digits=2),
                formatC(p, format="f", digits=3)))
}

# ===== Haupt-Funktion =====

analyze_cutoffs <- function(
    data,
    endpoint,
    cutoffs,                  # z.B. c("Hyperfiltration120_1_ja", ...)
    confounders,              # z.B. c("RRsyst_completed_mean", ...)
    var_hba1c,                # "highest_measured_hba1c_naisna_completed_mean"
    var_diabetesdauer,        # "Diabetesdauer_a_"
    var_protein               # "ProteinJa_1"
){
  summaries <- vector("list", length(cutoffs))
  table_rows <- vector("list", length(cutoffs))
  
  for (i in seq_along(cutoffs)){
    expo <- cutoffs[i]
    
    # ----- Chi^2 -----
    chi_stat <- chi_df <- chi_p <- NA_real_
    if (all(c(expo, endpoint) %in% names(data))){
      tbl <- table(data[[expo]], data[[endpoint]])
      chi <- suppressWarnings(tryCatch(chisq.test(tbl, correct = TRUE),
                                       error = function(e) NULL))
      if (!is.null(chi)){
        chi_stat <- unname(chi$statistic)
        chi_df   <- unname(chi$parameter)
        chi_p    <- unname(chi$p.value)
      }
    }
    
    # ----- unadjusted logit: endpoint ~ expo -----
    p_unadj <- NA_real_
    OR_unadj <- lcl_unadj <- ucl_unadj <- NA_real_
    if (all(c(expo, endpoint) %in% names(data))){
      mod_u <- tryCatch(glm(as.formula(paste(endpoint, "~", expo)),
                            data = data, family = binomial()),
                        error = function(e) NULL)
      if (!is.null(mod_u)){
        term_u <- .find_term(mod_u, expo)
        stats_u <- safe_or(mod_u, term_u)
        OR_unadj  <- stats_u["OR"]; lcl_unadj <- stats_u["lcl"]; ucl_unadj <- stats_u["ucl"]
        p_unadj   <- stats_u["p"]
      }
    }
    
    # ----- adjusted logit: endpoint ~ expo + confounder -----
    adj_ORs <- list(hba1c = NA_real_, dd = NA_real_, prot = NA_real_)
    adj_CIs <- list(hba1c = c(NA,NA), dd = c(NA,NA), prot = c(NA,NA))
    adj_ps  <- list(hba1c = NA_real_, dd = NA_real_, prot = NA_real_)
    hf_OR <- hf_lcl <- hf_ucl <- hf_p <- NA_real_
    omni_chi <- omni_df <- omni_p <- NA_real_
    
    adj_vars <- unique(c(expo, confounders))
    adj_vars <- adj_vars[adj_vars != endpoint]
    
    if (all(c(endpoint, adj_vars) %in% names(data))){
      fmla <- as.formula(paste(endpoint, "~", paste(adj_vars, collapse = " + ")))
      mod_a <- tryCatch(glm(fmla, data = data, family = binomial()),
                        error = function(e) NULL)
      if (!is.null(mod_a)){
        # Hyperfiltration im adjustierten Modell
        term_hf <- .find_term(mod_a, expo)
        st_hf <- safe_or(mod_a, term_hf)
        hf_OR <- st_hf["OR"]; hf_lcl <- st_hf["lcl"]; hf_ucl <- st_hf["ucl"]; hf_p <- st_hf["p"]
        
        # drei interessierende Variablen
        for (nm in c("hba1c","dd","prot")){
          v <- switch(nm, hba1c = var_hba1c, dd = var_diabetesdauer, prot = var_protein)
          term_v <- .find_term(mod_a, v)
          st_v <- safe_or(mod_a, term_v)
          adj_ORs[[nm]] <- st_v["OR"]; adj_CIs[[nm]] <- c(st_v["lcl"], st_v["ucl"]); adj_ps[[nm]] <- st_v["p"]
        }
        
        # Omnibus (LRT)
        omni_chi <- mod_a$null.deviance - mod_a$deviance
        omni_df  <- mod_a$df.null - mod_a$df.residual
        omni_p   <- pchisq(omni_chi, omni_df, lower.tail = FALSE)
      }
    }
    
    # ----- Text-Zusammenfassung (wie besprochen) -----
    chi_txt <- if (is.na(chi_p)) {
      "Chi2 nicht berechenbar"
    } else if (chi_p < 0.05) {
      glue("Chi2 signifikant X-squared = {formatC(chi_stat, format='f', digits=5)}, df = {chi_df}, p-value = {fsp(chi_p)}")
    } else {
      glue("Chi2 nicht signifikant X-squared = {formatC(chi_stat, format='f', digits=5)}, df = {chi_df}, p-value = {fsp(chi_p)}")
    }
    
    unadj_txt <- if (is.na(p_unadj)) {
      "Nicht adj: nicht berechenbar"
    } else if (p_unadj < 0.05) {
      glue("Nicht adj: signifikant (p = {fsp(p_unadj)})")
    } else {
      glue("Nicht adj: nicht signifikant (p = {fsp(p_unadj)})")
    }
    
    adj_hf_txt <- if (is.na(hf_p)) {
      "Adj: Hyperfiltration nicht berechenbar"
    } else if (hf_p < 0.05) {
      glue("Adj: Hyperfiltration signifikant (OR {fr(hf_OR,2)}, p = {fsp(hf_p)}); signifikant für:")
    } else {
      "Adj: nicht signifikant Hyperfiltration, signifikant für:"
    }
    
    sig_lines <- c()
    if (!is.na(adj_ps$hba1c) && adj_ps$hba1c < 0.05)
      sig_lines <- c(sig_lines, glue("  HBA1c OR: {fr(adj_ORs$hba1c,2)} p= {fsp(adj_ps$hba1c)}"))
    if (!is.na(adj_ps$dd) && adj_ps$dd < 0.05)
      sig_lines <- c(sig_lines,   glue("  Diabetesdauer OR:{fr(adj_ORs$dd,2)} p= {fsp(adj_ps$dd)}"))
    if (!is.na(adj_ps$prot) && adj_ps$prot < 0.05)
      sig_lines <- c(sig_lines,   glue("  Proteinurie OR: {fr(adj_ORs$prot,2)} p={fsp(adj_ps$prot)}"))
    if (length(sig_lines) == 0) sig_lines <- "  — keine weiteren signifikanten Kovariaten —"
    
    omni_txt <- if (is.na(omni_p)) {
      "Omnibus: nicht berechenbar"
    } else {
      glue("Omnibus:\n  Chi² = {fr(omni_chi,2)}\n  df = {omni_df}\n  p = {fsp(omni_p)}")
    }
    
    head_label <- expo
    summaries[[i]] <- paste0(
      head_label, ":\n",
      chi_txt, "\n",
      "Logrank:\n",
      "  ", unadj_txt, "\n",
      "  ", adj_hf_txt, "\n",
      paste(sig_lines, collapse = "\n"), "\n",
      omni_txt
    )
    
    # ----- Tabellenzeile (schlank, nur Gewünschtes) -----
    table_rows[[i]] <- tibble::tibble(
      cutoff_var  = expo,
      chi2_stat   = chi_stat,
      chi2_df     = chi_df,
      chi2_p      = chi_p,
      unadj_p     = p_unadj,
      HbA1c_OR    = adj_ORs$hba1c,
      HbA1c_p     = adj_ps$hba1c,
      Diabetesdauer_OR = adj_ORs$dd,
      Diabetesdauer_p  = adj_ps$dd,
      Proteinurie_OR   = adj_ORs$prot,
      Proteinurie_p    = adj_ps$prot,
      omnibus_p   = omni_p
    )
  }
  
  list(
    summaries = summaries,
    table = dplyr::bind_rows(table_rows)
  )
}

# ===== Beispiel-Aufruf =====
# (Bitte ggf. Namen/Objekte anpassen – hier wie in deinen Beispielen)

cutoffs <- c("Hyperfiltration120_1_ja",
             "Hyperfiltration130_1_ja",
             "Hyperfiltration140_1_ja",
             "Hyperfiltration150_1_ja",
             "HyperfiltrationMedian_1_ja")

confounders <- c("RRsyst_completed_mean",
                 "highest_measured_hba1c_naisna_completed_mean",
                 "BMI_completed_mean",
                 "UN_completed_mean",
                 "Diabetesdauer_a_",
                 "Geschlecht_1_m",
                 "Alter_a_einschluss",
                 "ProteinJa_1")

res <- analyze_cutoffs(
  data = subset_minusCKD,                       # <-- oder daten_ursp
  endpoint = "endpoint_reached_ja_1",
  cutoffs = cutoffs,
  confounders = confounders,
  var_hba1c = "highest_measured_hba1c_naisna_completed_mean",
  var_diabetesdauer = "Diabetesdauer_a_",
  var_protein = "ProteinJa_1"
)

# --- Text-Zusammenfassung ausgeben ---
cat(paste(res$summaries, collapse = "\n\n"))

# --- Tabelle der wichtigen Werte anschauen ---
res$table

# Optional: hübsch runden für Bericht
res$table %>%
  mutate(
    chi2_stat = fr(chi2_stat, 5),
    chi2_p    = fsp(chi2_p),
    unadj_p   = fsp(unadj_p),
    HbA1c_OR  = fr(HbA1c_OR, 2),
    HbA1c_p   = fsp(HbA1c_p),
    Diabetesdauer_OR = fr(Diabetesdauer_OR, 2),
    Diabetesdauer_p  = fsp(Diabetesdauer_p),
    Proteinurie_OR   = fr(Proteinurie_OR, 2),
    Proteinurie_p    = fsp(Proteinurie_p),
    omnibus_p = fsp(omnibus_p)
  )


kannst du mitr bitte 2 word tabellen machen erstellen, die Tabelle A: Testübersicht (Chi², unadj. LogReg, Omnibus).
zeigen?
Tabelle B: Signifikante Prädiktoren (HbA1c, Diabetesdauer, Proteinurie).

## Prompt 31

Fehler in UseMethod("select") : 
  nicht anwendbare Methode für 'select' auf Objekt der Klasse "flextable" angewendet

## Prompt 32

das hat geklappt danke, aber aus ästhetischen gründen wäre ein achsenflip nochs chön denke ich

## Prompt 33

brauche ich nur das, oder den vcorigen code trotzdem?

## Prompt 34

aber den vorigen code, der tabels erstellt kann ich rausnehmen? (im gegensatz zu einem mutate bcommand, wo ich den table noch bräuchte davor)

## Prompt 35

> library(tidyr)
> make_cutoff_tables_flipped <- function(res, filename = "Cutoff_Tables_Flipped.docx") {
+   
+   tbl <- res$table %>%
+     mutate(
+       chi2_stat = round(chi2_stat, 3),
+       chi2_p = fsp(chi2_p),
+       unadj_p = fsp(unadj_p),
+       omnibus_p = fsp(omnibus_p),
+       HbA1c_OR = fr(HbA1c_OR, 2),
+       HbA1c_p = fsp(HbA1c_p),
+       Diabetesdauer_OR = fr(Diabetesdauer_OR, 2),
+       Diabetesdauer_p = fsp(Diabetesdauer_p),
+       Proteinurie_OR = fr(Proteinurie_OR, 2),
+       Proteinurie_p = fsp(Proteinurie_p)
+     )
+   
+   # ---- Tabelle A: Testübersicht (flipped) ----
+   table_A <- tbl %>%
+     select(cutoff_var, chi2_stat, chi2_df, chi2_p, unadj_p, omnibus_p) %>%
+     pivot_longer(-cutoff_var, names_to = "Kennzahl", values_to = "Wert") %>%
+     pivot_wider(names_from = cutoff_var, values_from = Wert)
+   
+   ft_A <- flextable(table_A) %>%
+     set_header_labels(Kennzahl = "Testkennzahl") %>%
+     theme_booktabs() %>%
+     autofit()
+   
+   # ---- Tabelle B: Prädiktoren (flipped) ----
+   table_B <- tbl %>%
+     select(cutoff_var,
+            HbA1c_OR, HbA1c_p,
+            Diabetesdauer_OR, Diabetesdauer_p,
+            Proteinurie_OR, Proteinurie_p) %>%
+     pivot_longer(-cutoff_var, names_to = "Prädiktor", values_to = "Wert") %>%
+     pivot_wider(names_from = cutoff_var, values_from = Wert)
+   
+   ft_B <- flextable(table_B) %>%
+     set_header_labels(Prädiktor = "Prädiktor / Kennzahl") %>%
+     theme_booktabs() %>%
+     autofit()
+   
+   # ---- Word exportieren ----
+   doc <- read_docx() %>%
+     body_add_par("Tabelle A: Testübersicht (Cutoff-Spalten)", style = "heading 1") %>%
+     body_add_flextable(ft_A) %>%
+     body_add_par("", style = "Normal") %>%
+     body_add_par("Tabelle B: Signifikante Prädiktoren (Cutoff-Spalten)", style = "heading 1") %>%
+     body_add_flextable(ft_B)
+   
+   print(doc, target = filename)
+   message("Word-Datei gespeichert: ", filename)
+   
+   list(ft_A = ft_A, ft_B = ft_B)
+ }
> # Anwendung
> word_tables_flip <- make_cutoff_tables_flipped(res, "Cutoff_Tables_Flipped.docx")
Error in `pivot_longer()`:
! Can't combine `chi2_stat` <double> and `chi2_p` <character>.
Run `rlang::last_trace()` to see where the error occurred.

## Prompt 36

library(flextable)
library(officer)
library(dplyr)

make_cutoff_tables <- function(res, filename = "Cutoff_Tables.docx") {
  
  # --- Basisdaten vorbereiten ---
  tbl <- res$table %>%
    mutate(
      chi2_stat = round(chi2_stat, 3),
      chi2_p_num = chi2_p,
      unadj_p_num = unadj_p,
      omnibus_p_num = omnibus_p,
      HbA1c_OR  = fr(HbA1c_OR, 2),
      HbA1c_p_num = HbA1c_p,
      Diabetesdauer_OR = fr(Diabetesdauer_OR, 2),
      Diabetesdauer_p_num = Diabetesdauer_p,
      Proteinurie_OR = fr(Proteinurie_OR, 2),
      Proteinurie_p_num = Proteinurie_p
    ) %>%
    mutate(
      chi2_p = fsp(chi2_p_num),
      unadj_p = fsp(unadj_p_num),
      omnibus_p = fsp(omnibus_p_num),
      HbA1c_p = fsp(HbA1c_p_num),
      Diabetesdauer_p = fsp(Diabetesdauer_p_num),
      Proteinurie_p = fsp(Proteinurie_p_num)
    )
  
  # ---- Tabelle A: Testübersicht ----
  table_A <- tbl %>%
    select(cutoff_var, chi2_stat, chi2_df, chi2_p, unadj_p, omnibus_p,
           chi2_p_num, unadj_p_num, omnibus_p_num)
  
  ft_A <- flextable(table_A %>% select(-chi2_p_num, -unadj_p_num, -omnibus_p_num)) %>%
    set_header_labels(
      cutoff_var = "Cutoff",
      chi2_stat = "Chi²-Statistik",
      chi2_df   = "df",
      chi2_p    = "Chi² p-Wert",
      unadj_p   = "Unadj. p-Wert",
      omnibus_p = "Omnibus p-Wert"
    ) %>%
    theme_booktabs() %>%
    autofit()
  
  # Fett für signifikante p-Werte
  ft_A <- bold(ft_A, i = which(table_A$chi2_p_num < 0.05), j = "chi2_p", bold = TRUE)
  ft_A <- bold(ft_A, i = which(table_A$unadj_p_num < 0.05), j = "unadj_p", bold = TRUE)
  ft_A <- bold(ft_A, i = which(table_A$omnibus_p_num < 0.05), j = "omnibus_p", bold = TRUE)
  
  # ---- Tabelle B: Prädiktoren ----
  table_B <- tbl %>%
    select(cutoff_var,
           HbA1c_OR, HbA1c_p, HbA1c_p_num,
           Diabetesdauer_OR, Diabetesdauer_p, Diabetesdauer_p_num,
           Proteinurie_OR, Proteinurie_p, Proteinurie_p_num)
  
  ft_B <- flextable(table_B %>% select(-HbA1c_p_num, -Diabetesdauer_p_num, -Proteinurie_p_num)) %>%
    set_header_labels(
      cutoff_var = "Cutoff",
      HbA1c_OR = "HbA1c OR",
      HbA1c_p  = "HbA1c p-Wert",
      Diabetesdauer_OR = "Diabetesdauer OR",
      Diabetesdauer_p  = "Diabetesdauer p-Wert",
      Proteinurie_OR   = "Proteinurie OR",
      Proteinurie_p    = "Proteinurie p-Wert"
    ) %>%
    theme_booktabs() %>%
    autofit()
  
  # Fett für signifikante p-Werte
  ft_B <- bold(ft_B, i = which(table_B$HbA1c_p_num < 0.05), j = "HbA1c_p", bold = TRUE)
  ft_B <- bold(ft_B, i = which(table_B$Diabetesdauer_p_num < 0.05), j = "Diabetesdauer_p", bold = TRUE)
  ft_B <- bold(ft_B, i = which(table_B$Proteinurie_p_num < 0.05), j = "Proteinurie_p", bold = TRUE)
  
  # ---- Word exportieren ----
  doc <- read_docx() %>%
    body_add_par("Tabelle A: Testübersicht", style = "heading 1") %>%
    body_add_flextable(ft_A) %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("Tabelle B: Signifikante Prädiktoren", style = "heading 1") %>%
    body_add_flextable(ft_B)
  
  print(doc, target = filename)
  message("Word-Datei gespeichert: ", filename)
  
  list(ft_A = ft_A, ft_B = ft_B)
}

# ---- Anwendung ----
word_tables <- make_cutoff_tables(res, "Cutoff_Tables.docx")

Ich würde doch hier bleiben aber kannst du bei den Hyperfiltration...das _1_ja wegnehmen für die tables und sie automatisch so anpassen, dass sie auf ein hochformatigens A4 Blatt passen

## Prompt 37

der richtige Log-Rank p-Wert (aus survdiff) automatisch im Kaplan–Meier Plot eingeblendet wird,
damit du diese Diskrepanz nicht mehr hast?
ja das wäre toll, bitte sag mir einfach wo ich was ersetzten soll, weil ich den funktionierenden code nicht komplett austauschen will

## Prompt 38

wie würdest du das machen?

## Prompt 39

aber mein kaplan maier plot wird vor dem logrank test erzeugt, weil die daten dazwischen gefiltert werden oder?

## Prompt 40

kajnn ich nicht einfach den logrank test hinaufziehen vor die filterung ?

## Prompt 41

ja bitet, und achte auch auf dei Reihenfolge, damit die Übergabe klappt
