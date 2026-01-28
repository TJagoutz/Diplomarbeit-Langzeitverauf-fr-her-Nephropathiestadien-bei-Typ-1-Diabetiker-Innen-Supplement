# Fehlerbehebung bei Variablen

## Prompt 1

> print(doc, target = "Deskriptive_Statistik_Metrisch_grundpop.docx")
> datensatz <- subset_minusCKD  # Original dataset
> subset_minusCKD <- subset(datensatz, Kreatininclearance_endogen > 59.9)
> subset_Hyperfiltration140 <- subset(datensatz, Kreatininclearance_endogen>139.99)
> subset_Hyperfiltration130 <- subset(datensatz, Kreatininclearance_endogen>129.99)
> subset_Hyperfiltration150 <- subset(datensatz, Kreatininclearance_endogen>149.99)
> subset_HyperfiltrationMedian <- subset(datensatz, Kreatininclearance_endogen>99.99)
> subset_Hyperfiltration120 <- subset(datensatz, Kreatininclearance_endogen>119.99)
> datensatz <- Grundpop
> subset_Hyperfiltration140 <- subset(datensatz, Kreatininclearance_endogen>139.99)
> subset_Hyperfiltration130 <- subset(datensatz, Kreatininclearance_endogen>129.99)
> subset_Hyperfiltration150 <- subset(datensatz, Kreatininclearance_endogen>149.99)
> subset_HyperfiltrationMedian <- subset(datensatz, Kreatininclearance_endogen>99.99)
> subset_Hyperfiltration120 <- subset(datensatz, Kreatininclearance_endogen>119.99)
> # ## Tabelle Metrisch -----------------------------------------------------
> datensatz<- Grundpop
> # Required packages
> library(psych)
> library(dplyr)
> library(tibble)
> library(flextable)
> library(officer)
> # Beispiel: mehrere Datensätze in einer Liste
> datasets <- list(
+   "Gesamtpopulation" = datensatz,
+   "Hyperfiltration 120" = subset_Hyperfiltration120,
+   "Hyperfiltration 130" = subset_Hyperfiltration130,
+   "Hyperfiltration 140" = subset_Hyperfiltration140,
+   "Hyperfiltration 150" = subset_Hyperfiltration150,
+   "Hyperfiltration Median" = subset_HyperfiltrationMedian
+ )
> # Original Variablennamen
> original_vars <- c(
+   "Alter_a_einschluss", 
+   "Kreatininclearance_endogen", 
+   "NE_surv_a_tim", 
+   "Diabetesdauer_a_", 
+   "Manifestationsalter_a_", 
+   "highest_measured_hba1c_naisna_completed_mean",
+   "RRsyst_completed_mean",
+   "BMI_completed_mean",
+   "egfr_ckdepi2009_tim"
+ 
+   
+   
+ )
> # Neue, lesbare Labels
> readable_names <- c(
+   "Alter", 
+   "Filtrationsrate", 
+   "Nierenüberleben", 
+   "Diabetesdauer", 
+   "Manifestationsalter", 
+   "HbA1c",
+   "systolischer Blutdruck",
+   "BMI",
+   "eGFR (CKD-EPI)"
+ )
> # Funktion für eine strukturierte Statistik-Tabelle
> get_stats_table <- function(datensatz, original_vars, readable_names) {
+   results <- lapply(seq_along(original_vars), function(i) {
+     var <- original_vars[i]
+     values <- datensatz[[var]]
+     descr <- psych::describe(values)
+     
+     na_count <- sum(is.na(values))
+     total <- length(values)
+     na_percent <- round((na_count / total) * 100, 1)
+     
+     tibble(
+       Variable = readable_names[i],
+       Mean = round(descr$mean, 2),
+       SD = round(descr$sd, 2),
+       Median = round(median(values, na.rm = TRUE), 2),
+       IQR = round(IQR(values, na.rm = TRUE), 2),
+       Min = round(descr$min, 2),
+       Max = round(descr$max, 2),
+       n = descr$n,
+       NA_Count = na_count,
+       NA_Percent = paste0(na_percent, "%")
+     )
+   })
+   
+   bind_rows(results)
+ }
> # Export: Alle Tabellen in ein Word-Dokument
> doc <- read_docx()
> for (ds_name in names(datasets)) {
+   datensatz <- datasets[[ds_name]]
+   
+   stats_table <- get_stats_table(datensatz, original_vars, readable_names)
+   
+   ft <- flextable(stats_table) %>%
+     autofit() %>%
+     set_table_properties(width = 1.0, layout = "autofit")
+   
+   # Überschrift + Tabelle ins Dokument
+   doc <- doc %>%
+     body_add_par(ds_name, style = "heading 2") %>%
+     body_add_flextable(value = ft) %>%
+     body_add_par("", style = "Normal") # Leerzeile danach
+ }
Fehler in stats[1, 3] <- median(x, na.rm = na.rm) : 
  Anzahl der zu ersetzenden Elemente ist kein Vielfaches der Ersetzungslänge
Zusätzlich: Warnmeldung:
In mean.default(x, na.rm = na.rm) :
  Argument ist weder numerisch noch boolesch: gebe NA zurück
> print(doc, target = "Deskriptive_Statistik_Metrisch_grundpop.docx")

## Prompt 2

ich habe nur die variable rrsyst hinzugefügt, davor gingr es, ich brauche wsl. nur einen kurzen fix für diese variable
