# Dateizugriffsfehler beheben

## Prompt 1

> # Export each d

ataset as one CSV with selected columns
> for (ds_name in names(datasets)) {
+   datensatz <- datasets[[ds_name]]
+   if (all(raw_vars %in% names(datensatz))) {
+     data_out <- datensatz[, raw_vars]
+     write.csv(data_out, file = paste0("C:/Users/timmy/Downloads/", ds_name, "_raw.csv"), row.names = FALSE)
+     cat("✅ Exported:", ds_name, "\n")
+   } else {
+     cat("⚠️ Skipped:", ds_name, "- some variables missing\n")
+   }
+ }
Fehler in file(file, ifelse(append, "a", "w")) : 
  kann Verbindung nicht öffnen
Zusätzlich: Warnmeldung:
In file(file, ifelse(append, "a", "w")) :
  kann Datei 'C:/Users/timmy/Downloads/Gesamt_raw.csv' nicht öffnen: Permission denied


gestern funktionierte das noch

## Prompt 2

Boxplots zur Verteilung der glomerulären Filtration (mGFR in ml/min/1.73m2) der Grundpopulation und in den entsprechenden Hyperfiltrationsgruppen. Es ist die Anzahl der Patient*innen mit vorhandenen (n) und fehlenden (NA) Daten der jeweiligen Gruppe angegeben. Die einzelnen Datenpunkte für die Filtrationsrate jedes Falles einer Gruppe sind als Stripplot neben dem jeweiligen Boxplot aufgetragen.

im letzten Satz, gehört jeden Falles oder jedes Falles?
