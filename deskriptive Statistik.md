# deskriptive Statistik

## Prompt 1

Hey i am currently working on a project in R and i got a table for my dichotomous baseline data called table_long, now i want to change that table, should i do that in r or in pandas using google colab rather

## Prompt 2

Error in `mutate()`:
ℹ In argument: `abs_val = get(paste0("absh_", group))`.
Caused by error in `get()`:
! erstes Argument hat Länge > 1

## Prompt 3

labels <- c(
  "Geschlecht_1_m" = "Männlich",
  "Kind_1" = "Minderjährig",
  "ProteinJa_1" = "Proteinurie",
  "hypertonie_ja_1" = "Hypertension",
  "Hyperfiltration140_1_ja" = "Hyperfiltration >140",
  "Hyperfiltration130_1_ja" = "Hyperfiltration >130",
  "Hyperfiltration150_1_ja" = "Hyperfiltration >150",
  "HyperfiltrationMedian_1_ja" = "Hyperfiltration >Median",
  "HyperfiltrationMedianohneausreißer_1_ja" = "Hyperfiltration (no outliers)",
  "NE_1_ja" = "Nierenersatztherapie",
  "TX_1_ja" = "Transplantiert",
  "tot_1" = "Verstorben"
)

please use these renamings

## Prompt 4

> df <- df %>% select(-`0`)
Error in `select()`:
! Can't select columns that don't exist.
✖ Column `0` doesn't exist.
Run `rlang::last_trace()` to see where the error occurred.
> # 4. Pivot to wide format: one row per variable, with abs and rel side-by-side
> df_wide <- df %>%
+   select(group, type, `1`) %>%
+   pivot_wider(names_from = type, values_from = `1`) %>%
+   mutate(`n (%)` = paste0(abs, "\n(", rel, ")")) %>%
+   select(group, `n (%)`)
Error in `select()`:
! Can't select columns that don't exist.
✖ Column `type` doesn't exist.
Run `rlang::last_trace()` to see where the error occurred.

## Prompt 5

as i want to put 4 tables like that next to each other i thought the linebrak would be neater

## Prompt 6

please generate export code for waord

## Prompt 7

can you give me the whole export as one coed piece and what does this df clean part do?

## Prompt 8

Fehler: Objekt 'df_clean' nicht gefunden

## Prompt 9

kann ich genau den gleich code auch verwenden, wenn ich für die anderen subsets tabellen erstell, die gelich ausshene und table long heißen?

## Prompt 10

wie lönnte ich das anstellen? : 
Wenn du magst, kann ich dir auch zeigen, wie du mehrere dieser Tabellen nebeneinander platzierst, z. B. mit body_add_table() oder einer Word-Vorlage.

## Prompt 11

#speichere die einzelnen df ab, richtiger Name!
df_ges <-df_clean

df_120<- df_clean

df_130 <- df_clean

df_140<- df_clean

df_150<- df_clean

df_Median <- df_clean

Das sind meine data frames, ich möchte,dass die Tabelle ca. so aussieht:

## Prompt 12

ja bnitte weil die n unterschieden sich obviously

## Prompt 13

die n sind komplett falsch

## Prompt 14

und bitte baue in die Funktion auch df 120 ein

## Prompt 15

ich hab jetzt das, aber das klebt in word sehr am rand, ich weiß nicht wie ich das vernünftig layoute, sodass keine komischen zeiolenumbrüche entstehen

## Prompt 16

Ich möchte in einem nächsten schritt die metrischen variablen analysiseren. Ich habe diesen code :
# Required packages
library(psych)
library(dplyr)
library(tibble)
library(flextable)
library(officer)

# Define your datasets
datasets <- list(ds1 = da_daten)

# Original variable names (as in the data)
original_vars <- c(
  "Alter_a_einschluss", 
  "Kreatininclearance_endogen", 
  "NE_surv_a_tim", 
  "Diabetesdauer_totwenntot_a_tim", 
  "Manifestationsalter_a_", 
  "highest_measured_hba1c_naisna"
)

# New readable names (used in final table)
readable_names <- c(
  "Alter", 
  "Filtrationsrate", 
  "Nierenüberleben", 
  "Diabetesdauer", 
  "Manifestationsalter", 
  "höchster dokumentierter HbA1c"
)

# Function to compute and format the statistics
get_formatted_stats <- function(datensatz, varname) {
  values <- datensatz[[varname]]
  descr <- describe(values)
  
  na_count <- sum(is.na(values))
  total <- length(values)
  na_percent <- round((na_count / total) * 100, 1)
  
  paste0(
    round(descr$mean, 2), " (", round(descr$min, 2), " / ", round(descr$max, 2), ")\n",
    "n = ", descr$n, ", SD = ", round(descr$sd, 2), "\n",
    "NA = ", na_count, " (", na_percent, "%)"
  )
}

# Create the final table
pretty_table <- tibble(Variable = readable_names)

for (ds_name in names(datasets)) {
  datensatz <- datasets[[ds_name]]
  column <- sapply(original_vars, function(var) get_formatted_stats(datensatz, var))
  pretty_table[[ds_name]] <- column
}

# Create the flextable
ft <- flextable(pretty_table)
ft <- autofit(ft)
ft <- set_table_properties(ft, width = 1.0, layout = "autofit")

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Deskriptive_Statistik_Tabelle1.docx")
 der erzeugt eine tabelle wie in bild 1, beim letzten mal habe ich es aber geschafft, eine tabelle wie in bild 2 zu erzeugen, die ich dann separat für alle gruppen erzeuge und als tabelle 2a-e einfüge  (eine überschrift mit dem jeweiligen daztensatz wäre hilfreich)

warte bitte mit der antwort auf bild 2

## Prompt 17

perfekrt danke, in diesem schritt: datasets <- list(
  "Gesamtpopulation" = ds1,
  "Hyperfiltration 120" = ds120,
  "Hyperfiltration 130" = ds130,
  "Hyperfiltration 140" = ds140,
  "Hyperfiltration 150" = ds150,
  "Hyperfiltration Median" = dsMedian 

ist vor dem = der gewünschte name und hinter dem = der name des data frames?

## Prompt 18

danke, es handelt sich um die zeile mit den überschriften, gibt es eine möglichkeit diese zu iognorieren ( in excel oder in R, damit dieser Fehler nicht mehr auftritt)

## Prompt 19

ich bekomme immernoch NA =1

## Prompt 20

das problem ist, mein letzer eintrag und die letzte Zahl ijn der Liste stimmen überein, kann es sein, dass R die leere Zeile danach noch mitnimmt?

## Prompt 21

assign_df_for_table <- function(datensatz,spalten_urspr) {
  
  table_long <- absh_relh_table(spalten_urspr, datensatz)
print(table_long)#tabelle aller binären baselinewerte

as.data.frame(table_long)

names(table_long) <- c("Variable", "abs", "rel")

# relh-Zeilen mit % versehen (als Zeichen)
table_long <- table_long %>%
  mutate(across(c(abs, rel), ~ if_else(str_detect(Variable, "^relh_"),
                                       paste0(.x, "%"),
                                       as.character(.x))))

# Gruppennamen extrahieren & Typ (absh vs relh) kennzeichnen
table_long <- table_long %>%
  mutate(group = str_replace(Variable, "^(absh_|relh_)", ""),
         type = if_else(str_detect(Variable, "^absh_"), "abs", "rel"))

# 🧱 --- Schritt 2: Reshape: aus long → wide (eine Zeile pro Merkmal)
df_clean <- table_long %>%
  select(group, type, value = rel) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(`n (%)` = paste0(abs, "\n(", rel, ")")) %>%
  select(group, `n (%)`)

# 🏷️ --- Schritt 3: Deutsche Labels einsetzen
labels <- c(
  "Geschlecht_1_m" = "Männlich",
  "Kind_1" = "Minderjährig",
  "ProteinJa_1" = "Proteinurie",
  "hypertonie_ja_1" = "Hypertension",
  "Hyperfiltration140_1_ja" = "Hyperfiltration >140",
  "Hyperfiltration130_1_ja" = "Hyperfiltration >130",
  "Hyperfiltration150_1_ja" = "Hyperfiltration >150",
  "Hyperfiltration120_1_ja" = "Hyperfiltration >120",
  "HyperfiltrationMedian_1_ja" = "Hyperfiltration >Median",
  "NE_1_ja" = "Nierenersatztherapie",
  "TX_1_ja" = "Transplantiert",
  "tot_1" = "Verstorben"
)

df_clean <- df_clean %>%
  mutate(Variable = recode(group, !!!labels, .default = group)) %>%
  select(Variable, `n (%)`)

# 📄 --- Schritt 4: Für Word Zeilenumbruch korrekt setzen
df_clean <- df_clean %>%
  mutate(`n (%)` = str_replace_all(`n (%)`, "\\\\n|\\n", "\n"))

# 🖨️ --- Schritt 5: Als Word-Dokument exportieren
ft <- flextable(df_clean) %>%
  set_table_properties(width = 0.8, layout = "autofit") %>%
  align(j = 2, align = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  line_spacing(space = 1.0, part = "all")

  
}

Ich möchte diese comamnds als funktion zusammenfassen, da ich bisehr alle separat mit dem jeweiligen datensatz ausgeführt habe um dann das resultierende df_clean zuzuordnen: df_ges <-df_clean

df_120<- df_clean

df_130 <- df_clean

df_140<- df_clean

df_150<- df_clean

df_Median <- df_clean

## Prompt 22

muss ich datensatz jedes mal neu definieren oder ist das durch die definition im Aufrufen schon erledigt?

## Prompt 23

df_ges    <- assign_df_for_table(da_daten, spalten_urspr)
Error in `mutate()`:
! Can't transform a data frame with `NA` or `""` names.
Run `rlang::last_trace()` to see where the error occurred.
> rlang::last_trace()
<error/rlang_error>
Error in `mutate()`:
! Can't transform a data frame with `NA` or `""` names.
---
Backtrace:
    ▆
 1. ├─global assign_df_for_table(da_daten, spalten_urspr)
 2. │ └─table_long %>% ...
 3. ├─dplyr::mutate(...)
 4. └─dplyr:::mutate.data.frame(...)
Run rlang::last_trace(drop = FALSE) to see 4 hidden frames.
> df_ges    <- assign_df_for_table(da_daten, spalten_urspr)
Error in `mutate()`:
! Can't transform a data frame with `NA` or `""` names.
Run `rlang::last_trace()` to see where the error occurred.

das problem hatte ich vorher nicht
