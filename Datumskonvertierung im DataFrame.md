# Datumskonvertierung im DataFrame

## Prompt 1

for (spalte in spalten_urspr) {
  spalte_neu <- paste0(spalte,name_append)
  
  da_daten$[[spalte_neu]] <- as.Date(da_daten$[[spalte]], "%d.%m.%Y")
}

## Prompt 2

R feedbacks there is unexpected [[

## Prompt 3

myfunc_spal_exist <- function{
  if (spalten_urspr %in% names(da_daten)){
    
  } else {
      print("warining:",paste("Collum",spalte,"not found"))
    }
}

## Prompt 4

> for (spalte in spalten_urspr) {
+   myfunc_spal_exist(spalte, da_daten)
+   spalte_neu <- paste0(spalte,name_append)
+   
+   da_daten[[spalte_neu]] <- as.Date(da_daten[[spalte]], "%d.%m.%Y")
+ }
Error in `[[<-`:
! Assigned data `as.Date(da_daten[[spalte]], "%d.%m.%Y")` must be compatible with existing data.
✖ Existing data has 324 rows.
✖ Assigned data has 0 rows.
ℹ Only vectors of size 1 are recycled.
Caused by error in `vectbl_recycle_rhs_rows()`:
! Can't recycle input of size 0 to size 324.

## Prompt 5

for(spalte in spalten_urspr){
  myfunc_spal_exist(spalte, da_daten) 
  #absh
  x<- paste0("absh_",spalte) <- table(da_daten[[spalte]])
  #relh
  y <- paste0("relh_",spalte) <- round(prop.tabble(table(da_daten[spalte]))*100,2)
  #table
  cbind(x,y)

## Prompt 6

gibt es eine möglichkeit namen für zugewiesene variablen in einer for loop dynamisch anzupassen?

## Prompt 7

for(spalte in spalten_urspr){
  myfunc_spal_exist(spalte, da_daten) 
  #absh
  x<- paste0("absh_",spalte) <- table(da_daten[[spalte]])
  #relh
  y <- paste0("relh_",spalte) <- round(prop.tabble(table(da_daten[spalte]))*100,2)
  #table
  cbind(x,y)

## Prompt 8

ich möchte die tabelle speichern aber nicht direkt printen

## Prompt 9

#table
for (spalte in spalten_urspr) {
  if (paste0("absh_", spalte) %in% names(absh_list)) {
    table_list["tabelle_",spalte](cbind(
      Absolute = absh_list[[paste0("absh_", spalte)]],
      Relative = relh_list[[paste0("relh_", spalte)]]
    ))  
  }}
Print(table_list$tabelle_Kind_1)

## Prompt 10

die liste ist initialisiert

## Prompt 11

for (spalte in spalten_urspr) {
  if (paste0("absh_", spalte) %in% names(absh_list)) {
    table_list[[paste0("tabelle_",spalte)]](cbind(
      Absolute = absh_list[[paste0("absh_", spalte)]],
      Relative = relh_list[[paste0("relh_", spalte)]]
    ))  
  }}
print(table_list$tabelle_Kind_1)

## Prompt 12

for (spalte in spalten_urspr) {
  if (paste0("absh_", spalte) %in% names(absh_list)) {
    table_list[[paste0("tabelle_",spalte)]] <- cbind(
      Absolute = absh_list[[paste0("absh_", spalte)]],
      Relative = relh_list[[paste0("relh_", spalte)]]
    )
  }
}

## Prompt 13

ich bekomme kein output whatsoever bei print commands

## Prompt 14

auch bei print(length(absh_list)) bekomme ich kein output

## Prompt 15

der code selber funktioniert, aber wenn ich ihn als funktion abspeichern will nicht mehr

## Prompt 16

myfunc_absh_relh_table <- function(spalten_urspr, da_daten) {
  for(spalte in spalten_urspr){
  myfunc_spal_exist(spalte, da_daten) 
  #absh
  absh_list[[paste0("absh_",spalte)]] <- table(da_daten[[spalte]])
  #relh
  relh_list[[paste0("relh_",spalte)]] <- round(prop.table(table(da_daten[spalte]))*100,2)
  
}  
#table
for (spalte in spalten_urspr) {
  if (paste0("absh_", spalte) %in% names(absh_list)) {
    table_list[[paste0("tabelle_",spalte)]] <- cbind(
      Absolute = absh_list[[paste0("absh_", spalte)]],
      Relative = relh_list[[paste0("relh_", spalte)]]
    )
  } else {
    warining (paste0("absh_",spalte,"not found"))
  }
}
}

## Prompt 17

die listen wurden golbal definiert, kann ich dann einfach  nur die function ausführen?

## Prompt 18

myfunc_absh_relh_table(spalten_urspr, da_daten)

## Prompt 19

das ist der ganze code, er funktioniert auch wenn ich ihn schritt für schritt ausführe (ich weiß das spalten_usrpr erst nach der function definiert wird, aber das sollte ja passen weil die funktion ja erst danach ausgeführt werden soll). also ohne den code als function zu definieren läuft er, sobald ich die funktion abspeichern und audführen möchte nicht meht, woran kann das liegen. bitte nur eine kurze antwort

## Prompt 20

ich vergaß den code anzufügen, absh_list <- list()
relh_list <- list()
table_list <- list()

myfunc_absh_relh_table <- function(spalten_urspr, da_daten) {
  for(spalte in spalten_urspr){
  myfunc_spal_exist(spalte, da_daten) 
  #absh
  absh_list[[paste0("absh_",spalte)]] <- table(da_daten[[spalte]])
  #relh
  relh_list[[paste0("relh_",spalte)]] <- round(prop.table(table(da_daten[[spalte]]))*100,2)
  
}  
#table
for (spalte in spalten_urspr) {
  if (paste0("absh_", spalte) %in% names(absh_list)) {
    table_list[[paste0("tabelle_",spalte)]] <- cbind(
      Absolute = absh_list[[paste0("absh_", spalte)]],
      Relative = relh_list[[paste0("relh_", spalte)]]
    )
  } else {
    warning (paste0("absh_",spalte,"not found"))
  }
}
}
spalten_urspr <- list(
  "Kind_1",
  "NE_1_ja",
  "TX_1_ja",
  "tot_1",
  "ProteinJa_1",
  "Niere_1_OK_2_MiA_3_PU_4_NB",
  "Hyperfiltration140_1_ja",
  "Hyperfiltration130_1_ja",
  "Hyperfiltration150_1_ja",
  "HyperfiltrationMedian_1_ja",
  "HyperfiltrationMedianohneausreißer_1_ja"
)
myfunc_absh_relh_table(spalten_urspr, da_daten)
