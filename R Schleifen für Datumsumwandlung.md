# R Schleifen für Datumsumwandlung

## Prompt 1

kannst du mir folgende zeilen in eine loop umschreiben?da_daten$geboren_clean <- as.Date(da_daten$geboren, "%d.%m.%Y")
da_daten$gestorben_clean <- as.Date(da_daten$gestorben, "%d.%m.%Y")
da_daten$Sterbedatum_clean <- as.Date(da_daten$Sterbedatum, "%d.%m.%Y")
da_daten$DatumLetzteTherapie_clean <- as.Date(da_daten$DatumLetzteTherapie, "%d.%m.%Y")
da_daten$`Beginn Nierenersatztherapie_clean`<- as.Date(da_daten$`Beginn Nierenersatztherapie`, "%d.%m.%Y")
da_daten$`Webokra zuletzt_clean` <- as.Date(da_daten$`Webokra zuletzt`, "%d.%m.%Y")
da_daten$TX_Datum_clean <- as.Date(da_daten$TX_Datum, "%d.%m.%Y")
da_daten$erst_NE_dat_clean <- as.Date(da_daten$erst_NE_dat, "%d.%m.%Y")

## Prompt 2

ist es möglich eine liste nur mit den ursprünglichen spaltennamen zu erstellen, den zusatz _clean in eine variable zu speichern, und in einer eigenen loop der zweiten liste hinzuzufühgen?

## Prompt 3

wieso brauche ich bei da_daten[[spalte]] zwei [ um zuzugreifen?

## Prompt 4

#loop um daten zu runden
ursprüngliche_spalten <- c(
  "Alter_beitotwenntot_a",
  "Diabetesdauer_totwenntot_a_tim"
)

name_zusatz <- "_rounded"

for (spalte in ursprüngliche_spalten) {
  neuer_spaltenname <- paste0(spalte, name_zusatz)
  da_daten[[neuer_spaltenname]] <- round(da.daten[[spalte]], digits=0)
}

## Prompt 5

#loop um daten zu runden
ursprüngliche_spalten <- c(
  "Alter_beitotwenntot_a",
  "Diabetesdauer_totwenntot_a_tim"
)

name_zusatz <- "_rounded"

for (spalte in ursprüngliche_spalten) {
  neuer_spaltenname <- paste0(spalte, name_zusatz)
  da_daten[[neuer_spaltenname]] <- round(da_daten[[spalte]], digits=0)
}

## Prompt 6

environment komplett löschen R
