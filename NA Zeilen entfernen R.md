# NA Zeilen entfernen R

## Prompt 1

kannst du mir bitte ein paar zeilen code für R geben um aus einem datensatz (dataframe) NAs einer Spalte zu entfernen (die gewählten Fälle sollen aus dem dataframe entfernt werden), die jeweilige spalte und der datensatz sollen vor der funktion variabel verändert werden können

## Prompt 2

dplyr ist fine ich hätte nur gerne am anfang zeilen wie: 
data="da_daten"
column="krea"

## Prompt 3

super, ich möchte einen größeren Datensatz mit 329 PatientInnen analysierne, bei eingen werten gibt es missing values und ich möchte wisse, ob ich diese mit dem Median oder Mittelwerten der vorhandenen Werte füllen kann um die Power statistischer Tests zu erhalten. 
Dies darf ich jedoch nur wenn die wichtigsten parameter sich zwischen den Patienten mit vorhandenen und fehlenden Werten nicht zu sehr unterscheiden.
Die variablen sind folgende:  
 variable_list <- list(
"RRsyst",
  "highest_measured_hba1c_naisna",
  "ppBZ",
  "NBZ",
  "BMI",
  "UN"
)
Die Parameter zum analysierten würde ich gerne ebenfalls so in einer Liste definieren
Bitte gib mir eine funktion die jene Fälle mit missing values mit jenen ohne vergleicht. für jede variable gesondert und für jeden angegebenen Parameter gesondert. Falls du vorschläge für eine zusammenfassende visualisierung hast bin ich geren offen

## Prompt 4

Error in `mutate()`:
ℹ In argument: `p_value_numeric = as.numeric(p_value)`.
Caused by error:
! Objekt 'p_value' nicht gefunde

## Prompt 5

es gibt in der tabelle, die ich mit der obigen funktion erhalten, und dann hier eingefügt habe p_value, aber wie definiere ich diese tabelle als df?

## Prompt 6

Fehler in UseMethod("mutate") : 
  nicht anwendbare Methode für 'mutate' auf Objekt der Klasse "list" angewendet

## Prompt 7

difference_median < 2 ~ TRUE, warum muss der differnzen der medianwerte< 2 sein?

## Prompt 8

kannst du mir eine zeile code geben um für NAs in einer spalte zu prüfen?
