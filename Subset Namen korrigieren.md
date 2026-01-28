# Subset Namen korrigieren

## Prompt 1

datensatz <- da_daten
for (spalte in spalten_subset) {
  myfunc_spal_exist(spalten_subset, datensatz)
  mean <- mean(da_daten[[spalte]])
  sd <- sd(da_daten[[spalte]])
  cutoff_up <- mean+2*sd
  cutoff_low <- mean-2*sd
  df_name <- paste("subset_", deparse(substitute(datensatz)), "_", spalte, sep = "")
    assign(df_name, subset(datensatz, datensatz[[spalte]]<cutoff_up & datensatz[[spalte]]>cutoff_low))
    print(df_name)
}

i want the created subset to have a name that reflects on the originally used dataframe, right now im getting this:
"subset_datensatz_GF__ml_"
[1] "subset_datensatz_Alter_beitotwenntot_a"

## Prompt 2

but if i want to use a different data frame, dont i have to change the name in the deparse function too then?

## Prompt 3

for (spalte in spalten_subset) {
  myfunc_spal_exist(spalten_subset, datensatz)
  df_original_name <- deparse(substitute(datensatz))
  mean <- mean(da_daten[[spalte]])
  sd <- sd(da_daten[[spalte]])
  cutoff_up <- mean+2*sd
  cutoff_low <- mean-2*sd
  df_name <- paste("subset_", df_original_name, "_", spalte, sep = "")
    assign(df_name, subset(datensatz, datensatz[[spalte]]<cutoff_up & datensatz[[spalte]]>cutoff_low))
    print(df_name)
}

## Prompt 4

datensatz <- da_daten
df_original_name <- deparse(substitute(datensatz))
for (spalte in spalten_subset) {
  myfunc_spal_exist(spalten_subset, datensatz)
  mean <- mean(da_daten[[spalte]])
  sd <- sd(da_daten[[spalte]])
  cutoff_up <- mean+2*sd
  cutoff_low <- mean-2*sd
  df_name <- paste("subset_", df_original_name, "_", spalte, sep = "")
    assign(df_name, subset(datensatz, datensatz[[spalte]]<cutoff_up & datensatz[[spalte]]>cutoff_low))
    print(df_name)
}
still gives "subset_datensatz_GF__ml_"

## Prompt 5

subset_outliers <- function(datensatz, spalten_subset) {
  df_original_name <- deparse(substitute(datensatz))  # Automatically get original dataframe name
  
  for (spalte in spalten_subset) {
    myfunc_spal_exist(spalten_subset, datensatz)
    
    mean_val <- mean(datensatz[[spalte]], na.rm = TRUE)
    sd_val <- sd(datensatz[[spalte]], na.rm = TRUE)
    cutoff_up <- mean_val + 2 * sd_val
    cutoff_low <- mean_val - 2 * sd_val
    
    df_name <- paste("subset_", df_original_name, "_", spalte, sep = "")
    assign(df_name, subset(datensatz, datensatz[[spalte]] < cutoff_up & datensatz[[spalte]] > cutoff_low), envir = .GlobalEnv)
    
    print(df_name)
  }
}
subset_outliers(datensatz, spalten_subset)
still gives [1] "subset_datensatz_GF__ml_"
