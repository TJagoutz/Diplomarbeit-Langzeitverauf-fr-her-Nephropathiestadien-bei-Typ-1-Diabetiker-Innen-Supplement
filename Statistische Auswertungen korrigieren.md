# Statistische Auswertungen korrigieren

## Prompt 1

this:func_stat_descr <- function(spalten_urspr, da_daten){
  for (spalten in spalten_urspr){
    myfunc_spal_exist(spalten_urspr, da_daten)
    descr <- describeBy(da_daten[spalte])
    summary <- summary(da_daten[spalte])
    print(descr)
    print(summary)
  } 
}


func_stat_descr(spalten_urspr, da_daten)

gives me this:
             vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
              vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
              vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
              vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
              vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
              vars  n  mean   sd median trimmed   mad   min   max range skew kurtosis   se
NE_surv_a_tim    1 43 45.04 12.8  46.02   44.13 13.68 25.68 78.98  53.3  0.6     0.07 1.95
 NE_surv_a_tim  
 Min.   :25.68  
 1st Qu.:35.36  
 Median :46.02  
 Mean   :45.04  
 3rd Qu.:51.11  
 Max.   :78.98  
 NA's   :281    
Warnmeldungen:
1: In describeBy(da_daten[spalte]) : no grouping variable requested
2: In describeBy(da_daten[spalte]) : no grouping variable requested
3: In describeBy(da_daten[spalte]) : no grouping variable requested
4: In describeBy(da_daten[spalte]) : no grouping variable requested
5: In describeBy(da_daten[spalte]) : no grouping variable requested
6: In describeBy(da_daten[spalte]) : no grouping variable requested
>

## Prompt 2

vars   n  mean    sd median trimmed   mad   min   max range  skew kurtosis   se
X1    1 322 64.25 12.84  64.32   64.72 12.15 19.19 92.12 72.93 -0.36     0.21 0.72
function (object, ...) 
UseMethod("summary")
<bytecode: 0x00000292f62bc4b0>
<environment: namespace:base>
Fehler in stats[1, 3] <- median(x, na.rm = na.rm) : 
  Anzahl der zu ersetzenden Elemente ist kein Vielfaches der Ersetzungslänge
Zusätzlich: Warnmeldung:
In mean.default(x, na.rm = na.rm) :
  Argument ist weder numerisch noch boolesch: gebe NA zurück
>

## Prompt 3

whats a grouping variable for describeBy in r?

## Prompt 4

Fehler in xtfrm.data.frame(x) : kann nicht xtfrm Dataframe

## Prompt 5

will that dataframe will be saved in my workplace<ß

## Prompt 6

if i create a function with a loop to change my dataframe, is it possible to do it like 
function (subset_var, dataframe) and have a dataframe variable created, so i can change the dataframe i am using that function on?

## Prompt 7

mean <- mean(da_daten$GF__ml_)
print(mean)
sd <- sd(da_daten$GF__ml_)
print(sd)
describe(da_daten$GF__ml_)
cutoff_up <- (mean-2*sd)
print(cutoff_up)
subset_GF <- subset(da_daten, GF__ml_<cutoff_up & GF__ml_>cutoff_low)

Why do i get a dataframe with 0obs of 111 variables?

## Prompt 8

if ((datensatz == da_daten)
      x<-" "
    else 
      x<-"narrow_"
  )

## Prompt 9

datensatz <- data.frame(da_daten)
if (identical(datensatz, da_daten)) {
  x <- " "
} else {
  x <- "narrow_"
}
print(x)

why do i get narrow as output?

## Prompt 10

Fehler in if (all.equal(datensatz, da_daten) == TRUE) { : 
  Bedingung hat Länge > 1

## Prompt 11

i am getting narrow again

## Prompt 12

for (spalte in spalten_subset) {
  myfunc_spal_exist(spalten_subset, da_daten)
  mean <- mean(da_daten[[spalte]])
  sd <- sd(da_daten[[spalte]])
  cutoff_up <- mean+2*sd
  cutoff_low <- mean-2*sd
    df_name <- paste("subset_",spalte)
    assign(df_name, subset(da_daten, spalte<cutoff_up & spalte>cutoff_low))
    print(df_name)
}

## Prompt 13

now  i want to cahnge da_daten to datensatz (a cahngable variable for dataframes) and make it so th name of each created subset reflects which dataframe was used
