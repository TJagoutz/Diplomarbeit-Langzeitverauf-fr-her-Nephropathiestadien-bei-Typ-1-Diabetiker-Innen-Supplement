# KM Formel Konstruktion

## Prompt 1

km_formula <- as.formula(
    paste("Surv(", da_daten$Alter_beitotwenntot_a_rounded, ",", da_daten$tot_1, ") ~", da_daten$Hyperfiltration140_1_ja))

## Prompt 2

km_formula <- as.formula(
  paste0("Surv(", 
         "Alter_beitotwenntot_a_rounded, ", 
         "tot_1) ~ ", 
         "Hyperfiltration140_1_ja")
)
  
  print("🚀 DEBUG: Kaplan-Meier Formula")
  print(km_formula)  # Check if the formula is correctly generated
 
  km_fit <- tryCatch(
    survfit(km_formula, data = da_daten),  
    error = function(e) {
      print('ERRORRRRRRRRRR!!!!!!!!!!')
      warning(paste("❌ Kaplan-Meier fit failed:", e$message))
      return(NULL)
    }
  )
  
  
  if (!is.null(km_fit)) {
    print("✅ Kaplan-Meier Model Summary:")
    print(summary(km_fit))  # Ensure km_fit is not NULL
  } else {
    print("❌ Kaplan-Meier model failed")
  }
print("kmfitdone")


if (!is.null(km_fit)) {
  print("🚀 DEBUG: Plotting Kaplan-Meier")
  
  km_plot <- tryCatch(
    ggsurvplot(
      km_fit,
      data = da_daten,
      censor.shape = "|", censor.size = 4,
      conf.int = TRUE,
      pval = TRUE,
      xlab = "Time in years",
      break.time.by = 5,
      surv.median.line = "hv",
      ncensor.plot = TRUE,
      ggtheme = theme_bw()
    ),
    error = function(e) {
      warning(paste("❌ Kaplan-Meier plot failed:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(km_plot)) {
    print("❌ ggplot konnte nicht erstellt werden.")
  } else {
    print("✅ ggplot erfolgreich erstellt.")
  }
} else {
  print("❌ km_fit ist NULL, daher kein Plot möglich.")
}


[1] "✅ Kaplan-Meier Model Summary:"
Call: survfit(formula = km_formula, data = da_daten)

2 Beobachtungen als fehlend gelöscht 
                Hyperfiltration140_1_ja=0 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   26    173       1    0.994 0.00576        0.983        1.000
   34    172       1    0.988 0.00813        0.973        1.000
   35    171       1    0.983 0.00992        0.963        1.000
   36    170       1    0.977 0.01143        0.955        1.000
   40    169       2    0.965 0.01391        0.938        0.993
   41    167       1    0.960 0.01498        0.931        0.989
   42    166       1    0.954 0.01597        0.923        0.986
   43    165       3    0.936 0.01855        0.901        0.973
   44    162       1    0.931 0.01932        0.894        0.969
   45    161       1    0.925 0.02004        0.886        0.965
   47    160       1    0.919 0.02073        0.879        0.961
   48    159       2    0.908 0.02203        0.865        0.952
   49    157       1    0.902 0.02263        0.858        0.947
   50    155       2    0.890 0.02379        0.845        0.938
   51    152       1    0.884 0.02434        0.838        0.933
   52    151       5    0.855 0.02683        0.804        0.909
   53    146       1    0.849 0.02728        0.797        0.904
   54    144       1    0.843 0.02772        0.791        0.899
   55    142       2    0.831 0.02857        0.777        0.889
   56    139       3    0.813 0.02977        0.757        0.874
   57    132       1    0.807 0.03018        0.750        0.869
   58    128       2    0.795 0.03100        0.736        0.858
   59    123       2    0.782 0.03181        0.722        0.847
   60    118       1    0.775 0.03222        0.714        0.841
   61    115       2    0.762 0.03304        0.700        0.829
   62    110       4    0.734 0.03462        0.669        0.805
   65     94       1    0.726 0.03512        0.660        0.798
   66     89       3    0.702 0.03667        0.633        0.777
   68     80       1    0.693 0.03725        0.624        0.770
   70     71       1    0.683 0.03798        0.613        0.762
   74     49       1    0.669 0.03968        0.596        0.752
   76     38       1    0.652 0.04236        0.574        0.740
   78     29       1    0.629 0.04648        0.544        0.727
   79     27       1    0.606 0.05026        0.515        0.713

                Hyperfiltration140_1_ja=1 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   19    124       1    0.992 0.00803        0.976        1.000
   34    123       1    0.984 0.01131        0.962        1.000
   35    122       1    0.976 0.01380        0.949        1.000
   40    121       1    0.968 0.01587        0.937        0.999
   45    120       1    0.960 0.01767        0.926        0.995
   46    119       1    0.952 0.01927        0.915        0.990
   50    117       1    0.943 0.02075        0.904        0.985
   55    110       1    0.935 0.02226        0.892        0.980
   56    107       1    0.926 0.02371        0.881        0.974
   57    101       1    0.917 0.02519        0.869        0.968
   60     87       2    0.896 0.02868        0.841        0.954
   61     79       1    0.885 0.03048        0.827        0.946
   70     39       1    0.862 0.03719        0.792        0.938
   75     24       1    0.826 0.05006        0.733        0.930

> print("kmfitdone")
[1] "kmfitdone"
> if (!is.null(km_fit)) {
+   print("🚀 DEBUG: Plotting Kaplan-Meier")
+   
+   km_plot <- tryCatch(
+     ggsurvplot(
+       km_fit,
+       data = da_daten,
+       censor.shape = "|", censor.size = 4,
+       conf.int = TRUE,
+       pval = TRUE,
+       xlab = "Time in years",
+       break.time.by = 5,
+       surv.median.line = "hv",
+       ncensor.plot = TRUE,
+       ggtheme = theme_bw()
+     ),
+     error = function(e) {
+       warning(paste("❌ Kaplan-Meier plot failed:", e$message))
+       return(NULL)
+     }
+   )
+   
+   if (is.null(km_plot)) {
+     print("❌ ggplot konnte nicht erstellt werden.")
+   } else {
+     print("✅ ggplot erfolgreich erstellt.")
+   }
+ } else {
+   print("❌ km_fit ist NULL, daher kein Plot möglich.")
+ }
[1] "🚀 DEBUG: Plotting Kaplan-Meier"
[1] "❌ ggplot konnte nicht erstellt werden."
Warnmeldung:
In value[[3L]](cond) :
  ❌ Kaplan-Meier plot failed: Objekt des Typs 'symbol' ist nicht indizierbar
>   if (is.null(km_plot)) {
+     print("❌ ggplot konnte nicht erstellt werden.")
+   } else {
+     print("✅ ggplot erfolgreich erstellt.")
+   }
[1] "❌ ggplot konnte nicht erstellt werden."
> if (!is.null(km_fit)) {
+   print("🚀 DEBUG: Plotting Kaplan-Meier")
+   
+   km_plot <- tryCatch(
+     ggsurvplot(
+       km_fit,
+       data = da_daten,
+       censor.shape = "|", censor.size = 4,
+       conf.int = TRUE,
+       pval = TRUE,
+       xlab = "Time in years",
+       break.time.by = 5,
+       surv.median.line = "hv",
+       ncensor.plot = TRUE,
+       ggtheme = theme_bw()
+     ),
+     error = function(e) {
+       warning(paste("❌ Kaplan-Meier plot failed:", e$message))
+       return(NULL)
+     }
+   )
+   
+   if (is.null(km_plot)) {
+     print("❌ ggplot konnte nicht erstellt werden.")
+   } else {
+     print("✅ ggplot erfolgreich erstellt.")
+   }
+ } else {
+   print("❌ km_fit ist NULL, daher kein Plot möglich.")
+ }
[1] "🚀 DEBUG: Plotting Kaplan-Meier"
[1] "❌ ggplot konnte nicht erstellt werden."
Warnmeldung:
In value[[3L]](cond) :
  ❌ Kaplan-Meier plot failed: Objekt des Typs 'symbol' ist nicht indizierbar

## Prompt 3

km_formula <- Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja

  
  print("🚀 DEBUG: Kaplan-Meier Formula")
  print(km_formula)  # Check if the formula is correctly generated
 
  km_fit <- tryCatch(
    survfit(km_formula, data = da_daten),  
    error = function(e) {
      print('ERRORRRRRRRRRR!!!!!!!!!!')
      warning(paste("❌ Kaplan-Meier fit failed:", e$message))
      return(NULL)
    }
  )
  
  
  if (!is.null(km_fit)) {
    print("✅ Kaplan-Meier Model Summary:")
    print(summary(km_fit))  # Ensure km_fit is not NULL
  } else {
    print("❌ Kaplan-Meier model failed")
  }
print("kmfitdone")


if (!is.null(km_fit)) {
  print("🚀 DEBUG: Plotting Kaplan-Meier")
  
  km_plot <- tryCatch(
    ggsurvplot(
      fit = km_fit,
      data = da_daten,
      censor.shape = "|", censor.size = 4,
      conf.int = TRUE,
      pval = TRUE,
      xlab = "Time in years",
      break.time.by = 5,
      surv.median.line = "hv",
      ncensor.plot = TRUE,
      ggtheme = theme_bw()
    ),
    error = function(e) {
      warning(paste("❌ Kaplan-Meier plot failed:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(km_plot)) {
    print("❌ ggplot konnte nicht erstellt werden.")
  } else {
    print("✅ ggplot erfolgreich erstellt.")
  }
} else {
  print("❌ km_fit ist NULL, daher kein Plot möglich.")
}

print("📊 Kaplan-Meier Summary:")
if (!is.null(km_fit)) print(summary(km_fit))
print("Kaplan-Meier Summary done")

print("plot done")

 }
[1] "🚀 DEBUG: Plotting Kaplan-Meier"
[1] "❌ ggplot konnte nicht erstellt werden."
Warnmeldung:
In value[[3L]](cond) :
  ❌ Kaplan-Meier plot failed: Objekt des Typs 'symbol' ist nicht indizierbar
>

## Prompt 4

🔹 1️⃣ Ensure Column Names Are Correct
Run this command to check if the column names match exactly:

r
Kopieren
Bearbeiten
print(colnames(da_daten))
Make sure all three variables exist with the exact spelling:
can you give me a function checking this

Alter_beitotwenntot_a_rounded
tot_1
Hyperfiltration140_1_ja
If they don't match, update your formula accordingly.

## Prompt 5

1️⃣ Ensure Column Names Are Correct All true

2️⃣ Ensure Variables Are in da_daten all num:> print(str(da_daten$Alter_beitotwenntot_a_rounded))
 num [1:324] 48 56 56 38 50 52 47 62 31 54 ...
NULL
> print(str(da_daten$tot_1))
 num [1:324] 1 0 1 1 1 1 1 0 1 0 ...
NULL
🔹 3️⃣ Explicitly Define Formula with as.formula()
done
🔹 4️⃣ Ensure ggsurvplot() Receives a Valid survfit Object
done
🔹 5️⃣ Fix ggsurvplot() Call
done

Still Fehler in x$formula : Objekt des Typs 'symbol' ist nicht indizierbar

## Prompt 6

List of 19
 $ n        : int [1:2] 198 124
 $ time     : num [1:103] 26 28 31 34 35 36 38 40 41 42 ...
 $ n.risk   : num [1:103] 198 197 196 195 194 193 192 190 187 185 ...
 $ n.event  : num [1:103] 1 1 1 1 1 1 2 3 2 3 ...
 $ n.censor : num [1:103] 0 0 0 0 0 0 0 0 0 0 ...
 $ surv     : num [1:103] 0.995 0.99 0.985 0.98 0.975 ...
 $ std.err  : num [1:103] 0.00506 0.00718 0.00881 0.0102 0.01144 ...
 $ cumhaz   : num [1:103] 0.00505 0.01013 0.01523 0.02036 0.02551 ...
 $ std.chaz : num [1:103] 0.00505 0.00716 0.00879 0.01018 0.01141 ...
 $ strata   : Named int [1:2] 58 45
  ..- attr(*, "names")= chr [1:2] "Hyperfiltration140_1_ja=0" "Hyperfiltration140_1_ja=1"
 $ type     : chr "right"
 $ logse    : logi TRUE
 $ conf.int : num 0.95
 $ conf.type: chr "log"
 $ lower    : num [1:103] 0.985 0.976 0.968 0.96 0.953 ...
 $ upper    : num [1:103] 1 1 1 1 0.997 ...
 $ t0       : num 0
 $ na.action: 'omit' Named int [1:2] 14 176
  ..- attr(*, "names")= chr [1:2] "14" "176"
 $ call     : language survfit(formula = km_formula, data = da_daten)
 - attr(*, "class")= chr "survfit"
NULL

## Prompt 7

Objekt des Typs 'symbol' ist nicht indizierbar whats that in english?

## Prompt 8

result_key <- paste(dataset_name, var, event_var, sep = "_")
        
        results[[result_key]] <- list(
          kaplan_meier_plot = km_plot,
          logrank_test = logrank_test,
          unadjusted_cox_model = unadjusted_cox_summary,
          adjusted_cox_model = adjusted_cox_summary,
          cox_forest_plot = forest_plot
        )
can i in the end of my function print all names stored in the results list, so i have an easier time calling the results?

## Prompt 9

i want to censor the remaining cases at the end of my kaplan meier plot

## Prompt 10

1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 1 0 0 1 0 1 0 0 0 1 0 0 0 1 1 1 1 1 1 0 0 0 1 0 0 1 0 0 1 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0 1 0 0 0 0 1 0 1 1 0 0 0 0 0 0 1 0 0 1 0 0 0
[100] 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0
[199] 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 1 0 1 0 0 1 0 0 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 0 1 1 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0
[298] 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 0 0 1 0 1 0 1 0 0 0 0
thats my tot_1 list, no NAs however the remaining cases are not censored in the plot

## Prompt 11

km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var, ") ~", var)
          )
i have my formula like this at the moment 
can i do 
km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var == 1, ") ~", var)
          )
shor answer suffices

## Prompt 12

results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)

# Access Kaplan-Meier Plot
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$kaplan_meier_plot)

# Access Log-Rank Test
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$logrank_test)

# Access Unadjusted Cox Model
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$unadjusted_cox_model)

# Access Adjusted Cox Model
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$adjusted_cox_model)

# Access Cox Forest Plot
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$cox_forest_plot)



#alle resultate für einen parameter abrufen (in diesem fall Hyperfiltration140_1_ja, aus allen datensätzen und allen endpunkten)
list_results <- list(
  "kaplan_meier_plot",
  "unadjusted_cox_model",
  "adkusted_cox_model",
  "logrank_test",
  "cox_forest_plot"
)
list_daten <- list(
  "da_daten",
  "subset_minusCKD")
list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_1_ja"
)
list_var <- list("Hyperfiltration140_1_ja")

ich habe das und hätte gerne eine funktion, mit der ich nach ebdarf alle resuöltate für einen parameter abrufen kann (es könnte auch alle resultate für da_daten, dafür mit mehreren unabhängigen variablen sein)

## Prompt 13

thanks a lot that works like a charm
for my plots: kaplan maier plot: suppressWarnings({
          km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var, "==1) ~", var)
          )
          
          #print("🚀 DEBUG: Kaplan-Meier Formula")
          #print(km_formula)  # Check if the formula is correctly generated
          
          km_fit <- tryCatch(
            surv_fit(km_formula, data = da_daten),  
            error = function(e) {
              print('ERRORRRRRRRRRR!!!!!!!!!!')
              warning(paste("❌ Kaplan-Meier fit failed:", e$message))
              return(NULL)
            }
          )
          
          
          if (!is.null(km_fit)) {
            print("✅ Kaplan-Meier Model Summary:")
            #print(summary(km_fit))  # Ensure km_fit is not NULL
          } else {
            print("❌ Kaplan-Meier model failed")
          }
        })
        
                                
         if (!is.null(km_fit)) {
           #print("🚀 DEBUG: Plotting Kaplan-Meier")
           
           km_plot <- tryCatch(
             ggsurvplot(
               km_fit,
               data = da_daten,
               censor.shape = "|", censor.size = 4,
               conf.int = TRUE,
               pval = TRUE,
               xlab = "Time in years",
               break.time.by = 5,
               surv.median.line = "hv",
               ncensor.plot = TRUE,
               #risk.table = TRUE,  #optional
               cumevents = TRUE,  #optional
               cumevents.height = 0.2,  #optional
               ggtheme = theme_bw()
             ),
             error = function(e) {
               warning(paste("❌ Kaplan-Meier plot failed:", e$message))
               return(NULL)
             }
           )
           
           if (is.null(km_plot)) {
             print("❌ ggplot konnte nicht erstellt werden.")
           } else {
             print("✅ ggplot erfolgreich erstellt.")
           }
         } else {
           print("❌ km_fit ist NULL, daher kein Plot möglich.")
         }
and forest plots:
if (!is.null(adjusted_cox_model)) {
          cox_summary <- summary(adjusted_cox_model)
          hr_values <- exp(cox_summary$coefficients[, "coef"]) # Extract Hazard Ratios
          ci_lower <- exp(cox_summary$conf.int[, "lower .95"])  # Lower 95% CI
          ci_upper <- exp(cox_summary$conf.int[, "upper .95"])  # Upper 95% CI
          
     
          
          # Check for extreme values
          if (any(is.infinite(hr_values)) || any(ci_lower <= 0) || any(ci_upper >= 1e5)) {
            warning("⚠️ Skipping forest plot due to extreme HR values")
            forest_plot <- NULL
          } else {
            forest_plot <- tryCatch(
              ggforest(adjusted_cox_model, data =  model.frame(adjusted_cox_model)),
              error = function(e) {
                warning("⚠️ Forest plot failed for ", dataset_name, " - ", var, " with error: ", e$message)
                return(NULL)
              }
            )
          }
        } else {
          forest_plot <- NULL
        }
it would be cool to add what exact data (dataset,endpoint,variable) they come from as i might be retrieving a number of them at once, can i add that information (to tjhe title e.g.)

## Prompt 14

datensatz <- da_daten
confounder_list <- list(
  "RRsys",
  "HBA1c",
  "BMI"
)
ersatzwert <- "mean" #wert mit dem die NAs ersetzt werden sollen
for (conf in confounder_list) {
  ersatz_formel <- as.formula(paste(ersatzwert,"(",conf,")"))
  ersatz <- ersatz_formel
  new_name <- paste0(conf,"_completed_",ersatzwert)
  datensatz[new_name][is.na(datensatz[conf])] <-ersatz
}

## Prompt 15

thanks, however i wanted to approach in which i could replace mean for any other computable value without changing the function, hence the introduction of ersatzwert in the first place4

## Prompt 16

okay nice but insted of replacing i would rather create a new column named original variable(dynamically)_"ompleted"_used function (dynamically)

## Prompt 17

Error in `[[<-`:
! Assigned data `*vtmp*` must be compatible with existing data.
✖ Existing data has 324 rows.
✖ Assigned data has 0 rows.
ℹ Only vectors of size 1 are recycled.
Caused by error in `vectbl_recycle_rhs_rows()`:
! Can't recycle input of size 0 to size 324.
Run `rlang::last_trace()` to see where the error occurred.
Warnmeldung:
In mean.default(data[[conf]], na.rm = TRUE) :
  Argument ist weder numerisch noch boolesch: gebe NA zurück
