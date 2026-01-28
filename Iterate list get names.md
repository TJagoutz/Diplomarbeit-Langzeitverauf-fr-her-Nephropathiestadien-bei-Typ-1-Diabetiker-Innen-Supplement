# Iterate list get names

## Prompt 1

Hi, I am currently working in R and I have this list:
list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
)

Now I am iterating over this list and want to get the name of each entry.

## Prompt 2

In my for loops:
for (daten_name in names(list_daten)) {
    daten <- list_daten[[daten_name]]  # Extract dataset
    
    for (i in seq_along(list_surv_time)) {
      surv_time_var <- list_surv_time[[i]]
      endpoint_var <- list_endpoint[[i]]

How can I enumerate over my list

## Prompt 3

However, I print the index i and it returns ' '

## Prompt 4

List of 3
 $ : chr "Alter_beitotwenntot_a_rounded"
 $ : chr "NE_surv_all_a_tim"
 $ : chr "time_to_endpoint_a_tim"

## Prompt 5

I have this function:
list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
)

list_var <- list(
  "Hyperfiltration140_1_ja",
  "Hyperfiltration130_1_ja",
  "Hyperfiltration150_1_ja",
  "HyperfiltrationMedian_1_ja"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "Alter_beitotwenntot_a_rounded",
  "NE_surv_all_a_tim",
  "time_to_endpoint_a_tim"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
list_confounder <- list(
  "RRsyst",
  "RRdiast",  
  "NE_1_ja",
  "highest_measured_hba1c_naisna"
)

list_kathi <- list('K', 'A', 'T', 'H')
for (i in seq_along(list_surv_time)) {
  print(i)
}
  
kaplan_maier_logrank_coxreg_func <- function(list_daten, list_surv_time, list_endpoint, list_var, confounder_list){
  list_kaplan_graph <- list()
  list_coxreg <- list()
  list_coxgraph <- list()
  list_logrank <- list()
  list_forest <- list()
  
  for (daten_name in names(list_daten)) {
    daten <- list_daten[[daten_name]]  # Extract dataset
    
    for ( i in seq_along(list_surv_time)) {
      surv_time_var <- list_surv_time[[i]]
      endpoint_var <- list_endpoint[[i]]
     
      
      if (!surv_time_var %in% names(daten)) {
        print(paste("Warning:", surv_time_var, "not found in", daten_name))
        next
      }
      if (!endpoint_var %in% names(daten)) {
        print(paste("Warning:", endpoint_var, "not found in", daten_name))
        next
      }
      #daten <- na.omit(daten[, c(list_surv_time[[i]], list_endpoint[[i]])])
      surv_var <- Surv(daten[[surv_time_var]], daten[[endpoint_var]])
      print(surv_var)
      
      for (var in list_var) {
        if (!var %in% names(daten)) {
          print(paste("Warning:", var, "not found in", daten_name)) 
          next
        }
        
        fit <- survfit(Surv(daten[[list_surv_time[[i]]]], daten[[list_endpoint[[i]]]]) ~ daten[[var]], data = daten)
        graph <- ggsurvplot(
          fit,
          censor.shape = "|", censor.size = 4,
          conf.int = TRUE,
          pval = TRUE,
          xlab = "Time in years",
          break.time.by = 5,
          surv.median.line = "hv",
          ncensor.plot = TRUE,
          legend.labs = c("Hyperfiltration", "keine Hyperfiltration"),
          ggtheme = theme_bw()
        )
        
        logrank <- survdiff(Surv(daten[[list_surv_time[[i]]]], daten[[list_endpoint[[i]]]]) ~ daten[[var]], data = daten)
        cox_formula <- as.formula(paste(surv_time_var, "+", endpoint_var, "~", var, "+", paste(confounder_list, collapse = " + ")))
        cox <- coxph(cox_formula, data = daten)
        cox_zph <- cox.zph(cox)
        cox_graph <- ggcoxzph(cox_zph)
        forest <- ggforest(cox, main = "Hazard ratio", cpositions = c(0.02, 0.22, 0.4), refLabel = "reference", noDigits = 2)
        
        if (!is.list(list_kaplan_graph[[daten_name]])) list_kaplan_graph[[daten_name]] <- list()
        if (!is.list(list_kaplan_graph[[daten_name]][[endpoint_var]])) list_kaplan_graph[[daten_name]][[endpoint_var]] <- list()
        
        list_kaplan_graph[[daten_name]][[endpoint_var]][[var]] <- graph
        list_coxreg[[daten_name]][[endpoint_var]][[var]] <- cox
        list_coxgraph[[daten_name]][[endpoint_var]][[var]] <- cox_graph
        list_logrank[[daten_name]][[endpoint_var]][[var]] <- logrank
        list_forest[[daten_name]][[endpoint_var]][[var]] <- forest
      }
    }
  }
  
  return(list(kaplan = list_kaplan_graph, coxreg = list_coxreg, coxgraph = list_coxgraph, logrank = list_logrank, forest = list_forest))
}

And I get this error: Fehler in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
  Argumente implizieren unterschiedliche Anzahl Zeilen: 324, 1, 0

## Prompt 6

I think the issue must be the mssing data, can you correct my code

## Prompt 7

Processing dataset: da_daten 
Length after filtering missing values - Alter_beitotwenntot_a_rounded : 322 
Length after filtering missing values - tot_1 : 322 
Fehler in eval(fit$call$data) : Objekt 'daten_var_filtered' nicht gefunden

## Prompt 8

I think the issue must be the mssing data, can you correct my code. Lets solve it with na.omit()

## Prompt 9

daten_filtered <- na.omit(daten[, c(surv_time_var, endpoint_var, list_var, confounder_list)]) filtert diese funktion alle daten, die in mindestens einer angegeben Kategorie NA haben?

## Prompt 10

what command can i use to print the number of observations  (using the ID column) before and after filtering

## Prompt 11

kaplan_maier_logrank_coxreg_func <- function(list_daten, list_surv_time, list_endpoint, list_var, confounder_list){
  list_kaplan_graph <- list()
  list_coxreg <- list()
  list_coxgraph <- list()
  list_logrank <- list()
  list_forest <- list()
  
  for (daten_name in names(list_daten)) {
    daten <- list_daten[[daten_name]]  # Extract dataset
    
    for ( i in seq_along(list_surv_time)) {
      surv_time_var <- list_surv_time[[i]]
      endpoint_var <- list_endpoint[[i]]
     
      
      if (!surv_time_var %in% names(daten)) {
        print(paste("Warning:", surv_time_var, "not found in", daten_name))
        next
      }
      if (!endpoint_var %in% names(daten)) {
        print(paste("Warning:", endpoint_var, "not found in", daten_name))
        next
      }
      #daten_filtered <- na.omit(daten[, c(list_surv_time[[i]], list_endpoint[[i]])])

      surv_var <- Surv(daten[[surv_time_var]], daten[[endpoint_var]])
      
      for (var in list_var) {
        if (!var %in% names(daten)) {
          print(paste("Warning:", var, "not found in", daten_name)) 
          next
        }
        
        #daten_filtered <- na.omit(daten[, c(surv_time_var, endpoint_var, list_var, confounder_list)]) #n.a. daten filtern au allen genutzten variablen
        daten_filtered <- daten[!is.na(daten[[surv_time_var]]) & !is.na(daten[[endpoint_var]]), ] #NA daten filtern aus survival time und endpunkt
        cat("Total rows before filtering:", nrow(daten), "\n") #check wie viele zeilen eliminiert wurden
        cat("Total rows after filtering:", nrow(daten_filtered), "\n")
        
        fit <- survfit(Surv(daten_filtered[[list_surv_time[[i]]]], daten_filtered[[list_endpoint[[i]]]]) ~ daten_filtered[[var]], data = daten_filtered)
        graph <- ggsurvplot(
          fit,
          censor.shape = "|", censor.size = 4,
          conf.int = TRUE,
          pval = TRUE,
          xlab = "Time in years",
          break.time.by = 5,
          surv.median.line = "hv",
          ncensor.plot = TRUE,
          legend.labs = c("Hyperfiltration", "keine Hyperfiltration"),
          ggtheme = theme_bw()
        )
        
        logrank <- survdiff(Surv(daten_filtered[[list_surv_time[[i]]]], daten_filtered[[list_endpoint[[i]]]]) ~ daten_filtered[[var]], data = daten_filtered)
        cox_formula <- as.formula(paste(surv_time_var, "+", endpoint_var, "~", var, "+", paste(confounder_list, collapse = " + ")))
        cox <- coxph(cox_formula, data = daten_filtered)
        cox_zph <- cox.zph(cox)
        cox_graph <- ggcoxzph(cox_zph)
        forest <- ggforest(cox, main = "Hazard ratio", cpositions = c(0.02, 0.22, 0.4), refLabel = "reference", noDigits = 2)
        
        if (!is.list(list_kaplan_graph[[daten_name]])) list_kaplan_graph[[daten_name]] <- list()
        if (!is.list(list_kaplan_graph[[daten_name]][[endpoint_var]])) list_kaplan_graph[[daten_name]][[endpoint_var]] <- list()
        
        list_kaplan_graph[[daten_name]][[endpoint_var]][[var]] <- graph
        list_coxreg[[daten_name]][[endpoint_var]][[var]] <- cox
        list_coxgraph[[daten_name]][[endpoint_var]][[var]] <- cox_graph
        list_logrank[[daten_name]][[endpoint_var]][[var]] <- logrank
        list_forest[[daten_name]][[endpoint_var]][[var]] <- forest
      }
    }
  }
  
  return(list(kaplan = list_kaplan_graph, coxreg = list_coxreg, coxgraph = list_coxgraph, logrank = list_logrank, forest = list_forest))
}

## Prompt 12

Error in `daten[, filter_vars, drop = FALSE]`:
! Can't subset columns with `filter_vars`.
✖ `filter_vars` must be logical, numeric, or character, not a list.

## Prompt 13

Error in `daten[, filter_vars, drop = FALSE]`:
! Can't subset columns that don't exist.
✖ Columns `HyperfiltrationMedian` and ` RRsyst` don't exist.
Run `rlang::last_trace()` to see where the error occurred.
>

## Prompt 14

fit <- survfit(Surv(daten_filtered[[list_surv_time[[i]]]], daten_filtered[[list_endpoint[[i]]]]) ~ daten_filtered[[var]], data = daten_filtered)
        graph <- ggsurvplot(
          fit,
          censor.shape = "|", censor.size = 4,
          conf.int = TRUE,
          pval = TRUE,
          xlab = "Time in years",
          break.time.by = 5,
          surv.median.line = "hv",
          ncensor.plot = TRUE,
          legend.labs = c("Hyperfiltration", "keine Hyperfiltration"),
          ggtheme = theme_bw()
        )
        
gives back 
Objekt 'daten_filtered' nicht gefunden

## Prompt 15

Fehler in eval(fit$call$data) : Objekt 'daten_filtered' nicht gefunden
sitll gives back thio

## Prompt 16

Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
Call: survfit(formula = Surv(time = daten_filtered[[list_surv_time[[i]]]], 
    event = daten_filtered[[list_endpoint[[i]]]]) ~ daten_filtered[[var]], 
    data = daten_filtered)

                          n events median 0.95LCL 0.95UCL
daten_filtered[[var]]=0 164     61     NA    78.1      NA
daten_filtered[[var]]=1 105     12     NA      NA      NA
Fehler in FUN(X[[i]], ...) : Objekt 'daten_filtered' nicht gefunden

## Prompt 17

graph <- ggsurvplot(
            fit,
            data = daten_filtered,  # ✅ Explicitly specify data to prevent "object not found" error
            censor.shape = "|", censor.size = 4,
            conf.int = TRUE,
            pval = TRUE,
            xlab = "Time in years",
            break.time.by = 5,
            surv.median.line = "hv",
            ncensor.plot = TRUE,
            legend.labs = c("Hyperfiltration", "keine Hyperfiltration"),
            ggtheme = theme_bw()
          )
it looks like this

## Prompt 18

Fehler in FUN(X[[i]], ...) : Objekt 'daten_filtered' nicht gefunden

## Prompt 19

Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Checking daten_filtered before running survfit:
# A tibble: 6 × 8
  Alter_beitotwenntot_a tot_1 Hyperfiltration140_1_ja Hyperfiltration130_1_ja Hyperfiltration150_1_ja RRdiast NE_1_ja highest_measured_hba1c_naisna
                  <dbl> <dbl> <chr>                   <chr>                   <chr>                     <dbl>   <dbl>                         <dbl>
1                  48.3     1 0                       0                       0                           100       1                           6.9
2                  55.5     0 0                       1                       0                            80       0                           7.1
3                  55.7     1 0                       0                       0                            85       1                           7.7
4                  37.5     1 0                       0                       0                            80       1                          12.6
5                  50.0     1 0                       0                       0                            80       0                           9  
6                  51.7     1 0                       0                       0                           100       1                           5.7
tibble [269 × 8] (S3: tbl_df/tbl/data.frame)
 $ Alter_beitotwenntot_a        : num [1:269] 48.3 55.5 55.7 37.5 50 ...
 $ tot_1                        : num [1:269] 1 0 1 1 1 1 1 0 1 0 ...
 $ Hyperfiltration140_1_ja      : chr [1:269] "0" "0" "0" "0" ...
 $ Hyperfiltration130_1_ja      : chr [1:269] "0" "1" "0" "0" ...
 $ Hyperfiltration150_1_ja      : chr [1:269] "0" "0" "0" "0" ...
 $ RRdiast                      : num [1:269] 100 80 85 80 80 100 80 90 85 70 ...
 $ NE_1_ja                      : num [1:269] 1 0 1 1 0 1 1 1 1 0 ...
 $ highest_measured_hba1c_naisna: num [1:269] 6.9 7.1 7.7 12.6 9 5.7 6.7 9.5 6.6 9.5 ...
 - attr(*, "na.action")= 'omit' Named int [1:55] 14 16 25 28 29 45 47 52 53 57 ...
  ..- attr(*, "names")= chr [1:55] "14" "16" "25" "28" ...
NULL
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = Surv(time = daten_filtered[[list_surv_time[[i]]]], 
    event = daten_filtered[[list_endpoint[[i]]]]) ~ daten_filtered[[var]], 
    data = daten_filtered)

                          n events median 0.95LCL 0.95UCL
daten_filtered[[var]]=0 164     61     NA    78.1      NA
daten_filtered[[var]]=1 105     12     NA      NA      NA

## Prompt 20

Fehler in terms.formula(formula, data = data) : 
  ungültiger Term in Modellformel

## Prompt 21

Fehler in x$formula : Objekt des Typs 'symbol' ist nicht indizierbar

## Prompt 22

still the same error

## Prompt 23

✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.

## Prompt 24

✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.

## Prompt 25

Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 164     61     NA      78      NA
Hyperfiltration140_1_ja=1 105     12     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 128     56     78      66      NA
Hyperfiltration130_1_ja=1 141     17     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 192     63     NA      NA      NA
Hyperfiltration150_1_ja=1  77     10     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 132     57     78      67      NA
HyperfiltrationMedian_1_ja=1 137     16     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 165     32     NA      NA      NA
Hyperfiltration140_1_ja=1 105      3     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 129     30     NA      NA      NA
Hyperfiltration130_1_ja=1 141      5     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 193     33     NA      NA      NA
Hyperfiltration150_1_ja=1  77      2     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 133     30     NA      NA      NA
HyperfiltrationMedian_1_ja=1 137      5     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 164     68     NA    74.4      NA
Hyperfiltration140_1_ja=1 105     15     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 128     62   75.9    61.6      NA
Hyperfiltration130_1_ja=1 141     21     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 192     71     NA    78.1      NA
Hyperfiltration150_1_ja=1  77     12     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 132     63   75.9    62.3      NA
HyperfiltrationMedian_1_ja=1 137     20     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 143     45     NA      79      NA
Hyperfiltration140_1_ja=1 105     12     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 107     40     NA      76      NA
Hyperfiltration130_1_ja=1 141     17     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 171     47     NA      NA      NA
Hyperfiltration150_1_ja=1  77     10     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 111     41     NA      76      NA
HyperfiltrationMedian_1_ja=1 137     16     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 144     21     NA      NA      NA
Hyperfiltration140_1_ja=1 105      3     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 108     19     NA      NA      NA
Hyperfiltration130_1_ja=1 141      5     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 172     22     NA      NA      NA
Hyperfiltration150_1_ja=1  77      2     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 112     19     NA      NA      NA
HyperfiltrationMedian_1_ja=1 137      5     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 143     50     NA    78.1      NA
Hyperfiltration140_1_ja=1 105     15     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 107     44     79    74.4      NA
Hyperfiltration130_1_ja=1 141     21     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 171     53     NA      NA      NA
Hyperfiltration150_1_ja=1  77     12     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 111     45     79    74.4      NA
HyperfiltrationMedian_1_ja=1 137     20     NA      NA      NA
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.

## Prompt 26

Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 164     61     NA      78      NA
Hyperfiltration140_1_ja=1 105     12     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 128     56     78      66      NA
Hyperfiltration130_1_ja=1 141     17     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 192     63     NA      NA      NA
Hyperfiltration150_1_ja=1  77     10     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 132     57     78      67      NA
HyperfiltrationMedian_1_ja=1 137     16     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 165     32     NA      NA      NA
Hyperfiltration140_1_ja=1 105      3     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 129     30     NA      NA      NA
Hyperfiltration130_1_ja=1 141      5     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 193     33     NA      NA      NA
Hyperfiltration150_1_ja=1  77      2     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 270 
✅ Running survival analysis with 270 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 133     30     NA      NA      NA
HyperfiltrationMedian_1_ja=1 137      5     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 164     68     NA    74.4      NA
Hyperfiltration140_1_ja=1 105     15     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 128     62   75.9    61.6      NA
Hyperfiltration130_1_ja=1 141     21     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 192     71     NA    78.1      NA
Hyperfiltration150_1_ja=1  77     12     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 324 
Total rows after filtering: 269 
✅ Running survival analysis with 269 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 132     63   75.9    62.3      NA
HyperfiltrationMedian_1_ja=1 137     20     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 143     45     NA      79      NA
Hyperfiltration140_1_ja=1 105     12     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 107     40     NA      76      NA
Hyperfiltration130_1_ja=1 141     17     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 171     47     NA      NA      NA
Hyperfiltration150_1_ja=1  77     10     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: Alter_beitotwenntot_a_rounded endpoint_var: tot_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(Alter_beitotwenntot_a_rounded, tot_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 111     41     NA      76      NA
HyperfiltrationMedian_1_ja=1 137     16     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 144     21     NA      NA      NA
Hyperfiltration140_1_ja=1 105      3     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 108     19     NA      NA      NA
Hyperfiltration130_1_ja=1 141      5     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 172     22     NA      NA      NA
Hyperfiltration150_1_ja=1  77      2     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 249 
✅ Running survival analysis with 249 observations
✅ Variables extracted - surv_time_var: NE_surv_all_a_tim endpoint_var: NE_1_ja predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(NE_surv_all_a_tim, NE_1_ja) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 112     19     NA      NA      NA
HyperfiltrationMedian_1_ja=1 137      5     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration140_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration140_1_ja=0 143     50     NA    78.1      NA
Hyperfiltration140_1_ja=1 105     15     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration140_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration130_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration130_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration130_1_ja=0 107     44     79    74.4      NA
Hyperfiltration130_1_ja=1 141     21     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration130_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: Hyperfiltration150_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration150_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                            n events median 0.95LCL 0.95UCL
Hyperfiltration150_1_ja=0 171     53     NA      NA      NA
Hyperfiltration150_1_ja=1  77     12     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "Hyperfiltration150_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
Total rows before filtering: 299 
Total rows after filtering: 248 
✅ Running survival analysis with 248 observations
✅ Variables extracted - surv_time_var: time_to_endpoint_a_tim endpoint_var: endpoint_reached_ja_1 predictor_var: HyperfiltrationMedian_1_ja 
✅ Survival formula created: Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja 
✅ Checking fit object before ggsurvplot:
Call: survfit(formula = survival_formula, data = daten_filtered)

                               n events median 0.95LCL 0.95UCL
HyperfiltrationMedian_1_ja=0 111     45     79    74.4      NA
HyperfiltrationMedian_1_ja=1 137     20     NA      NA      NA
✅ Debugging before ggsurvplot:
Legend title: "HyperfiltrationMedian_1_ja" 
Legend labels: 0 1 
❌ Error in ggsurvplot: Objekt des Typs 'symbol' ist nicht indizierbar 
❌ Skipping this iteration due to ggsurvplot error.
