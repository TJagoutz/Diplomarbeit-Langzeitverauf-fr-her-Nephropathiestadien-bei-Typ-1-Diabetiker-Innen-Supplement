# Survival Analysis Results Export

## Prompt 1

I have a survival analysis,
it gives me this outputs:
Call:
survdiff(formula = Surv(as.numeric(get(time_var, daten_filtered)), 
    as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered))

                             N Observed Expected (O-E)^2/E (O-E)^2/V
get(var, daten_filtered)=0 151       55     41.8      4.19      9.02
get(var, daten_filtered)=1 144       24     37.2      4.70      9.02

 Chisq= 9  on 1 degrees of freedom, p= 0.003 
> # Access Unadjusted Cox Model
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$unadjusted_cox_model)
Call:
coxph(formula = unadjusted_cox_formula, data = daten_filtered)

  n= 295, number of events= 79 

                         coef exp(coef) se(coef)      z Pr(>|z|)   
hyperfiltration_egfr1 -0.7211    0.4862   0.2452 -2.941  0.00328 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                      exp(coef) exp(-coef) lower .95 upper .95
hyperfiltration_egfr1    0.4862      2.057    0.3007    0.7863

Concordance= 0.579  (se = 0.029 )
Likelihood ratio test= 9.32  on 1 df,   p=0.002
Wald test            = 8.65  on 1 df,   p=0.003
Score (logrank) test = 9.02  on 1 df,   p=0.003

> # Access Cox Forest Plot
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$cox_forest_plot)
> # Access Adjusted Cox Model
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$adjusted_cox_model)
Call:
coxph(formula = cox_formula, data = daten_filtered)

  n= 295, number of events= 79 

                                                  coef exp(coef)  se(coef)      z Pr(>|z|)    
hyperfiltration_egfr1                        -0.496670  0.608554  0.398845 -1.245  0.21303    
RRsyst_completed_mean                         0.022938  1.023203  0.008739  2.625  0.00867 ** 
highest_measured_hba1c_naisna_completed_mean -0.029642  0.970793  0.108115 -0.274  0.78396    
BMI_completed_mean                            0.008181  1.008214  0.042191  0.194  0.84626    
UN_completed_mean                             0.017970  1.018133  0.017823  1.008  0.31333    
Diabetesdauer_totwenntot_a_tim               -0.064921  0.937141  0.021814 -2.976  0.00292 ** 
Geschlecht_1_m...29                          -0.101486  0.903494  0.332651 -0.305  0.76030    
Alter_beitotwenntot_a                        -2.503642  0.081787  0.300366 -8.335  < 2e-16 ***
ProteinJa_1                                  -0.334420  0.715753  0.382222 -0.875  0.38161    
NBZ_completed_mean                            0.001002  1.001003  0.002430  0.412  0.68010    
ppBZ_completed_mean                          -0.001679  0.998323  0.002340 -0.717  0.47308    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                                             exp(coef) exp(-coef) lower .95 upper .95
hyperfiltration_egfr1                          0.60855     1.6432    0.2785    1.3298
RRsyst_completed_mean                          1.02320     0.9773    1.0058    1.0409
highest_measured_hba1c_naisna_completed_mean   0.97079     1.0301    0.7854    1.1999
BMI_completed_mean                             1.00821     0.9919    0.9282    1.0951
UN_completed_mean                              1.01813     0.9822    0.9832    1.0543
Diabetesdauer_totwenntot_a_tim                 0.93714     1.0671    0.8979    0.9781
Geschlecht_1_m...29                            0.90349     1.1068    0.4707    1.7341
Alter_beitotwenntot_a                          0.08179    12.2269    0.0454    0.1474
ProteinJa_1                                    0.71575     1.3971    0.3384    1.5139
NBZ_completed_mean                             1.00100     0.9990    0.9962    1.0058
ppBZ_completed_mean                            0.99832     1.0017    0.9938    1.0029

Concordance= 0.997  (se = 0.001 )
Likelihood ratio test= 645.4  on 11 df,   p=<2e-16
Wald test            = 90.58  on 11 df,   p=1e-14
Score (logrank) test = 386.4  on 11 df,   p=<2e-16

i want to save them in 3 cvs files
the result names are cerated using :

## Prompt 2

I have a survival analysis,
it gives me this outputs:
Call:
survdiff(formula = Surv(as.numeric(get(time_var, daten_filtered)), 
    as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered))

                             N Observed Expected (O-E)^2/E (O-E)^2/V
get(var, daten_filtered)=0 151       55     41.8      4.19      9.02
get(var, daten_filtered)=1 144       24     37.2      4.70      9.02

 Chisq= 9  on 1 degrees of freedom, p= 0.003 
> # Access Unadjusted Cox Model
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$unadjusted_cox_model)
Call:
coxph(formula = unadjusted_cox_formula, data = daten_filtered)

  n= 295, number of events= 79 

                         coef exp(coef) se(coef)      z Pr(>|z|)   
hyperfiltration_egfr1 -0.7211    0.4862   0.2452 -2.941  0.00328 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                      exp(coef) exp(-coef) lower .95 upper .95
hyperfiltration_egfr1    0.4862      2.057    0.3007    0.7863

Concordance= 0.579  (se = 0.029 )
Likelihood ratio test= 9.32  on 1 df,   p=0.002
Wald test            = 8.65  on 1 df,   p=0.003
Score (logrank) test = 9.02  on 1 df,   p=0.003

> # Access Cox Forest Plot
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$cox_forest_plot)
> # Access Adjusted Cox Model
> print(results[["da_daten_hyperfiltration_egfr_tot_1"]]$adjusted_cox_model)
Call:
coxph(formula = cox_formula, data = daten_filtered)

  n= 295, number of events= 79 

                                                  coef exp(coef)  se(coef)      z Pr(>|z|)    
hyperfiltration_egfr1                        -0.496670  0.608554  0.398845 -1.245  0.21303    
RRsyst_completed_mean                         0.022938  1.023203  0.008739  2.625  0.00867 ** 
highest_measured_hba1c_naisna_completed_mean -0.029642  0.970793  0.108115 -0.274  0.78396    
BMI_completed_mean                            0.008181  1.008214  0.042191  0.194  0.84626    
UN_completed_mean                             0.017970  1.018133  0.017823  1.008  0.31333    
Diabetesdauer_totwenntot_a_tim               -0.064921  0.937141  0.021814 -2.976  0.00292 ** 
Geschlecht_1_m...29                          -0.101486  0.903494  0.332651 -0.305  0.76030    
Alter_beitotwenntot_a                        -2.503642  0.081787  0.300366 -8.335  < 2e-16 ***
ProteinJa_1                                  -0.334420  0.715753  0.382222 -0.875  0.38161    
NBZ_completed_mean                            0.001002  1.001003  0.002430  0.412  0.68010    
ppBZ_completed_mean                          -0.001679  0.998323  0.002340 -0.717  0.47308    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                                             exp(coef) exp(-coef) lower .95 upper .95
hyperfiltration_egfr1                          0.60855     1.6432    0.2785    1.3298
RRsyst_completed_mean                          1.02320     0.9773    1.0058    1.0409
highest_measured_hba1c_naisna_completed_mean   0.97079     1.0301    0.7854    1.1999
BMI_completed_mean                             1.00821     0.9919    0.9282    1.0951
UN_completed_mean                              1.01813     0.9822    0.9832    1.0543
Diabetesdauer_totwenntot_a_tim                 0.93714     1.0671    0.8979    0.9781
Geschlecht_1_m...29                            0.90349     1.1068    0.4707    1.7341
Alter_beitotwenntot_a                          0.08179    12.2269    0.0454    0.1474
ProteinJa_1                                    0.71575     1.3971    0.3384    1.5139
NBZ_completed_mean                             1.00100     0.9990    0.9962    1.0058
ppBZ_completed_mean                            0.99832     1.0017    0.9938    1.0029

Concordance= 0.997  (se = 0.001 )
Likelihood ratio test= 645.4  on 11 df,   p=<2e-16
Wald test            = 90.58  on 11 df,   p=1e-14
Score (logrank) test = 386.4  on 11 df,   p=<2e-16

i want to save them in 3 cvs files
the result names are cerated using :
dataset/variable/endpoint/analysis

i want a function that creats the cvs files by first setting dataset, variable and endpoint and then create one file for each analysis (see name in the given output)
the cvs should be named accordingly so i haave an overview

## Prompt 3

Fehler: No tidy method for objects of class summary.coxph

## Prompt 4

Wrote CSVs to:
  analysis_outputs/da_daten/hyperfiltration_egfr/endpoint_reached_ja_1
Warnmeldung:
In data.frame(..., check.names = FALSE) :
  Zeilennamen wurden in einer short Variablen gefunden und wurden verworfen

## Prompt 5

Wrote CSVs to:
  analysis_outputs/da_daten/hyperfiltration_egfr/endpoint_reached_ja_1
Warnmeldung:
In data.frame(..., check.names = FALSE) :
  Zeilennamen wurden in einer short Variablen gefunden und wurden verworfen

what does that mean?

## Prompt 6

im ot sure i understand what i shoould change

## Prompt 7

i stil get the same warnign

## Prompt 8

does this function write 3 seperate cvs files as requested?

## Prompt 9

great i think that might have worked, however i dont find the cvs file, where was it stored to?

## Prompt 10

its not there

## Prompt 11

# Datasets (named)
datasets <- list(
  "Gesamt" = da_daten,
  "HF130" = subset_Hyperfiltration130,
  "HF140" = subset_Hyperfiltration140,
  "HF150" = subset_Hyperfiltration150,
  "HF_Median" = subset_HyperfiltrationMedian
)

# Output folder
dir.create("raw_datasets", showWarnings = FALSE)

# Export each dataset as one CSV with selected columns
for (ds_name in names(datasets)) {
  datensatz <- datasets[[ds_name]]
  if (all(raw_vars %in% names(datensatz))) {
    data_out <- datensatz[, raw_vars]
    write.csv(data_out, file = paste0("C:/Users/timmy/Downloads/", ds_name, "_raw.csv"), row.names = FALSE)
    cat("✅ Exported:", ds_name, "\n")
  } else {
    cat("⚠️ Skipped:", ds_name, "- some variables missing\n")
  }
}


save_survival_results <- function(results,
                                  dataset,
                                  variable,
                                  endpoint,
                                  output_dir = "C:/Users/timmy/Downloads") 
  {
  # ensure broom is installed
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop("Please install the broom package: install.packages('broom')")
  }
  
  # build the folder path and create it
  dir_path <- file.path(output_dir, dataset, variable, endpoint)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  # 1) survdiff → tidy via broom (no rownames issue here)
  sd_tbl <- broom::tidy(results$survdiff)
  sd_file <- file.path(
    dir_path,
    paste0(dataset, "_", variable, "_", endpoint, "_survdiff.csv")
  )
  write.csv(sd_tbl, sd_file, row.names = FALSE)
  
  # helper to turn a coxph or summary.coxph into a proper data.frame
  tidy_cox <- function(obj) {
    # raw coxph: broom has a method
    if (inherits(obj, "coxph")) {
      return(broom::tidy(obj, exponentiate = TRUE, conf.int = TRUE))
    }
    # summary.coxph: pull everything manually
    if (inherits(obj, "summary.coxph")) {
      coefs <- obj$coefficients
      conf  <- obj$conf.int
      # build data.frame, explicitly moving rownames → term
      df <- data.frame(
        term       = rownames(coefs),
        coef       = coefs[, "coef"],
        exp_coef   = coefs[, "exp(coef)"],
        se_coef    = coefs[, "se(coef)"],
        z          = coefs[, "z"],
        p.value    = coefs[, "Pr(>|z|)"],
        conf.low   = conf[, "lower .95"],
        conf.high  = conf[, "upper .95"],
        stringsAsFactors = FALSE
      )
      return(df)
    }
    stop("Object is not a coxph or summary.coxph")
  }
  
  # 2) unadjusted
  ua_tbl  <- tidy_cox(results$unadjusted_cox_model)
  ua_file <- file.path(
    dir_path,
    paste0(dataset, "_", variable, "_", endpoint, "_unadjusted_cox.csv")
  )
  write.csv(ua_tbl, ua_file, row.names = FALSE)
  
  # 3) adjusted
  adj_tbl  <- tidy_cox(results$adjusted_cox_model)
  adj_file <- file.path(
    dir_path,
    paste0(dataset, "_", variable, "_", endpoint, "_adjusted_cox.csv")
  )
  write.csv(adj_tbl, adj_file, row.names = FALSE)
  
  message("Wrote CSVs to:\n  ", dir_path)
}



save_survival_results(
  results  = results[["da_daten_hyperfiltration_egfr_endpoint_reached_ja_1"]],
  dataset  = "da_daten",
  variable = "hyperfiltration_egfr",
  endpoint = "endpoint_reached_ja_1",
  output_dir = "analysis_outputs"
)
i cant find the output in the given path

## Prompt 12

perfect now i ave my cvs export, can you write a code to create tow publishable tables in colab
it should be one table for the logrank test and the unasjusted table and one for the adjusted model, or three seperate tebles whatever you think looks better
also please indicate significant results with stars and show me all the initial commands (mounting and importing)

## Prompt 13

can you provide me the code

## Prompt 14

th files are in Meine Ablage > Statistik DA > DATEN, how do i write the path

## Prompt 15

11 
     12 # 6) Read data
---> 13 survdiff_df = pd.read_csv(survdiff_file)
     14 unadj_df    = pd.read_csv(unadj_file)
     15 adj_df      = pd.read_csv(adj_file)

4 frames
/usr/local/lib/python3.11/dist-packages/pandas/io/common.py in get_handle(path_or_buf, mode, encoding, compression, memory_map, is_text, errors, storage_options)
    871         if ioargs.encoding and "b" not in ioargs.mode:
    872             # Encoding
--> 873             handle = open(
    874                 handle,
    875                 ioargs.mode,

FileNotFoundError: [Errno 2] No such file or directory: '/content/drive/MyDrive/Statistik/DA/DATEN/da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_survdiff.csv'

## Prompt 16

KeyError                                  Traceback (most recent call last)
/usr/local/lib/python3.11/dist-packages/pandas/core/indexes/base.py in get_loc(self, key)
   3804         try:
-> 3805             return self._engine.get_loc(casted_key)
   3806         except KeyError as err:

index.pyx in pandas._libs.index.IndexEngine.get_loc()

index.pyx in pandas._libs.index.IndexEngine.get_loc()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

KeyError: 'chisq'

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
2 frames
/usr/local/lib/python3.11/dist-packages/pandas/core/indexes/base.py in get_loc(self, key)
   3810             ):
   3811                 raise InvalidIndexError(key)
-> 3812             raise KeyError(key) from err
   3813         except TypeError:
   3814             # If we have a listlike key, _check_indexing_error will raise

KeyError: 'chisq'

## Prompt 17

---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
/usr/local/lib/python3.11/dist-packages/pandas/core/indexes/base.py in get_loc(self, key)
   3804         try:
-> 3805             return self._engine.get_loc(casted_key)
   3806         except KeyError as err:

index.pyx in pandas._libs.index.IndexEngine.get_loc()

index.pyx in pandas._libs.index.IndexEngine.get_loc()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

pandas/_libs/hashtable_class_helper.pxi in pandas._libs.hashtable.PyObjectHashTable.get_item()

KeyError: 'chisq'

The above exception was the direct cause of the following exception:

KeyError                                  Traceback (most recent call last)
2 frames
/usr/local/lib/python3.11/dist-packages/pandas/core/indexes/base.py in get_loc(self, key)
   3810             ):
   3811                 raise InvalidIndexError(key)
-> 3812             raise KeyError(key) from err
   3813         except TypeError:
   3814             # If we have a listlike key, _check_indexing_error will raise

KeyError: 'chisq'
i get this erroe, how can i fix it

## Prompt 18

# 1) survdiff → tidy via broom (no rownames issue here)
  sd_tbl <- broom::tidy(results$survdiff)
  sd_file <- file.path(
    dir_path,
    paste0(dataset, "_", variable, "_", endpoint, "_survdiff.csv")
  )
  write.csv(sd_tbl, sd_file, row.names = FALSE)
  
  # helper to turn a coxph or summary.coxph into a proper data.frame
  tidy_cox <- function(obj) {
    # raw coxph: broom has a method
    if (inherits(obj, "coxph")) {
      return(broom::tidy(obj, exponentiate = TRUE, conf.int = TRUE))
    }
    # summary.coxph: pull everything manually
    if (inherits(obj, "summary.coxph")) {
      coefs <- obj$coefficients
      conf  <- obj$conf.int
      # build data.frame, explicitly moving rownames → term
      df <- data.frame(
        term       = rownames(coefs),
        coef       = coefs[, "coef"],
        exp_coef   = coefs[, "exp(coef)"],
        se_coef    = coefs[, "se(coef)"],
        z          = coefs[, "z"],
        p.value    = coefs[, "Pr(>|z|)"],
        conf.low   = conf[, "lower .95"],
        conf.high  = conf[, "upper .95"],
        stringsAsFactors = FALSE
      )
      return(df)
    }
    stop("Object is not a coxph or summary.coxph")
  }
  Thats a snippet of the cvs export function in R, the lower part is a helper function thats used to create the cvs files for adjusted and unadjusted cox models subsequently.
That works fine, however the surfdiff function seems to fail as i get an empty cvs for surfdiff, can you write a helper function for that case too, that pulls the information manually when broom fails (or just pulls it manualy) 
the survdiff output looks like:
Call:
survdiff(formula = Surv(as.numeric(get(time_var, daten_filtered)), 
    as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered))

                             N Observed Expected (O-E)^2/E (O-E)^2/V
get(var, daten_filtered)=0 151       55     41.8      4.19      9.02
get(var, daten_filtered)=1 144       24     37.2      4.70      9.02

 Chisq= 9  on 1 degrees of freedom, p= 0.003 
>

## Prompt 19

Fehler in pchisq(chisq, df) : 
  Nicht-numerisches Argument für mathematische Funktion

## Prompt 20

---------------------------------------------------------------------------
KeyError                                  Traceback (most recent call last)
<ipython-input-5-af56f1bbc127> in <cell line: 0>()
     35 })
     36 unadj_df['Signif'] = unadj_df['P-value'].apply(significance)
---> 37 unadj_table = unadj_df[['Variable','HR','95% CI Lower','95% CI Upper','P-value','Signif']]
     38 
     39 # 9) Table 3: Adjusted Cox Model

2 frames
/usr/local/lib/python3.11/dist-packages/pandas/core/indexes/base.py in _raise_if_missing(self, key, indexer, axis_name)
   6250 
   6251             not_found = list(ensure_index(key)[missing_mask.nonzero()[0]].unique())
-> 6252             raise KeyError(f"{not_found} not in index")
   6253 
   6254     @overload

KeyError: "['HR'] not in index"

## Prompt 21

nice thanks can you give me a quick export function so i can add the tables to my thesis

## Prompt 22

python-input-5-4a68bac0bc89> in <cell line: 0>()
      1 export_tables_for_thesis(
----> 2     logrank_df,
      3     unadj_table,
      4     adj_table,
      5     '/content/drive/My Drive/Statistik DA/Tables'

NameError: name 'logrank_df' is not defined

## Prompt 23

from docx import Document this results in an error ModuleNotFoundError                       Traceback (most recent call last)
<ipython-input-7-fc7dc2b77ee6> in <cell line: 0>()
----> 1 from docx import Document
      2 import pandas as pd
      3 
      4 def add_table_with_caption(doc, df, caption):
      5     # add caption (as a paragraph styled “Caption”)

ModuleNotFoundError: No module named 'docx

## Prompt 24

cool that works, 2 more tewaks i need: 
1) in the first function generating the csv files please 
a) change the names of the variables in this manner: "Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
    "Geschlecht_1_m...29" = "Geschlecht, 1=M",
    "Alter_beitotwenntot_a" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja",
    "NBZ_completed_mean" = "Nüchternblutzucker",
    "ppBZ_completed_mean" = "postprandialer Blutzucker"b) round the results to 3 decimal places 
2) when generating the table use a style that fits the attached document

## Prompt 25

thanks but that are changes id like to do in the colab code you provided earlier

## Prompt 26

# Complete Colab cell: Build tables and export for thesis

# 1) Mount Google Drive (Colab only)
try:
    from google.colab import drive
    drive.mount('/content/drive')
except ModuleNotFoundError:
    pass  # Already mounted or not in Colab

# 2) Imports
import os
import pandas as pd
import scipy.stats as stats
from IPython.display import display

# 3) Significance stars helper
def significance(p):
    if pd.isna(p):
        return ''
    if p < 0.001:
        return '***'
    elif p < 0.01:
        return '**'
    elif p < 0.05:
        return '*'
    else:
        return ''

# 4) Path to your CSVs directory in Drive
base_path = "/content/drive/My Drive/Statistik DA/DATEN"

# 5) File paths
survdiff_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_survdiff.csv"
)
unadj_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_unadjusted_cox.csv"
)
adj_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_adjusted_cox.csv"
)

# 6) Read CSVs
survdiff_df = pd.read_csv(survdiff_file)
unadj_df    = pd.read_csv(unadj_file)
adj_df      = pd.read_csv(adj_file)

# 7) Build Table 1: Log‑rank Test Summary
if 'chisq' in survdiff_df.columns:
    chisq = survdiff_df['chisq'].dropna().iloc[0]
elif 'statistic' in survdiff_df.columns:
    chisq = survdiff_df['statistic'].dropna().iloc[0]
else:
    raise KeyError(f"No χ² column; found {survdiff_df.columns.tolist()}")
p_val = 1 - stats.chi2.cdf(chisq, df=1)
logrank_df = pd.DataFrame({
    'Chi-square': [chisq],
    'df':         [1],
    'P-value':    [p_val],
    'Signif':     [significance(p_val)]
})

# 8) Build Table 2: Unadjusted Cox Model
# detect HR column
hr_col = next((c for c in ['exp(coef)','estimate','exp_coef'] if c in unadj_df.columns), None)
if not hr_col:
    raise KeyError(f"No HR column; found {unadj_df.columns.tolist()}")
unadj_df = unadj_df.rename(columns={
    'term': 'Variable',
    hr_col: 'HR',
    'conf.low': '95% CI Lower',
    'conf.high': '95% CI Upper',
    'p.value': 'P-value'
})
unadj_df['Signif'] = unadj_df['P-value'].apply(significance)
unadj_table = unadj_df[['Variable','HR','95% CI Lower','95% CI Upper','P-value','Signif']]

# 9) Build Table 3: Adjusted Cox Model
hr_col_adj = next((c for c in ['exp(coef)','estimate','exp_coef'] if c in adj_df.columns), None)
if not hr_col_adj:
    raise KeyError(f"No HR column in adjusted; found {adj_df.columns.tolist()}")
adj_df = adj_df.rename(columns={
    'term': 'Variable',
    hr_col_adj: 'HR',
    'conf.low': '95% CI Lower',
    'conf.high': '95% CI Upper',
    'p.value': 'P-value'
})
adj_df['Signif'] = adj_df['P-value'].apply(significance)
adj_table = adj_df[['Variable','HR','95% CI Lower','95% CI Upper','P-value','Signif']]

# 10) Display tables for review
print("## Table 1: Log‑rank Test Summary")
display(logrank_df)
print("\n## Table 2: Unadjusted Cox Model")
display(unadj_table)
print("\n## Table 3: Adjusted Cox Model")
display(adj_table)

# 11) Export function
def export_tables_for_thesis(logrank_df, unadj_table, adj_table, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    # CSVs
    logrank_df.to_csv(os.path.join(output_dir, 'table_logrank.csv'), index=False)
    unadj_table.to_csv(os.path.join(output_dir, 'table_unadjusted_cox.csv'), index=False)
    adj_table.to_csv(os.path.join(output_dir, 'table_adjusted_cox.csv'), index=False)
    # LaTeX
    with open(os.path.join(output_dir, 'table_logrank.tex'), 'w') as f:
        f.write(logrank_df.to_latex(index=False,
                                    caption='Log–rank Test Summary',
                                    label='tab:logrank',
                                    float_format="%.3f"))
    with open(os.path.join(output_dir, 'table_unadjusted_cox.tex'), 'w') as f:
        f.write(unadj_table.to_latex(index=False,
                                     caption='Unadjusted Cox Proportional Hazards Model',
                                     label='tab:unadjusted_cox',
                                     float_format="%.3f"))
    with open(os.path.join(output_dir, 'table_adjusted_cox.tex'), 'w') as f:
        f.write(adj_table.to_latex(index=False,
                                   caption='Adjusted Cox Proportional Hazards Model',
                                   label='tab:adjusted_cox',
                                   float_format="%.3f"))
    print(f"✅ Exported CSVs and LaTeX to {output_dir}")

# 12) Call export (adjust path as needed)
export_dir = "/content/drive/My Drive/Statistik DA/Tables"
export_tables_for_thesis(logrank_df, unadj_table, adj_table, export_dir)

I have this code and i like the way it wors i;d just llike to round all numbers to 3 decimals and rename all variables like:
"Hyperfiltration140_1_ja" = "Hyperfiltration",
    "Hyperfiltration130_1_ja"= "Hyperfiltration",
    "Hyperfiltration150_1_ja"= "Hyperfiltration",
    "HyperfiltrationMedian_1_ja" = "Hyperfiltration",
    "RRsyst_completed_mean" = "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean" = "HbA1c",
    "BMI_completed_mean" = "BMI",
    "UN_completed_mean" = "BUN",
    "Diabetesdauer_totwenntot_a_tim" = "Diabetesdauer",
    "Geschlecht_1_m...29" = "Geschlecht, 1=M",
    "Alter_beitotwenntot_a" = "Alter",
    "ProteinJa_1" = "Proteinurie, 1=ja",
    "NBZ_completed_mean" = "Nüchternblutzucker",
    "ppBZ_completed_mean" = "postprandialer Blutzucker"

## Prompt 27

!pip install python-docx

from docx import Document
import pandas as pd

def add_table_with_caption(doc, df, caption):
    # add caption (as a paragraph styled “Caption”)
    p = doc.add_paragraph(caption, style='Caption')
    # then insert the table
    table = doc.add_table(rows=1, cols=len(df.columns))
    hdr_cells = table.rows[0].cells
    for i, col in enumerate(df.columns):
        hdr_cells[i].text = str(col)
    for _, row in df.iterrows():
        cells = table.add_row().cells
        for i, col in enumerate(df.columns):
            cells[i].text = str(row[col])
    table.style = 'Light List Accent 1'  # pick a clean built-in style
    doc.add_paragraph()  # spacing

# Load your tables
logrank = pd.read_csv('/content/drive/My Drive/Statistik DA/Tables/table_logrank.csv')
unadj   = pd.read_csv('/content/drive/My Drive/Statistik DA/Tables/table_unadjusted_cox.csv')
adj     = pd.read_csv('/content/drive/My Drive/Statistik DA/Tables/table_adjusted_cox.csv')

# Create the document
doc = Document()
doc.add_heading('Chapter 3 – Results', level=1)

add_table_with_caption(doc, logrank, 'Table 3.1  Log-rank Test Summary')
add_table_with_caption(doc, unadj,   'Table 3.2  Unadjusted Cox Proportional Hazards Model')
add_table_with_caption(doc, adj,    'Table 3.3  Adjusted Cox Proportional Hazards Model')

# Save
doc.save('/content/drive/My Drive/Statistik DA/Tables/explorative_results.docx')

okay now i have this and i generally like how it works, i now i can adjust the style in word however it'd be cool if the initially generated tables matched the style i used so far (see atteched picture

## Prompt 28

# Complete Colab cell: Build tables and export for thesis

# 1) Mount Google Drive (Colab only)
try:
    from google.colab import drive
    drive.mount('/content/drive')
except ModuleNotFoundError:
    pass  # Already mounted or not in Colab

# 2) Imports
import os
import pandas as pd
import scipy.stats as stats
from IPython.display import display

# 3) Significance stars helper
def significance(p):
    if pd.isna(p):
        return ''
    if p < 0.001:
        return '***'
    elif p < 0.01:
        return '**'
    elif p < 0.05:
        return '*'
    else:
        return ''

# 4) Your name‐mapping dict
name_map = {
    "Hyperfiltration140_1_ja":       "Hyperfiltration",
    "Hyperfiltration130_1_ja":       "Hyperfiltration",
    "Hyperfiltration150_1_ja":       "Hyperfiltration",
    "HyperfiltrationMedian_1_ja":    "Hyperfiltration",
    "RRsyst_completed_mean":         "systolischer Blutdruck",
    "highest_measured_hba1c_naisna_completed_mean": "HbA1c",
    "BMI_completed_mean":            "BMI",
    "UN_completed_mean":             "BUN",
    "Diabetesdauer_totwenntot_a_tim":"Diabetesdauer",
    "Geschlecht_1_m...29":           "Geschlecht, 1=M",
    "Alter_beitotwenntot_a":         "Alter",
    "ProteinJa_1":                   "Proteinurie, 1=ja",
    "NBZ_completed_mean":            "Nüchternblutzucker",
    "ppBZ_completed_mean":           "postprandialer Blutzucker"
}


# 5) Path to your CSVs directory in Drive
base_path = "/content/drive/My Drive/Statistik DA/DATEN"

# 6) File paths
survdiff_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_survdiff.csv"
)
unadj_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_unadjusted_cox.csv"
)
adj_file = os.path.join(
    base_path,
    "da_daten_hyperfiltration_egfr_endpoint_reached_ja_1_adjusted_cox.csv"
)

# 7) Read CSVs
survdiff_df = pd.read_csv(survdiff_file)
unadj_df    = pd.read_csv(unadj_file)
adj_df      = pd.read_csv(adj_file)

# 8) Round all numeric columns to 3 decimals
def round_all(df):
    num_cols = df.select_dtypes(include='number').columns
    df[num_cols] = df[num_cols].round(3)
    return df

survdiff_df = round_all(survdiff_df)
unadj_df    = round_all(unadj_df)
adj_df      = round_all(adj_df)

# 9) Build Table 1: Log‑rank Test Summary
if 'chisq' in survdiff_df.columns:
    chisq = survdiff_df['chisq'].dropna().iloc[0]
elif 'statistic' in survdiff_df.columns:
    chisq = survdiff_df['statistic'].dropna().iloc[0]
else:
    raise KeyError(f"No χ² column; found {survdiff_df.columns.tolist()}")
p_val = 1 - stats.chi2.cdf(chisq, df=1)
logrank_df = pd.DataFrame({
    'Chi-square': [chisq],
    'df':         [1],
    'P-value':    [p_val],
    'Signif':     [significance(p_val)]
})

# 10) Build Table 2: Unadjusted Cox Model
# detect HR column
hr_col = next((c for c in ['exp(coef)','estimate','exp_coef'] if c in unadj_df.columns), None)
if not hr_col:
    raise KeyError(f"No HR column; found {unadj_df.columns.tolist()}")
unadj_df = unadj_df.rename(columns={
    'term': 'Variable',
    hr_col: 'HR',
    'conf.low': '95% CI Lower',
    'conf.high': '95% CI Upper',
    'p.value': 'P-value'
})
#10a Recode variables
unadj_df['Variable'] = unadj_df['Variable'].map(name_map).fillna(unadj_df['Variable'])
unadj_df['Signif'] = unadj_df['P-value'].apply(significance)
unadj_table = unadj_df[['Variable','HR','95% CI Lower','95% CI Upper','P-value','Signif']]

# 11) Build Table 3: Adjusted Cox Model
hr_col_adj = next((c for c in ['exp(coef)','estimate','exp_coef'] if c in adj_df.columns), None)
if not hr_col_adj:
    raise KeyError(f"No HR column in adjusted; found {adj_df.columns.tolist()}")
adj_df = adj_df.rename(columns={
    'term': 'Variable',
    hr_col_adj: 'HR',
    'conf.low': '95% CI Lower',
    'conf.high': '95% CI Upper',
    'p.value': 'P-value'
})
#11a Recode variables
adj_df['Variable'] = adj_df['Variable'].map(name_map).fillna(adj_df['Variable'])
adj_df['Signif'] = adj_df['P-value'].apply(significance)
adj_table = adj_df[['Variable','HR','95% CI Lower','95% CI Upper','P-value','Signif']]

# 12) Display tables for review
print("## Table 1: Log‑rank Test Summary")
display(logrank_df)
print("\n## Table 2: Unadjusted Cox Model")
display(unadj_table)
print("\n## Table 3: Adjusted Cox Model")
display(adj_table)

# 13) Export function
def export_tables_for_thesis(logrank_df, unadj_table, adj_table, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    # CSVs
    logrank_df.to_csv(os.path.join(output_dir, 'table_logrank.csv'), index=False)
    unadj_table.to_csv(os.path.join(output_dir, 'table_unadjusted_cox.csv'), index=False)
    adj_table.to_csv(os.path.join(output_dir, 'table_adjusted_cox.csv'), index=False)
    # LaTeX
    with open(os.path.join(output_dir, 'table_logrank.tex'), 'w') as f:
        f.write(logrank_df.to_latex(index=False,
                                    caption='Log–rank Test Summary',
                                    label='tab:logrank',
                                    float_format="%.3f"))
    with open(os.path.join(output_dir, 'table_unadjusted_cox.tex'), 'w') as f:
        f.write(unadj_table.to_latex(index=False,
                                     caption='Unadjusted Cox Proportional Hazards Model',
                                     label='tab:unadjusted_cox',
                                     float_format="%.3f"))
    with open(os.path.join(output_dir, 'table_adjusted_cox.tex'), 'w') as f:
        f.write(adj_table.to_latex(index=False,
                                   caption='Adjusted Cox Proportional Hazards Model',
                                   label='tab:adjusted_cox',
                                   float_format="%.3f"))
    print(f"✅ Exported CSVs and LaTeX to {output_dir}")

# 14) Call export (adjust path as needed)
export_dir = "/content/drive/My Drive/Statistik DA/Tables"
export_tables_for_thesis(logrank_df, unadj_table, adj_table, export_dir)

I have this however the p value in the logrank test is not getting rounded properly
