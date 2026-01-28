# Survival Analysis Function

## Prompt 1

#3.3.1)kaplan Maier Plot 
list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
)
my list of variables is:
1 meaning true, i want to compare the outcomes of true/false groups for each variable 
list_var <- list(
  "Hyperfiltration140_1_ja",
  "Hyperfiltration130_1_ja",
  "Hyperfiltration150_1_ja",
  "HyperfiltrationMedian_1_ja"
)
my lsit of time to endpoint/age at event: 
as is now this list and the event reached list need to have their corresponding values in the same order and need to be of the same length
list_surv_time <- list(
  "Alter_beitotwenntot_a_rounded",
  "NE_surv_all_a_tim",
  "time_to_endpoint_a_tim"
)
the list of events is: 1 meaning event happened
list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_ja_1"
)
the list of confunders is:
list_confounder <- list(
  "RRsyst",
  "RRdiast",  
  "NE_1_ja",
  "highest_measured_hba1c_naisna"
)

i have those lists each needs to be a changable set of data as i want to analyse various variables, confunders in various data sets. All the names in the other lists are column names of the data sets 

i need a function that works with this data and can give me a logrank test,a logrank test, a coxph test,  kaplan graph, a  a coxgraph and a forest plot, preferably using s"survival" and "survminer" librarys (survminer especially for kaplan graph, forest plot and cox graph)
 ideally the function can iterate through all of the lists in one run and then store the result in a systematic way so i only need one run of analyzing before being able to look at all the results for every endpoint, and variable in every given data set 
if possible the plots generated should have god labeling and fitting titles for each situation and be visually appealing

## Prompt 2

data = daten_filtered,
              censor.shape = "|", censor.size = 4,
              conf.int = TRUE,
              pval = TRUE,
              xlab = "Time in years",
              break.time.by = 5,
              surv.median.line = "hv",
              ncensor.plot = TRUE,
              ggtheme = theme_bw()
lets add these modifications to the plot function
also i think i forgot to mentoin that i have NA values in almost every confunder/variable/endpoint and this caused errors before, so if you could include a filter to prevent theat that would be nice

## Prompt 3

almost perfect, only when storing the results would you mind using the endpoint instead of the time

## Prompt 4

Error in `dataset[, required_cols]`:
! Can't subset columns with `required_cols`.
✖ `required_cols` must be logical, numeric, or character, not a list.

## Prompt 5

Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden

## Prompt 6

Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden

## Prompt 7

Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden

## Prompt 8

still the same error

## Prompt 9

---- Checking first rows of daten_filtered ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <chr>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      
Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden
Zusätzlich: Warnmeldungen:
1: Using an external vector in selections was deprecated in tidyselect 1.1.0.
ℹ Please use `all_of()` or `any_of()` instead.
  # Was:
  data %>% select(time_var)

  # Now:
  data %>% select(all_of(time_var))

See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
ℹ Please use `all_of()` or `any_of()` instead.
  # Was:
  data %>% select(event_var)

  # Now:
  data %>% select(all_of(event_var))

See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
3: Using an external vector in selections was deprecated in tidyselect 1.1.0.
ℹ Please use `all_of()` or `any_of()` instead.
  # Was:
  data %>% select(var)

  # Now:
  data %>% select(all_of(var))

See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
>

## Prompt 10

Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden
> # Example usage
> results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)
Converting Hyperfiltration140_1_ja to factor in da_daten 

---- Checking first rows of daten_filtered ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      
Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden

## Prompt 11

onverting Hyperfiltration140_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration130_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 1                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 1                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 1                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration150_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting HyperfiltrationMedian_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            48     1 0                         
2                            56     0 1                         
3                            56     1 0                         
4                            38     1 0                         
5                            50     1 0                         
6                            52     1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              36.8       1 0                         
2              55.5       0 1                         
3              53.9       1 0                         
4              35.8       1 0                         
5              50.0       0 0                         
6              33.9       1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   36.8                     1 0                         
2                   55.5                     0 1                         
3                   53.9                     1 0                         
4                   35.8                     1 0                         
5                   50.0                     1 0                         
6                   33.9                     1 0                         
Converting Hyperfiltration140_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration130_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 1                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 1                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 1                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration150_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting HyperfiltrationMedian_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            56     0 1                         
2                            56     1 0                         
3                            54     0 1                         
4                            62     1 0                         
5                            62     1 0                         
6                            83     0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              55.5       0 1                         
2              53.9       1 0                         
3              53.8       0 1                         
4              34.8       1 0                         
5              61.6       0 0                         
6              82.7       0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   55.5                     0 1                         
2                   53.9                     1 0                         
3                   53.8                     0 1                         
4                   34.8                     1 0                         
5                   61.6                     1 0                         
6                   82.7                     0 0                         
Warnmeldungen:
1: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
4: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
5: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
6: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
7: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
8: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite.

## Prompt 12

it seems to work now., butprint(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]])  # Now uses the event variable name instead of the time variable gives me$cox_model
Call:
coxph(formula = surv_obj ~ daten_filtered[[var]], data = daten_filtered)

  n= 322, number of events= 88 

                          coef exp(coef) se(coef)     z Pr(>|z|)    
daten_filtered[[var]]1 -1.1658    0.3117   0.2836 -4.11 3.96e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                       exp(coef) exp(-coef) lower .95 upper .95
daten_filtered[[var]]1    0.3117      3.208    0.1788    0.5434

Concordance= 0.616  (se = 0.023 )
Likelihood ratio test= 21.22  on 1 df,   p=4e-06
Wald test            = 16.89  on 1 df,   p=4e-05
Score (logrank) test = 18.89  on 1 df,   p=1e-05
however no graph is printed

## Prompt 13

also in the influece of the confunders is nowhere to be seen in the results

## Prompt 14

Error in `filter()`:
ℹ In argument: `complete.cases(across(all_of(c(time_var, event_var, var, list_confounder))))`.
Caused by error in `across()`:
ℹ In argument: `all_of(c(time_var, event_var, var, list_confounder))`.
Caused by error in `all_of()`:
! Can't subset elements.
✖ Subscript must be numeric or character, not a list.
Run `rlang::last_trace()` to see where the error occurred.
>

## Prompt 15

Fehler in FUN(X[[i]], ...) : Objekt 'surv_obj' nicht gefunden

## Prompt 16

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 0                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 0                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration130_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 1                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 1                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 1                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 1                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 1                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 1                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration150_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 0                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 0                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting HyperfiltrationMedian_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                             170     100       1                           6.9
2                            56     0 1                             120      80       0                           7.1
3                            56     1 0                             130      85       1                           7.7
4                            38     1 0                             130      80       1                          12.6
5                            50     1 0                             140      80       0                           9  
6                            52     1 0                             150     100       1                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            48     1 0                         
2                            56     0 1                         
3                            56     1 0                         
4                            38     1 0                         
5                            50     1 0                         
6                            52     1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                       <dbl>   <dbl>                         <dbl>
1              36.8       1 0                             170     100                           6.9
2              55.5       0 1                             120      80                           7.1
3              53.9       1 0                             130      85                           7.7
4              35.8       1 0                             130      80                          12.6
5              50.0       0 0                             140      80                           9  
6              33.9       1 0                             150     100                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              36.8       1 0                         
2              55.5       0 1                         
3              53.9       1 0                         
4              35.8       1 0                         
5              50.0       0 0                         
6              33.9       1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                             170     100       1                           6.9
2                   55.5                     0 1                             120      80       0                           7.1
3                   53.9                     1 0                             130      85       1                           7.7
4                   35.8                     1 0                             130      80       1                          12.6
5                   50.0                     1 0                             140      80       0                           9  
6                   33.9                     1 0                             150     100       1                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   36.8                     1 0                         
2                   55.5                     0 1                         
3                   53.9                     1 0                         
4                   35.8                     1 0                         
5                   50.0                     1 0                         
6                   33.9                     1 0                         
Converting Hyperfiltration140_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 0                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 0                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 0                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration130_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 1                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 1                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 1                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 1                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 1                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 1                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration150_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 0                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 0                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 0                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting HyperfiltrationMedian_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 1                             120      80       0                           7.1
2                            56     1 0                             130      85       1                           7.7
3                            54     0 1                             110      70       0                           9.5
4                            62     1 0                             140      80       1                           8  
5                            62     1 0                             140      90       0                          10.7
6                            83     0 0                             150      70       0                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            56     0 1                         
2                            56     1 0                         
3                            54     0 1                         
4                            62     1 0                         
5                            62     1 0                         
6                            83     0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                       <dbl>   <dbl>                         <dbl>
1              55.5       0 1                             120      80                           7.1
2              53.9       1 0                             130      85                           7.7
3              53.8       0 1                             110      70                           9.5
4              34.8       1 0                             140      80                           8  
5              61.6       0 0                             140      90                          10.7
6              82.7       0 0                             150      70                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              55.5       0 1                         
2              53.9       1 0                         
3              53.8       0 1                         
4              34.8       1 0                         
5              61.6       0 0                         
6              82.7       0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 1                             120      80       0                           7.1
2                   53.9                     1 0                             130      85       1                           7.7
3                   53.8                     0 1                             110      70       0                           9.5
4                   34.8                     1 0                             140      80       1                           8  
5                   61.6                     1 0                             140      90       0                          10.7
6                   82.7                     0 0                             150      70       0                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   55.5                     0 1                         
2                   53.9                     1 0                         
3                   53.8                     0 1                         
4                   34.8                     1 0                         
5                   61.6                     1 0                         
6                   82.7                     0 0                         
Warnmeldungen:
1: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
4: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
5: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
6: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
7: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
8: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
> print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$cox_model)
Call:
coxph(formula = surv_obj ~ daten_filtered[[var]], data = daten_filtered)

  n= 269, number of events= 73 

                          coef exp(coef) se(coef)      z Pr(>|z|)    
daten_filtered[[var]]1 -1.2529    0.2857   0.3159 -3.966 7.32e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                       exp(coef) exp(-coef) lower .95 upper .95
daten_filtered[[var]]1    0.2857      3.501    0.1538    0.5306

Concordance= 0.629  (se = 0.024 )
Likelihood ratio test= 20.17  on 1 df,   p=7e-06
Wald test            = 15.73  on 1 df,   p=7e-05
Score (logrank) test = 17.89  on 1 df,   p=2e-05

## Prompt 17

Converting Hyperfiltration140_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 0                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 0                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration130_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 1                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 1                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 1                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 1                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 1                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 1                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting Hyperfiltration150_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            48     1 0                      
2                            56     0 0                      
3                            56     1 0                      
4                            38     1 0                      
5                            50     1 0                      
6                            52     1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              36.8       1 0                          170     100                           6.9
2              55.5       0 0                          120      80                           7.1
3              53.9       1 0                          130      85                           7.7
4              35.8       1 0                          130      80                          12.6
5              50.0       0 0                          140      80                           9  
6              33.9       1 0                          150     100                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              36.8       1 0                      
2              55.5       0 0                      
3              53.9       1 0                      
4              35.8       1 0                      
5              50.0       0 0                      
6              33.9       1 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                          170     100       1                           6.9
2                   55.5                     0 0                          120      80       0                           7.1
3                   53.9                     1 0                          130      85       1                           7.7
4                   35.8                     1 0                          130      80       1                          12.6
5                   50.0                     1 0                          140      80       0                           9  
6                   33.9                     1 0                          150     100       1                           5.7
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   36.8                     1 0                      
2                   55.5                     0 0                      
3                   53.9                     1 0                      
4                   35.8                     1 0                      
5                   50.0                     1 0                      
6                   33.9                     1 0                      
Converting HyperfiltrationMedian_1_ja to factor in da_daten 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                             170     100       1                           6.9
2                            56     0 1                             120      80       0                           7.1
3                            56     1 0                             130      85       1                           7.7
4                            38     1 0                             130      80       1                          12.6
5                            50     1 0                             140      80       0                           9  
6                            52     1 0                             150     100       1                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            48     1 0                         
2                            56     0 1                         
3                            56     1 0                         
4                            38     1 0                         
5                            50     1 0                         
6                            52     1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                       <dbl>   <dbl>                         <dbl>
1              36.8       1 0                             170     100                           6.9
2              55.5       0 1                             120      80                           7.1
3              53.9       1 0                             130      85                           7.7
4              35.8       1 0                             130      80                          12.6
5              50.0       0 0                             140      80                           9  
6              33.9       1 0                             150     100                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              36.8       1 0                         
2              55.5       0 1                         
3              53.9       1 0                         
4              35.8       1 0                         
5              50.0       0 0                         
6              33.9       1 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                   36.8                     1 0                             170     100       1                           6.9
2                   55.5                     0 1                             120      80       0                           7.1
3                   53.9                     1 0                             130      85       1                           7.7
4                   35.8                     1 0                             130      80       1                          12.6
5                   50.0                     1 0                             140      80       0                           9  
6                   33.9                     1 0                             150     100       1                           5.7
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   36.8                     1 0                         
2                   55.5                     0 1                         
3                   53.9                     1 0                         
4                   35.8                     1 0                         
5                   50.0                     1 0                         
6                   33.9                     1 0                         
⚠️ No Kaplan-Meier plot found!
⚠️ No Cox Forest plot found!
Converting Hyperfiltration140_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 0                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 0                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration140_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 0                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration140_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration140_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration130_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 1                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration130_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 1                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 1                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration130_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 1                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 1                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration130_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration130_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 1                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting Hyperfiltration150_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 0                          120      80       0                           7.1
2                            56     1 0                          130      85       1                           7.7
3                            54     0 1                          110      70       0                           9.5
4                            62     1 0                          140      80       1                           8  
5                            62     1 0                          140      90       0                          10.7
6                            83     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration150_1_ja
                          <dbl> <dbl> <fct>                  
1                            56     0 0                      
2                            56     1 0                      
3                            54     0 1                      
4                            62     1 0                      
5                            62     1 0                      
6                            83     0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                    <dbl>   <dbl>                         <dbl>
1              55.5       0 0                          120      80                           7.1
2              53.9       1 0                          130      85                           7.7
3              53.8       0 1                          110      70                           9.5
4              34.8       1 0                          140      80                           8  
5              61.6       0 0                          140      90                          10.7
6              82.7       0 0                          150      70                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja Hyperfiltration150_1_ja
              <dbl>   <dbl> <fct>                  
1              55.5       0 0                      
2              53.9       1 0                      
3              53.8       0 1                      
4              34.8       1 0                      
5              61.6       0 0                      
6              82.7       0 0                      

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 0                          120      80       0                           7.1
2                   53.9                     1 0                          130      85       1                           7.7
3                   53.8                     0 1                          110      70       0                           9.5
4                   34.8                     1 0                          140      80       1                           8  
5                   61.6                     1 0                          140      90       0                          10.7
6                   82.7                     0 0                          150      70       0                           8.4
Factor levels for Hyperfiltration150_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 Hyperfiltration150_1_ja
                   <dbl>                 <dbl> <fct>                  
1                   55.5                     0 0                      
2                   53.9                     1 0                      
3                   53.8                     0 1                      
4                   34.8                     1 0                      
5                   61.6                     1 0                      
6                   82.7                     0 0                      
Converting HyperfiltrationMedian_1_ja to factor in subset_minusCKD 

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                            56     0 1                             120      80       0                           7.1
2                            56     1 0                             130      85       1                           7.7
3                            54     0 1                             110      70       0                           9.5
4                            62     1 0                             140      80       1                           8  
5                            62     1 0                             140      90       0                          10.7
6                            83     0 0                             150      70       0                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  Alter_beitotwenntot_a_rounded tot_1 HyperfiltrationMedian_1_ja
                          <dbl> <dbl> <fct>                     
1                            56     0 1                         
2                            56     1 0                         
3                            54     0 1                         
4                            62     1 0                         
5                            62     1 0                         
6                            83     0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 6
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja RRsyst RRdiast highest_measured_hba1c_naisna
              <dbl>   <dbl> <fct>                       <dbl>   <dbl>                         <dbl>
1              55.5       0 1                             120      80                           7.1
2              53.9       1 0                             130      85                           7.7
3              53.8       0 1                             110      70                           9.5
4              34.8       1 0                             140      80                           8  
5              61.6       0 0                             140      90                          10.7
6              82.7       0 0                             150      70                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  NE_surv_all_a_tim NE_1_ja HyperfiltrationMedian_1_ja
              <dbl>   <dbl> <fct>                     
1              55.5       0 1                         
2              53.9       1 0                         
3              53.8       0 1                         
4              34.8       1 0                         
5              61.6       0 0                         
6              82.7       0 0                         

---- Checking Surv() Inputs ----
# A tibble: 6 × 7
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                   <dbl>                 <dbl> <fct>                       <dbl>   <dbl>   <dbl>                         <dbl>
1                   55.5                     0 1                             120      80       0                           7.1
2                   53.9                     1 0                             130      85       1                           7.7
3                   53.8                     0 1                             110      70       0                           9.5
4                   34.8                     1 0                             140      80       1                           8  
5                   61.6                     1 0                             140      90       0                          10.7
6                   82.7                     0 0                             150      70       0                           8.4
Factor levels for HyperfiltrationMedian_1_ja : 0 1 

---- Final Check Before Surv() ----
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   55.5                     0 1                         
2                   53.9                     1 0                         
3                   53.8                     0 1                         
4                   34.8                     1 0                         
5                   61.6                     1 0                         
6                   82.7                     0 0                         
⚠️ No Kaplan-Meier plot found!
⚠️ No Cox Forest plot found!
Warnmeldungen:
1: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
2: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
3: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
4: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
5: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
6: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
7: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 
8: In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
  Loglik converged before variable  4 ; coefficient may be infinite. 

printing the conf adjusted cox model explicitly works

## Prompt 18

Warnmeldung:
In .add_surv_median(p, fit, type = surv.median.line, fun = fun,  :
  Median survival not reached.
>

## Prompt 19

suppressWarnings(
  km_plot <- ggsurvplot(
    survfit(Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ Hyperfiltration140_1_ja, data = da_daten),
    data = da_daten,
    censor.shape = "|", censor.size = 4,
    conf.int = TRUE,
    pval = TRUE,
    xlab = "Time in years",
    break.time.by = 5,
    surv.median.line = "hv",
    ncensor.plot = TRUE,
    ggtheme = theme_bw()
  )
)
print(km_plot)
this worked perfectly fine, i cahnged daten_filtered to da_daten because the filtering process does take place inside the loop. hover i am not sure we need to use the filtered data for kaplan maier in the loop either, is there anything i am overlooking

## Prompt 20

perfect so if im not mistaken inside the loop there is mainly a storing issue for the kaplan maier plots as well as the forest plots. would you mind giving me the updated and finalized code

## Prompt 21

rror in `da_daten[, c(time_var, event_var, var, list_confounder)]`:
! Can't subset columns with `c(time_var, event_var, var, list_confounder)`.
✖ `c(time_var, event_var, var, list_confounder)` must be logical, numeric, or character, not a list.
Run `rlang::last_trace()` to see where the error occurred.
>

## Prompt 22

Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 23

"Alter_beitotwenntot_a_rounded"
[1] "Alter_beitotwenntot_a_rounded"
[1] "NE_surv_all_a_tim"
[1] "time_to_endpoint_a_tim"
[1] TRUE
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 24

Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] TRUE
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 25

analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  for (dataset_name in names(list_daten)) {
    da_daten <- list_daten[[dataset_name]]
    
    for (var in list_var) {
      # Convert categorical variable to factor
      if (!is.factor(da_daten[[var]])) {
        da_daten[[var]] <- as.factor(da_daten[[var]])
        message("Converting ", var, " to factor in ", dataset_name, "\n")
      }
      
      for (i in seq_along(list_surv_time)) {
        time_var <- list_surv_time[[i]]
        event_var <- list_endpoint[[i]]
        results <- list()
        
        for (dataset_name in names(list_daten)) {
          dataset <- list_daten[[dataset_name]]
          
          for (i in seq_along(list_surv_time)) {
            time_var <- list_surv_time[[i]]
            event_var <- list_endpoint[[i]]  
            
            print(paste("Processing:", dataset_name, time_var, event_var))  # Debugging
            
            required_cols <- c(time_var, event_var, var, unlist(list_confounder))
            
            if (!all(required_cols %in% names(dataset))) {
              warning("Some required columns are missing in dataset:", dataset_name)
              next  # Skip this iteration if columns are missing
            }
            
            daten_filtered <- dataset[, required_cols, drop = FALSE]  # Subset safely
            results[[paste0(dataset_name, "_", time_var, "_", event_var)]] <- daten_filtered
          }
        }
        
        }        
        print(time_var %in% names(da_daten))
        # Define result storage key
        result_key <- paste(dataset_name, var, event_var, sep = "_")
        
        # ---- 1️⃣ Kaplan-Meier Plot (UNADJUSTED) ----
        suppressWarnings({
          km_fit <- survfit(Surv(da_daten[[time_var]], da_daten[[event_var]]) ~ da_daten[[var]], data = da_daten)
          km_plot <- ggsurvplot(
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
          )
        })
        
        # Store Kaplan-Meier plot
        results[[result_key]]$kaplan_meier_plot <- km_plot
        
        # ---- 2️⃣ Cox Proportional Hazards Model (ADJUSTED) ----
        cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
        )
        
        cox_model <- coxph(cox_formula, data = daten_filtered)
        
        # Store Cox model summary
        results[[result_key]]$cox_model <- summary(cox_model)
        
        # ---- 3️⃣ Cox Forest Plot ----
        suppressWarnings({
          forest_plot <- ggforest(cox_model, data = daten_filtered)
        })
        
        # Store Forest plot
        results[[result_key]]$cox_forest_plot <- forest_plot
      }
    }
  
  
  return(results)
}

# ---- ✅ Example Usage ----
results <- analyze_survival(list_daten, list_var, list_surv_time, list_endpoint, list_confounder)

# Print Kaplan-Meier Plot
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$kaplan_meier_plot)

# Print Cox Model
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$cox_model)

# Print Cox Forest Plot
print(results[["da_daten_Hyperfiltration140_1_ja_tot_1"]]$cox_forest_plot)
thats my current code

## Prompt 26

wait for two more message before answering please

## Prompt 27

thats the error:

## Prompt 28

[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: da_daten NE_surv_all_a_tim NE_1_ja"
[1] "Processing: da_daten time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] "Processing: subset_minusCKD Alter_beitotwenntot_a_rounded tot_1"
[1] "Processing: subset_minusCKD NE_surv_all_a_tim NE_1_ja"
[1] "Processing: subset_minusCKD time_to_endpoint_a_tim endpoint_reached_ja_1"
[1] TRUE
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 29

i wanted a function that with one run can do a comprehensive survival analysis based on 5 paramete(lists) i provided, list_daten <- list(
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
the function should do a kaplan maier graph a coxgraph a forest plot (using survminer) as well as a logrank an unadjusted coxph and a cox analysis adjusted for the counfunders in list_confounder

list_surv_time gives the time to endpoint for the corresponding endpoind reached yes/no in list_endpoint
list_var are the variables i want to stratify by (do people with hyperfiltration live shorter)
key issues with the function were that we neded a filtered data set (especially for the cox and forest plots) as i have NA values in my data but want to use the unfiltered data for the kaplan maier plots
also labeling and transfering the data inside the function caused a few errors so defining the type of variable for each variable could be helpful 
lastly the issue was mainly that the plots were not stored correctly so i had no access to them after running the function
I simply want a working function for my analysis
also inside the graph function please use these parameters:
suppressWarnings({
        km_fit <- survfit(Surv(da_daten[[time_var]], da_daten[[event_var]]) ~ da_daten[[var]], data = da_daten)
        km_plot <- ggsurvplot(
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
        )
      })

## Prompt 30

i get this error
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden
before answering please check this conversation as i think we've been there and have fixed this error once, mabye you are able to find the solution(or part of it above) hopefully it hasnt been deleted allready

## Prompt 31

[1] "Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden
solltest du mit systematischem debugging beginnen bitte führe alle möglichen schritte, die du nach der reihe duirchgehn würdest zugleich aus(bzw.  zumindest schon reaktionen auf 5 verschiedene outcomes) und provide mir den entsprechenden code dankeschön

## Prompt 32

[1] "🔍 Checking dataset: da_daten"
  [1] "ID"                                                      "Platzhalter...2"                                         "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                         "geboren"                                                 "untersucht"                                             
  [7] "gestorben"                                               "OEDTRID"                                                 "Sterbedatum"                                            
 [10] "Sterbediagnose"                                          "LetzteBekannteTherapie"                                  "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                   "Beginn Nierenersatztherapie"                             "Webokra zuletzt"                                        
 [16] "Check Web"                                               "...17"                                                   "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                             "Alter_beitot_d"                                          "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                  "tot_1"                                                   "DD_survival"                                            
 [25] "survival"                                                "NE_1_ja"                                                 "DD_NEsurvival"                                          
 [28] "NEsurvival"                                              "Geschlecht_1_m...29"                                     "TX"                                                     
 [31] "TX_1_ja"                                                 "TX_Datum"                                                "...33"                                                  
 [34] "erst_NE_dat"                                             "erst_NE"                                                 "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                           "Diabetesdauer_a_"                                        "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                           "Manifestationsalter_a_"                                  "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                      "RRsyst"                                                  "RRdiast"                                                
 [46] "Groesse__m_"                                             "Gewicht__kg_"                                            "BMI"                                                    
 [49] "HbA1c...49"                                              "HbA1c...50"                                              "HbA1c...51"                                             
 [52] "NBZ"                                                     "ppBZ"                                                    "Kreatinin_serum"                                        
 [55] "UN"                                                      "Kreatininclearance_endogen"                              "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                         "Geschlecht_1_m...59"                                     "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                           "RHC__ml_min_"                                            "GF__ml_"                                                
 [64] "FF"                                                      "Albumin_Harn__0_negativ__1_positiv_"                     "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                        "OB_0_Mia1_maA2_fehlt3"                                   "MikroJa_1"                                              
 [70] "ProteinJa_1"                                             "hypertonie_ja_1"                                         "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                              "Hyperfiltration140_1_ja"                                 "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                 "HyperfiltrationMedian_1_ja"                              "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                   "Höherer HbA1c AW/AX"                                     "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                           "survivaltime_a_tim"                                      "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                  "...86"                                                   "NOTIZEN"                                                
 [88] "...88"                                                   "...89"                                                   "...90"                                                  
 [91] "...91"                                                   "egfr_ckdepi2021_tim"                                     "min(scr/K)oder 1"                                       
 [94] "alpha"                                                   "max(scr/K)oder1"                                         "´-1,012 wenn frau"                                      
 [97] "...97"                                                   "geboren_clean"                                           "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                       "DatumLetzteTherapie_clean"                               "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                   "TX_Datum_clean"                                          "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                           "Diabetesdauer_totwenntot_a_tim_rounded"                  "NE_surv_a_tim_rounded"                                  
✅ Converting Hyperfiltration140_1_ja to factor in da_daten

[1] "🚀 Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
[1] "✅ Data Preview:"
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden
thats the error
please continue working in the same manner trying to cover multiple possibilities of the problem  in the debugging approach and providing the new code

## Prompt 33

[1] "🔍 Checking dataset: da_daten"
  [1] "ID"                                                      "Platzhalter...2"                                         "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                         "geboren"                                                 "untersucht"                                             
  [7] "gestorben"                                               "OEDTRID"                                                 "Sterbedatum"                                            
 [10] "Sterbediagnose"                                          "LetzteBekannteTherapie"                                  "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                   "Beginn Nierenersatztherapie"                             "Webokra zuletzt"                                        
 [16] "Check Web"                                               "...17"                                                   "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                             "Alter_beitot_d"                                          "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                  "tot_1"                                                   "DD_survival"                                            
 [25] "survival"                                                "NE_1_ja"                                                 "DD_NEsurvival"                                          
 [28] "NEsurvival"                                              "Geschlecht_1_m...29"                                     "TX"                                                     
 [31] "TX_1_ja"                                                 "TX_Datum"                                                "...33"                                                  
 [34] "erst_NE_dat"                                             "erst_NE"                                                 "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                           "Diabetesdauer_a_"                                        "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                           "Manifestationsalter_a_"                                  "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                      "RRsyst"                                                  "RRdiast"                                                
 [46] "Groesse__m_"                                             "Gewicht__kg_"                                            "BMI"                                                    
 [49] "HbA1c...49"                                              "HbA1c...50"                                              "HbA1c...51"                                             
 [52] "NBZ"                                                     "ppBZ"                                                    "Kreatinin_serum"                                        
 [55] "UN"                                                      "Kreatininclearance_endogen"                              "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                         "Geschlecht_1_m...59"                                     "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                           "RHC__ml_min_"                                            "GF__ml_"                                                
 [64] "FF"                                                      "Albumin_Harn__0_negativ__1_positiv_"                     "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                        "OB_0_Mia1_maA2_fehlt3"                                   "MikroJa_1"                                              
 [70] "ProteinJa_1"                                             "hypertonie_ja_1"                                         "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                              "Hyperfiltration140_1_ja"                                 "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                 "HyperfiltrationMedian_1_ja"                              "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                   "Höherer HbA1c AW/AX"                                     "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                           "survivaltime_a_tim"                                      "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                  "...86"                                                   "NOTIZEN"                                                
 [88] "...88"                                                   "...89"                                                   "...90"                                                  
 [91] "...91"                                                   "egfr_ckdepi2021_tim"                                     "min(scr/K)oder 1"                                       
 [94] "alpha"                                                   "max(scr/K)oder1"                                         "´-1,012 wenn frau"                                      
 [97] "...97"                                                   "geboren_clean"                                           "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                       "DatumLetzteTherapie_clean"                               "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                   "TX_Datum_clean"                                          "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                           "Diabetesdauer_totwenntot_a_tim_rounded"                  "NE_surv_a_tim_rounded"                                  
✅ Converting Hyperfiltration140_1_ja to factor in da_daten

[1] "🚀 Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
[1] "✅ Data Preview:"
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden
please continue working in the same manner trying to cover multiple possibilities of the problem (while exploring the previous conversation for possible solutions)  in the debugging approach and providing the new code

## Prompt 34

[1] "🔍 Checking dataset: da_daten"
  [1] "ID"                                                      "Platzhalter...2"                                         "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                         "geboren"                                                 "untersucht"                                             
  [7] "gestorben"                                               "OEDTRID"                                                 "Sterbedatum"                                            
 [10] "Sterbediagnose"                                          "LetzteBekannteTherapie"                                  "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                   "Beginn Nierenersatztherapie"                             "Webokra zuletzt"                                        
 [16] "Check Web"                                               "...17"                                                   "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                             "Alter_beitot_d"                                          "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                  "tot_1"                                                   "DD_survival"                                            
 [25] "survival"                                                "NE_1_ja"                                                 "DD_NEsurvival"                                          
 [28] "NEsurvival"                                              "Geschlecht_1_m...29"                                     "TX"                                                     
 [31] "TX_1_ja"                                                 "TX_Datum"                                                "...33"                                                  
 [34] "erst_NE_dat"                                             "erst_NE"                                                 "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                           "Diabetesdauer_a_"                                        "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                           "Manifestationsalter_a_"                                  "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                      "RRsyst"                                                  "RRdiast"                                                
 [46] "Groesse__m_"                                             "Gewicht__kg_"                                            "BMI"                                                    
 [49] "HbA1c...49"                                              "HbA1c...50"                                              "HbA1c...51"                                             
 [52] "NBZ"                                                     "ppBZ"                                                    "Kreatinin_serum"                                        
 [55] "UN"                                                      "Kreatininclearance_endogen"                              "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                         "Geschlecht_1_m...59"                                     "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                           "RHC__ml_min_"                                            "GF__ml_"                                                
 [64] "FF"                                                      "Albumin_Harn__0_negativ__1_positiv_"                     "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                        "OB_0_Mia1_maA2_fehlt3"                                   "MikroJa_1"                                              
 [70] "ProteinJa_1"                                             "hypertonie_ja_1"                                         "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                              "Hyperfiltration140_1_ja"                                 "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                 "HyperfiltrationMedian_1_ja"                              "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                   "Höherer HbA1c AW/AX"                                     "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                           "survivaltime_a_tim"                                      "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                  "...86"                                                   "NOTIZEN"                                                
 [88] "...88"                                                   "...89"                                                   "...90"                                                  
 [91] "...91"                                                   "egfr_ckdepi2021_tim"                                     "min(scr/K)oder 1"                                       
 [94] "alpha"                                                   "max(scr/K)oder1"                                         "´-1,012 wenn frau"                                      
 [97] "...97"                                                   "geboren_clean"                                           "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                       "DatumLetzteTherapie_clean"                               "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                   "TX_Datum_clean"                                          "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                           "Diabetesdauer_totwenntot_a_tim_rounded"                  "NE_surv_a_tim_rounded"                                  
✅ Converting Hyperfiltration140_1_ja to factor in da_daten

[1] "🔄 Checking time_var: Alter_beitotwenntot_a_rounded | event_var: tot_1"
[1] "🚀 Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
[1] "✅ Data Preview (Before Filtering):"
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 35

[1] "🔍 Checking dataset: da_daten"
  [1] "ID"                                                      "Platzhalter...2"                                         "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                         "geboren"                                                 "untersucht"                                             
  [7] "gestorben"                                               "OEDTRID"                                                 "Sterbedatum"                                            
 [10] "Sterbediagnose"                                          "LetzteBekannteTherapie"                                  "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                   "Beginn Nierenersatztherapie"                             "Webokra zuletzt"                                        
 [16] "Check Web"                                               "...17"                                                   "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                             "Alter_beitot_d"                                          "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                  "tot_1"                                                   "DD_survival"                                            
 [25] "survival"                                                "NE_1_ja"                                                 "DD_NEsurvival"                                          
 [28] "NEsurvival"                                              "Geschlecht_1_m...29"                                     "TX"                                                     
 [31] "TX_1_ja"                                                 "TX_Datum"                                                "...33"                                                  
 [34] "erst_NE_dat"                                             "erst_NE"                                                 "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                           "Diabetesdauer_a_"                                        "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                           "Manifestationsalter_a_"                                  "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                      "RRsyst"                                                  "RRdiast"                                                
 [46] "Groesse__m_"                                             "Gewicht__kg_"                                            "BMI"                                                    
 [49] "HbA1c...49"                                              "HbA1c...50"                                              "HbA1c...51"                                             
 [52] "NBZ"                                                     "ppBZ"                                                    "Kreatinin_serum"                                        
 [55] "UN"                                                      "Kreatininclearance_endogen"                              "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                         "Geschlecht_1_m...59"                                     "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                           "RHC__ml_min_"                                            "GF__ml_"                                                
 [64] "FF"                                                      "Albumin_Harn__0_negativ__1_positiv_"                     "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                        "OB_0_Mia1_maA2_fehlt3"                                   "MikroJa_1"                                              
 [70] "ProteinJa_1"                                             "hypertonie_ja_1"                                         "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                              "Hyperfiltration140_1_ja"                                 "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                 "HyperfiltrationMedian_1_ja"                              "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                   "Höherer HbA1c AW/AX"                                     "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                           "survivaltime_a_tim"                                      "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                  "...86"                                                   "NOTIZEN"                                                
 [88] "...88"                                                   "...89"                                                   "...90"                                                  
 [91] "...91"                                                   "egfr_ckdepi2021_tim"                                     "min(scr/K)oder 1"                                       
 [94] "alpha"                                                   "max(scr/K)oder1"                                         "´-1,012 wenn frau"                                      
 [97] "...97"                                                   "geboren_clean"                                           "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                       "DatumLetzteTherapie_clean"                               "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                   "TX_Datum_clean"                                          "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                           "Diabetesdauer_totwenntot_a_tim_rounded"                  "NE_surv_a_tim_rounded"                                  
✅ Converting Hyperfiltration140_1_ja to factor in da_daten

[1] "🔄 Checking time_var: Alter_beitotwenntot_a_rounded | event_var: tot_1"
[1] "🚀 Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
[1] "✅ Data Preview (Before Filtering):"
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden

## Prompt 36

1] "🔍 Checking dataset: da_daten"
  [1] "ID"                                                      "Platzhalter...2"                                         "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                         "geboren"                                                 "untersucht"                                             
  [7] "gestorben"                                               "OEDTRID"                                                 "Sterbedatum"                                            
 [10] "Sterbediagnose"                                          "LetzteBekannteTherapie"                                  "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                   "Beginn Nierenersatztherapie"                             "Webokra zuletzt"                                        
 [16] "Check Web"                                               "...17"                                                   "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                             "Alter_beitot_d"                                          "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                  "tot_1"                                                   "DD_survival"                                            
 [25] "survival"                                                "NE_1_ja"                                                 "DD_NEsurvival"                                          
 [28] "NEsurvival"                                              "Geschlecht_1_m...29"                                     "TX"                                                     
 [31] "TX_1_ja"                                                 "TX_Datum"                                                "...33"                                                  
 [34] "erst_NE_dat"                                             "erst_NE"                                                 "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                           "Diabetesdauer_a_"                                        "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                           "Manifestationsalter_a_"                                  "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                      "RRsyst"                                                  "RRdiast"                                                
 [46] "Groesse__m_"                                             "Gewicht__kg_"                                            "BMI"                                                    
 [49] "HbA1c...49"                                              "HbA1c...50"                                              "HbA1c...51"                                             
 [52] "NBZ"                                                     "ppBZ"                                                    "Kreatinin_serum"                                        
 [55] "UN"                                                      "Kreatininclearance_endogen"                              "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                         "Geschlecht_1_m...59"                                     "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                           "RHC__ml_min_"                                            "GF__ml_"                                                
 [64] "FF"                                                      "Albumin_Harn__0_negativ__1_positiv_"                     "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                        "OB_0_Mia1_maA2_fehlt3"                                   "MikroJa_1"                                              
 [70] "ProteinJa_1"                                             "hypertonie_ja_1"                                         "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                              "Hyperfiltration140_1_ja"                                 "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                 "HyperfiltrationMedian_1_ja"                              "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                   "Höherer HbA1c AW/AX"                                     "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                           "survivaltime_a_tim"                                      "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                  "...86"                                                   "NOTIZEN"                                                
 [88] "...88"                                                   "...89"                                                   "...90"                                                  
 [91] "...91"                                                   "egfr_ckdepi2021_tim"                                     "min(scr/K)oder 1"                                       
 [94] "alpha"                                                   "max(scr/K)oder1"                                         "´-1,012 wenn frau"                                      
 [97] "...97"                                                   "geboren_clean"                                           "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                       "DatumLetzteTherapie_clean"                               "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                   "TX_Datum_clean"                                          "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                           "Diabetesdauer_totwenntot_a_tim_rounded"                  "NE_surv_a_tim_rounded"                                  
✅ Converting Hyperfiltration140_1_ja to factor in da_daten

[1] "🔄 Checking time_var: Alter_beitotwenntot_a_rounded | event_var: tot_1"
 [1] "da_daten"        "dataset_name"    "event_var"       "i"               "list_confounder" "list_daten"      "list_endpoint"   "list_surv_time"  "list_var"        "results"        
[11] "time_var"        "var"            
[1] "🚀 Processing: da_daten Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja"
[1] "✅ Data Preview (Before Filtering):"
# A tibble: 6 × 7
  Alter_beitotwenntot_a_rounded tot_1 Hyperfiltration140_1_ja RRsyst RRdiast NE_1_ja highest_measured_hba1c_naisna
                          <dbl> <dbl> <fct>                    <dbl>   <dbl>   <dbl>                         <dbl>
1                            48     1 0                          170     100       1                           6.9
2                            56     0 0                          120      80       0                           7.1
3                            56     1 0                          130      85       1                           7.7
4                            38     1 0                          130      80       1                          12.6
5                            50     1 0                          140      80       0                           9  
6                            52     1 0                          150     100       1                           5.7
Fehler in FUN(X[[i]], ...) : Objekt 'time_var' nicht gefunden
please continue to double check, check also the previous conversation, the place in the code where the error is occuring and look what kind of imput that function needs, and also if the form of time_var fits those needs

## Prompt 37

FUN(X[[i]], ...)
14.
FUN(X[[i]], ...)
13.
lapply(lapply(varnames, as.name), eval, data, env)
12.
stats::get_all_vars(.formula, data = data)
11.
.extract.survfit(fit, data)
10.
.pvalue(fit, data = data, method = method, pval = pval, pval.coord = pval.coord,
pval.method.coord = pval.method.coord, get_coord = get_coord,
test.for.trend = test.for.trend)
9.
dplyr::select(., variable, dplyr::everything())
8.
.pvalue(fit, data = data, method = method, pval = pval, pval.coord = pval.coord,
pval.method.coord = pval.method.coord, get_coord = get_coord,
test.for.trend = test.for.trend) %>% dplyr::select(variable,
dplyr::everything())
7.
surv_pvalue(fit, method = log.rank.weights, data = data, pval = pval,
pval.coord = pval.coord, pval.method.coord = pval.method.coord,
test.for.trend = test.for.trend)
6.
(function (fit, data = NULL, fun = NULL, color = NULL, palette = NULL,
linetype = 1, break.x.by = NULL, break.y.by = NULL, break.time.by = NULL,
surv.scale = c("default", "percent"), xscale = 1, conf.int = FALSE,
conf.int.fill = "gray", conf.int.style = "ribbon", conf.int.alpha = 0.3, ...
5.
do.call(ggsurvplot_core, opts)
4.
ggsurvplot(km_fit, data = da_daten, censor.shape = "|", censor.size = 4,
conf.int = TRUE, pval = TRUE, xlab = "Time in years", break.time.by = 5,
surv.median.line = "hv", ncensor.plot = TRUE, ggtheme = theme_bw())
3.
withCallingHandlers(expr, warning = function(w) if (inherits(w,
classes)) tryInvokeRestart("muffleWarning"))
2.
suppressWarnings({
km_fit <- tryCatch(survfit(Surv(as.numeric(get(time_var,
da_daten)), as.numeric(get(event_var, da_daten))) ~ get(var,
da_daten), data = da_daten), error = function(e) { ...
1.
analyze_survival(list_daten, list_var, list_surv_time, list_endpoint,
list_confounder)
thats the error treceback

## Prompt 38

analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  for (dataset_name in names(list_daten)) {
    da_daten <- list_daten[[dataset_name]]
    
    print(paste("🔍 Checking dataset:", dataset_name))
    print(names(da_daten))  # Show dataset structure
    
    for (var in list_var) {
      if (!(var %in% names(da_daten))) {
        warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
        next
      }
      
      if (!is.factor(da_daten[[var]])) {
        da_daten[[var]] <- as.factor(da_daten[[var]])
        message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
      }
      
      for (i in seq_along(list_surv_time)) {
        time_var <- unlist(list_surv_time)[i]# ✅ Convert list to vector
        event_var <- unlist(list_endpoint)[i]  # ✅ Convert list to vector
        
        # 🚨 **Debugging: Ensure `time_var` exists before proceeding** 🚨
        print(paste("🔄 Checking time_var:", time_var, "| event_var:", event_var))
        print(ls())  # ✅ Check all variables in scope
        
        stopifnot(!is.null(time_var), !is.null(event_var))  # ✅ Catch NULL values early
        
        if (!(time_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        }
        if (!(event_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        }
        
        print(paste("🚀 Processing:", dataset_name, time_var, event_var, var))
        
        required_cols <- c(time_var, event_var, var, unlist(list_confounder))
        missing_cols <- setdiff(required_cols, names(da_daten))
        
        if (length(missing_cols) > 0) {
          warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
          next
        }
        
        print("✅ Data Preview (Before Filtering):")
        print(head(da_daten[ , required_cols, drop = FALSE]))
        print(paste("TESTTIM", daten[time_var],"TESTTIMENDE"))
        # ---- 1️⃣ Kaplan-Meier Plot (UNFILTERED DATA) ----
        suppressWarnings({
          km_fit <- tryCatch(
            survfit(Surv(daten[time_var], 
                         daten[event_var] ~ get(var, da_daten), 
                    data = da_daten)),
            error = function(e) {
              warning(paste("❌ Kaplan-Meier plot failed:", e$message))
              return(NULL)
            }
          )
          print("kmfitdone")
        
                                
          if (!is.null(km_fit)) {
            km_plot <- ggsurvplot(
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
            )
          } else {
            km_plot <- NULL
          }
        })
        print("plot done")
        # ---- 2️⃣ Filter Data for Cox Models (REMOVE NA) ----
        daten_filtered <- da_daten[complete.cases(da_daten[ , required_cols]), ]
        
        print("✅ Data Preview (After Filtering):")
        print(head(daten_filtered))
        print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                        as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
        # ---- 4️⃣ Unadjusted Cox Model ----
        unadjusted_cox_model <- tryCatch(
          coxph(Surv(as.numeric(get(time_var, daten_filtered)), 
                     as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered), 
                data = daten_filtered),
          error = function(e) {
            warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
        
        # ---- 5️⃣ Adjusted Cox Model ----
        cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
        )
        
        adjusted_cox_model <- tryCatch(
          coxph(cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
        
        # ---- 6️⃣ Cox Forest Plot ----
        forest_plot <- tryCatch(
          ggforest(adjusted_cox_model, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Forest plot failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
        # ---- 7️⃣ Store Results ----
        result_key <- paste(dataset_name, var, event_var, sep = "_")
        
        results[[result_key]] <- list(
          kaplan_meier_plot = km_plot,
          logrank_test = logrank_test,
          unadjusted_cox_model = unadjusted_cox_summary,
          adjusted_cox_model = adjusted_cox_summary,
          cox_forest_plot = forest_plot
        )
        
        print(paste("✅ Successfully stored results for:", result_key))
      }
    }
  }
  
  return(results)
}
this works now, only the kaplan maier plots and forest plots results aren NULL

## Prompt 39

also on the end of the results it says :Es gab 40 Warnungen (Anzeige mit warnings())

## Prompt 40

46: In value[[3L]](cond) :
  ⚠️ Forest plot failed for subset_minusCKD - Hyperfiltration130_1_ja with error: Namen passen nicht zu den vorhergehenden Namen46: In value[[3L]](cond) :
  ⚠️ Forest plot failed for subset_minusCKD - Hyperfiltration130_1_ja with error: Namen passen nicht zu den vorhergehenden Namen

## Prompt 41

Forest plot failed for subset_minusCKD - Hyperfiltration140_1_ja with error: log - axis(), 'at' creation, _LARGE_ range: invalid {xy}axp or par; nint=7
	 axp[0:1]=(1e-307,1e+308), usr[0:1]=(0,inf); i=615, ni=87

## Prompt 42

Unadjusted Cox Model failed for da_daten - HyperfiltrationMedian_1_ja with error: unbenutztes Argument (data = daten_filtered)

## Prompt 43

unadjusted_cox_model <- tryCatch(
          coxph(Surv(daten_filtered[[time_var]], 
                     daten_filtered[[event_var]] ~ daten_filtered[[var]], 
                     data = daten_filtered)),
          error = function(e) {
            warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )

## Prompt 44

In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula

## Prompt 45

do i need that precaution for both adjusted and unadjusted cox?

## Prompt 46

isnt point 2 contradictory to unadjusted_cox_formula <- as.formula(
  paste("Surv(", time_var, ",", event_var, ") ~", var)
)

unadjusted_cox_model <- tryCatch(
  coxph(unadjusted_cox_formula, data = daten_filtered),
  error = function(e) {
    warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
    return(NULL)
  }
)

## Prompt 47

if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        unadjusted_cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var)
        )
        
        unadjusted_cox_model <- tryCatch(
          coxph(unadjusted_cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
        
        # ---- 5️⃣ Adjusted Cox Model ----
        if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
        )
        
        adjusted_cox_model <- tryCatch(
          coxph(cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
        

the warning In coxph(cox_formula, data = daten_filtered) :
  a variable appears on both the left and right sides of the formula still apears

## Prompt 48

In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  ... :
  Loglik converged before variable  4 ; coefficient may be infinite.

## Prompt 49

analyze_survival <- function(list_daten, list_var, list_surv_time, list_endpoint, list_confounder) {
  
  results <- list()
  
  for (dataset_name in names(list_daten)) {
    da_daten <- list_daten[[dataset_name]]
    
    print(paste("🔍 Checking dataset:", dataset_name))
    print(names(da_daten))  # Show dataset structure
    
    for (var in list_var) {
      if (!(var %in% names(da_daten))) {
        warning(paste("⚠️ Variable", var, "not found in", dataset_name, "- Skipping"))
        next
      }
      
      if (!is.factor(da_daten[[var]])) {
        da_daten[[var]] <- as.factor(da_daten[[var]])
        message("✅ Converting ", var, " to factor in ", dataset_name, "\n")
      }
      
      for (i in seq_along(list_surv_time)) {
        time_var <- unlist(list_surv_time)[i]# ✅ Convert list to vector
        event_var <- unlist(list_endpoint)[i]  # ✅ Convert list to vector
        
        # 🚨 **Debugging: Ensure `time_var` exists before proceeding** 🚨
        print(paste("🔄 Checking time_var:", time_var, "| event_var:", event_var))
        print(ls())  # ✅ Check all variables in scope
        
        stopifnot(!is.null(time_var), !is.null(event_var))  # ✅ Catch NULL values early
        
        if (!(time_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing time variable:", time_var, "in", dataset_name))
        }
        if (!(event_var %in% names(da_daten))) {
          stop(paste("❌ ERROR: Missing event variable:", event_var, "in", dataset_name))
        }
        
        print(paste("🚀 Processing:", dataset_name, time_var, event_var, var))
        
        required_cols <- c(time_var, event_var, var, unlist(list_confounder))
        missing_cols <- setdiff(required_cols, names(da_daten))
        
        if (length(missing_cols) > 0) {
          warning(paste("⚠️ Missing required columns in", dataset_name, ":", paste(missing_cols, collapse = ", ")))
          next
        }
        
        print("✅ Data Preview (Before Filtering):")
        print(head(da_daten[ , required_cols, drop = FALSE]))
        print(paste("TESTTIM", da_daten[[time_var]],"TESTTIMENDE"))
        # ---- 1️⃣ Kaplan-Meier Plot (UNFILTERED DATA) ----
        suppressWarnings({
          km_fit <- tryCatch(
            survfit(Surv(da_daten[[time_var]], 
                         da_daten[[event_var]] ~ da_daten[[var]], 
                    data = da_daten)),
            error = function(e) {
              warning(paste("❌ Kaplan-Meier plot failed:", e$message))
              return(NULL)
            }
          )
          print("kmfitdone")
        
                                
          if (!is.null(km_fit)) {
            km_plot <- ggsurvplot(
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
            )
          } else {
            km_plot <- NULL
          }
        })
        print("plot done")
        # ---- 2️⃣ Filter Data for Cox Models (REMOVE NA) ----
        daten_filtered <- da_daten[complete.cases(da_daten[ , required_cols]), ]
        
        print("✅ Data Preview (After Filtering):")
        print(head(daten_filtered))
        print(paste("📊 Rows before filtering:", nrow(da_daten), "| After filtering:", nrow(daten_filtered)))
        
        # ---- 3️⃣ Log-Rank Test ----
        logrank_test <- tryCatch(
          survdiff(Surv(as.numeric(get(time_var, daten_filtered)), 
                        as.numeric(get(event_var, daten_filtered))) ~ get(var, daten_filtered)),
          error = function(e) {
            warning("⚠️ Log-Rank Test failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        
        # ---- 4️⃣ Unadjusted Cox Model ---
        if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        unadjusted_cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var)
        )
        
        unadjusted_cox_model <- tryCatch(
          coxph(unadjusted_cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Unadjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        unadjusted_cox_summary <- if (!is.null(unadjusted_cox_model)) summary(unadjusted_cox_model) else NULL
        
        # ---- 5️⃣ Adjusted Cox Model ----
        if (var %in% c(time_var, event_var)) {
          warning(paste("⚠️ Skipping variable", var, "because it is used as time or event variable in", dataset_name))
          next  # Skip this iteration
        }
        cox_formula <- as.formula(
          paste("Surv(", time_var, ",", event_var, ") ~", var, "+", paste(list_confounder, collapse = " + "))
        )
        
        adjusted_cox_model <- tryCatch(
          coxph(cox_formula, data = daten_filtered),
          error = function(e) {
            warning("⚠️ Adjusted Cox Model failed for ", dataset_name, " - ", var, " with error: ", e$message)
            return(NULL)
          }
        )
        adjusted_cox_summary <- if (!is.null(adjusted_cox_model)) summary(adjusted_cox_model) else NULL
        
        # ---- 6️⃣ Cox Forest Plot ----
        print(("GGFOREST TEST"))
        print(names(adjusted_cox_model$coefficients))
        print(names(daten_filtered))
        print(setdiff(list_confounder, names(daten_filtered)))
        print("GGFORTES TEST ENDE")
        
        daten_filtered <- daten_filtered %>%
          mutate(across(where(is.numeric), ~ ifelse(. > 1e6, log1p(.), .)))  
        
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
        
        # ---- 7️⃣ Store Results ----
        result_key <- paste(dataset_name, var, event_var, sep = "_")
        
        results[[result_key]] <- list(
          kaplan_meier_plot = km_plot,
          logrank_test = logrank_test,
          unadjusted_cox_model = unadjusted_cox_summary,
          adjusted_cox_model = adjusted_cox_summary,
          cox_forest_plot = forest_plot
        )
        
        print(paste("✅ Successfully stored results for:", result_key))
      }
    }
  }
  
  return(results)
}

this is my function, and it works without warnings, however the kaplan maier part still gives NULL as result when wanting to print it

## Prompt 50

if (!is.null(km_fit)) print(summary(km_fit)) doesnt print km_fit summary 
however     survfit(Surv(as.numeric(da_daten[[time_var]]), as.numeric(da_daten[[event_var]])) ~ da_daten[[var]], data = da_daten), doesnt work as i get an error that time_var wasnt found

## Prompt 51

🚀 DEBUG: Checking input before Kaplan-Meier"
[1] "Dataset: subset_minusCKD | Time: time_to_endpoint_a_tim | Event: endpoint_reached_ja_1 | Stratify By: HyperfiltrationMedian_1_ja"
[1] "✅ Column Names in Dataset:"
  [1] "ID"                                                     
  [2] "Platzhalter...2"                                        
  [3] "Platzhalter...3"                                        
  [4] "Platzhalter...4"                                        
  [5] "geboren"                                                
  [6] "untersucht"                                             
  [7] "gestorben"                                              
  [8] "OEDTRID"                                                
  [9] "Sterbedatum"                                            
 [10] "Sterbediagnose"                                         
 [11] "LetzteBekannteTherapie"                                 
 [12] "DatumLetzteTherapie"                                    
 [13] "ZentrumLetzteTherapie"                                  
 [14] "Beginn Nierenersatztherapie"                            
 [15] "Webokra zuletzt"                                        
 [16] "Check Web"                                              
 [17] "...17"                                                  
 [18] "Alter_d_tim"                                            
 [19] "Alter_a_tim"                                            
 [20] "Alter_beitot_d"                                         
 [21] "Alter_beitotwenntot_a"                                  
 [22] "Kind_1"                                                 
 [23] "tot_1"                                                  
 [24] "DD_survival"                                            
 [25] "survival"                                               
 [26] "NE_1_ja"                                                
 [27] "DD_NEsurvival"                                          
 [28] "NEsurvival"                                             
 [29] "Geschlecht_1_m...29"                                    
 [30] "TX"                                                     
 [31] "TX_1_ja"                                                
 [32] "TX_Datum"                                               
 [33] "...33"                                                  
 [34] "erst_NE_dat"                                            
 [35] "erst_NE"                                                
 [36] "NE_surv_d_tim"                                          
 [37] "NE_surv_a_tim"                                          
 [38] "Diabetesdauer_a_"                                       
 [39] "Diabetesdauer_totwenntot_a_tim"                         
 [40] "Diabetesdauer_alslebend_a_tim"                          
 [41] "Manifestationsalter_a_"                                 
 [42] "Alter_ber"                                              
 [43] "Injektionsfrequenz"                                     
 [44] "RRsyst"                                                 
 [45] "RRdiast"                                                
 [46] "Groesse__m_"                                            
 [47] "Gewicht__kg_"                                           
 [48] "BMI"                                                    
 [49] "HbA1c...49"                                             
 [50] "HbA1c...50"                                             
 [51] "HbA1c...51"                                             
 [52] "NBZ"                                                    
 [53] "ppBZ"                                                   
 [54] "Kreatinin_serum"                                        
 [55] "UN"                                                     
 [56] "Kreatininclearance_endogen"                             
 [57] "KreaClber_Kreatininclearance_berechnet__Cockroft_Gault_"
 [58] "Clearance_MDRD2"                                        
 [59] "Geschlecht_1_m...59"                                    
 [60] "Clear_80_1"                                             
 [61] "ÖDGclearquart"                                          
 [62] "RHC__ml_min_"                                           
 [63] "GF__ml_"                                                
 [64] "FF"                                                     
 [65] "Albumin_Harn__0_negativ__1_positiv_"                    
 [66] "Albumin_quantitativ"                                    
 [67] "Mikroalbuminurie"                                       
 [68] "OB_0_Mia1_maA2_fehlt3"                                  
 [69] "MikroJa_1"                                              
 [70] "ProteinJa_1"                                            
 [71] "hypertonie_ja_1"                                        
 [72] "...72"                                                  
 [73] "Niere_1_OK_2_MiA_3_PU_4_NB"                             
 [74] "Hyperfiltration140_1_ja"                                
 [75] "Hyperfiltration130_1_ja"                                
 [76] "Hyperfiltration150_1_ja"                                
 [77] "HyperfiltrationMedian_1_ja"                             
 [78] "HyperfiltrationMedianohneausreißer_1_ja"                
 [79] "endpoint_reached_ja_1"                                  
 [80] "Höherer HbA1c AW/AX"                                    
 [81] "highest_measured_hba1c"                                 
 [82] "highest_measured_hba1c_naisna"                          
 [83] "survivaltime_a_tim"                                     
 [84] "NE_surv_all_a_tim"                                      
 [85] "time_to_endpoint_a_tim"                                 
 [86] "...86"                                                  
 [87] "NOTIZEN"                                                
 [88] "...88"                                                  
 [89] "...89"                                                  
 [90] "...90"                                                  
 [91] "...91"                                                  
 [92] "egfr_ckdepi2021_tim"                                    
 [93] "min(scr/K)oder 1"                                       
 [94] "alpha"                                                  
 [95] "max(scr/K)oder1"                                        
 [96] "´-1,012 wenn frau"                                      
 [97] "...97"                                                  
 [98] "geboren_clean"                                          
 [99] "gestorben_clean"                                        
[100] "Sterbedatum_clean"                                      
[101] "DatumLetzteTherapie_clean"                              
[102] "Beginn Nierenersatztherapie_clean"                      
[103] "Webokra zuletzt_clean"                                  
[104] "TX_Datum_clean"                                         
[105] "erst_NE_dat_clean"                                      
[106] "Alter_beitotwenntot_a_rounded"                          
[107] "Diabetesdauer_totwenntot_a_tim_rounded"                 
[108] "NE_surv_a_tim_rounded"                                  
[1] "✅ First 6 Rows:"
# A tibble: 6 × 3
  time_to_endpoint_a_tim endpoint_reached_ja_1 HyperfiltrationMedian_1_ja
                   <dbl>                 <dbl> <fct>                     
1                   55.5                     0 1                         
2                   53.9                     1 0                         
3                   53.8                     0 1                         
4                   34.8                     1 0                         
5                   61.6                     1 0                         
6                   82.7                     0 0                         
[1] "Type of time_var: numeric"
[1] "Type of event_var: numeric"
[1] "kmfitdone"
[1] "📊 Kaplan-Meier Summary:"

## Prompt 52

actually km_fit does not seem to work:
 suppressWarnings({
          km_fit <- tryCatch(
            survfit(Surv(da_daten[[time_var]], 
                         da_daten[[event_var]] ~ da_daten[[var]], 
                    data = da_daten)),
            error = function(e) {
              warning(paste("❌ Kaplan-Meier plot failed:", e$message))
              return(NULL)
           }
          )

## Prompt 53

ich habe noch einen fehler in der ggplot funktion   if (!is.null(km_fit)) {
            km_plot <- ggsurvplot(
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
            )
          } else {
            km_plot <- NULL
          }
         kimfit sollte jetzt klappen

## Prompt 54

i have the following part in my code:
suppressWarnings({
          km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var, ") ~", var)
          )
          
          print("🚀 DEBUG: Kaplan-Meier Formula")
          print(km_formula)  # Check if the formula is correctly generated
          
          km_fit <- tryCatch(
            survfit(km_formula, data = da_daten),  
            error = function(e) {
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
        })
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
        

when running it 
I get: 
[1] "🚀 DEBUG: Kaplan-Meier Formula"
Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja
<environment: 0x000002b0deaffb38>
[1] "✅ Kaplan-Meier Model Summary:"
Call: survfit(formula = km_formula, data = da_daten)

## Prompt 55

I have the folling piece of code
suppressWarnings({
          km_formula <- as.formula(
            paste("Surv(", time_var, ",", event_var, ") ~", var)
          )
          
          print("🚀 DEBUG: Kaplan-Meier Formula")
          print(km_formula)  # Check if the formula is correctly generated
          
          km_fit <- tryCatch(
            survfit(km_formula, data = da_daten),  
            error = function(e) {
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
        })
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
        
        print("📊 Kaplan-Meier Summary:")
        if (!is.null(km_fit)) print(summary(km_fit))
        print("Kaplan-Meier Summary done")
        
        print("plot done")

as result i get the following:
1)
[1] "🚀 DEBUG: Kaplan-Meier Formula"
Surv(time_to_endpoint_a_tim, endpoint_reached_ja_1) ~ HyperfiltrationMedian_1_ja
<environment: 0x000002b0deaffb38>
[1] "✅ Kaplan-Meier Model Summary:"
Call: survfit(formula = km_formula, data = da_daten)
#this indicates that km_fit is created and is not NULL right?
next we get the summary of km_fit:
2)
2 Beobachtungen als fehlend gelöscht 
                HyperfiltrationMedian_1_ja=0 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
 26.4    134       1    0.993 0.00743        0.978        1.000
 31.1    133       1    0.985 0.01047        0.965        1.000
 33.7    132       1    0.978 0.01278        0.953        1.000
 34.8    131       1    0.970 0.01470        0.942        0.999
 35.2    130       1    0.963 0.01637        0.931        0.995
 35.5    129       1    0.955 0.01787        0.921        0.991
 36.0    128       1    0.948 0.01922        0.911        0.986
 36.9    127       1    0.940 0.02047        0.901        0.981
 39.9    126       1    0.933 0.02162        0.891        0.976
 40.2    125       1    0.925 0.02270        0.882        0.971
 40.9    124       1    0.918 0.02371        0.873        0.966
 41.4    123       1    0.910 0.02467        0.863        0.960
 41.5    122       1    0.903 0.02557        0.854        0.955
 42.6    121       1    0.896 0.02642        0.845        0.949
 44.1    120       1    0.888 0.02724        0.836        0.943
 44.1    119       1    0.881 0.02801        0.827        0.937
 44.5    118       1    0.873 0.02875        0.819        0.931
 45.0    117       1    0.866 0.02946        0.810        0.925
 45.9    116       1    0.858 0.03013        0.801        0.919
 46.0    115       1    0.851 0.03078        0.793        0.913
 46.3    114       1    0.843 0.03140        0.784        0.907
 48.3    113       1    0.836 0.03200        0.775        0.901
 48.5    112       1    0.828 0.03257        0.767        0.895
 48.6    111       1    0.821 0.03312        0.758        0.888
 49.3    110       1    0.813 0.03365        0.750        0.882
 49.6    109       1    0.806 0.03416        0.742        0.876
 49.6    108       1    0.799 0.03465        0.733        0.869
 49.9    107       1    0.791 0.03512        0.725        0.863
 51.0    105       1    0.784 0.03559        0.717        0.856
 51.1    104       1    0.776 0.03603        0.708        0.850
 51.3    103       1    0.768 0.03646        0.700        0.843
 51.9    102       1    0.761 0.03687        0.692        0.837
 52.4    101       1    0.753 0.03727        0.684        0.830
 53.5    100       1    0.746 0.03765        0.676        0.823
 53.9     99       1    0.738 0.03802        0.667        0.817
 55.5     97       1    0.731 0.03838        0.659        0.810
 56.4     93       1    0.723 0.03876        0.651        0.803
 56.4     92       1    0.715 0.03913        0.642        0.796
 57.2     90       1    0.707 0.03949        0.634        0.789
 57.4     88       1    0.699 0.03985        0.625        0.782
 58.1     84       1    0.691 0.04024        0.616        0.774
 58.9     83       1    0.682 0.04061        0.607        0.767
 60.2     80       1    0.674 0.04098        0.598        0.759
 61.6     74       1    0.665 0.04143        0.588        0.751
 62.3     71       1    0.655 0.04189        0.578        0.743
 62.4     68       1    0.646 0.04237        0.568        0.734
 64.0     65       1    0.636 0.04287        0.557        0.726
 65.0     63       1    0.626 0.04336        0.546        0.717
 65.8     59       1    0.615 0.04390        0.535        0.707
 66.4     57       1    0.604 0.04444        0.523        0.698
 74.4     31       1    0.585 0.04708        0.499        0.685
 75.9     25       1    0.561 0.05068        0.470        0.670
 78.1     20       1    0.533 0.05538        0.435        0.654
 79.0     17       1    0.502 0.06036        0.397        0.635

                HyperfiltrationMedian_1_ja=1 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
 19.2    163       1    0.994 0.00612        0.982        1.000
 26.8    162       1    0.988 0.00862        0.971        1.000
 31.8    161       1    0.982 0.01053        0.961        1.000
 33.7    160       1    0.975 0.01212        0.952        1.000
 35.3    159       1    0.969 0.01351        0.943        0.996
 39.7    158       1    0.963 0.01475        0.935        0.993
 42.4    157       1    0.957 0.01588        0.926        0.989
 44.9    156       1    0.951 0.01692        0.918        0.985
 45.5    155       1    0.945 0.01789        0.910        0.981
 46.4    154       1    0.939 0.01880        0.903        0.976
 49.5    151       1    0.932 0.01967        0.895        0.972
 49.8    150       1    0.926 0.02050        0.887        0.967
 52.4    148       1    0.920 0.02130        0.879        0.963
 54.8    141       1    0.913 0.02212        0.871        0.958
 55.7    134       1    0.907 0.02298        0.863        0.953
 57.0    128       1    0.900 0.02387        0.854        0.948
 59.5    115       1    0.892 0.02491        0.844        0.942
 60.3    109       1    0.884 0.02599        0.834        0.936
 60.7    107       1    0.875 0.02703        0.824        0.930
 62.0    101       1    0.867 0.02812        0.813        0.924
 69.5     60       1    0.852 0.03114        0.793        0.915
 70.3     57       1    0.837 0.03399        0.773        0.907
 74.6     36       1    0.814 0.04022        0.739        0.897
 76.9     29       1    0.786 0.04763        0.698        0.885
#further indicating km_fit is being created correctly
followed by:
3)[1] "kmfitdone" #a manual print indicating that the code reached this point-from earlier debgging step
[1] "🚀 DEBUG: Plotting Kaplan-Meier" 
[1] "❌ ggplot konnte nicht erstellt werden." #km plot is null
[1] "📊 Kaplan-Meier Summary:"
Call: survfit(formula = km_formula, data = da_daten)

this is followed by another summary of km_fit indicating that km_fit still exist and wasnt overwritten
so if im not mistaken the problem must lie in ggsurvplot()
additionally i get the warning:❌ Kaplan-Meier plot failed: Objekt des Typs 'symbol' ist nicht indizierbar
which leads me to think km_fit might not be inputted correctly into ggsurvplot, however the function explanation explicitly lists surv()objects as possible inputs
