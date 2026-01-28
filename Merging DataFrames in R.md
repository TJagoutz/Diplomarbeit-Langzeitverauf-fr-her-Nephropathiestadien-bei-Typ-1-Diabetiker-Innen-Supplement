# Merging DataFrames in R

## Prompt 1

is there a command in r, that can merge to dataframes together, so that the new dataframe only has the values both of the previous dataframes had.

## Prompt 2

i have a dataset with 60 columns i want to eliminate outliers for various columns (i have a function for that) my function creats an individual subset for each column i want to creat a subset for and now i want to merge some of those subsets so i get a dataset without outliers for all those variables

## Prompt 3

merge_subset_func <- function(subset_list) {
  df_name <- "merged_subset"
  new_subset <- subset_list[1]
  for (subset in subset_list) {
    new_subset <- merge(new_subset,subset, by = c("untersucht"))
    df_name <- paste(df_name, "_", subset, sep = "")
  }
  assign(df_name, new_subset)
  print(df_name)
}
subset_list <- list(
  "subset_da_daten_GF__ml_",
  "subset_da_datan_Alter_beitotwenntot_a"
)
merge_subset_func(subset_list)

## Prompt 4

merge_subset_func <- function(subset_list) {
  if (length(subset_list <2)){
    stop("need at least 2 subsets to merge")
  }
  new_subset <- get(subset_list[1])
  for (i in 2:length(subset_list)){
    subset <- get(subset_list[i])
    new_subset <- merge(new_subset,subset, by = "untersucht", NA = TRUE )
    df_name <- paste("merged_subset", "_", subset_list[1], "_", subset_list[i], sep = "")
  }
  assign(df_name, new_subset, envir = .GlobalEnv)
  print(df_name)
}
subset_list <- list(
  "subset_da_daten_GF__ml_",
  "subset_da_datan_Alter_beitotwenntot_a"
)
merge_subset_func(subset_list)
output:
Fehler in merge_subset_func(subset_list) : 
  need at least 2 subsets to merge
Zusätzlich: Warnmeldungen:
1: In merge_subset_func(subset_list) : NAs durch Umwandlung erzeugt
2: In merge_subset_func(subset_list) : NAs durch Umwandlung erzeugt

## Prompt 5

Fehler in get(subset_list[1]) : ungültiges erstes Argument

## Prompt 6

i got a new dataframe now. but ist has over 90000 observations however i expected it to become smaller as i was eliminating ore cases

## Prompt 7

can i create a new "ID" column in my dataframe assign unique values to each case?

## Prompt 8

Fehler in fix.by(by.x, x) : 'by' muss gültige Spalte spezifizieren
da_daten$ID <- 1:nrow(da_daten)
new_subset <- get(subset_list[[1]])
  for (i in 2:length(subset_list)){
    subset <- get(subset_list[[i]])
    new_subset <- merge(new_subset,subset, by = "ID")
    df_name <- paste(df_name, subset_list[[i]], sep = "_")

## Prompt 9

all data frames have id columns, however as they are subsets with eliminated cases (outliers) naturally the ID of the 5th row in one dataset is not the same as the other (but that is intended isnt it)

## Prompt 10

Why is merge(by = "ID") a problem?
If each subset removes different cases, "ID" values will no longer refer to the same individuals across datasets.
Example:
makefile
Kopieren
Bearbeiten
subset1:
ID   Name  Age
1    Alice  25
3    Charlie 35

subset2:
ID   Name  Score
1    Alice   90
2    Bob     85
merge(subset1, subset2, by = "ID") → Only keeps "Alice", because "ID = 3" doesn’t exist in subset2, and "ID = 2" doesn’t exist in subset1.

but isnt that exactly what i want, as my goal is to eliminate oultiers in different categories and have a data frame without any outliers in the end?

## Prompt 11

datensatz <- da_daten #originaler datensatz#
df_original_name <- "da_daten" #scheint im namen des subsets auf 
spalten_subset <- list(
  "Alter_beitotwenntot_a",
  "GF__ml_"
) #für jede spalte wird ein eigenes subset erstellt
subset_outliers(datensatz,spalten_subset)

i use this code to merge to subsets with eliminated outliers in one variable each, the dataframe created seems to reduce the cases correctly however it doubles the variables 
merge_subset_func <- function(subset_list) {
  if (length(subset_list)<2){
    stop("need at least 2 subsets to merge")
  }
  df_name <- paste("merged_subset", subset_list[[1]], sep = "_")
  
  new_subset <- get(subset_list[[1]])
  for (i in 2:length(subset_list)){
    subset <- get(subset_list[[i]])
    new_subset <- merge(new_subset,subset, by = "ID")
    df_name <- paste(df_name, subset_list[[i]], sep = "_")
  }
  assign(df_name, new_subset, envir = .GlobalEnv)
  print(paste("Merged dataset stored as:", df_name))
}
subset_list <- list(
  "subset_minusoutliers_da_daten_Alter_beitotwenntot_a",
  "subset_minusoutliers_da_daten_GF__ml_"
)
merge_subset_func(subset_list)

## Prompt 12

but why would i have the described duplicates in my data if both subsets emerge from the same original data frame?

## Prompt 13

Merging in R without duplicating columns with same names between datasets? e.g. without age.x and age.y

## Prompt 14

for (i in list_var){
  histo_name <- paste0("histo_",list_var[i])
  col1 <- col_list[i]
  graph <- ggplot(datensatz, aes(x=i))+
    geom_histogram(aes(y=..density..), fill=col1, color="grey", binwidth = 1, main=i)+
    geom_density(fill="red", alpha=0.2)
  assign(histo_name, graph)
  print(graph)
}

## Prompt 15

: In geom_histogram(aes(y = ..density..), fill = col1, color = "grey",  :
  All aesthetics have length 1, but the data has 324 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
2: In geom_density(fill = "red", alpha = 0.2) :
  All aesthetics have length 1, but the data has 324 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
3: Computation failed in `stat_bin()`.
Caused by error in `scales[[x]]$dimension()`:
! Versuch eine Nicht-Funktion anzuwenden 
4: Computation failed in `stat_density()`.
Caused by error in `scales[[flipped_names(flipped_aes)$x]]$dimension()`:
! Versuch eine Nicht-Funktion anzuwenden 
>

## Prompt 16

histogram_with_density <- function(list_var, col_list){
for (i in seq_along(list_var)){
  myfunc_spal_exist(list_var, datensatz)
  histo_name <- paste0("histo_",list_var[i])
  col1 <- col_list[i]
  graph <- ggplot(datensatz, aes(x=list_var[i]))+
    geom_histogram(aes(y=..density..), fill=col1, color="grey", binwidth = 1)+
    geom_density(fill="red", alpha=0.2)
  assign(histo_name, graph)
  Sys.sleep(0.5)
  }
}#printing inside loop doesnt work
histogram_with_density(list_var, col_list)

print(histo_Manifestationsalter_a_rounded)

Warnmeldungen:
1: In geom_histogram(aes(y = ..density..), fill = col1, color = "grey",  :
  All aesthetics have length 1, but the data has 324 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
2: In geom_density(fill = "red", alpha = 0.2) :
  All aesthetics have length 1, but the data has 324 rows.
ℹ Please consider using `annotate()` or provide this layer with data containing a single row.
3: Computation failed in `stat_bin()`.
Caused by error in `scales[[x]]$dimension()`:
! Versuch eine Nicht-Funktion anzuwenden 
4: Computation failed in `stat_density()`.
Caused by error in `scales[[flipped_names(flipped_aes)$x]]$dimension()`:
! Versuch eine Nicht-Funktion anzuwenden

## Prompt 17

can i instead of using .GlobalEnv use a return command?

## Prompt 18

histogram_with_density <- function(list_var, col_list, datensatz) {
  histograms <- list()  # Initialize empty list to store plots
  
  for (i in seq_along(list_var)) {
    myfunc_spal_exist(list_var[i], datensatz)  # Check if variable exists
    
    var_name <- list_var[i]  # Extract variable name
    col1 <- col_list[i]  # Get color
    
    # Create the histogram
    graph <- ggplot(datensatz, aes_string(x = var_name)) +
      geom_histogram(aes(y = ..density..), fill = col1, color = "grey", binwidth = 1) +
      geom_density(fill = "red", alpha = 0.2) +
      ggtitle(var_name) +
      theme_minimal()
    
    histograms[[var_name]] <- graph  # Store the graph in the list with variable name as key
    
    print(graph)  # Display the plot
    Sys.sleep(0.5)  # Optional pause
  }
  
  return(histograms)  # Return all histograms as a list
}

## Prompt 19

histograms <- histogram_with_density(list_var, col_list, datensatz)
Fehler in histograms[[var_name]] <- graph : ungültiger Indextyp 'list'

## Prompt 20

can i get the  y axis to depict the absolute value(e.g. 22 for bmi) instead of a density value

## Prompt 21

boxplot_func <- function(list_var, col_list, datensatz) {
  boxplots <- list()  # Initialize empty list to store plots

for (i in seq_along(list_var)) {
  myfunc_spal_exist(list_var[i], datensatz)  # Check if variable exists
  
  var_name <- list_var[[i]]  # Extract variable name
  col1 <- col_list[i]  # Get color
  
    graph <- ggplot(datensatz,aes(y=list_var[i]))+
    geom_boxplot(fill=col1,)
    
    boxplots[[var_name]] <- graph  # Store the graph in the list with variable name as key
    
    print(graph)  # Display the plot
    Sys.sleep(0.5)  # Optional pause
}
  
  return(boxplots)  # Return all histograms as a list
}

# Run the function and store the results
boxplots <- boxplot_func(list_var, col_list, datensatz)

## Prompt 22

list_var <- list(
  "highest_measured_hba1c_naisna",
  "BMI",
  "GF__ml_",
  "Alter_beitotwenntot_a_rounded",
  "Manifestationsalter_a_rounded",
  "Diabetesdauer_totwenntot_a_tim_rounded",
  "NE_surv_a_tim_rounded"
)
col_list <- list(
  "steelblue",
  "darkgreen",
  "cyan",
  "black",
  "yellow",
  "beige"
)
thats my variable and my colour lists for above given histogram and boxplot functions
all variables are metric variables but graphs are printet only until alter_beitotwenntot_rounded

## Prompt 23

this is the code of myfunc_spal_exist, is that the problem?
myfunc_spal_exist <- function(spalten, da_daten){
  if (spalte %in% names(da_daten)){
    
  } else {
    warning(paste("Column", spalte, "not found in the data frame."))
  }
}

## Prompt 24

Error in `geom_boxplot()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
! Objekt 'Manifestationsalter_a_rounded' nicht gefunden
Run `rlang::last_trace()` to see where the error occurred.
Warnmeldungen:
1: Removed 33 rows containing non-finite outside the scale range (`stat_boxplot()`). 
2: Removed 9 rows containing non-finite outside the scale range (`stat_boxplot()`). 
3: Removed 2 rows containing non-finite outside the scale range (`stat_boxplot()`). 
4: In myfunc_spal_exist(list_var[i], datensatz) :
  Column Manifestationsalter_a_rounded not found in the data frame. Skipping...
>

## Prompt 25

datensatz <- da_daten
list_var <- list(
  "highest_measured_hba1c_naisna",
  "BMI",
  "GF__ml_",
  "Alter_beitotwenntot_a_rounded",
  "Manifestationsalter_a_",
  "Diabetesdauer_totwenntot_a_tim_rounded",
  "NE_surv_a_tim_rounded"
)
col_list <- list(
  "steelblue",
  "darkgreen",
  "cyan",
  "black",
  "yellow",
  "beige",
  "orange"
)
histogram_with_normaldis <- function(datensatz, list_var, col_list){
histo_mit_normver <- list()

for (i in list_var){
  var_name <- list_var[[i]]
  var <- list_var[i]
  col <- col_list[i]
  var_clean<- na.omit(var)
  var_clean<- var[is.finite(var)]
  df_var<- data.frame(variable=var_clean)
  
  graph<- ggplot(dfvar, aes_string(x = var_name)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = col, color = "grey", alpha = 1.0) +
    stat_function(fun = dnorm, args = list(mean = mean(df_var$variable), sd = sd(df_var$variable)), 
                  color = "red", linewidth = 1) +
    labs(title = paste("histogramm", var_name, "mit Normalverteilung"),
         x = var_name,
         y = "Dichte") +
    theme_minimal() +
    xlim(min(df_var$variable) - 10, max(df_var$variable) + 10)
  
  histo_mit_normver[[var_name]]<-graph
  print(graph)
  Sys.sleep(0.5)
  
  } 
 return(histo_mit_normver)
}
histo_mit_normver <- histogram_with_normaldis(datensatz, list_var, col_list)

## Prompt 26

thanks, just a question for point nr 6, shouldn't all missing values be removed above (step4)

## Prompt 27

thanks it works now,
in my previous approach without a loop i have adhjustied the bins and binwidths for each histogram to improve presentability <

## Prompt 28

i want to use approach 2, inside ggplot i still use binwidth=binwidth?

## Prompt 29

as a last step i would like to have some indication obout what the absolute values of frequency are (until now i only see the density)

## Prompt 30

can i instead get a noemal distribution curve instead of the density curve

## Prompt 31

thanks that seems to work, now i have two more adjustments i want to make,
first in every histogrma there are two values outside the scale range, i want to try to include them in the graph
secondly i want to give n=number of values for each histogram
can i simply define number <- count(var_clean) and then ad in ggplot number to the title?

## Prompt 32

my xlim was       xlim(min(df_var$variable, na.rm = TRUE) - 10, max(df_var$variable, na.rm = TRUE) + 10) #beestimmung einer x achsen range, schließt +2 ausreißer pro graph aus
but shouldnt all values be included in that xlim as it uses max and mimn as bpundaries

## Prompt 33

func_shapiro <- function(list_var, datensatz){
  shapiro_list <- list()
  for(var in list_var){
    result <-   shapiro.test(datensatz[[var]])
    shapiro_list[[var]] <- result
  }
  return(shapiro_list)
}
shapiro_list <- func_shapiro(list_var, datensatz)

print(shapiro_list[[BMI]])

## Prompt 34

func_shapiro <- function(list_var, datensatz){
  
  shapiro_list <- list()
  for(var in list_var){
    
    if (!list_var[[var]] %in% names(datensatz)) {
      warning(paste("Variable", var_name, "not found in dataset. Skipping..."))
      next  # Skip to the next iteration if the variable is missing
    }  
    
    result <-   shapiro.test(datensatz[[var]])
    shapiro_list[[var]] <- result
  }
  return(shapiro_list)
}
shapiro_list <- func_shapiro(list_var, datensatz) #create global list to store all shapiro results

print(shapiro_list[["BMI"]])#print example

## Prompt 35

test_for_normdis <- function(list_var, datensatz){
  
  shapiro_list <- list()
  qqnorm_list <- list ()
  qqline_list <- list ()
  korsakov_list <- list ()
  normdis_list <- list()
  for(var in list_var){
    
    if (!(var %in% names(datensatz))) {
      warning(paste("Variable", var, "not found in dataset. Skipping..."))
      next  # Skip to the next iteration if the variable is missing
    }  
    
    variable <- datensatz[[var]]
    
    result_shap <-   shapiro.test(variable)
    result_qqnorm <- qqnorm(variable, main=var)
    result_qqline <- qqline(variable)
    result_ks <- ks.test(variable, "pnorm")
    
    shapiro_list[[var]] <- result_shap
    qqnorm_list[[var]] <- result_qqnorm
    qqline_list[[var]] <- result_qqline
    korsakov_list[[var]] <- result_ks
    
    print("shapiro")
    print(paste(,var, result_shap,))
    print("Kolmogorov")
    print(paste(var, result_ks))
  }
  normdis_list[[shapiro]] <- shapiro_list
  normdis_list[[qqnorm]] <- qqnorm_list
  normdis_list[[qqline]] <- qqline_list
  normdis_list[[ks]] <- korsakov_list
  
  return(normdis_list)
}
normdis_list <- test_for_normdis(list_var, datensatz) #create global list to store all normaldis_tes results

print(normdis_list[shapiro_list[["BMI"]]])#print example

## Prompt 36

Fehler in ggplot(df_qq, aes(sample = sample)) : 
  konnte Funktion "ggplot" nicht finden
Zusätzlich: Warnmeldung:
In ks.test.default(variable, "pnorm", mean = mean(variable), sd = sd(variable)) :
  ties should not be present for the one-sample Kolmogorov-Smirnov test

## Prompt 37

can i include a r value for correlation in the qqplot?

## Prompt 38

thanks it is possible (and does it make sense?) to include a pearson quotient(Rvalue?) in the plot ?

## Prompt 39

datensatz <- da_daten
subset_minusCKD <- subset(datensatz, datensatz[GF__ml_]>59.9)
subset_list <- append(subset_minusCKD)
rm(subset_minusCKD)

## Prompt 40

i already have a subset list with two subsets and  i dont want to overwrite it but merely add the newest subset

## Prompt 41

chisquare_test_fun <- function(list_daten, list_var) {
  
  list_chisquare_results <- list()
  list_xtabs <- list()
  for (datensatz in list_daten) {
    list_chisquare_results[[datensatz]] <- list()
    list_xtabs[[datensatz]] <- list()
    for (var in list_var) {
      table <- xtabs(~datenstz[var]+datensatz[endpoint_reached_ja_1])
      chisq_res <- chisq.test(datensatz[var], datensatz[endpoint_reached_ja_1])
      list_chisquare_results[[datensatz[[var]]]]<- chisq_res
      list_xtabs[[datensatz[[var]]]] <- table
    }
  }
  return(list_xtabs, list_chisquare_results, .GlobalEnv)
}

## Prompt 42

do i still need to introduce list_xtabs in the beginning

## Prompt 43

does dataset_name <- paste0("Dataset_", i) always return Dataset_1, Dataset_2, or will it return Dataset_name of variable

## Prompt 44

> chisq_results <- chisquare_test_fun(subset_list, list_var)
Fehler in list_chisquare_results[[dataset_name]] <- list() : 
  Versuch weniger als ein Element aus OneIndex zu wählen

## Prompt 45

datensatz <- da_daten
subset_minusCKD <- subset(datensatz, GF__ml_>59.9)
subset_list <- append(subset_list,list(subset_minusCKD))
rm(subset_minusCKD)

list_daten <- list( "da_daten",
                    "subset_minusCKD"
  
)

list_var <- list("Hyperfiltration140_1_ja",
                 "Hyperfiltration130_1_ja",
                 "Hyperfiltration150_1_ja",
                 "HyperfiltrationMedian_1_ja",
                 "HyperfiltrationMedianohneausreißer_1_ja"
  
)

chisquare_test_fun <- function(list_daten, list_var) {
  
  # Initialize lists to store results
  list_chisquare_results <- list()
  list_xtabs <- list()
  
  for (i in seq_along(list_daten)) {
    datensatz <- list_daten[[i]]
    dataset_name <- names(list_daten)[i] # Create a unique name for each dataset
    
    # Initialize nested lists for this dataset
    list_chisquare_results[[dataset_name]] <- list()
    list_xtabs[[dataset_name]] <- list()
    
    for (var in list_var) {
      # Ensure the variable exists in the dataset
      if (!(var %in% names(datensatz))) {
        warning(paste("Variable", var, "not found in", dataset_name, "Skipping..."))
        next
      }
      
      # Create contingency table
      table <- xtabs(~ datensatz[[var]] + datensatz[["endpoint_reached_ja_1"]])
      
      # Perform Chi-Square test
      chisq_res <- chisq.test(datensatz[[var]], datensatz[["endpoint_reached_ja_1"]])
      
      # Store results in lists
      list_chisquare_results[[dataset_name]][[var]] <- chisq_res
      list_xtabs[[dataset_name]][[var]] <- table
    }
  }
  
  return(list(xtabs = list_xtabs, chisq_results = list_chisquare_results))
}

# Example usage
chisq_results <- chisquare_test_fun(subset_list, list_var)

## Prompt 46

subset_minusCKD = subset_list[[length(subset_list)]] # Last added subset
) can i instead of using the last added subset also get it by using its name in the list?

## Prompt 47

if i dont store my subsets insinde a list can i still call it by subset_minusCKD=subset_minusCKD?

## Prompt 48

logreg_fun <- function(datensatz, list_var, confunder_list) {
  for (i in seq_along(list_var)) {
    variable <- list_var[i]
    variable_name <- name(list_var[i])
    #0model
    model0 <- glm(i~1, datensatz, family=binominal())  
    summary0 <- summary(model0)
    #testmodel
    model1 <- glm(i~ confunder_list[1;n])
    summary1 <- summary(model1)
    #omnibusTest
    modelchi <- model1$null.deviance - model1$deviance
    chifd <- model1$df.null - model1$df.residual
    chisqp <- 1-qchisq(modelchi, chidf)
    
    }
  }

## Prompt 49

perfect thanks, can i now use print(logreg_result) with the original names of my variables?

## Prompt 50

i forgot to add the oR calculation in the beginning, before i had a seperate function
 exp_endpoint<- exp(cbind(OR = coefficients(model1_endpoint), confint(model1_endpoint)))

can i simply add that like:
logreg_result[[variable]] <- list(

## Prompt 51

i forgot to add the oR calculation in the beginning, before i had a seperate function
 exp_endpoint<- exp(cbind(OR = coefficients(model1_endpoint), confint(model1_endpoint)))

can i simply add that like:
logreg_result[[variable]] <- list(
model0 = summary0,
model1 = summary1,
omnibus_test = list (
    chi_square = modelchi,
    df = chifd,
    p_value = qhisqp
   )
oR = exp(
    cbind(OR = coefficients(model1), confint(model1)
    )
)
