# R Script Fehlerbehebung

## Prompt 1

list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
)
list_var <- list(
  "Hyperfiltration140_1_ja",
  "Hyperfiltration140_1_ja",
  "Hyperfiltration140_1_ja",
  "HyperfiltrationMedian"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "Alter_beitotwenntot_a",
  "NE_surv_all_a_tim",
  "time_to_endpoint_a_tim"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_1_ja"
)
list_confunder <- list(
  " RRsyst",
  "RRdiast",  
  "NE_1_ja",
  "highest_measured_hba1c_naisna"
)
  
kaplan_maier_logrank_coxreg_func <- function(list_daten, list_surv, list_var, confunder_list){
  list_kaplan_graph <- list()
  list_coxreg <- list()
  list_coxgraph <- list()
  list_logrank <- list()
  list_forest <- list()
  for (daten in list_daten) {
    daten_name <- names(daten)
    for (i in seq_along(list_surv_time)) {
      if (!list_surv_time[[i]] %in% names(daten)) {
        print(paste("Warning", list_surv_time[i], "not found in", daten))
        next
      }
      if (!list_endpoint[[i]] %in% names(daten)) {
          print(paste("Warning", list_endpoint[i], "not found in", daten))
          next
      }
      endpoint_name <- list_endpoint[[i]]
      surv_var <- Surv(daten[list_surv_time[i]], daten[list_endpoint[i]])
      
    
      
        for (var in list_var) {
          if (!var %in% names(daten)) {
            print(paste("Warning", var, "not found in", daten))
            next
          }
          if (!var %in% names(daten)) {
            print(paste("Warning", var, "not found in", daten))
            next
          }
          fit <- survfit(surv_var~var, daten)
          graph <- ggsurvplot(fit,+
                                censor.shape == "|", censor.size=4, +
                                conf.int == TRUE, +
                                pval == TRUE, +
                                xlab == "Time in years", +
                                break.time.by == 5, +
                                surv.median.line == "hv", +
                                ncensor.polt == TRUE,+
                                legend.labs == c("Hyperfiltration","keine Hyperfiltration"),+
                                ggtheme == theme_bw()
                                )
          
          logrank <- survdiff(surv_var~var, daten)
          cox_formula_part1 <- paste(confunder_list, collapse = " + ")
          cox_formula <- paste(surv_var, "~",var,"+", cox_formula_part1)
          cox <- coxph(as.formula(cox_formula), datensatz)
          cox.zph.cox <- cox.zph(cox)
          cox_graph <- ggcoxzph(cox.zph.cox, resid=TRUE, se=TRUE, df=4, nsmo=40, point.col="red", caption="Cox-Graph", ggtheme=theme_survminer())
          forest<- ggforest(cox, main="Hazard ratio", cpositions = c(0.02, 0.22, 0.4), refLabel= "reference", noDigits=2)
          
          list_kaplan_graph[[daten_name]][[endpoint_name]][[var]] <- graph
          list_coxreg[[daten_name]][[endpoint_name]][[var]] <- cox
          list_coxgraph[[daten_name]][[endpoint_name]][[var]] <- coxgraph
          list_logrank[[daten_name]][[endpoint_name]][[var]] <- fit
          list_forest[[daten_name]][[endpoint_name]][[var]] <- forest
          
        }
    }
  }
  return(list(kaplan = list_kaplan_graph, coxreg = list_coxreg, coxgraph = list_coxgraph, logrank = list_logrank, forest = list_forest))
}


# Example usage
cox_fun_results <- kaplan_maier_logrank_coxreg_func(list_daten, list_surv, list_var, confunder_list)

# **Access results** syntax=print(cox_fun_results$wanted result["daten"]][["endpoint"]][["variable"]])
#wanted endpoint options are: kaplan, coxreg, coxgraph, logrank, forest
print(cox_fun_results$coxreg[["da_daten"]][["tot_1"]][["Hyperfiltration140_1_ja"]])   # Chi-square test for Hyperfiltration140_1_ja

#alle resultate für einen parameter abrufen (in diesem fall Hyperfiltration140_1_ja, aus allen datensätzen und allen endpunkten)
list_results <- list(
  "kaplan",
  "coxreg",
  "coxgraph",
  "logrank",
  "forest"
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

for (daten in list_daten) {
  name_daten <- list_daten[[daten]]
  for (endpoint in list_endpoint) {
    if (endpoint %in% names(daten)) {
      print(paste("Warning:",endpoint,"not found in", daten))
      next
      name_endpoint <- list_endpoint[[endpoint]]
    }
    for (var in list_var){
      if (var %in% names(daten)){
        print(paste("Warning", var, "not found in", daten))
        next
        }
      name_var <- list_var[[var]]
      for (result in list_results) {
      print(cox_fun_results[result][[name_daten]][[name_endpoint]][[name_var]])
      }
    }
  }
}

## Prompt 2

rror in `daten[[list_surv_time[i]]]`:
! Can't extract column with `list_surv_time[i]`.
✖ `list_surv_time[i]` must be numeric or character, not a list.

## Prompt 3

is there mistake here:
#3.3.1)kaplan Maier Plot 
list_daten <- list(
  da_daten = da_daten,
  subset_minusCKD = subset_minusCKD 
)
list_var <- list(
  "Hyperfiltration140_1_ja",
  "Hyperfiltration140_1_ja",
  "Hyperfiltration140_1_ja",
  "HyperfiltrationMedian"
)
#list surv_time und list_endpoint müssen korrespondierende Endpunkte und Zeiten an der gleichen Stelle in der Liste haben
list_surv_time <- list(
  "Alter_beitotwenntot_a",
  "NE_surv_all_a_tim",
  "time_to_endpoint_a_tim"
)

list_endpoint <- list(
  "tot_1",
  "NE_1_ja",
  "endpoint_reached_1_ja"
)
list_confunder <- list(
  " RRsyst",
  "RRdiast",  
  "NE_1_ja",
  "highest_measured_hba1c_naisna"
)

## Prompt 4

Fehler in paste("surv_var ~", var) : 
  cannot coerce type 'closure' to vector of type 'character'

## Prompt 5

Fehler in paste("surv_var ~", var) : 
  cannot coerce type 'closure' to vector of type 'character'

## Prompt 6

Fehler in as.data.frame.default(x[[i]], optional = TRUE) : 
  kann Klasse ‘"function"’ nicht in data.frame umwandeln

## Prompt 7

Fehler in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
  Argumente implizieren unterschiedliche Anzahl Zeilen: 324, 3, 1, 0

## Prompt 8

table(length(daten[[list_surv_time[[i]]]]), length(daten[[list_endpoint[[i]]]]))
Fehler in list_surv_time[[i]] : Indizierung außerhalb der Grenzen
>

## Prompt 9

cox_fun_results <- kaplan_maier_logrank_coxreg_func(list_daten, list_surv_time, list_endpoint, list_var, list_confunder)
[1] "Warning: Hyperfiltration140_1_ja not found in da_daten"
[1] "Warning: Hyperfiltration130_1_ja not found in da_daten"
[1] "Warning: Hyperfiltration150_1_ja not found in da_daten"
[1] "Warning: HyperfiltrationMedian not found in da_daten"
[1] "Warning: NE_surv_all_a_tim not found in da_daten"
[1] "Warning: time_to_endpoint_a_tim not found in da_daten"
[1] "Warning: Hyperfiltration140_1_ja not found in subset_minusCKD"
[1] "Warning: Hyperfiltration130_1_ja not found in subset_minusCKD"
[1] "Warning: Hyperfiltration150_1_ja not found in subset_minusCKD"
[1] "Warning: HyperfiltrationMedian not found in subset_minusCKD"
[1] "Warning: NE_surv_all_a_tim not found in subset_minusCKD"
[1] "Warning: time_to_endpoint_a_tim not found in subset_minusCKD"

## Prompt 10

how do i get da_data as string from da_data as df
