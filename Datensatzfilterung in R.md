# Datensatzfilterung in R

## Prompt 1

ich programmiere gerade eine statisitk in R und bei der erstellung folgenden subsets:subset_GFml_minusoutliers <- subset(da_daten, GF__ml_>49.24, GF__ml_<207.70)
  subset_da_daten_minusoutliers_gf_alter <- subset(subset_GFml_minusoutliers, Alter_beitotwenntot_a_gerunded>38.18, Alter_beitotwenntot_a_gerunded<90.28)
bekomme ich einen datensatz "subset_da_daten_minusoutliers_gf_alter" mit 0 Variablen, obwohl es im datensatz "subsets:subset_GFml_minusoutliers" genügend observations mit Alter_beitotwenntot_a_gerunded zwischen 38 und 90 gibt

## Prompt 2

danke, ich mache auch eine logistische regression mit folgendem command beim Nullmodell(model0): "model0_endpoint <- glm(endpoint_reached_ja_1~1, da_daten, family=binomial())" dabei bekomme ich als Null deviance:408.22, beim alternativmodell (model1) verwende ich: "model1_endpoint<- glm(endpoint_reached_ja_1~ Hyperfiltration_ja_1 + RRsyst + RRdiast + HbA1c...49 + HbA1c...50 + HbA1c...51 + Alter_beitotwenntot_a, da_daten, family=binomial())" und bekomme als Null deviance: 65.930. Ich dachte die Noull edviance im Alternativmodell sollte die gleiche sein, da ja auf ihrer basis die verbesserung der vorhersage bestimmt wird

## Prompt 3

danke, punkt 2 war korrekt, gibt es eine alternative zum glm command um auch nicht vollständige beobachtungen zu nutzen?

## Prompt 4

ist es möglich folgenden code "#4.4.1kaplan Maier Plot 


#4.4.2///Overall Überleben///
surv_tot_withoutoutliers_data <- Surv(subset_da_daten_minusoutliers_gf_alter$Alter_beitotwenntot_a, subset_da_daten_minusoutliers_gf_alter$tot_1)

#4.4.2.1Hyperfiltration140
kaplan_tot_140_withoutoutliers_data  <- survfit(surv_tot_withoutoutliers_data ~ Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_tot_140_withoutoutliers_data , xlab = "Überlebenszeit in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","orange"))
legend("bottomleft", title="Hyperfiltration140", c("nein","ja"), fill = c("green4", "orange"))

  #logrank Test
survdiff(surv_tot_withoutoutliers_data ~Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_tot_140_withoutoutliers_data  <- coxph(surv_tot_withoutoutliers_data ~Hyperfiltration140_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_tot_140_withoutoutliers_data 

#4.4.2.2Hyperfiltration130
kaplan_tot_130_withoutoutliers_data  <- survfit(surv_tot_withoutoutliers_data ~ Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_tot_130_withoutoutliers_data , xlab = "Überlebenszeit in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","coral4"))
legend("bottomleft", title="Hyperfiltration130", c("nein","ja"), fill = c("green4", "coral4"))

  #logrank Test
survdiff(surv_tot_withoutoutliers_data ~Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_tot_130_withoutoutliers_data  <- coxph(surv_tot_withoutoutliers_data ~Hyperfiltration130_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_tot_130_withoutoutliers_data 

#4.4.2.3Hyperfiltration150
kaplan_tot_150_withoutoutliers_data  <- survfit(surv_tot_withoutoutliers_data ~ Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_tot_150_withoutoutliers_data , xlab = "Überlebenszeit in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","darkorchid4"))
legend("bottomleft", title="Hyperfiltration150", c("nein","ja"), fill = c("green4", "darkorchid4"))

  #logrank Test
survdiff(surv_tot_withoutoutliers_data ~Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)
  
  #Cox Regression
Cox_tot_150_withoutoutliers_data  <- coxph(surv_tot_withoutoutliers_data ~Hyperfiltration150_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_tot_150_withoutoutliers_data 

#4.4.2.4HyperfiltrationMedian
kaplan_tot_median_withoutoutliers_data  <- survfit(surv_tot_withoutoutliers_data ~ HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_tot_median_withoutoutliers_data , xlab = "Überlebenszeit in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","deepskyblue1"))
legend("bottomleft", title="HyperfiltrationMedian", c("nein","ja"), fill = c("green4", "deepskyblue1"))

  #logrank Test
survdiff(surv_tot_withoutoutliers_data ~HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_tot_median_withoutoutliers_data  <- coxph(surv_tot_withoutoutliers_data ~HyperfiltrationMedian_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_tot_median_withoutoutliers_data 


#Median in subset_da_daten_minusoutliers_gf_alter= 132, Median ohne ausreißer in da_daten ist 132-> daher ist die Hyperfiltrationsaufteilung gleich und die analyse wäre identisch

#4.4.3 ///NierenÜberleben///
surv_niere_withoutoutliers_data  <- Surv(subset_da_daten_minusoutliers_gf_alter$NE_surv_all_a_tim, subset_da_daten_minusoutliers_gf_alter$NE_1_ja)

#4.4.3.1Hyperfiltration140
kaplan_niere_140_withoutoutliers_data  <- survfit(surv_niere_withoutoutliers_data ~ Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_niere_140_withoutoutliers_data , xlab = "Nierenüberleben in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","orange"))
legend("bottomleft", title="Hyperfiltration140", c("nein","ja"), fill = c("green4", "orange"))

  #logrank Test
survdiff(surv_niere_withoutoutliers_data ~Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_niere_140_withoutoutliers_data  <- coxph(surv_niere_withoutoutliers_data ~Hyperfiltration140_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_niere_140_withoutoutliers_data 

#4.4.3.2Hyperfiltration130
kaplan_niere_130_withoutoutliers_data  <- survfit(surv_niere_withoutoutliers_data ~ Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_niere_130_withoutoutliers_data , xlab = "Nierenüberleben in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","coral4"))
legend("bottomleft", title="Hyperfiltration130", c("nein","ja"), fill = c("green4", "coral4"))

  #logrank Test
survdiff(surv_niere_withoutoutliers_data ~Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_niere_130_withoutoutliers_data  <- coxph(surv_niere_withoutoutliers_data ~Hyperfiltration130_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_niere_130_withoutoutliers_data 

#4.4.3.3Hyperfiltration150
kaplan_niere_150_withoutoutliers_data  <- survfit(surv_niere_withoutoutliers_data ~ Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_niere_150_withoutoutliers_data , xlab = "Nierenüberleben in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","darkorchid4"))
legend("bottomleft", title="Hyperfiltration150", c("nein","ja"), fill = c("green4", "darkorchid4"))

  #logrank Test
survdiff(surv_niere_withoutoutliers_data ~Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_niere_150_withoutoutliers_data  <- coxph(surv_niere_withoutoutliers_data ~Hyperfiltration150_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_niere_150_withoutoutliers_data 

#4.4.3.4HyperfiltrationMedian
kaplan_niere_median_withoutoutliers_data <- survfit(surv_niere_withoutoutliers_data ~ HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_niere_median_withoutoutliers_data , xlab = "Nierenüberleben in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","deepskyblue1"))
legend("bottomleft", title="HyperfiltrationMedian", c("nein","ja"), fill = c("green4", "deepskyblue1"))

  #logrank Test
survdiff(surv_niere_withoutoutliers_data ~HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_niere_median_withoutoutliers_data  <- coxph(surv_niere_withoutoutliers_data ~HyperfiltrationMedian_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_niere_median_withoutoutliers_data 

#Median in subset_da_daten_minusoutliers_gf_alter= 132, Median ohne ausreißer in da_daten ist 132-> daher ist die Hyperfiltrationsaufteilung gleich und die analyse wäre identisch


#4.4.4 ///Edpunkt_Überleben///
surv_endpoint_withoutoutliers_data  <- Surv(subset_da_daten_minusoutliers_gf_alter$time_to_endpoint_a_tim, subset_da_daten_minusoutliers_gf_alter$endpoint_reached_ja_1)

#4.4.4.1Hyperfiltration140
kaplan_endpoint_140_withoutoutliers_data  <- survfit(surv_endpoint_withoutoutliers_data ~ Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_endpoint_140, xlab = "Zeit bis Tot oder NET in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","orange"))
legend("bottomleft", title="Hyperfiltration140", c("nein","ja"), fill = c("green4", "orange"))

  #logrank Test
survdiff(surv_endpoint_withoutoutliers_data ~Hyperfiltration140_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_endpoint_140_withoutoutliers_data  <- coxph(surv_endpoint_withoutoutliers_data ~Hyperfiltration140_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_endpoint_140_withoutoutliers_data 

#4.4.4.2Hyperfiltration130
kaplan_endpoint_130_withoutoutliers_data  <- survfit(surv_endpoint_withoutoutliers_data ~ Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_endpoint_130_withoutoutliers_data , xlab = "Zeit bis Tot oder NET in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","coral4"))
legend("bottomleft", title="Hyperfiltration130", c("nein","ja"), fill = c("green4", "coral4"))

  #logrank Test
survdiff(surv_endpoint_withoutoutliers_data ~Hyperfiltration130_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_endpoint_130_withoutoutliers_data  <- coxph(surv_endpoint_withoutoutliers_data ~Hyperfiltration130_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_endpoint_130_withoutoutliers_data 

#4.4.4.3Hyperfiltration150
kaplan_endpoint_150_withoutoutliers_data  <- survfit(surv_endpoint_withoutoutliers_data ~ Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)


  #Plot
plot(kaplan_endpoint_150_withoutoutliers_data , xlab = "Zeit bis Tot oder NET in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","darkorchid4"))
legend("bottomleft", title="Hyperfiltration150", c("nein","ja"), fill = c("green4", "darkorchid4"))

  #logrank Test
survdiff(surv_endpoint_withoutoutliers_data ~Hyperfiltration150_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_endpoint_150_withoutoutliers_data  <- coxph(surv_endpoint_withoutoutliers_data ~Hyperfiltration150_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_endpoint_150_withoutoutliers_data 

#4.4.4.4HyperfiltrationMedian
kaplan_endpoint_median_withoutoutliers_data  <- survfit(surv_endpoint_withoutoutliers_data ~ HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)


   #Plot
plot(kaplan_endpoint_median_withoutoutliers_data , xlab = "Zeit bis Tot oder NET in Jahren", ylab="Prozent Überleben", yscale=100, col=c("green4","deepskyblue1"))
legend("bottomleft", title="HyperfiltrationMedian", c("nein","ja"), fill = c("green4", "deepskyblue1"))

  #logrank Test
survdiff(surv_endpoint_withoutoutliers_data ~HyperfiltrationMedian_1_ja, subset_da_daten_minusoutliers_gf_alter)

  #Cox Regression
Cox_endpoint_median_withoutoutliers_data  <- coxph(surv_endpoint_withoutoutliers_data ~HyperfiltrationMedian_1_ja + RRsyst + RRdiast + NE_1_ja + highest_measured_hba1c_naisna, subset_da_daten_minusoutliers_gf_alter)
Cox_endpoint_median_withoutoutliers_data " durch eine loop kürzer zu schreiben und das gleiche ergebnis zu bekommen?
