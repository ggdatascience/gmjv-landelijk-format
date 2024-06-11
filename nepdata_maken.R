library(haven)
library(labelled)
library(dplyr)

file_path = "" #een .sav bestand

data = haven::read_spss(file_path,user_na =T) %>%
  labelled::user_na_to_na()

#Wat achtegrond & inhoudelijke variabelen selecteren
data <- data %>% select(Geslacht_standaardisatie, AGLFA401, Gemeentecode, GGDregio, AGOWS403, LFBWA403, OJWHB407)

names(data) <- c("geslacht","leeftijd_3cat","gemeentecode","ggdregio","opleiding_3cat","sport","weerbaarheid")

#Simpele variabelnamen namen
var_label(data$geslacht) <- "Geslacht"
var_label(data$leeftijd_3cat) <- "Leeftijd"
var_label(data$gemeentecode) <- "Gemeente"
var_label(data$ggdregio) <- "GGD-Regio"
var_label(data$opleiding_3cat) <- "Hoogste opleiding (huidig of afgerond)"


#alle variabelen overhoop gooien
randomiseer_variabelen <- function(data, var){

  #Var & val labels opslaan
  huidig_var_label <- var_label(data[[var]])
  huidig_value_labels <- val_labels(data[[var]])
  
  nieuwe_waarden <- sample(unname(value_labels),nrow(data),replace = T)
  
  data[[var]] <- nieuwe_waarden

  #Var & val labels weer terugzetten
  var_label(data[[var]]) <- huidig_var_label
  val_labels(data[[var]]) <- huidig_value_labels
  
  return(data)

}

nieuwer_df <- data %>% 
  randomiseer_variabelen("geslacht") %>% 
  randomiseer_variabelen("leeftijd_3cat") %>% 
  randomiseer_variabelen("gemeentecode") %>% 
  randomiseer_variabelen("ggdregio") %>% 
  randomiseer_variabelen("opleiding_3cat") %>% 
  randomiseer_variabelen("sport") %>% 
  randomiseer_variabelen("weerbaarheid")


haven::write_sav(nieuwer_df, "fictieve_data.sav")
