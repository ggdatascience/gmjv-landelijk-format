# Script met helpfuncties voor GMJV 2024.
# Packages ----------------------------------------------------------------

# TODO alle alt-texten nakijken. Door recente aanpassingen mogelijk niet meer waterdicht
# TODO code opschonen; dataverwerking voor grafiek in eigen functies stoppen om complexiteit / lengte van functies te verkleinen.

# Het script maakt gebruik van een aantal packages
# Deze moeten bij de eerste keer lokaal worden geinstalleerd. 

# benodigde packages installeren als deze afwezig zijn
pkg_nodig = c("gt", "dplyr", "ggplot2", "tidyr", "stringr", "labelled", 
              "survey", "glue", "plotly", "forcats", "openxlsx", "showtext")

for (pkg in pkg_nodig) {
  if (system.file(package = pkg) == "") {
    install.packages(pkg)
  }
}
# Hieronder worden de benodige packages geladen
library(gt)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr) # Voor str_replace
library(labelled) # Package om te werken met datalabels, o.a. voor to_character()
library(survey) # Package om te werken met gewogen gemiddelds incl. betrouwbaarheidsintervallen
library(glue) #om strings aangenaam aan elkaar te plakken
library(plotly)
library(forcats)
library(openxlsx)#om cbs populatiebestand te kunnen lezen
library(showtext)
#library(cowplot) #alleen gebruikt voor spacing legenda staafdiagram vergelijking.
#voorlopig uit omdat het alt-text overhoop gooit. 


# fonts grafieken --------------------------------------------------------
#voor gemak; 1 plek waar we alle tekstopmaak voor ggplot kunnen aanpassen

#alle ggplot grafieken hebben overal dezelfde font-family. 
font_family = "Open Sans"

#Titels
titel_size <- 40

#Algemeen
font_size <- 30

#line_height; ruimte tussen regelsregel
line_height <- .5

#percentages die boven/naast of in balken staan
#LET OP size is hierbij op een een andere schaal. omdat het om een geom_ element gaat.
geom_text_percentage <- 10

#Labels op de X-as (waar die relevant zijn)
as_label_size <- 30

#legend titles
#algemeen:default
legend_title_size_cirkel <- 30

#legend keys
legend_text_size <- 30
legend_text_size_cirkel <- 25

#caption
caption_size <- 20

# utility ----------------------------------------------------------------

##### Survey functies gebaseerd op oud tabellenboekscript ####

#survey_design_maken()
#Functie om survey design te maken. Functioneert niet heel anders standaard survey fucntie svydesign(), geeft bij veelvoorkomende fouten leesbare errorberichten

survey_design_maken <- function(data = NULL, strata = NULL, gewichten = NULL){
  
  #Controleren of variabelen wel bestaan & er een survey design gemaakt kan worden
  #Als strata OF gewicht NULL is ERROR geven
  if(is.null(data[[strata]]) | is.null(data[[gewichten]]) ){
    
    #Warning strata
    if(is.null(data[[strata]])){
      #Geef error voor relevante variabel.
      stop(paste("Fout bij survey design:  strata = ", strata, "bestaat niet in data. controleer instellingen"))
    }
    #Warning gewichten
    if(is.null(data[[gewichten]])){
      #Geef warning voor relevante variabelen.
      stop(paste("Fout bij survey design: gewichten = ", gewichten, "bestaat niet in data. controleer instellingen"))
    }
    
    #geef leeg object als output
    return(NULL)
    #Ook controle op aantal rijen met valide gegevens voor strata & gewichten
    #Als er alleen maar missing zijn: ERROR geven
  }else if(all(is.na(data[[strata]])) | all(is.na(data[[gewichten]]))){
    
    error_strata <- ifelse(all(is.na(data[[strata]])),paste0("alle data voor stratum: '",strata,"' is missing. 
                                                             \n Pas dit aan in het SPSS bestand"),"")
    error_gewichten <- ifelse(all(is.na(data[[gewichten]])),paste0("\n Alle data voor gewicht: '",gewichten,"' is missing. 
                                                                   \n Pas dit aan in het SPSS bestand.  
                                                                   \n Of wijzig de instelling: 'algemeen:missing_weegfactoren' in de configuratie.
                                                                   \nGeldige instellingen zijn: 'fout', 'verwijderen', een getal waarmee missings vervangen worden"),"")
    
    
    stop(paste(error_strata,"\n _________ \n", error_gewichten))
    return(NULL)
    
  }else {
    
    #Surveydesign maken
    svydesign(ids = ~1,
              strata = data[[strata]],
              weights = data[[gewichten]],
              data = data)
  }
}


## Voorbeeld:
# data = monitor_df
# strata_var = "Stratum"
# gewicht_var = "Standaardisatiefactor"

# design <- survey_design_maken(data = monitor_df, strata = 'Stratum', 'Standaardisatiefactor')

#prop_ci_berekenen()
#functie die per antwoordmogelijkheid betrouwbaarheidsintervallen berekend voor proporties van in een kruistabel.
#kan met of zonder crossings gebruikt worden. 
#enkele fouten worden afgevangen met warnings & een veelvoorkomende warning dat er NaN's zijn uitgerekend bij lege velden is onderdrukt.

#Deze functie wordt niet direct gebruikt in het script, maar wordt binnen de functie bereken_kruistabel gebruikt.

#Reken confindence interval uit voor proportie van bepaalde waarde binnen variabele o.b.v. surveydesign
prop_ci_berekenen <- function(data = NULL, variabele = NULL, nummer = NULL, crossing = NULL, survey_design = NULL){
  
  if(is.null(data[[variabele]])){
    warning(paste("fout bij CI berekenen:", variabele, "bestaat niet in data. controleer naam variabele"))
    
    return(NULL)
  }else{
    
    #survey package is vrij oud en werkt daarom wat vreemd in vergelijking met abdere onderdelen van R.
    #formules kunnen niet zomaar dynamisch aangemaakt worden. Oplossing:
    #We maken een string die de formule zou moeten zijn & evaluaren dat als expressie middels eval(parse(text = ..))
    
    #Tekst voor formulie CI zonder crossings    
    tekst_formule <- paste0("~I(",variabele,"==",nummer,")")
    
    #tekst crossing
    #Alleen upper en lower bound ophalen. 
    if(is.null(crossing)){
      
      #supressWarnings i.v.m. lege datasets die toch een csv moeten hebben. Het is niet erg als er NaN's produced zijn.
      ci <- suppressWarnings(attr(svyciprop(eval(parse(text = tekst_formule)), survey_design, method='xlogit', na.rm=TRUE),"ci"))
    }else{
      
      tekst_crossing <- paste0("~",crossing)  
      
      #BIJ CI  met crossings komt 't regelmatig voor dat er 0 obs op een crossing + antwoord bestaan.
      #Script kan dan geen CI uitrekenen voor die cellen en maakt dan NaN. 
      #Warning in functie  uitgezet (suprresswarnings) en eigen warning ingevoegd ter verduidelijking.
      
      #Correctie: Warning helemaal uitgezet; komt erg vaak voor en is eigenlijk ook geen probleem.
      
      # if(any(table(data[[crossing]], data[[variabele]]) == 0)){
      #   warning(paste("\nEr zijn lege cellen bij de crossing van:\n", crossing, " met", variabele, "\n Dit zorgt voor NaN bij berekenen CI proporties"))
      # } 
      
      
      #Het kan voorkomen dat er alleen maar missings zijn in een subset voor antwoord op een variabele
      #In dat geval kan er niks uitgerekend worden door svyby. Rijen moeten toch in output komen.
      #NA's genereren met juiste formaat o.b.v. crossings.
      
      #Als er alleen maar missings zijn op de waarde 'nummer'  van een variabele in een subset  
      if(nrow(data[which(data[[variabele]] == nummer),]) == 0 ){
        
        levels_crossing <- unname(val_labels(data[[crossing]])) 
        ci <- as.data.frame(cbind("crossing_var" = levels_crossing,"estimate" = NA,"2.5%" = NA,"97.5%" = NA))
        
      }else{
        #supresswarnings ingevoerd. Warnings worden gegeven bij de schattig van confindence intervals voor lege cellen. Dat is niet erg.
        ci <- suppressWarnings(svyby(formula = eval(parse(text = tekst_formule)),by = eval(parse(text = tekst_crossing)), survey_design, svyciprop, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE))
        
        colnames(ci) <- c("crossing_var","estimate","2.5%","97.5%")
      }
      
    }
    
    return(ci)
    
    
  }
}

## Voorbeeld
# prop_ci_berekenen(variabele="FLCAO205", nummer = 1)

# prop_ci_berekenen(variabele="FLCAO205", nummer = 1, crossing = "geslacht_3cat")
# prop_ci_berekenen(variabele = "GZGGA402", nummer = 1, crossing = "AGOJB401")

#bereken_kruistabel()
#Functie die een kruistabel maaakt per variabele. Kan zonder of met crossing.
#Maakt een kruistabel
#Deze functie wordt niet direct gebruikt in het script, maar wordt binnen de maak-grafieken functies gebruikt
bereken_kruistabel <- function(data, survey_design = NULL, variabele = NULL, crossing = NULL,
                               var_jaar = NULL, min_observaties_per_vraag = params$default_nvar,
                               min_observaties_per_antwoord = params$default_ncel) { 
  
  
  #Check of er wel data is
  if (nrow(data) == 0) {
    stop(glue("
    LEGE dataset ingevoerd
    Er is geen data gevonden met standaardisatiefactor voor var_inhoud {paste(variabele)} 
    met gekozen filters. Pas je invoer aan.
    
    Een veelvoorkomende oorzaak is dat regiocode/gemeentecode niet bestaat of geen standaardisatiefactor heeft
    "
    ))
  }
  
  #survey pikt het niet als delen v formules niet in .globalEnv staan. Daarom <<- en later
  #opruimen.
  data_global <<- data
  
  #Niet-valide invoer afvangen & warning geven
  if(is.null(data_global[[variabele]])) {
    
    warning(paste("Variabele", variabele, "bestaat niet in dataset. Kruistabel wordt niet berekend."))
    return(NULL)
    
  }
  
  # Speciale situatie: Vergelijking tussen jaren
  if (!is.null(var_jaar) & !is.null(crossing)) {
    
    warning(paste("Er is een crossingsvariabele ", crossing, " en een jaar variabele ", var_jaar, "ingevuld. Verwijder één van de twee. Kruistabel wordt niet berekend."))
    return(NULL)
    
  } else if (!is.null(var_jaar) & is.null(crossing)) {
    
    crossing = var_jaar
    
  }
  
  #Bereken kruistabel  
  
  antwoorden <-  attr(data_global[[variabele]], "labels") # value labels
  
  #Formule maken voor in svytable
  formule_tekst <- paste0("~ ", substitute(data_global), "[['", variabele,"']]")
  
  tb <- svytable(formula = eval(parse(text = formule_tekst)), design = survey_design) 
  
  #Als het aantal waarden in tb meer is dan het aantal antwoorden (obv value labels)
  #Betekent dit dat er een ongelabelde waarde is. Dat kan betekenen dat het een Missing is die niet zo is vastgelegd in spss
  #Of het kan betekenen dat een valide waarde ongelabeld is. Vanwege deze onduidelijkheid: harde error met melding
  if(length(antwoorden) < length(tb)){
    
    stop(paste("Er zijn ongelabelde waarden. Controleer variabele", variabele, "in .sav bestand"))
    
  }
  
  #svytable slaat antwoorden zonder respondenten over; aanvullen.
  #Als er iets in tb zit; maar het is niet even lang als het aantal antwoorden
  if(length(tb) > 0 & length(tb) < length(antwoorden)){
    
    #verschil lengte tb met lengte antwoorden 
    n_ontbrekende_antwoorden <- length(antwoorden) - length(tb) 
    
    lege_antwoorden <- rep(0,n_ontbrekende_antwoorden)
    names(lege_antwoorden) <- unname(antwoorden)[!unname(antwoorden) %in% names(tb)]
    
    tb <- c(tb, lege_antwoorden) 
    
    #Volgorde van NAMEN tb matchen aan antwoorden.
    tb <- tb[order(match(names(tb),antwoorden))]
    
  }
  
  ct <- prop.table(tb) * 100 # ct bevat estimates als percentages
  
  #Als er geen crossings zijn
  if(is.null(crossing)){
    
    #Check of voldaan wordt aan kleine N regels.
    #Als er minder min_observaties_per_antwoord per antwoordoptie zijn OF 
    #minder dan min_observaties zijn bij een vraag; maak CI's en estimates NA.
    
    if (any(table(data_global[[variabele]]) < min_observaties_per_antwoord) |
        exists('lege_antwoorden') & min_observaties_per_antwoord != 0 |
        sum(table(data_global[[variabele]])) < min_observaties_per_vraag){
      
      confidence_intervals <- matrix(data = NA, 
                                     nrow = length(ct), 
                                     ncol = 2, 
                                     dimnames = list(paste0("[", 1:length(ct),",]"), c("2.5%", "97.5%")))
      
      estimate <- ct %>% replace(values = NA)
      
    } else {
      #reken voor ieder antwoord de ci uit
      confidence_intervals <- t(sapply(unname(antwoorden), function(x){prop_ci_berekenen(data = data_global, 
                                                                                         variabele = variabele,
                                                                                         nummer = x, 
                                                                                         survey_design = survey_design)}))
      
      #Het kan voorkomen dat een variabele alleen maar missing kent voor een bepaalde subset
      #Toch willen we  rijen hebben die alle antwoord-niveuas vastleggen.
      #Als je direct in cbind een lege vector aanroept wordt deze kolom simpelweg niet toegevoegd, dit zorgt voor problemen bij het maken van dataframes met
      #rbind; daarvoor moet er een gelijk aantal kolommen zijn.
      
      #Daarom  expliciet NA toewijzen als  ontbrekende levels op een variabele in  een subset
      #resulteren in vectors die leeg zijn. Geld hier alleen voor ct/estimate. 
      estimate <- ct
      if(length(estimate) == 0){estimate <- NA}
      
    }
    
    n_weighted <- tb
    if(length(n_weighted) == 0){n_weighted <- NA}
    
    kruistabel <- cbind("varcode" = variabele,
                        "waarde" = as.numeric(antwoorden),
                        "label" = names(antwoorden),
                        "n_weighted" = n_weighted,
                        "estimate" = estimate,
                        "ci_upper" = confidence_intervals[,2],
                        "ci_lower" = confidence_intervals[,1],
                        "n_unweighted" = table(factor(data_global[[variabele]], levels = antwoorden))
    )
    
    #Als er wel crossings zijn
  }else{
    
    #Warning als crossing niet bestaat
    if(is.null(data_global[[crossing]])){
      
      stop(paste("Crossing",crossing, "bestaat niet in dataset. Kijk configuratie en dataset na."))
      return(NULL)
      
    }else{
      
      #Als er missings zijn op de crossing. 
      if(any(is.na(data_global[[crossing]]))){
        
        warning(paste("Missing data gevonden bij crossing ",crossing))
        
      }
      
      #CI uitrekenen
      confidence_intervals <- lapply(unname(antwoorden), function(x){prop_ci_berekenen(data = data_global,
                                                                                       variabele = variabele,
                                                                                       nummer = x, 
                                                                                       crossing = crossing, 
                                                                                       survey_design = survey_design)})
      
      #CI van verschillende antwoordmogelijkheden samenvoegen
      confidence_intervals <- do.call(rbind, confidence_intervals)
      
      
      #Gewogen Pop count uitrekenen 
      population_count <- lapply(unname(antwoorden), function(x){
        
        #Als var. in subset alleen maar NA heeft; lege tabel voor pop.count maken
        if(all(is.na(data_global[[variabele]]))){
          levels_crossing <- unname(val_labels(data_global[[crossing]]))
          
          cbind("antwoord" = x, crossing_var = levels_crossing, n_weighted = "NA") %>%
            as.data.frame()
          
        }else{
          
          tekst_formule = paste0("~I(",variabele,"==",x,")")
          tekst_crossing = paste0("~",crossing)
          
          tabel <- svyby(formula = eval(parse(text = tekst_formule)), by = eval(parse(text = tekst_crossing)), survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE) 
          
          #3e index van pop.count tabel = gewogen aantal repsondenten die een antwoord gaven
          cbind("antwoord" = x,"crossing_var" = unname(tabel[1]), "n_weighted" = unname(tabel[3]) )
        }
        
      })
      
      #Pop-count van versch. antwoordmogelijkheden samenvoegen
      population_count <- do.call(rbind, population_count)
      
      #Wat zijn alle crossing_levels?
      crossing_levels <- attr(data_global[[crossing]],"labels")
      
      #Maak matrix met info kruistabel per crossing-level
      kruistabel <- lapply(crossing_levels, function(x){
        #Het kan voorkomen dat een level v.e. crossing niet bestaat in een subset (bv een school die geen data heeft v. een spec. leerjaar)
        #Toch willen we  rijen hebben die de antwoorden per alle  crossing-levels vastlegt.
        #Als je direct in cbind een lege vector aanroept wordt deze kolom simpelweg niet toegevoegd, dit zorgt voor problemen bij het maken van dataframes met
        #rbind; rbind vereist  een gelijk aantal kolommen,
        
        #Daarom  expliciet NA's toewijzen als ontbrekende levels op een crossing in een subset resulteren in lege vectors
        
        #Als de lengte van pop.count-vector bij crossing-level 0 is; dan NA. Anders pop.count vector
        n_weighted <- population_count[population_count$crossing_var == x ,3]
        #NA maken als leeg is
        if(length(n_weighted) == 0){n_weighted <- NA}
        
        #Ook voor estimate
        estimate <- confidence_intervals[confidence_intervals$crossing_var == x,2]*100
        if(length(estimate) == 0){estimate <- NA}
        
        #Ditzelfde moet ook gebeuren voor confidence intervals
        
        #Check of voldaan wordt aan kleine N regels.
        # Confidence intervals + estimates moeten NA woden als het aantal observaties
        # op een vraag per level v.e. crossing lager is dan de opgegeven min_obs. 
        # OF als er te weinig observaties per antwoordoptie zijn.
        # LET OP: gebruik dit altijd op data zonder lege missing categorieën, bv. door eerst functie verwijder_9_onbekend() over data te runnen.
        
        te_weinig_obs <- sum(table(factor(data_global[[variabele]][data_global[[crossing]] == x], levels = antwoorden))) < min_observaties_per_vraag |
          any(table(factor(data_global[[variabele]][data_global[[crossing]] == x], levels = antwoorden)) < min_observaties_per_antwoord)
        
        ci_upper <- confidence_intervals[confidence_intervals$crossing_var == x,4]
        
        if(te_weinig_obs | length(ci_upper) == 0){
          ci_upper <- NA
          estimate <- NA
        }
        
        ci_lower <- confidence_intervals[confidence_intervals$crossing_var == x,3]
        
        if(te_weinig_obs | length(ci_lower) == 0){
          ci_lower <- NA
          estimate <- NA
        }
        
        #Kolommen koppelen
        cbind("varcode" = variabele,
              "waarde" = as.numeric(antwoorden),
              "label" = names(antwoorden),
              "crossing" = crossing,
              "crossing_var" = names(crossing_levels[crossing_levels == x]),
              "n_weighted" = n_weighted,
              "estimate" = estimate,
              "ci_upper" = ci_upper,
              "ci_lower" = ci_lower,
              #n_unweighted moet (net als alle andere vectors) de lengte hebben van alle mogelijke values in de variabele
              #Wanneer table() een factor-variabele gevoerd krijgt worden ook alle values/levels gegeven, ook waar geen observaties voor zijn.
              "n_unweighted" =  table(factor(data_global[[variabele]][data_global[[crossing]] == x], levels = antwoorden)))
        
      })
      
      kruistabel <- do.call(rbind, kruistabel)
      
    }
  }
  
  #variabelen aanpassen voor plotfuncties
  kruistabel <- kruistabel %>% 
    as_tibble() %>%
    mutate(
      label = factor(label),
      estimate = as.numeric(estimate) %>% round(),
      weggestreept = ifelse(is.na(estimate),10,NA) %>% as.numeric(),
      ci_upper = as.numeric(ci_upper) * 100,
      ci_lower = as.numeric(ci_lower) * 100
    ) %>% 
    
    rename(!!sym(variabele):= label, aantal_antwoord = n_unweighted, percentage = estimate) 
  
  if(!is.null(crossing)){
    kruistabel <- kruistabel %>% 
      mutate(crossing_var = factor(crossing_var)) %>% 
      rename(!!sym(crossing):= crossing_var)
  }
  
  #data_global uit globalEnvironment verwijderen
  rm(data_global, envir = .GlobalEnv)
  
  return(kruistabel)
  
  
}

## Voorbeeld:
#bereken_kruistabel(data = monitor_df_regio, survey_design = design_regio, variabele = 'Stedelijkheid')
#bereken_kruistabel(data = monitor_df_regio, survey_design = design_regio, variabele = 'GZGGA402', crossing = "AGLFA401")

#TODO vervangen met algemenere functie OF verwijderen en eindgebruikers instrueren
#hun variabele goed te coderen zodat alle missing ook user missing zijn
verwijder_9_onbekend <- function(data, var){
  value_labels <- val_labels(data[[var]])
  nieuwe_value_labels <- value_labels[value_labels!= 9]
  #Omdat 9 is verwijderd uit de nieuwe val labels, resulteert
  #het toeschrijven vna die labels aan de variabele
  #in NA's voor de waarde 9.
  val_labels(data[[var]]) <- nieuwe_value_labels
  
  return(data)
}

#Functie om labelled doubles om te zetten naar characters o.b.v. de labels (ipv de numerieke waarden)
labelled_naar_character <- function(data,var){
  
  var_factor <- to_factor(data[[var]])
  var_character <- as.character(var_factor)
  
  return(var_character)
} 


kruistabel_met_subset <- function(data, variabele = NULL, crossing = NULL, subsetvar,
                                  survey_design = NULL,
                                  nvar = params$default_nvar,
                                  ncel = params$default_ncel){
  
  #alle lvls van subsetvar
  alle_subsets <- unique(data[[subsetvar]])
  
  meerdere_kruistabellen <- lapply(alle_subsets, function(lvl_subset){
    
    label_val = val_label(data[[subsetvar]], lvl_subset)
    
    #Als label_val NULL is gaat het om een missing. Overslaan.
    if(is.null(label_val)){
      return(NULL)
    }
    
    #subset maken v design
    subset_design <<- subset(survey_design, get(subsetvar) == lvl_subset)
    #subset maken van dataset
    subset_data <<- data %>% filter(!!sym(subsetvar) == lvl_subset)
    
    bereken_kruistabel(data = subset_data, variabele = variabele,
                       crossing = crossing, survey_design = subset_design,
                       min_observaties_per_vraag = nvar,
                       min_observaties_per_antwoord = ncel
    ) %>%
      mutate(!!sym(subsetvar) := label_val)
    
  }) %>% do.call(rbind,.)
  
  rm(subset_design, subset_data, envir = .GlobalEnv) #cleanup; tijdelijke subsetsinfo verwijderen.
  
  return(meerdere_kruistabellen)
  
  
}


maak_alt_text <- function(data, plot, doelgroep = "jongvolwassenen", type_grafiek = "staafdiagram",
                          vars_crossing = "onderdeel",label_inhoud, label_crossings = NULL, niveaus){
  
  
  plot_data <- ggplot_build(plot)
  
  percentages = plot_data[["plot"]][["data"]][["percentage"]]
  
  #0 of NA naar "Onbekend" zetten
  percentages[is.na(percentages) | percentages == 0] <- "Onbekend"
  
  percentages[percentages != "Onbekend"] <- paste0(percentages[percentages != "Onbekend"],"%") #alles wat niet Onbekend is "%" geven
  
  #labels grafiek maken o.b.v. vars_crossing
  labels = lapply(vars_crossing,function(var){
    plot_data[["plot"]][["data"]][[var]] %>% as.character()
  }) %>% do.call(paste,.)
  
  
  string_waarden = paste0(labels,": ",percentages, collapse = ", ")
  
  #string_waarden schoonmaken en ingevoegde regeleinden wissen
  string_waarden = str_replace_all(string_waarden,"\n","")
  
  #niveaus waarop data wordt getoond
  niveau_string <- niveaus %>% str_replace("nl","Nederland") %>% 
    str_replace("regio",params$regionaam) %>% 
    str_replace("gemeente",val_label(data$Gemeentecode, params$gemeentecode)) %>% 
    paste0(collapse = " en ")
  
  if(length(niveaus) >  2){
    niveau_string <- niveau_string %>% str_replace(" en",",") 
  }
  
  
  
  if(is.null(label_crossings)){
    #Grafiek zonder crossings:
    glue("{type_grafiek} met percentages voor de indicator '{label_inhoud}' bij {doelgroep} in {niveau_string}:  {string_waarden}")
  } else{
    #Grafiek met crossings
    label_crossings = label_crossings %>% paste(collapse = " en ")
    glue("{type_grafiek} met percentages voor de indicator '{label_inhoud}' bij {doelgroep} per {label_crossings} in {niveau_string}: {string_waarden}")  
  }
  
  
}

cbs_populatie_opschonen <- function(file = params$path_cbs_data,
                                    sheet = NULL){
  
  #niet als dataframe aangeleverd. ontbrekende kolomkoppen. meerdere rijen boven dataset.
  if (file.exists(file)) { # Als file bestaat, inladen
    
    cbs_df <- openxlsx::read.xlsx(paste(file),
                                  sheet = sheet)[-c(1:4),] #1e 4 rijen verwijderen
    
  } else { # Als file niet bestaat, error
    
    stop(glue("Bestand: {file} is niet aanwezig in working directory op locatie
               '{params$path_cbs_data}' voeg bestand toe of pas het bestandspad in
              de parameter 'path_cbs_data' aan "))                              
    
  }
  
  cbs_df <- cbs_df %>%
    rename( #varlabels fixen
      "gemeentecode" = 1,
      "regio" = 2,
      "man_16-17" = 3,
      "man_18-20" = 4,
      "man_21-25" = 5,
      "vrouw_16-17" = 6,
      "vrouw_18-20" = 7,
      "vrouw_21-25" = 8,
      "totaal" = 9) 
  
  
  if(sheet == "Tabel1"){
    cbs_df <- cbs_df %>% select(-gemeentecode)
  }
  
  
  cbs_df %>% 
    select(-totaal) %>% 
    pivot_longer(
      contains("_"),
      values_to = "aantal",
      names_to = "categorie") %>% 
    mutate(
      geslacht = str_extract(categorie,".*(?=_)"),
      leeftijd = str_extract(categorie,"(?<=_).*")
    ) %>% 
    select(-categorie)
  
}  


#Functie die markdown syntax om een grafiek en tabel heen genereert om een panel-tabset te maken
#Geeft twee 'tabbladen' met een grafiek en tabel.
#Moet in combinatie met results 'asis' gebruikt worden
#wordt in grafiekfuncties aangeroepen wanneer grafiek_en_tabel = TRUE.

maak_panel_tabset <- function(grafiek, tabel){
  
  tabset_items = list("Grafiek" = grafiek,
                      "Tabel" = tabel) 
  
  #Genereer markdown-syntax om R code voor tabel & grafiek heen
  cat("::: {.panel-tabset}\n\n") # maak panel tabset div
  
  #Plaats R objecten voor  grafiekn & tabel onder h2 headers met naam
  #object (Grafiek & Tabel)
  purrr::iwalk(tabset_items, ~ {
    cat('## ', .y, '\n\n')
    
    print(.x)
    
    cat('\n\n')
    
  })
  
  cat(":::\n") #sluit panel tabset div af
  
}

#Functie om regeleinden te maken voor x-as labels
#Aslabels moeten niet in elkaar overlopen. Hiervoor wordt de parameter X_as_label_wrap gebruikt om regeleinden in te voegen
#str_wrap = voeg linebreak in bij eerstvolgende woordeind na x aantal characters.
maak_regeleinden <- function(label_vector, x_as_label_wrap, x_as_regels_toevoegen = 0){
  
  #pas str_wrap toe
  label_vector <-  str_wrap(label_vector, x_as_label_wrap)
  
  #tbv uitlijnen van grafieken die naast elkaar staan met een ongelijke hoeveelheid regels in de x-aslables kunnen witregels toegevoegd
  #worden aan aslasbels. Het aantal witregels dat toegevoegd moet worden is afh. van hoeveel regels een aslabel al heeft.
  
  #tel het aantal regels voor ieder aslabel; tel alle regeleinden + 1
  n_regels_label <- label_vector %>% str_count("\n") + 1
  
  #welke index van de label_vector heeft de meeste regels? daar willen we eventueel regels aan toevoegen 
  index_meeste_regels <- which(n_regels_label == max(n_regels_label)) 
  
  #maak een character met het aantal gevraagde regeleinden uit x_as_regels_toevoegen
  extra_regeleinden <- rep("\n",x_as_regels_toevoegen) %>% str_c(collapse = "")
  
  #Voeg regeleinden toe aan het label met de meeste regels
  label_vector[index_meeste_regels] <- paste0(label_vector[index_meeste_regels], extra_regeleinden)
  
  return(label_vector)
}

#Functie maken om kleuren te bepalen

bepaal_kleuren <- function(kleuren = params$default_kleuren_grafiek, 
                           niveaus = "regio") {
  
  # bepaal kleuren per niveau
  bepaalde_kleuren <- lapply(niveaus, function(x){
    
    if (x == "gemeente") {
      kleur <- kleuren[c(6:8)]
    } else if (x == "regio") {
      kleur <- kleuren[c(1:5)]
    } else {
      kleur <- kleuren[c(9,10,8)]
    }
    
    kleur 
    
  }) %>% do.call(c,.)
  
  return(bepaalde_kleuren)
  
}

valideer_kleurvector <- function(kleuren) {
  # Regex voor geldige patronen
  #Begint met '#'
  #Daarna 3 OF 6 alfanumerieke tekens
  #Daarna niks
  hex_patroon <- "^#([A-Fa-f0-9]{3}|[A-Fa-f0-9]{6})$"
  
  #controleer geldigheid
  is_geldig <- grepl(hex_patroon, kleuren)
  
  return(is_geldig)
}




# Tabelfuncties -------------------------------------------------------
maak_responstabel <- function(data, crossings, missing_label = "Onbekend",
                              kleuren = params$default_kleuren_responstabel,
                              huidig_jaar = 2024,
                              jaarvar = "AGOJB401",
                              niveaus = "regio"
                              
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
    }
  
  #TODO Hoe om te gaan met missing waarden? Meetellen als 'onbekend' of niet weergeven?
  #In laatste geval kloppen de totalen van crossings onderling niet. Kan prima zijn
  #Voorlopige keuze: Missings weergeven als "onbekend"
  
  #filteren op jaar; of jaar als crossing
  if(jaarvar %in% crossings){
    #niks doen
  } else{
    data <- data %>% filter(!!sym(jaarvar) == huidig_jaar)
  }
  
  #filteren op niveau 
  #mag max 1 niveau invoeren
  if(length(niveaus) > 1 ){
    stop(glue(
      "Kies maximaal 1 niveau.
    niveaus: {paste(niveaus,collapse = ',')}
    "))
  } else{
    if(niveaus == "regio"){
      data <- data %>% filter(GGDregio == params$regiocode)
    } else if (niveaus == "gemeente") {
      data <- data %>% 
        filter(Gemeentecode == params$gemeentecode) %>%
        filter(!is.na(Standaardisatiefactor_gemeente))
    } else if (niveaus == "nl"){
      #niks doen
    } else{
      stop(glue("
     foute waarde bij niveaus ingevoerd. Kies 'nl', 'regio' of 'gemeente'
          niveaus: {niveaus}"))
    }
    #filteren op niveau
  }
  
  aantallen_per_crossing <- lapply(crossings, function(x){
    
    if(is.null(data[[x]])){
      
      warning(glue("De variabele {x} bestaat niet in de data. Typefout?"))
      return(NULL)
      
    }
    
    #Variabelen naar character omzetten
    data[[x]] <- labelled_naar_character(data, x)
    
    #Aantallen uitrekenen. Missing labelen als missing_label
    aantallen_df = data %>% 
      group_by(!!sym(x)) %>% 
      summarise(n = n()) %>%
      rename(crossing = 1) %>% 
      mutate(crossing = replace(crossing, is.na(crossing),missing_label))  
    
    #Als het de 1e crossing is; totaal_df bovenaan df toevoegen
    #NB Is maar 1 totaal in responstabel; dus n per crossing worden als gelijk beschouwd.
    if(x == crossings[1]){
      totaal_df <- data.frame(
        crossing = "Totaal",
        n = sum(aantallen_df$n)
      )
      
      rbind(totaal_df,aantallen_df)
    } else {
      #Anders gewoon aantallen teruggeven
      aantallen_df
    }
    
  }) %>% do.call(rbind,.) #dataframes per crossing aan elkaar plakken
  
  if(is.null(aantallen_per_crossing)){
    
    warning(glue("De variabelen: {str_c(crossings, collapse = ', ')} bestaan niet. Er kan geen tabel gemaakt worden"))
    return(NULL)
  }
  
  
  #gegeven een vector met alle crossings willen we afwisselend een andere kleur geven
  #aan iedere even en oneven crossing. Dus c("gender","klas","opleiding") 
  #moet de crossing "gender" een andere kleur geven dan "klas" en dezelfde kleur als "opleiding"
  
  #We weten van te voren niet hoeveel niveaus een crossing heeft dus moet dat uitgerekend worden
  levels_crossings <- lapply(crossings, function(x){
    
    data[[x]] %>% unique() %>% length()    
  }) %>% unlist()
  
  #twee vectoren aanmaken die gevuld zullen worden met de rij-indexen voor kleur 1 en 2 
  kleur_1_indexen <- c(1) #aanmaken met waarde 1 = bovenste totaalrij
  kleur_2_indexen <- c()
  current_index <- 1
  
  
  for(i in 1:length(levels_crossings)){
    
    #even index
    if(i %% 2 == 0){
      
      start_index = current_index + 1 
      eind_index <- current_index + levels_crossings[i]
      bereik = c(start_index:eind_index )
      current_index <- max(bereik)
      
      kleur_1_indexen <- c(kleur_1_indexen, bereik)
    } else {
      #oneven index
      
      start_index = current_index + 1 
      eind_index <- current_index + levels_crossings[i]
      bereik = c(start_index:eind_index )
      current_index <- max(bereik)
      
      kleur_2_indexen <- c(kleur_2_indexen,bereik)
    }
    
    
  }
  
  #we willen een vector met de rijnummers van alle oneven crossings 
  
  
  #aangepaste versie van joliens versie voor gemeenteprofielen
  aantallen_per_crossing %>% 
    gt() %>% 
    # Bovenste rij roze kleur
    tab_style(style = cell_fill(color = kleuren[1]), locations = cells_column_labels()) %>% 
    
    # Totaal en gender donkergroen
    tab_style(style = cell_fill(color = kleuren[2]), locations = cells_body(rows = kleur_1_indexen)) %>% 
    # Klas lichtgroen
    tab_style(style = cell_fill(color = kleuren[3]), locations = cells_body(rows = kleur_2_indexen)) %>%
    #Wit lettertype
    tab_style(style = cell_text(color = kleuren[4]), locations = cells_column_labels()) %>% 
    tab_style(style = cell_text(color = kleuren[4]), locations = cells_body()) %>% 
    # Kolomnaam Aantal vetgedrukt
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>% 
    # Pas kolomnaam Aantal aan
    cols_label(matches("n") ~ "Aantal ingevulde vragenlijsten") %>%
    # Verwijder kolomnaam boven eerste kolom
    cols_label(matches("crossing") ~ "") %>% 
    # Per default wordt de tabel gecentreerd op de pagina. Zet deze volledig naar links.
    tab_options(table.margin.left = 0,
                table.margin.right = 0) 
  
}


# Grafiekfuncties ---------------------------------------------------------
maak_staafdiagram_dubbele_uitsplitsing <- function(data, var_inhoud, 
                                                   var_crossing_groep = NULL,
                                                   var_crossing_kleur = NULL,
                                                   titel = "",
                                                   kleuren = params$default_kleuren_grafiek,
                                                   nvar = params$default_nvar,
                                                   ncel = params$default_ncel,
                                                   alt_text = NULL,
                                                   caption = "",
                                                   huidig_jaar = 2024,
                                                   jaarvar = "AGOJB401",
                                                   niveaus = "regio",
                                                   tabel_en_grafiek = FALSE,
                                                   toon_y = FALSE,
                                                   x_as_label_wrap = 20,
                                                   x_as_regels_toevoegen = 0,
                                                   max_as = 101
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  if(!labelled::is.labelled(data[[var_inhoud]])){
    stop(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  
  if(val_labels(data[[var_inhoud]]) %>% length() > 2){
    stop(
      glue("{var_inhoud} is geen dichotome variabele. Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
  }
  
  #Dubbele uitsplitsing betekend 2 crosisngs. 1 bepaald kleur ander bepaald groep (X-positie)
  #1 van die crossings kan niveau zijn.
  #valide invoer is:
  
  #Als beide crossing typpen zijn ingevuld mag er maar 1 niveau zijn en wordt op dat niveau gefilterd.
  #anders error
  
  if(
    !( #Als 1 van onderstaande niet waar is dan mag het niet
      
      #1 van de twee crossings ontbreekt en er zijn meer niveaus DAT MAG
      (xor(is.null(var_crossing_groep), is.null(var_crossing_kleur)) & length(niveaus) > 1) | #OF
      #Beide crossings zijn ingevuld en er is 1 niveau DAT MAG OOK
      ((!is.null(var_crossing_groep) & !is.null(var_crossing_kleur)) & length(niveaus) == 1)
    )
  ){
    
    invoer_kleur = ifelse(is.null(var_crossing_groep),"Leeg",var_crossing_groep)
    invoer_groep = ifelse(is.null(var_crossing_kleur),"Leeg",var_crossing_kleur)
    
    stop(
      
      glue("Foute invoer!
      Deze functie kan hebben:
      - var_crossing_groep EN var_crossing_kleur + 1 niveau
      - var_crossing_groep OF var_crossing_kleur + meerdere niveaus
      
      Huidige invoer:
      var_crossing_kleur: {invoer_kleur}
      var_crossing_groep: {invoer_groep}
      niveaus: {paste(niveaus,collapse = ',')}")
    )
  }
  
  # Bepaal kleuren
  kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  
  #Checken of crossvar is ingevoerd en in dat geval jaarvar is
  crossing_is_jaar <- ifelse(
    jaarvar %in% c(var_crossing_groep, var_crossing_kleur), TRUE, FALSE)
  
  
  #Loop over alle niveaus & bereken kruistabel per niveau en evt. jaaruitsplitsing. sla op in df.
  df_plot <- lapply(niveaus, function(x){
    
    #design en dataset bepalen o.b.v. niveau
    if(x == "nl"){
      design_x <- design_land
      subset_x <- data
      niveau_label <- "Nederland"
      
    } else if(x == "regio"){
      design_x <- design_regio
      subset_x <- data %>% filter(GGDregio == params$regiocode)
      niveau_label <- val_label(data$GGDregio, params$regiocode)
      
    } else if (x == "gemeente"){
      design_x <- design_gem
      subset_x <- data %>% 
        filter(Gemeentecode == params$gemeentecode) %>%
        filter(!is.na(Standaardisatiefactor_gemeente))
      niveau_label <- val_label(data$Gemeentecode, params$gemeentecode)
      
    } else{
      
      stop(glue("niveau bestaat niet
                niveau: {paste(niveaus,collapse = ',')}"))
    }
    
    #niet filteren als jaar als crossing is geselecteerd
    if(crossing_is_jaar){
      
      #niet filteren 
      data_temp <<- subset_x
      design_temp <<- design_x 
    } else{
      #standaard alleen laatste jaar overhouden
      #subset data
      design_temp <<- subset(design_x, get(jaarvar) == huidig_jaar)
      #subset maken v design
      data_temp <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
    }
    
    #Als er 1 niveau is zijn er dus 2 crossings: kruistabel met subset
    if(length(niveaus) == 1){
      
      kruistabel_met_subset(data_temp, variabele = var_inhoud,
                            crossing = var_crossing_kleur,
                            subsetvar = var_crossing_groep,
                            nvar = nvar,
                            ncel = ncel,
                            #TODO aangeven dat de crossing op groepsniveau een subset is
                            survey_design = design_temp) %>%
        mutate(niveau = niveau_label) %>% 
        filter(waarde == 1)
      
      #Anders moet voor elk niveau in de lapply een kruistabel zonder subset uitgerekend worden
    } else{
      
      var_crossing = ifelse(is.null(var_crossing_groep), var_crossing_kleur, var_crossing_groep)
      
      bereken_kruistabel(data_temp, 
                         variabele = var_inhoud,
                         crossing = var_crossing,
                         survey_design = design_temp,
                         min_observaties_per_vraag = nvar,
                         min_observaties_per_antwoord = ncel) %>%
        mutate(niveau = niveau_label) %>% 
        filter(waarde == 1)
      
    }
    
    
  })  %>% do.call(rbind,.)
  
  
  
  #opruimen; data & design temp verwijderen uit global Env
  rm(data_temp, design_temp, envir = .GlobalEnv)
  
  #voeg regeleinden toe als nodig:
  #x_as_label_wrap bepaald wanneer labels te lang zijn om naast elkaar te passen
  #x_as_regels_toeveogen bepaald of er regels toegevoegd moeten worden om plots die naast elkaar staan
  #uit te lijnen
  df_plot[[var_crossing_groep]] <- maak_regeleinden(df_plot[[var_crossing_groep]], x_as_label_wrap, x_as_regels_toevoegen)
  
  #als er meerdere niveaus zijn geselecteerd moet "niveau" ingevuld worden bij de lege crossing
  var_crossing_groep = ifelse(is.null(var_crossing_groep), "niveau", var_crossing_groep)
  var_crossing_kleur = ifelse(is.null(var_crossing_kleur), "niveau", var_crossing_kleur)
  
  #Kleuren toewijzen 
  namen_kleuren <- unique(df_plot[[var_crossing_kleur]])
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  #Plot maken
  plot = ggplot(df_plot) +
    #staven
    geom_col(aes(x = !!sym(var_crossing_groep), y = percentage, fill = !!sym(var_crossing_kleur)),
             position = position_dodge(width = 0.8), width = 0.8,
             na.rm = T
    ) +
    #tekst 
    geom_text(aes(x = !!sym(var_crossing_groep),
                  y = percentage,
                  label = paste0(percentage,"%"),
                  group = !!sym(var_crossing_kleur),
                  vjust = -1),
              position = position_dodge2(width = 0.8),
              size = geom_text_percentage, # Hier grootte van percentages aanpassen
              na.rm = T
    ) +
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = !!sym(var_crossing_groep),
                   y = weggestreept, color = !!sym(var_crossing_kleur)),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
    ) +
    #titel met str_wrap zodat lange titels niet buiten het plotgebied lopen
    ggtitle(str_wrap(titel, 50)) +
    #Hier worden de kleuren en volgorde bepaald Voor de fill van de balken
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE,
                                           label.position = "right", title.position = "top")) +
    #zelfde voor de kleur van de sterretjes
    scale_color_manual(values= kleuren) +
    
    #Y-as axis ticks definiëren en met expand zorgen dat die netjes aansluit op 0.
    scale_y_continuous(
      limits = c(0,max_as),
      breaks = seq(0,100, by = 10),
      labels = paste0(seq(0,100, by = 10),"%"),
      expand = expansion(mult = c(0, 0.05))) + 
    
    #Opmaak plot
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1,"cm"),
          legend.position = "bottom",
          axis.line.x.bottom = element_line(linewidth = 1),
          #Grootte tekst (behalve annotatie boven balken):
          text = element_text(family = font_family,
                              lineheight = line_height
                              ), 
          plot.title = element_text(hjust = .5, size = titel_size),
          axis.text = element_text(size = as_label_size),
          legend.text = element_text(size = legend_text_size),
          plot.caption = element_text(size = caption_size, hjust = 0.5)
          
    )
  
  #Plot verder opmaken o.b.v. instellingen
  #toon y: laat Y as zien met rechte lijn + ticklabels
  if(toon_y){
    
    plot <-  plot +
      theme(
        axis.line.y.left = element_line(linewidth = 1)
      )
  } else {
    #geen y-labels
    plot <- plot +
      theme(
        axis.text.y = element_blank()
      )
  }
  
  
  #alt text aanmaken voor grafiek als deze niet handmatig is opgegeven
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(data, plot, niveaus = niveaus, 
                              label_inhoud = var_label(data[[var_inhoud]]),
                              label_crossings =  c(
                                var_label(data[[var_crossing_groep]]),
                                var_label(data[[var_crossing_kleur]])
                              ),
                              vars_crossing = c(var_crossing_groep, var_crossing_kleur)) %>% 
      paste0(". ",caption)
  }
  
  #Alt text en caption toevoegen
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  
  #default: alleen plot uitspugen
  if(!tabel_en_grafiek){
    
    return(plot)
    
  } else {
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    grafiek = plot
    
    #tabel in elkaar zetten; als titel is ingevuld; titel anders var-label
    
    label_inhoud = ifelse(nchar(titel) > 0,
                          titel,
                          var_label(data[[var_inhoud]])) #label inhoud var ophalen
    
    tabel = plot$data %>% #data uit plot halen
      select(
        all_of(c(var_crossing_groep, var_crossing_kleur)),
        percentage) %>% #var_crossings & percentage bewaren 
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      ) %>% 
      pivot_wider(names_from = !!sym(var_crossing_groep),
                  values_from =  percentage) %>% #crossing_groep = kolom
      rename(" " = 1) %>% #varlabel crossing_kleur verwijderen
      gt() %>% #gt tabel van maken
      tab_header(label_inhoud) %>% #label toevoegen
      tab_caption(caption)
    
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)    
  }
  
}


maak_staafdiagram_vergelijking <- function(data, var_inhoud, var_crossings, 
                                           titel = "",
                                           subtitel = FALSE,
                                           kleuren = params$default_kleuren_grafiek,
                                           nvar = params$default_nvar, ncel = params$default_ncel,
                                           alt_text = NULL,
                                           caption = "",
                                           huidig_jaar = 2024,
                                           jaarvar = "AGOJB401",
                                           niveaus = "regio",
                                           tabel_en_grafiek = FALSE,
                                           toon_y = FALSE,
                                           ongewogen = FALSE
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  
  
  if(!labelled::is.labelled(data[[var_inhoud]])){
    stop(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele. 
              Maak complete labels aan of kies een andere variabele"))
  }
  if(val_labels(data[[var_inhoud]]) %>% length() > 2){
    stop(
      glue("{var_inhoud} is geen dichotome variabele. 
           Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
  }
  
  #jaarvar kan hier niet een crossing zijn. 
  if(jaarvar %in% var_crossings){
    stop(glue("De jaarvariabele '{jaarvar}' kan geen kruisvariabele zijn in dit grafiektype.
              Kies een ander grafiektype of verwijder {jaarvar} uit var_crossings"))
    
  }
  
  #niveaus kan hier max length 1 hebben
  if(length(niveaus) > 1){
    stop(glue("Dit grafiektype kan op 1 niveau filteren er zijn meer niveaus ingevoerd.
              Voer maximaal 1 niveau in of kies een ander grafiektype.
              niveaus: {paste(niveaus, collapse = ',')}"))
  }
  
  # Bepaal kleuren
  kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  
  #design en dataset bepalen o.b.v. regio
  if(niveaus == "nl"){
    
    design_x <- design_land
    subset_x <- data
    niveau_label <- "Nederland" # niveau voor subtitel
    
  } else if(niveaus == "regio"){
    
    design_x <- design_regio
    subset_x <- data %>% filter(GGDregio == params$regiocode)
    niveau_label <- val_label(data$GGDregio, params$regiocode) # niveau voor subtitel
    
  } else if (niveaus == "gemeente"){
    
    design_x <<- design_gem
    subset_x <<- data %>% 
      filter(Gemeentecode == params$gemeentecode) %>%
      filter(!is.na(Standaardisatiefactor_gemeente))
    niveau_label <- val_label(data$Gemeentecode, params$gemeentecode) # niveau voor subtitel
    
  } else{
    
    stop(glue("niveau bestaat niet
                niveau: {niveaus}"))
  }
  
  # niveau_label niet tonen als argument subtitel = FALSE
  if (subtitel == FALSE) {
    niveau_label = ""
  }
  
  #design altijd filteren op jaar in maak_staafdiagram_vergelijking
  design_temp <<- subset(design_x, get(jaarvar) == huidig_jaar)
  #subset maken v design
  data_temp <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
  
  
  
  df_plot <- lapply(var_crossings, function(crossing){
    
    var_label_crossing = var_label(data[[crossing]])
    
    df_crossing <- bereken_kruistabel(data_temp, variabele = var_inhoud, crossing = crossing,
                                      survey_design = design_temp,
                                      min_observaties_per_vraag = nvar,
                                      min_observaties_per_antwoord = ncel) %>% 
      #toevoeging jan 2025 tbv argument 'ongewogen'. variabele aantal_groep. zodat ongewogen aantal berekend kan worden.
      group_by(!!sym(crossing)) %>% 
      mutate(aantal_groep = sum(as.numeric(aantal_antwoord))) %>% 
      ungroup() %>% 
      filter(waarde == 1) %>% 
      rename(onderdeel = !!sym(crossing)) %>%  #crossinglevel naar 'onderdeel' hernoemen
      mutate(groep = factor(var_label_crossing)#,  #varlabel crossing als 'groep' toevoegen
             #weggestreept = as.numeric(weggestreept)
      )      
    df_crossing$kleuren <- kleuren[1:nrow(df_crossing)]
    
    df_crossing
    
  }) %>% do.call(rbind,.)
  
  #temp dataframe & design verwijderen uit globalEnv.
  rm(data_temp, design_temp, envir = .GlobalEnv)
  
  #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
  groep_levels <- df_plot$groep %>% unique()
  df_plot$groep <- factor(df_plot$groep, levels = groep_levels)
  
  
  #volgorde onderdeel vastzetten o.b.v dataframe
  onderdeel_levels <- df_plot$onderdeel %>% unique()
  df_plot$onderdeel <- factor(df_plot$onderdeel, levels = onderdeel_levels)
  
  
  kleuren <- df_plot$kleuren
  names(kleuren) <- onderdeel_levels
  
  #Als ongewogen = TRUE moeten ongewogen percentages uitgerekend worden. Dit is eigenlijk veel simpeler
  #en sneller door R berekend dan de gewogen data & zou idealiter ingebouwd worden op een manier waarbij R géén gewogen cijfers hoeft
  #te berekenen. Vanwege beperkte tijd & capiciteit halen we de ongewogen percentages uit de bestaande output uit de
  #functie die gewogen cijfers berekend in bereken_kruistabel().
  #we overschrijven de variabele percentage met een percentage o.b.v. aantal_antwoord
  if(ongewogen){
      mutate(
        percentage = case_when(
          is.na(weggestreept) ~ round(as.numeric(aantal_antwoord) / aantal_groep * 100, 0),
          TRUE ~ NA
          )
        )
    
  }
  
  plot = ggplot(df_plot) +
    geom_col(aes(x = groep, y = percentage, fill = onderdeel),
             position = position_dodge(width = 0.8), width = 0.8,
             na.rm = T) +
    geom_text(aes(x = groep,
                  y = percentage,
                  label = paste0(percentage,"%"),
                  vjust = -1),
              position = position_dodge2(width = 0.8),
              size = geom_text_percentage, # Hier grootte van percentages aanpassen
              na.rm = T
    ) +
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = groep, y = weggestreept, color = onderdeel),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
    ) +
    ggtitle(str_wrap(titel, 50), subtitle = niveau_label) + 
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE,
                                           label.position = "right", title.position = "top")) +
    #kleuren voor sterretje
    scale_color_manual(values= kleuren) + 
    
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, by = 10),
                       labels = paste0(seq(0,100, by = 10),"%"),
                       expand = expansion(mult = c(0, 0.05))
    ) +
    coord_cartesian(ylim = c(0,100))+
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(), #text met naam crossing uitgezet.
          legend.title = element_blank(),
          legend.spacing.x = unit(.1, 'cm'),
          legend.position = "bottom",
          axis.line.x.bottom = element_line(linewidth = 1, colour = "black"),
          #Grootte tekst (behalve annotatie boven balken):
          text = element_text(family = font_family,
                              lineheight = line_height), 
          plot.title = element_text(hjust = .5, size = titel_size),
          plot.subtitle = element_text(hjust = .5, size = (titel_size - 5), face = "italic"),
          axis.text = element_text(size = as_label_size),
          legend.text = element_text(size = legend_text_size),
          plot.caption = element_text(size = caption_size, hjust = 0.5)
          )
  
  
  #toon y: laat Y as zien met rechte lijn + ticklabels
  if(toon_y){
    plot <-  plot +
      theme(
        axis.line.y.left = element_line(linewidth = 1)
      )
  } else {
    #geen y-labels
    plot <- plot +
      theme(
        axis.text.y = element_blank()
      )
  }
  
  
  #Alt text toevoegen als deze niet handmatig is opgegeven
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(data, plot, niveaus = niveaus, 
                              label_inhoud = var_label(data[[var_inhoud]]),
                              label_crossings = sapply(var_crossings, function(var) var_label(data[[var]]))) %>% 
      paste0(". ",caption)
    
    
  } 
  
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  # Dit verpest alt-text.
  # Mogelijk is alt-text overbodig omdat we voor tabset-panels gaan met tabellen
  # Dan zou onderstaande verfijning nog interessant kunnen zijn 
  # #Onnodig complexe logica om een plot met DEZELFDE kleuren die VERSCHILLENDE
  # #inhoud aanduiden  met een vooraf onbekend aantal kruisvariabelen & levels werkbaar te maken
  # #Door leesbaarheid te vergroten met: Spacing tussen legenda-elementen
  # 
  # #Spacing is afh. van aantal staven in staafdiagram
  # #bij 5 lvls is het ongeveer 3 cm
  # #bij 7 levels is het ongeveer 2.15
  # 
  # #TODO formule afstellen
  # #formule
  # slope = -.25 #coefficient; hoe verder in de min hoe harder spacing afneemt bij meer staven
  # basis_spacing = 4.25 #baseline waarde spacing. hoe hoger, hoe meer spacing
  # spacing <- basis_spacing +  slope * nrow(df_plot)
  # 
  # #TODO tekst kleiner maken o.b.v. aantal staven
  # key_size = (10 - nrow(df_plot))/10
  # 
  # 
  # #Zoek de index(en) op van van plek waar in legenda gesplitst moet worden
  # #door df_plot$crossing te checken op verandering in waarde (is nieuwe crossing)
  # #werkt door dmv lag te kijken op welke index een vector niet meer dezelfde waarde bevat
  # # (- 1 omdat we )
  # splits_hier = which(df_plot$crossing != dplyr::lag(df_plot$crossing)) - 1 
  # splits_hier = splits_hier * 3 #keer drie omdat iedere legend key element uit 3 grobs bestaat
  # 
  # #Legenda uit plot halen
  # legend <- cowplot::get_plot_component(plot + theme(legend.position = "top",
  #                                                    legend.key.size = unit(key_size,"cm")), 'guide-box-top')
  # legend_gtable <- legend$grobs[[1]]  # TableGrob ophalen
  # 
  # #Loop over iedere index voor splitsing 
  # for(i in 1:length(splits_hier)){
  #   
  #   splits = splits_hier[i]-1+i #Omdat elke toevoeging van een kolom de Grobtable ook langer maakt
  #   #moet er steeds + 1 gedaan worden op het ingegeven aantal.
  #   #min-1 om 1e index klopend te maken
  #   
  #   #Voeg spacing to in grobtable
  #   legend_gtable <- gtable::gtable_add_cols(legend_gtable, unit(spacing, "cm"), splits) 
  #   
  # }
  # 
  # #links lege ruimte toevoegen
  # legend_gtable <- gtable::gtable_add_cols(legend_gtable, unit(3,"cm"),0)
  # #Legenda uit oorspronkelijk plot verwijderen
  # plot_no_legend <- plot + theme(legend.position = "none")
  # 
  # # voeg de gemodificeerde legenda weer samen met plot
  # plot <- cowplot::plot_grid(plot_no_legend, legend_gtable,
  #                            ncol = 1,
  #                            rel_heights = c(1, 0.2))
  
  
  if(!tabel_en_grafiek){
    
    return(plot)
    
  } else {
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    grafiek = plot
    
    #tabel in elkaar zetten; als titel is ingevuld; titel anders var-label
    
    label_inhoud = ifelse(nchar(titel) > 0,
                          titel,
                          var_label(data[[var_inhoud]])) #label inhoud var ophalen
    
    tabel = plot$data %>% #data uit plot halen
      select(onderdeel, percentage) %>% #groepnaam & percentage bewaren
      rename("groep" = onderdeel) %>% #hernoemen variabele
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      ) %>%
      gt() %>% #gt_tabel van maken 
      tab_header(label_inhoud) %>% #label inhoud als header
      tab_caption(caption)
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)    
  }
}


maak_staafdiagram_meerdere_staven <- function(data, var_inhoud, var_crossing = NULL, 
                                              titel = "",
                                              kleuren = params$default_kleuren_grafiek,
                                              flip = FALSE,
                                              nvar = params$default_nvar, ncel = params$default_ncel,
                                              alt_text = NULL,
                                              caption = "",
                                              huidig_jaar = 2024,
                                              jaarvar = "AGOJB401",
                                              niveaus = "regio",
                                              var_inhoud_waarde = NULL,
                                              tabel_en_grafiek = FALSE,
                                              toon_y = FALSE,
                                              toon_aslabel = TRUE,
                                              x_as_label_wrap = NULL,
                                              x_as_regels_toevoegen = 0,
                                              aflopend = FALSE,
                                              max_as = 101,
                                              ongewogen = FALSE
                                              
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  #Keuzes die we gebruikers willen bieden mbt niveau:
  # - filteren op niveau OF
  # - uitsplitsen op niveau
  # Implementatie:
  #als length(niveaus) = 1; filteren op niveau
  #als length(niveaus) > 1; uitsplitsen op niveau & var_crossing onmogelijk maken 
  
  if(length(niveaus) > 1 & !is.null(var_crossing)){
    stop(glue("
    Grafiek kan niet met meerdere niveaus & een crossing werken.
    Selecteer 1 niveau of verwijder var_crossing
    niveaus: {paste(niveaus, collapse = ',')}
    var_crossing: {var_crossing}"))
  }
  
  for(var in var_inhoud){
    if(!labelled::is.labelled(data[[var]])){
      stop(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
    }
  }
  
  # Bepaal kleuren
  if (is.null(var_crossing)) {
    
    kleur <- ifelse(niveaus == "gemeente", kleuren[6],
                    ifelse(niveaus == "regio", kleuren[4],
                           kleuren[11]))

    kleuren <- kleur
  } else {
    kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  }

  #TODO Optie om het correcter te doen dan gebruikelijk
  # regio zonder gemeente vs gemeente. 
  
  remove_legend = F #Legenda alleen nodig als er crossings zijn. Gaat op TRUE als er geen crossings zijn.
  
  #annotatie balken met percentages moet een beetje bijgesteld worden. bijstelling is afh.
  #van orientatie grafiek
  v_just_text = ifelse(flip,0.5,-1)
  h_just_text = ifelse(flip,-1,0.5)
  
  #Checken of crossvar is ingevoerd en in dat geval jaarvar is
  crossing_is_jaar <- ifelse(is.null(var_crossing), FALSE,
                             ifelse(var_crossing != jaarvar, FALSE, TRUE))
  
  #Loop over alle niveaus & bereken kruistabel per niveau en evt. jaaruitsplitsing. sla op in df.
  df_plot <- lapply(niveaus, function(x){
    
    
    #design en dataset bepalen o.b.v. niveau
    if(x == "nl"){
      design_x <- design_land
      subset_x <- data
      niveau_label <- "Nederland"
      
    } else if(x == "regio"){
      design_x <- design_regio
      subset_x <- data %>% filter(GGDregio == params$regiocode)
      niveau_label <- val_label(data$GGDregio, params$regiocode)
      
    } else if (x == "gemeente"){
      design_x <- design_gem
      subset_x <- data %>% 
        filter(Gemeentecode == params$gemeentecode) %>%
        filter(!is.na(Standaardisatiefactor_gemeente))
      niveau_label <- val_label(data$Gemeentecode, params$gemeentecode)

    } else{
      
      stop(glue("niveau bestaat niet
                niveau: {paste(niveaus,collapse = ',')}"))
    }
    
    #niet filteren als jaar als crossing is geselecteerd
    if(crossing_is_jaar){
      
      #niet filteren 
      data_temp <<- subset_x
      design_temp <<- design_x 
    } else{
      #standaard alleen laatste jaar overhouden
      #subset data
      design_temp <<- subset(design_x, get(jaarvar) == huidig_jaar)
      #subset maken v design
      data_temp <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
    }

    #Er kunnen meerdere var_inhouds ingevoerd worden als dat zo is: interne loop over die var inhouds
    #In dat geval worden de variabelen
    #Als dichotoom behandeld: de waarde "1" wordt voor die set var_inhoud getoond met het var-label
    
    if(length(var_inhoud) > 1){
      #Als er 1 var_inhoud wordt ingevoerd wordt de vraag als meerkeuzevraag behandeld:
      #Alle waarden worden getoond voor die var_inhoud met het val-label
      
      lapply(var_inhoud, function(y){
        
        #kruistabel maken voor 1 variabele; alle waarden tonen
        
        kruistabel_df <- bereken_kruistabel(
          data_temp,
          variabele = y,
          crossing = var_crossing,
          survey_design = design_temp,
          min_observaties_per_vraag = nvar,
          min_observaties_per_antwoord = ncel
          ) 
        
        #toevoeging jan 2025 tbv argument 'ongewogen'. 
        #variabele aantal_groep maken . zodat ongewogen aantal berekend kan worden.
        
        #groepeervar: variabele aanmaken die ofwel var_crossing vastlegt; danwel arbitraire waarde "1"
        #dit is nodig voor group_by() functie in pipe (Kan niet met NULL omgaan). 
        if(!is.null(var_crossing)){
          
          kruistabel_df$groepeervar <- kruistabel_df[[var_crossing]]
          
        } else{
          
          kruistabel_df$groepeervar <- "1"
        }


        kruistabel_df %>%         
          group_by(groepeervar) %>%
          mutate(aantal_groep = sum(as.numeric(aantal_antwoord))) %>% #maak aantal groep
          ungroup() %>%
          filter(waarde == 1) %>% 
          mutate(niveau = niveau_label,
                 var_label = var_label(data[[y]]) %>% as.factor()  #Nieuwe variabele met varlabel var_inhoud; y
          ) %>% 
          select(-all_of(y))
      }) %>% do.call(rbind,.)
      
      
      
    } else {
      #kruistabel maken voor 1 variabele; alle waarden tonen
      
      kruistabel_df <- bereken_kruistabel(
        data_temp,
        variabele = var_inhoud,
        crossing = var_crossing,
        survey_design = design_temp,
        min_observaties_per_vraag = nvar,
        min_observaties_per_antwoord = ncel
        ) %>%
        mutate(niveau = niveau_label)
      
      #toevoeging jan 2025 tbv argument 'ongewogen'. 
      #variabele aantal_groep maken . zodat ongewogen aantal berekend kan worden.
      
      #groepeervar: variabele aanmaken die ofwel var_crossing vastlegt; danwel arbitraire waarde "1"
      #dit is nodig voor group_by() functie in pipe (Kan niet met NULL omgaan). 
      if(!is.null(var_crossing)){
        
        kruistabel_df$groepeervar <- kruistabel_df[[var_crossing]]
        
      } else{
        
        kruistabel_df$groepeervar <- "1"
      }
      
      kruistabel_df %>%         
        group_by(groepeervar) %>%
        mutate(aantal_groep = sum(as.numeric(aantal_antwoord))) %>% #maak aantal groep
        ungroup()
      
    }
    
  })  %>% do.call(rbind,.)
  
  #temp dataframe & design verwijderen uit globalEnv.
  rm(data_temp, design_temp, envir = .GlobalEnv)
  
  
  
  #Afhandelen van meerdere var_inhouds: var_inhoud wordt var_label
  if(length(var_inhoud) > 1){
    var_inhoud_plot <- "var_label"
  } else{
    var_inhoud_plot <- var_inhoud
  }
  
  #Afhandelen van var_inhoud_waarde
  if(!is.null(var_inhoud_waarde)){
    
    #Filteren op ingegeven waarde
    df_plot <- df_plot %>% filter(waarde == var_inhoud_waarde)
    
    if(nrow(df_plot) < 1){
      stop(
        glue(
          "Filter var_inhoud_waarde heeft alle data weggefilterd. Pas var_inhoud_waarde aan.
      waarden in var_inhoud '{var_inhoud}':{unique(data[[var_inhoud]]) %>% unname() %>% paste0(collapse = ',')}
      ingevuld bij var_inhoud_waarde: {var_inhoud_waarde}
      "))
    }
    
    #var_label ipv val_label toewijzen als var_inhoud
    #niet van toepassing bij meerdere niveaus (dan wordt var_label in plot gebruikt)
    if(length(var_inhoud) == 1){
      df_plot[[var_inhoud]] <- var_label(data[[var_inhoud]])
      
      
      
    }
  }
  
  #Afhandelen van niveaus & invoer crossing
  #Als er meerdere niveaus zijn ingegeven nemen niveaus de plek in van een crossing
  if(length(niveaus) > 1){
    
    var_crossing = "niveau"
    
  } else{
    #Als er geen crossing is ingevoerd: dummy crossing maken zodat plot met beide kan omgaan
    if(is.null(var_crossing)){
      df_plot$leeg = ""
      var_crossing = "leeg"
      remove_legend = T
    }
  }
  
  #voeg regeleinden toe als nodig:
  #x_as_label_wrap bepaald wanneer labels te lang zijn om naast elkaar te passen
  #x_as_regels_toeveogen bepaald of er regels toegevoegd moeten worden om plots die naast elkaar staan
  #uit te lijnen
  
  #als er geen waarde voor X_as_label_wrap is opgegeven, default instellen
  #obv flip
  #bij normnale orientatie moet er snel geknipt worden (weinig horizontale ruimte)
  #bij flip moet er minder snel geknipt worden (Weinig verticale ruimte)
  if(is.null(x_as_label_wrap)){
    x_as_label_wrap = ifelse(flip,40,20)
  }
  
  #Bepalen welke variabele in df_plot aangesproken moet worden om regeleinden in te voegen.
  #'var_label' als er meerdere variabelen bij var_inhoud zijn ingevoerd
  #'Var inhoud bij als er 1 variabele bij var_inhoud is ingevoerd.
  x_as_label <- ifelse(length(var_inhoud) > 1, "var_label",var_inhoud)
  
  #regeleinden invoegen
  df_plot[[x_as_label]] <- maak_regeleinden(df_plot[[x_as_label]], x_as_label_wrap, x_as_regels_toevoegen)
 
  #kleuren aan labels koppelen
  namen_kleuren <- unique(df_plot[[var_crossing]])
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  #als 1 var_inhoud: volgorde plot sorteren obv waarde
  if(length(var_inhoud) == 1){
    df_plot <- df_plot %>%
      mutate(!!sym(var_inhoud_plot) := fct_reorder(!!sym(var_inhoud_plot),
                                                   as.numeric(waarde),
                                                   .desc = aflopend)
      )
  } else{
  #anders sorteren obv volgorde van voorkomen in vector var_inhoud
    df_plot <- df_plot %>% 
      mutate(volgorde = match(varcode, var_inhoud), #wijs nummer toe o.b.v plaats in var-inhoud vector
             !!sym(var_inhoud_plot):= fct_reorder(!!sym(var_inhoud_plot),
                                                  volgorde,
                                                  .desc = aflopend)) 
    
    
  }
  
  
  #Niveau moet ook een gesorteerde factorvariabele worden zodat kleurcoderingen altijd kloppen
  #Anders krijg je alfabetische indeling v kleuren.
  
  if(var_crossing != "leeg"){
  df_plot <- df_plot %>% 
    mutate(volgorde = match(niveau, namen_kleuren), #volgorde van factorvar bepalen op volgorde namen_kleuren
           niveau = fct_reorder(niveau,volgorde))
  }
  
  #Als ongewogen = TRUE moeten ongewogen percentages uitgerekend worden. Dit is eigenlijk veel simpeler
  #en sneller door R berekend dan de gewogen data & zou idealiter ingebouwd worden op een manier waarbij R géén gewogen cijfers hoeft
  #te berekenen. Vanwege beperkte tijd & capiciteit halen we de ongewogen percentages uit de bestaande output uit de
  #functie die gewogen cijfers berekend in bereken_kruistabel().
  #we overschrijven de variabele percentage met een percentage o.b.v. aantal_antwoord
  if(ongewogen){
    df_plot <- df_plot %>% 
      mutate(
        percentage = case_when(
          is.na(weggestreept) ~ round(as.numeric(aantal_antwoord) / aantal_groep * 100, 0),
          TRUE ~ NA
          )
        )

    
  }
  
  
  plot =
    ggplot(df_plot) +
    geom_col(aes(x = !!sym(var_inhoud_plot), y = percentage, fill = !!sym(var_crossing)),
             position = position_dodge(width = 0.8), width = 0.8,
             na.rm = T) +
    geom_text(aes(x = !!sym(var_inhoud_plot),
                  y = percentage,
                  group = !!sym(var_crossing),
                  label =  paste0(percentage,"%"),
                  vjust = v_just_text,
                  hjust = h_just_text),
              position = position_dodge2(width = 0.8),
              size = geom_text_percentage, # Hier grootte van percentages aanpassen
              na.rm = T) +
    
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = !!sym(var_inhoud_plot), y = weggestreept, color = !!sym(var_crossing)),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
    ) +
    
    ggtitle(str_wrap(titel, 50)) +
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE,
                                           label.position = "right", title.position = "top")
    ) +
    
    #kleuren voor sterretje
    scale_color_manual(values= kleuren) + 
    
    scale_y_continuous(limits = c(0,max_as),
                       breaks = seq(0,100, by = 10),
                       labels = paste0(seq(0,100, by = 10),"%"),
                       expand = expansion(mult = c(0, 0.05))
    ) +
    #lange labels opsplitsen
    #   scale_x_discrete(labels = function(x) str_wrap(x, width = n_characters_str_wrap)) + 
    
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1,"cm"),
          legend.position = "bottom",
          axis.line.x.bottom = element_line(linewidth = 1),
          #Grootte tekst (behalve annotatie boven balken):
          text = element_text(family = font_family,
                              lineheight = line_height), 
          plot.title = element_text(hjust = .5, size = titel_size),
          axis.text = element_text(size = as_label_size),
          legend.text = element_text(size = legend_text_size),
          plot.caption = element_text(size = caption_size, hjust = 0.5)
    )
  

  #Aslabel verwijderen als als dat is aangegeven.
  if(!toon_aslabel){
    plot <- plot +  theme(axis.text = element_blank()) 
  }
  
  #toon y: laat Y as zien met rechte lijn + ticklabels
  if(toon_y){
    
    plot <-  plot +
      theme(
        axis.line.y.left = element_line(linewidth = 1)
      )
  } else {     #geen y-labels
    #dit is onnodig complex; heeft te maken met op laatste moment toon_y=FALSE optie
    #verzocht krijgen + flip van coordinaten
    if(flip){ #Als flip; axis text X verwijderen & X lijn verwijderen.
      plot <- plot +
        theme(
          axis.line.x.bottom = element_blank(),
          axis.text.x = element_blank(),
          axis.line.y = element_line(linewidth = 1) #en wel de y-lijn bewaren
        )
    } else{
      #Als geen-y-labels en geen flip:  hetzelfde doen maar dan op de y-as
      plot <- plot +
        theme(
          axis.line.y.left = element_blank(),
          axis.text.y = element_blank()
        )
    }
  }
  
  if(is.null(alt_text)){
    
    if(length(var_inhoud) > 1){
      
      
      alt_text <- maak_alt_text(data,
                                plot,
                                niveaus = niveaus,
                                label_inhoud = "",
                                label_crossings = var_label(data[[var_crossing]]),
                                vars_crossing = c("var_label",var_crossing)
      ) %>% 
        str_replace("voor de indicator ''","voor verschillende indicatoren") %>% 
        paste0(". ",caption)
      
      
    } else{
      
      alt_text <- maak_alt_text(data,
                                plot,
                                label_inhoud = var_label(data[[var_inhoud]]),
                                label_crossings = var_label(data[[var_crossing]]),
                                vars_crossing = c(var_crossing,var_inhoud),
                                niveaus = niveaus
      ) %>% 
        paste0(". ",caption)
    }
    
  }
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  #Legenda verwijderen als er geen crossings zijn.
  if(remove_legend){
    plot <- plot + theme(legend.position = "none")
  }
  
  if(flip){
    plot <- plot +
      
      coord_flip()
  }
  
  
  
  if(!tabel_en_grafiek){
    
    return(plot)
    
  } else {
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    grafiek = plot
    
    #tabel in elkaar zetten; als 
    #1 var_inhoud en titel niet is ingevuld: var_label
    #anders titel
    
    if(length(var_inhoud) == 1){
      label_inhoud = ifelse(nchar(titel) > 0,
                            titel,
                            var_label(data[[var_inhoud]])) #label inhoud var ophalen
      
    } else {
      label_inhoud = titel
    }
    
    tabel = plot$data %>% #data uit plot halen
      select(all_of(c(var_crossing, var_inhoud_plot)),
             percentage) %>% #crossing, var_inhoud & percentage bewaren
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      )
    
    #als er geen crossing is; alleen lege variabele verwijderen
    if(var_crossing == "leeg"){
      tabel <- tabel %>% select(-all_of(var_crossing))
      
    } else {
      #als er wel een crossing is; pivot_wider() crossing wordt kolom
      tabel <- tabel %>% 
        pivot_wider(names_from = !!sym(var_crossing), 
                    values_from = percentage) 
    }
    
    tabel <- tabel %>% 
      rename(" " = 1) %>% #header 
      gt() %>% #gt_tabel van maken 
      tab_header(label_inhoud) %>% #label inhoud als header
      tab_caption(caption)
    
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)
  }
  
  
  
  
}


#TODO Overal chekc inbouwen of een var wel een lbl+dbl is. Of niet afh. van maak_kruistabel() output.
maak_staafdiagram_uitsplitsing_naast_elkaar <- function(data, var_inhoud, var_crossings = NULL, titel = "", subtitel = FALSE,
                                                        kleuren = params$default_kleuren_grafiek,
                                                        kleuren_per_crossing = F, fade_kleuren = F,
                                                        flip = FALSE, 
                                                        nvar = params$default_nvar,
                                                        ncel = params$default_ncel,
                                                        alt_text = NULL,
                                                        caption = "",
                                                        huidig_jaar = 2024,
                                                        jaarvar = "AGOJB401",
                                                        niveaus = "regio",
                                                        tabel_en_grafiek = FALSE,
                                                        toon_y = FALSE,
                                                        x_as_label_wrap = NULL,
                                                        x_as_regels_toevoegen = 0,
                                                        max_as = 101
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  if(!labelled::is.labelled(data[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  if(val_labels(data[[var_inhoud]]) %>% length() > 2){
    warning(
      glue("{var_inhoud} is geen dichotome variabele. Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
    
    #TODO crossings ook valideren op aanwezigheid labels
  }
  
  #jaarvar kan hier niet een crossing zijn. 
  if(jaarvar %in% var_crossings){
    stop(glue("De jaarvariabele '{jaarvar}' kan geen kruisvariabele zijn in dit grafiektype.
              Kies een ander grafiektype of verwijder {jaarvar} uit var_crossings"))
    
  }
  
  #niveaus kan hier max length 1 hebben
  if(length(niveaus) > 1){
    stop(glue("Dit grafiektype kan op 1 niveau filteren er zijn meer niveaus ingevoerd.
              Voer maximaal 1 niveau in of kies een ander grafiektype.
              niveaus: {paste(niveaus,collapse = ',')}"))
  }
  
  # Bepaal kleuren
  kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  
  #o.b.v. de orientatie van de grafiek (flip = horizontaal)
  #De correctie van de geom_text aanpassen  zodat deze netjes in het midden v.e. balk komt 
  v_just_text = ifelse(flip,0.5,-1.5)
  h_just_text = ifelse(flip,-.5,0.5)
  
  
  #design en dataset bepalen o.b.v. regio
  if(niveaus == "nl"){
    
    design_x <- design_land
    subset_x <- data
    niveau_label <- "Nederland" # niveau voor subtitel
    
  } else if(niveaus == "regio"){
    
    design_x <- design_regio
    subset_x <- data %>% filter(GGDregio == params$regiocode)
    niveau_label <- val_label(data$GGDregio, params$regiocode) # niveau voor subtitel
    
  } else if (niveaus == "gemeente"){
    
    design_x <<- design_gem
    subset_x <<- data %>% 
      filter(Gemeentecode == params$gemeentecode) %>%
      filter(!is.na(Standaardisatiefactor_gemeente))
    niveau_label <- val_label(data$Gemeentecode, params$gemeentecode) # niveau voor subtitel
    
  } else{
    
    stop(glue("niveau bestaat niet
                niveau: {niveaus}"))
  }
  
  # niveau_label niet tonen als argument subtitel = FALSE
  if (subtitel == FALSE) {
    niveau_label = ""
  }
  
  #design altijd filteren op jaar in maak_staafdiagram_vergelijking
  design_jaar <<- subset(design_x, get(jaarvar) == huidig_jaar)
  #subset maken v design
  data_jaar <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
  
  
  #% voor iedere crossing appart uitrekenen.
  df_plot <- lapply(var_crossings, function(crossing){
    
    var_label_crossing = var_label(data_jaar[[crossing]])
    
    df_crossing <- bereken_kruistabel(data_jaar, variabele = var_inhoud, crossing = crossing,
                                      survey_design = design_jaar,
                                      min_observaties_per_vraag = nvar,
                                      min_observaties_per_antwoord = ncel
                                      
    ) %>% 
      filter(waarde == 1) %>% 
      rename(onderdeel = !!sym(crossing)) %>%  #crossinglevel naar 'onderdeel' hernoemen
      mutate(groep = factor(var_label_crossing),  #varlabel crossing als 'groep' toevoegen
             weggestreept = as.numeric(weggestreept)
      ) 
    
    #Als iedere crossing eigen kleuren moet hebben; kleuren toewijzen aan dataframe
    if(kleuren_per_crossing){
      n_crossing <- which(crossing == var_crossings)
      
      kleur_crossing = kleuren[n_crossing]
      
      #als de kleuren steeds lichter moeten worden binnen een crossing; palette maken
      #dat afloopt naar wit en hier een sample uit nemen
      if(fade_kleuren){
        
        kleur_fade <- colorRampPalette(c(kleur_crossing,"#FFFFFF"))
        n_kleuren <- nrow(df_crossing)
        
        kleuren_palet <- kleur_fade(n_kleuren + 4) #Als we niet + iets doen de laatste crossing wit.
        
        kleur_crossing <- kleuren_palet[1:n_kleuren]
      }
      
      df_crossing$kleur <- kleur_crossing 
    }
    
    
    
    df_crossing
    
  }) %>% do.call(rbind,.)
  
  #temp dataframe & design verwijderen uit globalEnv.
  rm(data_jaar, design_jaar, envir = .GlobalEnv)
  
  
  #Als alles leeg is: leeg plot teruggeven:
  if(all(is.na(df_plot$percentage))){
    warning(glue("Plot kan niet gemaakt worden! Te weinig observaties voor {var_inhoud}. De instellingen zijn: nvar = {nvar} en ncel = {ncel}"))
    
    #leeg plot tonen & functie vroegtijdig afbreken
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label =  glue("Onvoldoende respondenten in {niveaus}
                                voor grafiek: {str_wrap(titel, 40)}"),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.margin = margin(50, 50, 50, 50)) # Add margin to take up space)
    )
  }
  
  
  #voeg regeleinden toe als nodig:
  #x_as_label_wrap bepaald wanneer labels te lang zijn om naast elkaar te passen
  #x_as_regels_toeveogen bepaald of er regels toegevoegd moeten worden om plots die naast elkaar staan
  #uit te lijnen
  
  #als er geen waarde voor X_as_label_wrap is opgegeven, default instellen
  #obv flip
  #bij normnale orientatie moet er snel geknipt worden (weinig horizontale ruimte)
  #bij flip moet er minder snel geknipt worden (Weinig verticale ruimte)
  if(is.null(x_as_label_wrap)){
    x_as_label_wrap = ifelse(flip,50,10)
  }
  
  #regeleinden invoegen
  df_plot$onderdeel <- maak_regeleinden(df_plot$onderdeel, x_as_label_wrap, x_as_regels_toevoegen)
  
  
  if(kleuren_per_crossing){
    
    namen = df_plot$onderdeel
    kleuren = df_plot$kleur
    names(kleuren) <- namen
    
  } else{
    
    kleuren <- rep(kleuren[1],nrow(df_plot))
  }
  
  #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
  df_plot$groep <- factor(df_plot$groep)
  #volgorde onderdeel vastzetten o.b.v dataframe 
  df_plot$onderdeel <- factor(df_plot$onderdeel, levels = df_plot$onderdeel)
  
  #na.rm werkt niet bij geom_bar() met position_stack. Hierdoor komt er een warning.

  plot <- ggplot(df_plot) +
    geom_bar(aes(x = onderdeel, y = percentage, fill = onderdeel),
             stat = "identity", width = 0.8
    ) +
    geom_text(aes(x = onderdeel,
                  y = percentage,
                  label = paste0(percentage,"%"),
                  vjust = v_just_text,
                  hjust = h_just_text),
              color = "black",
              position = position_dodge2(width = 0.8),
              size = geom_text_percentage, # Hier grootte van percentages aanpassen
              na.rm = T
    ) +
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = onderdeel, y = weggestreept, color = onderdeel),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
    ) +
    
    #kleuren voor sterretjes
    scale_color_manual(values= kleuren) +
    #kleuren voor balken
    scale_fill_manual(values = kleuren) + 
    
    ggtitle(str_wrap(titel, 50), subtitle = niveau_label) +
    theme(panel.background = element_blank(),
          legend.position = "none",
          axis.line.y.left = element_line(linewidth = 1),
          #Grootte tekst (behalve annotatie boven balken):
          text = element_text(family = font_family,
                              lineheight = line_height), 
          plot.title = element_text(hjust = .5, size = titel_size),
          plot.subtitle = element_text(hjust = .5, size = (titel_size - 5), face = "italic"),
          axis.text = element_text(size = as_label_size),
          legend.text = element_text(size = legend_text_size),
          plot.caption = element_text(size = caption_size, hjust = 0.5)
    )
  
  
  if(flip){
    plot <- plot + 
      scale_y_continuous(limits = c(0,max_as),
                         expand = expansion(mult = c(0, 0.05))) +
      # scale_x_discrete(labels = function(x) str_wrap(x,width = 50)) +
      coord_flip() +
      theme(axis.title = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x =  element_blank(),
      )
  } else{
    plot <- plot + 
      theme(axis.line.x.bottom = (element_line(linewidth = 1))) +
      scale_y_continuous(limits = c(0,max_as),
                         expand = expansion(mult = c(0, 0))) +
      #  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(
        strip.background = element_blank(),
        
      ) + xlab("")
    
  }
  
  #bij flip is er al geen as met percentages meer
  #alleen bij !flip de aslijn & tekst verwijderen wanner !toon_y
  if(!toon_y & !flip){
    plot <- plot + 
      theme(
        axis.line.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.y.left = element_blank()
      )
    
  }
  
  
  #Alt text maken o.b.v. data als geen eigen tekst is ingegeven
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(data, plot, niveaus = niveaus,
                              label_inhoud = var_label(data[[var_inhoud]]),
                              label_crossings = sapply(var_crossings, function(var) var_label(data[[var]]))
    ) %>% 
      paste0(". ",caption)
    
  } 
  
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  
  if(!tabel_en_grafiek){
    
    return(plot)
    
  } else {
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    grafiek = plot
    
    #tabel in elkaar zetten; als titel is ingevuld; titel anders var-label
    
    label_inhoud = ifelse(nchar(titel) > 0,
                          titel,
                          var_label(data[[var_inhoud]])) #label inhoud var ophalen
    
    tabel = plot$data %>% #data uit plot halen
      select(onderdeel, percentage) %>% #groepnaam & percentage bewaren
      rename("groep" = onderdeel) %>% #hernoemen variabele
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      ) %>%
      gt() %>% #gt_tabel van maken 
      tab_header(label_inhoud) %>%  #label inhoud als header
      tab_caption(caption)
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)
  }
  
  
}

#horizontaal gestapeld staafdiagram
maak_staafdiagram_gestapeld <- function(data, var_inhoud, var_crossing = NULL, titel = "", 
                                        subtitel = "",
                                        kleuren = params$default_kleuren_grafiek, x_label = "",
                                        nvar = params$default_nvar, ncel = params$default_ncel,
                                        alt_text = NULL,
                                        caption = "",
                                        huidig_jaar = 2024,
                                        jaarvar = "AGOJB401",
                                        niveaus = "regio",
                                        tabel_en_grafiek = FALSE,
                                        toon_y = FALSE,
                                        x_as_label_wrap = 20,
                                        x_as_regels_toevoegen = 0,
                                        aflopend = FALSE,
                                        max_as = 101
){
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  #TODO
  #nu wordt de hele grafiek uitgezet zodra 1 percentage te laag is. bij gebruik var_crossing
  #kan het nuttig zijn alleen de crossings met te lage percentages te verwijderen en de grafiek toch
  #te tonen. dan wel zorgen dat er warning wordt gemaakt.
  
  
  if(!labelled::is.labelled(data[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  
  
  #Als een crossing is opgegeven kunnen er niet meerdere niveaus opgegeven worden
  if(!is.null(var_crossing) & length(niveaus) > 1){
    stop(glue("
    Foute invoer:
    var_crossing is ingevuld en er zijn meerdere niveaus opgegeven.
    Die combinatie is niet mogelijk voor dit grafiektype.
    valide invoer is:
    - lege var_crossing en meerdere niveaus
    - var_crossing ingevuld en 1 niveau
    
    Pas de invoer aan of kies een ander grafiektype
    var_crossing: {var_crossing}
    niveaus: {paste(niveaus,collapse = ', ')}
              "))
  }
  
  # Bepaal kleuren
  if (length(niveaus) > 1 ) {
    kleuren
  } else {
    kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  }
  
  
  #Checken of crossvar is ingevoerd en in dat geval jaarvar is
  crossing_is_jaar <- ifelse(is.null(var_crossing), FALSE,
                             ifelse(var_crossing != jaarvar, FALSE, TRUE))
  
  #Loop over alle niveaus & bereken kruistabel per niveau en evt. jaaruitsplitsing. sla op in df.
  df_plot <- lapply(niveaus, function(x){
    
    #design en dataset bepalen o.b.v. niveau
    if(x == "nl"){
      design_x <- design_land
      subset_x <- data
      niveau_label <- "Nederland"
      
    } else if(x == "regio"){
      design_x <- design_regio
      subset_x <- data %>% filter(GGDregio == params$regiocode)
      niveau_label <- val_label(data$GGDregio, params$regiocode)
      
    } else if (x == "gemeente"){
      design_x <- design_gem
      subset_x <- data %>% 
        filter(Gemeentecode == params$gemeentecode) %>%
        filter(!is.na(Standaardisatiefactor_gemeente))
      niveau_label <- val_label(data$Gemeentecode, params$gemeentecode)
      
    } else{
      
      stop(glue("niveau bestaat niet
                niveau: {paste(niveaus,collapse = ',')}"))
    }
    
    #niet filteren als jaar als crossing is geselecteerd
    if(crossing_is_jaar){
      
      #niet filteren 
      data_temp <<- subset_x
      design_temp <<- design_x 
    } else{
      #standaard alleen laatste jaar overhouden
      #subset data
      design_temp <<- subset(design_x, get(jaarvar) == huidig_jaar)
      #subset maken v design
      data_temp <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
    }
    
    #kruistabel maken (gaat automatisch zonder crossing als meerdere niveaus zijn opgegeven)
    #crossing is dan namelijk nog steeds default: null
    df_plot <- bereken_kruistabel(data_temp, variabele = var_inhoud,
                                  crossing = var_crossing, survey_design = design_temp,
                                  min_observaties_per_vraag = nvar,
                                  min_observaties_per_antwoord = ncel
    ) %>% 
      mutate(niveau = niveau_label)
    
  })  %>% do.call(rbind,.)
  
  
  #temp dataframe & design verwijderen uit globalEnv.
  rm(design_temp, data_temp, envir = .GlobalEnv)
  
  
  #regeleinden invoegen
  df_plot[[var_inhoud]] <- maak_regeleinden(df_plot[[var_inhoud]],x_as_label_wrap, x_as_regels_toevoegen)
  
  
  #als meerde niveaus zijn var_crossing = niveau
  if(length(niveaus) > 1){
    
    var_crossing = "niveau"
    
  } else{
    #Als crossing niet is ingevuld; dummy crossing maken zodat plot met beide kan omgaan
    if(is.null(var_crossing)){
      df_plot$leeg = ""
      var_crossing = "leeg"
      remove_legend = T
    }
  }
  
  #Als er een te lage N is: plot kan niet gemaakt worden. 
  #TODO checken of een leeg plot de standaardrapportage niet verpest
  if(any(is.na(df_plot$percentage))){
    warning(glue("Plot kan niet gemaakt worden! Te weinig observaties voor {var_inhoud}. De instellingen zijn: nvar = {nvar} en ncel = {ncel}"))
    
    #leeg plot tonen & functie vroegtijdig afbreken
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label =  glue("Onvoldoende respondenten in {niveaus}
                                voor grafiek: {str_wrap(titel, 40)}"),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.margin = margin(50, 50, 50, 50)) # Add margin to take up space)
    )
  }
  
  #factor levels van var_inhoud goed zetten o.b.v. variabele waarde
  df_plot <- df_plot %>% 
    mutate(!!sym(var_inhoud) := fct_reorder(!!sym(var_inhoud),
                                            as.numeric(waarde),
                                            .desc = !aflopend) #logica omdraaien. bij aflopend willen we laagste waarde links hebben.
           )
  
  namen_kleuren <- levels(df_plot[[var_inhoud]])
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  plot <- ggplot(df_plot, aes(x = percentage, y = !!sym(var_crossing),
                              fill = !!sym(var_inhoud))) + 
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(
      label = paste0(round(percentage),"%")),
      color = "#FFFFFF",
      position = position_stack(vjust = 0.5),
      size = geom_text_percentage) + # Hier grootte van percentages aanpassen
    
    scale_fill_manual(values= kleuren) +
    scale_x_continuous(
      limits = c(0,max_as),
      breaks = seq(0,101, by = 10),
      labels = paste0(seq(0,100, by = 10),"%"),
      expand = expansion(mult = c(0, 0.05)))+
    ggtitle(str_wrap(titel, 50), subtitle = subtitel) +
    ylab(x_label) + 
    theme(
      axis.title = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      legend.spacing.x = unit(.1,"cm"),
      legend.position = "bottom",
      axis.line.y.left = element_line(linewidth = 1),
      legend.justification = c(1,0), 
      #Grootte tekst (behalve annotatie boven balken):
      text = element_text(family = font_family,
                          lineheight = line_height), 
      plot.title = element_text(hjust = .5, size = titel_size),
      plot.subtitle = element_text(hjust = .5, size = (titel_size - 5), face = "italic"),
      axis.text = element_text(size = as_label_size),
      legend.text = element_text(size = legend_text_size),
      plot.caption = element_text(size = caption_size, hjust = 0.5)
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  if(toon_y){
    plot <- plot +
      theme(
        axis.line.x.bottom = element_line(linewidth = 1)
      )
    
  } else{
    
    plot <- plot +
      theme(
        axis.text.x = element_blank()
      )
    
  }
  
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(data, plot, niveaus = niveaus,
                              type_grafiek = "gestapeld staafdiagram",
                              label_inhoud = var_label(data[[var_inhoud]]),
                              label_crossings = var_label(data[[var_crossing]]),
                              vars_crossing = c(var_crossing,var_inhoud)) %>% 
      paste0(". ",caption)
    
  }
  
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  if(!tabel_en_grafiek){
    
    return(plot)
    
  } else {
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    grafiek = plot
    
    #tabel in elkaar zetten; als titel is ingevuld; titel anders var-label
    
    label_inhoud = ifelse(nchar(titel) > 0,
                          titel,
                          var_label(data[[var_inhoud]])) #label inhoud var ophalen
    
    tabel <- plot$data %>% #data uit plot halen
      select(all_of(c(var_inhoud,var_crossing)), percentage) %>% 
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      )
    
    #als er geen crossing is; alleen lege variabele verwijderen
    if(var_crossing == "leeg"){
      tabel <- tabel %>% select(-all_of(var_crossing))
      
    } else {
      #als er wel een crossing is; pivot_wider() crossing wordt kolom
      tabel <- tabel %>% 
        pivot_wider(names_from = !!sym(var_crossing), 
                    values_from = percentage) 
    }
    
    tabel <- tabel %>% 
      rename(" " = 1) %>% #header 
      gt() %>% #gt_tabel van maken 
      tab_header(label_inhoud) %>%  #label inhoud als header
      tab_caption(caption)
    
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)
    
  }
  
  
  
}

#TODO titel tekst kan wegvallen bij Cirkeldiagram

maak_cirkeldiagram <- function(data, var_inhoud, titel = "", kleuren = params$default_kleuren_grafiek,
                               nvar = params$default_nvar, ncel = params$default_ncel, alt_text = NULL,
                               niveaus = "regio", huidig_jaar = 2024, jaarvar = "AGOJB401",
                               caption = "",
                               aflopend = FALSE,
                               tabel_en_grafiek = FALSE,
                               x_as_label_wrap = 50
) {
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  if(!labelled::is.labelled(data[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  
  #niveaus kan hier max length 1 hebben
  if(length(niveaus) > 1){
    stop(glue("Dit grafiektype kan op 1 niveau filteren. Er zijn meer niveaus ingevoerd.
              Voer maximaal 1 niveau in of kies een ander grafiektype.
              niveaus: {paste(niveaus,collapse = ',')}"))
  }
  
  # Bepaal kleuren
  kleuren <- bepaal_kleuren(kleuren = kleuren, niveaus = niveaus)
  
  #design en dataset bepalen o.b.v. regio
  if(niveaus == "nl"){
    
    design_x <- design_land
    subset_x <- data
    
  } else if(niveaus == "regio"){
    
    design_x <- design_regio
    subset_x <- data %>% filter(GGDregio == params$regiocode)
    
  } else if (niveaus == "gemeente"){
    
    design_x <<- design_gem
    subset_x <<- data %>% 
      filter(Gemeentecode == params$gemeentecode) %>%
      filter(!is.na(Standaardisatiefactor_gemeente))
    
  } else{
    
    stop(glue("niveau bestaat niet
                niveau: {niveaus}"))
  }
  
  #design altijd filteren op jaar in maak_staafdiagram_vergelijking
  design_temp <<- subset(design_x, get(jaarvar) == huidig_jaar)
  #subset maken v design
  data_temp <<- subset_x %>% filter(!!sym(jaarvar) == huidig_jaar)
  
  #kruistabel maken
  df_plot <- bereken_kruistabel(data_temp,
                                variabele = var_inhoud,
                                survey_design = design_temp,
                                min_observaties_per_vraag = nvar,
                                min_observaties_per_antwoord = ncel
  ) %>% 
    mutate(weggestreept = as.numeric(weggestreept)) 
  
  #temp design verwijderen uit globalEnv.
  rm(design_temp, data_temp, envir = .GlobalEnv)
  
  #regeleinden IN val-labels toevoegen zodat ze niet te lang op 1 regel lopen ze
  df_plot <- df_plot %>% mutate(
    !!sym(var_inhoud) := str_wrap(!!sym(var_inhoud),x_as_label_wrap) 
  )
  
  
  #Als er een percentage ontbreekt door te weinig obs; laten zien dat dit zo is
  #functie eerder afbreken
  if(any(is.na(df_plot$percentage))){
    
    warning(glue("Plot kan niet gemaakt worden! Te weinig observaties voor {var_inhoud}. De instellingen zijn: nvar = {nvar} en ncel = {ncel}"))
    
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label =  gglue("Onvoldoende respondenten in {niveaus}
                                voor grafiek: {str_wrap(titel, 40)}"),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.margin = margin(50, 50, 50, 50)) # Add margin to take up space)
    )
  }
  
  
  #volgorde levels cirkeldiagram bepalen. parameter desc bepaald richting.
  df_plot <- df_plot %>%
    mutate(!!sym(var_inhoud) := fct_reorder(!!sym(var_inhoud),
                                            as.numeric(waarde),
                                            .desc = !aflopend) #logica omdraaien. bij clockwise; Laagste waarde is eerste stuk v/d klok
           )
  
  #kleuren labelen o.b.v. levels var_inhoud
  namen_kleuren <- levels(df_plot[[var_inhoud]])
  
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  if(!aflopend){ #kleuren omdraaien bij aflopend
    kleuren <- rev(kleuren)
  }
  
  
  # GGPLOT PIE CHART
  plot <- ggplot(df_plot, aes(x = "", y = percentage, fill = !!sym(var_inhoud))) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(percentage,"%")),
              position = position_stack(vjust = 0.5),
              color = "#FFFFFF",
              size = geom_text_percentage) +
    coord_polar(theta = "y") +
    ggtitle(str_wrap(titel, 50)) +
    scale_fill_manual(
      str_wrap(var_label(data[[var_inhoud]]),50), #var_label is naam legend
      label = str_wrap(namen_kleuren,x_as_label_wrap), #legend key labels; val_labels
      values = kleuren,
      guid = guide_legend(reverse = T)
      ) + 
    theme_void() +
    theme(
      #Grootte tekst (behalve annotatie boven balken):
      text = element_text(family = font_family,
                          lineheight = line_height),
      plot.title = element_text(hjust = .5, size = titel_size),
      #We willen geen axis tekst bij een cirkeldiagram
      #axis.text = element_text(size = as_label_size),
      legend.text = element_text(size = legend_text_size_cirkel),
      legend.title = element_text(size = legend_title_size_cirkel),
      plot.caption = element_text(size = caption_size, hjust = 0.5)
    )
  
  
  #TODO Alt text logica updaten omdat we nu niet meer ploty maar ggplot2 gebruiken
  #TODO Hopelijk besluiten dat de alt-text logica volledig weggegooid mag wordne.
  #alt text toevoegen als deze niet handmatig is toegevoegd
  #kan niet met maak_alt_text omdat hier plotly gebruikt wordt ipv ggplot
  if(is.null(alt_text)){
    
    doelgroep = "jongvolwassenen"
    onderwerp = var_label(data[[var_inhoud]])
    
    waarden = paste0(
      df_plot[[var_inhoud]], ": ",
      df_plot$percentage, "%",
      collapse = ", "
    ) 
    
    
    alt_text <- glue("Cirkeldiagram voor de indicator '{onderwerp}' bij {doelgroep}: {waarden}") %>% 
      paste0(". ",caption)
    
  }
  
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  if(!tabel_en_grafiek){
    #alleen plot wanneer tabel niet gevraagd is
    plot
  } else{
    
    #Als gekozen is beide een grafiek en tabel in 'tabset panels' te plaatsen
    #tbv digitoegankelijkheid; maak een leesbare tabel
    
    
    #tabel in elkaar zetten; als titel is ingevuld; titel anders var-label
    label_inhoud = ifelse(nchar(titel) > 0,
                          titel,
                          var_label(data[[var_inhoud]])) #label inhoud var ophalen
    
    tabel <- plot$data %>% #data uit plot halen
      select(all_of(var_inhoud), percentage) %>% 
      mutate(
        percentage = case_when(  
          is.na(percentage) ~ "*",#* toevoegen als te weinig obs 
          TRUE ~ paste0(percentage, "%") #% teken toevoegen
        )
      )  %>% 
      rename(" " = 1) %>% #header 
      gt() %>% #gt_tabel van maken 
      tab_header(label_inhoud) %>%  #label inhoud als header
      tab_caption(caption)
    
    #genereer markdown syntax om plot & grafiek heen voor panel tabset
    maak_panel_tabset(plot,tabel)    
    
  }

}

bol_met_cijfer <- function(getal, omschrijving = NA, omschrijving2 = NA, niveau = NA,
                           kleur = params$default_kleuren_grafiek[c(6, 4, 11)], kleur_outline = "#FFFFFF", 
                           kleur_getal = "#FFFFFF", kleur_omschrijving = "#305A5B"){
  
  # Check input
  if(length(getal) < 1 | length(getal) > 2 ) {
    
    stop(glue("
    Er is geen getal of meer dan 2 getallen ingevoerd. Dit kan niet.
    Vul 1 of 2 getallen in als input.
    niveaus: {paste(getal, collapse = ',')}"))
    
  }
  
  alt_tekst <- getal
  
  # Maak een stukje svg code aan waarin de ingevoerde tekst is opgenomen en een variabele voor het aanvullen van de alternatieve tekst
  tekst <- c()
  
  for (regel in c(omschrijving, omschrijving2)){
    if(!is.na(regel)){
      
      alt_tekst <- paste(alt_tekst, regel)
      
      if(length(tekst) == 0){
        
        tekst <- paste0('<tspan>', regel, '</tspan>')
        
      } else if (length(tekst) > 0){
        
        tekst <- paste0(tekst, '<tspan x=112 text-anchor="middle" dy="1em">', regel, '</tspan>')
        
      }
    } else {
      
      # Als geen omschrijving meegegeven is, voer dan spatie in
      if(length(tekst) == 0){
        
        tekst <- paste0('<tspan>', " ", '</tspan>')
        
      } else if (length(tekst) > 0){
        
        tekst <- paste0(tekst, '<tspan x=112 text-anchor="middle" dy="1em">', " ", '</tspan>')
        
      }
    }
  }
  
  # Voeg niveau toe aan alt_tekst en bepaal kleur
  if(any(!is.na(niveau))) {
    
    alt_tekst <- paste(alt_tekst, "in de", niveau)
    
    # kleur <- ifelse(niveau == "Gemeente", kleur[1],
    #                 ifelse(niveau == "Regio", kleur[2],
    #                        kleur[3]))
    # 
  } else {
    
    # Als geen niveau meegegeven is, voer dan spatie in
    niveau = " "
    
    kleur <- kleur[3]
    
  }
  
  if (is.na(omschrijving) & is.na(omschrijving) & length(getal) == 1) {
    # Als geen omschrijvingen en 1 getal, maak dan de afbeelding kleiner.
    
    viewbox = "0 0 50 75"
    
    # Voeg de ingevoerde informatie op de juiste plekken in de svg code met behulp van glue
    svg_code <- glue::glue('<svg role="img" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" style="shape-rendering:geometricPrecision; text-rendering:geometricPrecision; image-rendering:optimizeQuality;"
                viewBox="{viewbox}"> # Origineel 0 0 225 75
                <title>{alt_tekst}</title>
                
                <g id="circle">
                    <circle style="fill:{kleur};" cx="25" cy="25" r="25" stroke = "{kleur_outline}">
                    </circle>
                    <text x=25 y="25" text-anchor="middle" fill="{kleur_getal}" stroke="{kleur_getal}" stroke-width="1px" dy=".3em" font-size="1em">{getal}</text>
                </g>
                
                <g id="niveau">
                <text x=25 y="60" text-anchor="middle" fill="{kleur}" stroke="{kleur}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-weight = "bold">{niveau}</text>
                </g>
                
                </svg>')
    
  } else {
    # Grotere viewbox bij omschrijving of 2 getallen en geen omschrijving
    
    viewbox = "0 0 225 100"
    
    # svg-code afhankelijk van 1 of 2 getallen als input
    if (length(getal) == 1) {
      # svg_code voor 1 getal
      # Voeg de ingevoerde informatie op de juiste plekken in de svg code met behulp van glue
      svg_code <- glue::glue('<svg role="img" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" style="shape-rendering:geometricPrecision; text-rendering:geometricPrecision; image-rendering:optimizeQuality;"
                viewBox="{viewbox}"> # Origineel 0 0 225 75
                <title>{alt_tekst}</title>
                
                <g id="circle">
                    <circle style="fill:{kleur};" cx="112" cy="25" r="25" stroke = "{kleur_outline}">
                    </circle>
                    <text x=112 y="25" text-anchor="middle" fill="{kleur_getal}" stroke="{kleur_getal}" stroke-width="1px" dy=".3em" font-size="1em">{getal}</text>
                </g>
                
                <g id="niveau">
                <text x=112 y="60" text-anchor="middle" fill="{kleur}" stroke="{kleur}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-weight = "bold">{niveau}</text>
                </g>
                
                <g id="tekst">
                <text x=112 y="80" text-anchor="middle" fill="{kleur_omschrijving}" stroke="{kleur_omschrijving}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-style = "italic">{tekst}</text>
                </g>
                
                </svg>')
      
    } else {
      # alt_tekst in één zin zetten
      alt_tekst = paste0(alt_tekst[1], ". ", alt_tekst[2])
      
      # svg_code voor 2 getallen
      # Voeg de ingevoerde informatie op de juiste plekken in de svg code met behulp van glue
      svg_code <- glue::glue('<svg role="img" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" style="shape-rendering:geometricPrecision; text-rendering:geometricPrecision; image-rendering:optimizeQuality;"
                viewBox="{viewbox}"> # Origineel 0 0 225 75
                <title>{alt_tekst}</title>
                
                <g id="circle">
                    <circle style="fill:{kleur[1]};" cx="75" cy="25" r="25" stroke = "{kleur_outline}">
                    </circle>
                    <text x=75 y="25" text-anchor="middle" fill="{kleur_getal}" stroke="{kleur_getal}" stroke-width="1px" dy=".3em" font-size="1em">{getal[1]}</text>
                </g>
                
                <g id="niveau">
                <text x=75 y="60" text-anchor="middle" fill="{kleur[1]}" stroke="{kleur[1]}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-weight = "bold">{niveau[1]}</text>
                </g>
                
                <g id="circle">
                    <circle style="fill:{kleur[2]};" cx="150" cy="25" r="25" stroke = "{kleur_outline}">
                    </circle>
                    <text x=150 y="25" text-anchor="middle" fill="{kleur_getal}" stroke="{kleur_getal}" stroke-width="1px" dy=".3em" font-size="1em">{getal[2]}</text>
                </g>
                
                <g id="niveau">
                <text x=150 y="60" text-anchor="middle" fill="{kleur[2]}" stroke="{kleur[2]}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-weight = "bold">{niveau[2]}</text>
                </g>
                
                <g id="tekst">
                <text x=112 y="80" text-anchor="middle" fill="{kleur_omschrijving}" stroke="{kleur_omschrijving}" stroke-width="0.01px" dy=".3em" font-size="0.5em" font-style = "italic">{tekst}</text>
                </g>
                
                </svg>')
      
    }
    
  }
  
  return(svg_code %>% knitr::raw_html())
  
}



maak_grafiek_cbs_bevolking <- function(data, gem_code = params$gemeentecode,
                                       crossing_cbs = "leeftijd",
                                       niveaus = c("nl","regio","gemeente"),
                                       missing_label = "Onbekend",
                                       missing_bewaren = TRUE,
                                       kleuren = params$default_kleuren_grafiek,
                                       titel = "",
                                       x_label = "",
                                       alt_text = NULL,
                                       caption = "",
                                       huidig_jaar = 2024,
                                       jaarvar = "AGOJB401",
                                       tabel_en_grafiek = FALSE,
                                       toon_y = FALSE
){
  
  
  #Kleuren valideren: zijn het allemaal geldige hexwaarden?
  if(!all(valideer_kleurvector(kleuren))){ #Als NIET alle kleurcodes valide zijn: print warning
    
    warning(
      glue("LET OP. Fout in kleuren. Controleer de hex-codes
           {paste(kleuren, collapse = ', ')}")
    )
  }
  
  #leeftijd en geslacht aan varnamen uit monitor koppelen
  crossing_monitor <- c("leeftijd" = "AGLFA401",
                        "geslacht" = "AGGSA401")
  
  #vector monitor crossvars filteren op invoer
  crossing_monitor <- crossing_monitor[names(crossing_monitor) %in% crossing_cbs]
  
  
  #cbs populatiedata lezen voor NL / regio / gem 
  cbs_regio <- cbs_populatie_opschonen(sheet = "Tabel1") %>%
    mutate(regio = ifelse(is.na(regio),"Totaal Nederland",regio)) %>% 
    filter(regio %in% c(params$regionaam,"Totaal Nederland"))
  
  cbs_gemeente <- cbs_populatie_opschonen(sheet = "Tabel2") %>%
    mutate(gemeentecode = str_extract(gemeentecode,"[:digit:]*") %>% as.numeric()) %>% 
    filter(gemeentecode == gem_code) %>% 
    select(-gemeentecode)

  # cbs_gemeente hernoemen als label van gemeente is aangepast
  cbs_gemeente$regio <- case_when(str_detect(val_label(data$Gemeentecode,gem_code), cbs_gemeente$regio) ~ val_label(data$Gemeentecode,gem_code),
                                  TRUE ~ cbs_gemeente$regio)
    
  cbs_populatiedata <- rbind(cbs_regio, cbs_gemeente) %>% 
    mutate(aantal = as.numeric(aantal))
  
  #data agregeren op crossing
  cbs_populatiedata <- cbs_populatiedata %>% group_by(regio, !!sym(crossing_cbs)) %>% 
    summarise(aantal = sum(aantal)) %>% 
    rename("crossing" = 2) %>% 
    mutate(type = "Bevolking")
  
  
  #response monitor ophalen voor crossing
  #aantal labels opslaan
  aantal_labels <- length(val_labels(data[[crossing_monitor]]))
  
  #Variabelen naar character omzetten
  data[[crossing_monitor]] <- labelled_naar_character(data, crossing_monitor)
  
  #data filteren op jaar
  data <- data %>% 
    filter(!!sym(jaarvar) == huidig_jaar)
  
  #Aantallen uitrekenen. Missing labelen als missing_label
  aantallen_regio <- data %>% 
    filter(GGDregio == params$regiocode) %>% 
    group_by(!!sym(crossing_monitor)) %>% 
    summarise(aantal = n()) %>%
    rename(crossing = 1) %>% 
    mutate(regio = params$regionaam,
           crossing = replace(crossing, is.na(crossing),missing_label),
           type = "Deelnemers"
    )  
  
  # Check kleine N
  if(sum(aantallen_regio$aantal) < params$default_nvar | any(aantallen_regio$aantal < params$default_ncel)){
    aantallen_regio$aantal <- NA
  } else if (nrow(aantallen_regio) != aantal_labels) { # Checken of er nog labels missen, dan zijn hebben deze groepen 0 respondenten en moeten dus niet getoond worden
    aantallen_regio$aantal <- NA
  }
  
  aantallen_gemeente <- data %>% 
    filter(Gemeentecode == gem_code) %>% 
    group_by(!!sym(crossing_monitor)) %>% 
    summarise(aantal = n()) %>%
    rename(crossing = 1) %>% 
    mutate(regio = val_label(data$Gemeentecode,gem_code),
           crossing = replace(crossing, is.na(crossing),missing_label),
           type = "Deelnemers"
    )
  
  # Check kleine N
  if(sum(aantallen_gemeente$aantal) < params$default_nvar | any(aantallen_gemeente$aantal < params$default_ncel)){
    aantallen_gemeente$aantal <- NA
  } else if (nrow(aantallen_gemeente) != aantal_labels) { # Checken of er nog labels missen, dan zijn hebben deze groepen 0 respondenten en moeten dus niet getoond worden
    aantallen_gemeente$aantal <- NA
  }
  
  aantallen_landelijk <-  data %>% 
    group_by(!!sym(crossing_monitor)) %>% 
    summarise(aantal = n()) %>%
    rename(crossing = 1) %>% 
    mutate(regio = "Totaal Nederland",
           crossing = replace(crossing, is.na(crossing),missing_label),
           type = "Deelnemers"
    )  
  
  # Check kleine N
  if(sum(aantallen_landelijk$aantal) < params$default_nvar | any(aantallen_landelijk$aantal < params$default_ncel)){
    aantallen_landelijk$aantal <- NA
  } else if (nrow(aantallen_landelijk) != aantal_labels) { # Checken of er nog labels missen, dan zijn hebben deze groepen 0 respondenten en moeten dus niet getoond worden
    aantallen_landelijk$aantal <- NA
  }

  
  #regio en gemeente responsedata samenvoegen  
  monitor_responsedata <- rbind(aantallen_regio, aantallen_gemeente, aantallen_landelijk) %>% 
    mutate(crossing = str_remove(crossing," jaar") %>% #" jaar" verwijderen uit lft crossing van monitordata
             tolower()) %>% #alles tolower om verschil man Man vrouw Vrouw weg te strepen.
    select(regio, crossing, aantal, type) %>%
    drop_na(aantal) # Verwijder rijen zonder aantallen 
  
  #als missing_bewaren FALSE is; verwijderen
  if(!missing_bewaren){
    monitor_responsedata <- monitor_responsedata %>% filter(crossing != missing_label)
  }
  
  df_plot <- rbind(cbs_populatiedata,monitor_responsedata) %>% #data samenvoegen 
    #% berekenen 
    mutate(label = paste(type,regio)) %>%
    group_by(label) %>% 
    mutate(percentage = aantal/sum(aantal)*100)
  
  #filteren op opgevraagde niveaus
  #named vector maken die inhoud regiovar in df_plot koppelt aan vector niveaus()
  niveaus_plot = c("nl" = "Totaal Nederland",
                   "regio" = params$regionaam,
                   "gemeente" = aantallen_gemeente$regio %>% unique)
  #named vector filteren op ingevoerde niveaus
  niveaus_plot <- niveaus_plot[names(niveaus_plot) %in% niveaus]
  #dataframe filteren
  df_plot <- df_plot %>% filter(regio %in% niveaus_plot)
  
  #Aantal in te voeren kleuren baseren op aantal niveaus crossing
  namen_kleuren <- unique(df_plot$crossing)
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  # gewenste volgorde van weergave in plot:
  # type > niveau
  #- type = Bevolking, deelnemers
  #- niveau = Nederland, regio, gem
  
  #vector maken om factor levels vast te zetten
  type = c("Bevolking","Deelnemers")
  niveau = unname(niveaus_plot)
  #alle combinaties type niveau maken
  levels_factor = expand.grid(type,niveau) %>% 
    mutate(label = paste(Var1, Var2)) %>% 
    #filteren zodat alleen bestaande data in factor lvls komt
    filter(label %in% df_plot$label) %>% 
    pull(label)
  
  #factorvariabele maken van df_plot$label
  df_plot$label <- factor(df_plot$label, levels = rev(levels_factor)) 
  
  #volgorde crossings ook vastzetten
  df_plot$crossing <- factor(df_plot$crossing,
                             levels = df_plot$crossing %>% unique() %>% rev() #factor van maken en omdraaien.
                             )
  
  plot <-  ggplot(df_plot, aes(x = percentage, y = label, fill = crossing)) + 
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(
      label = paste0(round(percentage),"%")),
      color = "#FFFFFF",
      position = position_stack(vjust = 0.5),
      size = geom_text_percentage) + #Hier grootte percentages aanpassen
    
    scale_fill_manual(values= kleuren,
                      labels = function(x) str_wrap(x, width = 40)
    ) +
    scale_x_continuous(
      limits = c(0,101),
      breaks = seq(0,101, by = 25),
      labels = paste0(seq(0,100, by = 25),"%"),
      expand = expansion(mult = c(0, 0.05)))+
    ggtitle(str_wrap(titel, 50)) + 
    ylab(x_label) + 
    theme(
      axis.title = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      legend.spacing.x = unit(.1,"cm"),
      legend.position = "bottom",
      legend.justification = c(1,0), 
      axis.line.y.left = element_line(linewidth = 1),
      #Grootte tekst (behalve annotatie boven balken):
      text = element_text(family = font_family,
                          lineheight = line_height), 
      plot.title = element_text(hjust = .5, size = titel_size),
      axis.text = element_text(size = as_label_size),
      legend.text = element_text(size = legend_text_size),
      plot.caption = element_text(size = caption_size, hjust = 0.5)
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  
  if(toon_y){
    
    plot <- plot +
      theme(
        axis.line.x.bottom = element_line(linewidth = 1),
      )
  } else{
    plot <- plot + 
      theme(
        axis.text.x = element_blank()
      )
    
  }
  
  
  if(is.null(alt_text)){
    # maak_alt_text() niet geschikt omdat t ook andere data dan monitordata betreft
    df_sorted <- df_plot %>% 
      arrange(desc(label)) #plot sorteren op labelvolgorde in plot
    
    onderdeel_labels = df_sorted$label %>% unique() %>% as.character() #vector van alle onderdelen
    
    #per onderdeel; zet leeftijd bijbehored percentage naast elkaar
    str_waarden <- lapply(onderdeel_labels, function(x){
      
      df_onderdeel <- df_sorted %>% filter(label == x)
      
      waarde_labels = paste0(df_onderdeel$crossing, ": ",
                             round(df_onderdeel$percentage,0),"%",
                             collapse = ", ")
      
      paste0(x, ": ", waarde_labels)
      
      
    }) %>% unlist() %>% 
      paste0(collapse = ", ")
    
    #alt text construeren v onderdelen
    alt_text <- paste0(
      "gestapeld staafdiagram met percentages per ",
      crossing_cbs," in de bevolking en bij deelnemers: ",
      str_waarden
    ) %>% 
      paste0(". ",caption)
    
    
  }
  
  plot <- plot + labs(alt = alt_text,
                      caption = caption)
  
  
  #Hier panel tabset logica;
  
  if(!tabel_en_grafiek){
    
    plot
  } else {
    
    tabel = plot$data %>%
      ungroup() %>% 
      select(-c(.group, type, aantal, regio)) %>% 
      mutate(percentage = round(percentage,1)) %>% 
      pivot_wider(names_from = crossing, 
                  values_from = percentage) %>% 
      arrange(label) %>%
      mutate(label = as.character(label)) %>% 
      rename(" " = 1) %>% 
      gt() %>% 
      tab_header(glue("Percentages in populatie en deelnemers per {var_label(data[[crossing_monitor]])}")) %>% 
      tab_caption(caption)
    
    
    
    maak_panel_tabset(plot, tabel)
  }

}


# Tekstfuncties -----------------------------------------------------------
maak_vergelijking <- function(data, var_inhoud, variabele_label = NULL, 
                              var_crossing = NULL, value = 1, niveaus = "regio",
                              huidig_jaar = 2024, var_jaar = "AGOJB401", met_percentage_huidig = FALSE,
                              met_percentage_vorig_bij_gelijk_gebleven = FALSE) {
  
  # Check input: Als var_crossing is ingevuld, dan mag maar één niveau ingevuld zijn
  if(!is.null(var_crossing) & length(niveaus) > 1){
    stop(glue("
    Tekstfunctie kan niet met meerdere niveaus & een crossing werken.
    Selecteer 1 niveau of verwijder var_crossing.
    niveaus: {paste(niveaus, collapse = ',')}
    crossing: {var_crossing}"))
  }
  
  # Check input: Als var_crossing niet is ingevuld, dan moeten meerdere niveaus ingevuld zijn
  if(is.null(var_crossing) & length(niveaus) == 1){
    stop(glue("
    Er is geen vergelijking ingevuld. Vul een crossing of meerdere niveaus in voor de vergelijking.
    Selecteer 1 niveau of verwijder var_crossing.
    niveaus: {paste(niveaus, collapse = ',')}
    crossing: NULL"))
  }
  
  # Checken of var_crossing is ingevoerd en in dat geval var_jaar is
  crossing_is_jaar <- ifelse(is.null(var_crossing), FALSE,
                             ifelse(var_crossing != var_jaar, FALSE, TRUE))
  
  #Loop over alle niveaus & bereken kruistabel per niveau en evt. jaaruitsplitsing. sla op in df.
  result <- lapply(niveaus, function(x){
    
    # Design en dataset bepalen o.b.v. niveau
    if(x == "nl"){
      
      design_x <- design_land
      subset_x <- data
      
    } else if(x == "regio"){
      
      design_x <- design_regio
      subset_x <- data %>% filter(GGDregio == params$regiocode)
      
    } else if (x == "gemeente"){
      
      design_x <- design_gem
      subset_x <- data %>% 
        filter(Gemeentecode == params$gemeentecode) %>%
        filter(!is.na(Standaardisatiefactor_gemeente))
      
    } else {
      
      stop(glue("niveau bestaat niet
              niveau: {x}"))
      
    }
    
    # Niet filteren als jaar als crossing is geselecteerd
    if(crossing_is_jaar){
      
      data_x <<- subset_x
      design_temp <<- design_x 
      
    } else {
      
      # Standaard alleen laatste jaar overhouden
      # Subset maken van design
      design_temp <<- subset(design_x, get(var_jaar) == huidig_jaar)
      
      # Subset maken van data
      data_x <<- subset_x %>% filter(!!sym(var_jaar) == huidig_jaar)
      
    }
    
    #Als er 1 niveau is, is er ook een crossing: bereken_kruistabel met crossing
    if(length(niveaus) == 1){
      
      # Bereken gewogen cijfers
      bereken_kruistabel(data = data_x, 
                         survey_design = design_temp, 
                         variabele = var_inhoud, 
                         crossing = var_crossing) %>%
        mutate(niveau = x) %>% 
        filter(waarde %in% value) # Filter de gegevens voor value eruit. Standaard is dit 1.
      
      #Anders moet voor elk niveau in de lapply een kruistabel zonder crossing uitgerekend worden
    } else {
      
      # Bereken gewogen cijfers
      bereken_kruistabel(data = data_x, 
                         survey_design = design_temp, 
                         variabele = var_inhoud) %>%
        mutate(niveau = x) %>% 
        filter(waarde %in% value) # Filter de gegevens voor value eruit. Standaard is dit 1.
      
    }
  })  %>% do.call(rbind,.)
  
  #temp design verwijderen uit globalEnv.
  rm(design_temp, data_x, envir = .GlobalEnv)

  # Sorteer data van oudere data naar nieuwere data
  if (crossing_is_jaar) {
    result <- result %>% arrange(AGOJB401)
  }

  # Check NAs in result$percentage. Stop als deze er zijn.
  if (any(is.na(result$percentage))) {
    
    warning("Indicator heeft geen waarde voor een of meerdere crossings. Daarom kan niet vergeleken worden.")
    
    return(paste("", var_label(var_inhoud))) # Toon niks als indicator geen waarde heeft.
    
  }
  
  # Check lengte van kruistabel
  if (nrow(result) < 2 | nrow(result) > 3) {
    
    stop(paste("Er zijn te weinig of te veel vergelijkingen. Controleer vergelijkingsvariabele ", var_crossing, "."))
    
  }  
  
  # Maak label voor vergelijking
  if (length(niveaus) > 1) {
    
    warning("Je maakt een vergelijking tussen niveaus. 
      Houdt er rekening mee dat deelnemers op een lager niveau (bv. gemeente)
      ook aanwezig zijn op een hoger niveau (bv. regio) en deze vergelijking 
      dus statistisch niet correct is.")
    
    crossings <- niveaus
    
    var_crossing <- "niveau"
    
  } else {
    
    crossings <- case_when(result[var_crossing] == "Man" ~ "mannen", 
                           result[var_crossing] == "Vrouw" ~ "vrouwen",
                           result[var_crossing] == '16-17 jaar' ~ '16-17 jarigen',
                           result[var_crossing] == '18-20 jaar' ~ '18-20 jarigen',
                           result[var_crossing] == '21-25 jaar' ~ '21-25 jarigen',
                           .default = tolower(labelled_naar_character(result, var_crossing)))
    
    # Maak label voor niveau
    label_niveau <- case_when(niveaus == "gemeente" ~ "de gemeente",
                              niveaus == "regio" ~ "de regio",
                              niveaus == "nl" ~ "Nederland",
                              .default = "")
  }
  
  # Maak label voor variabele 
  if (is.null(variabele_label)) {
    label <- tolower(var_label(data[var_inhoud]))
  } else {
    label <- variabele_label
  }
  
  if (var_crossing == "niveau") { 
    
    if (length(niveaus) == 2) {
      # Vergelijk 2 niveaus      
      
      crossings <- case_when(niveaus == "gemeente" ~ "in de gemeente",
                             niveaus == "regio" ~ "regionaal",
                             niveaus == "nl" ~ "landelijk",
                             .default = "")
      
      resultaat_vergelijking <- case_when(result$ci_lower[1] > result$ci_upper[2] ~ " hoger dan ",
                                          result$ci_lower[2] > result$ci_upper[1] ~ " lager dan ",
                                          TRUE ~ " gelijk aan ")
      
      if (resultaat_vergelijking == " gelijk aan ") {
        
        return(paste0("Het percentage dat ", label, " is ", crossings[1], resultaat_vergelijking, 
                      crossings[2], "."))
        
      } else {
        
        return(paste0("Het percentage dat ", label, " is ", crossings[1], " (", result$percentage[1], "%)", 
                      resultaat_vergelijking, crossings[2], " (", result$percentage[2], "%)."))
        
      }
      
    } else if (length(niveaus) == 3) {
      # Vergelijk 3 niveaus
      
      crossings <- case_when(niveaus == "gemeente" ~ "de gemeente",
                             niveaus == "regio" ~ "regionaal",
                             niveaus == "nl" ~ "landelijk",
                             .default = "")
      
      resultaat_vergelijking1 <- case_when(result$ci_lower[1] > result$ci_upper[2] ~ " lager dan ",
                                           result$ci_lower[2] > result$ci_upper[1] ~ " hoger dan ",
                                           TRUE ~ " gelijk aan ")
      
      resultaat_vergelijking2 <- case_when(result$ci_lower[1] > result$ci_upper[3] ~ " lager dan ",
                                           result$ci_lower[3] > result$ci_upper[1] ~ " hoger dan ",
                                           TRUE ~ " gelijk aan ")
      
      return(paste0("Het percentage dat ", label, " is ", crossings[2], " (", result$percentage[2], "%)", 
                    resultaat_vergelijking1, "en ", crossings[3], " (", result$percentage[3], "%)", 
                    resultaat_vergelijking2, crossings[1], " (", result$percentage[1], "%)."))
      
    } else {
      
      stop("Er zijn meer dan 3 niveaus opgegeven. Dit kan niet. Vul minder niveaus in. ")
      
    }
    
  } else if (var_crossing == 'AGOJB401') { # Vergelijking 2 jaren
    
    #Toevoeging 27-11 Pieter; Optie om % huidig jaar ook te tonen
    huidig_percentage <- ""
    if(met_percentage_huidig){
      
    huidig_percentage <- paste0(" (",result$percentage[2],"%)")
      
    }
    
    vorig_percentage_bij_gelijk <- "."
    if(met_percentage_vorig_bij_gelijk_gebleven){
      
      vorig_percentage_bij_gelijk <- paste0(" (",result$percentage[1],"%).")
      
    }
    
    
    resultaat_vergelijking <- case_when(result$ci_lower[1] > result$ci_upper[2] ~ " afgenomen ",
                                        result$ci_lower[2] > result$ci_upper[1] ~ " toegenomen ",
                                        TRUE ~ " gelijk gebleven ")
    
    if (resultaat_vergelijking == " gelijk gebleven ") {
      
      return(paste0("In ", label_niveau, " is het percentage jongvolwassenen dat ", label, huidig_percentage, resultaat_vergelijking, 
                    "t.o.v. ", crossings[1], vorig_percentage_bij_gelijk))
      
    } else {
      
      return(paste0("In ", label_niveau, " is het percentage jongvolwassenen dat ", label, huidig_percentage, resultaat_vergelijking, 
                    "t.o.v. ", crossings[1], " (", result$percentage[1], "%)."))
      
    } 
  } else if (nrow(result) == 2 ) { # Vergelijk 2 groepen:
    
    resultaat_vergelijking <- case_when(result$ci_lower[1] > result$ci_upper[2] ~ " hoger dan ",
                                        result$ci_lower[2] > result$ci_upper[1] ~ " lager dan ",
                                        TRUE ~ " gelijk aan ")
    
    if (resultaat_vergelijking == " gelijk aan ") {
      
      return(paste0("In ", label_niveau, " is het percentage dat ", label, " onder ", crossings[1], resultaat_vergelijking, 
                    "dat van ", crossings[2], "."))
      
    } else {
      
      return(paste0("In ", label_niveau, " is het percentage dat ", label, " onder ", crossings[1], " (", result$percentage[1], "%)",
                    resultaat_vergelijking, "dat van ", crossings[2], " (", result$percentage[2], "%)."))
      
    } 
    
  } else if (nrow(result) == 3 ) { # Vergelijking 3 groepen:
    
    resultaat_vergelijking1 <- case_when(result$ci_lower[1] > result$ci_upper[2] ~ " lager dan ",
                                         result$ci_lower[2] > result$ci_upper[1] ~ " hoger dan ",
                                         TRUE ~ " gelijk aan ")
    
    resultaat_vergelijking2 <- case_when(result$ci_lower[1] > result$ci_upper[3] ~ " lager dan ",
                                         result$ci_lower[3] > result$ci_upper[1] ~ " hoger dan ",
                                         TRUE ~ " gelijk aan ")
    
    if (resultaat_vergelijking1 == " gelijk aan " & resultaat_vergelijking2 == " gelijk aan ") {
      
      return(paste0("In ", label_niveau, " is het percentage dat ", label, " gelijk onder ", crossings[1], 
                    ", ", crossings[2], " en ", crossings[3], "."))    
      
    } else {
      
      return(paste0("In ", label_niveau, " is het percentage dat ", label, " onder ", crossings[2], " (", result$percentage[2], "%)",
                    resultaat_vergelijking1, "en onder ", crossings[3], " (", result$percentage[3], "%)",
                    resultaat_vergelijking2, "dat van ", crossings[1], " (", result$percentage[1], "%)."))
      
    }
  }
}

maak_top <- function(data, var_inhoud, toon_label = T, value = 1, niveau = "regio", top = 1,
                     huidig_jaar = 2024, var_jaar = "AGOJB401") {
  
  # Maximaal één niveau als input
  if(length(niveau) > 1){
    
    stop(glue("
    Top kan niet gemaakt worden over meerdere niveaus.
    Selecteer 1 niveau.
    niveaus: {paste(niveau, collapse = ',')}"))
    
  }
  
  # Design en dataset bepalen o.b.v. niveau
  if(niveau == "nl"){
    
    design_x <- design_land
    subset_x <- data
    
  } else if(niveau == "regio"){
    
    design_x <- design_regio
    subset_x <- data %>% filter(GGDregio == params$regiocode)
    
  } else if (niveau == "gemeente"){
    
    design_x <- design_gem
    subset_x <- data %>% 
      filter(Gemeentecode == params$gemeentecode) %>%
      filter(!is.na(Standaardisatiefactor_gemeente))
    
  } else{
    
    stop(glue("niveau bestaat niet
              niveau: {niveau}"))
    
  }
  
  # Standaard alleen laatste jaar overhouden
  # Subset maken van design
  design_temp <<- subset(design_x, get(var_jaar) == huidig_jaar)
  
  # Subset maken van data
  data_x <<- subset_x %>% filter(!!sym(var_jaar) == huidig_jaar)
  
  # Bereken gewogen cijfers
  list <- lapply(var_inhoud, function(x){bereken_kruistabel(data = data_x, 
                                                            survey_design = design_temp, 
                                                            variabele = x
  )}) 
  
  # Selecteer relevante rijen en voeg dataframes samen
  if (length(var_inhoud) == 1) {
    
    list <- purrr::map(list, function(x) { x %>% select(varcode, waarde, percentage, all_of(var_inhoud)) })
    
  } else {
    
    list <- purrr::map(list, function(x) { x %>% select(varcode, waarde, percentage) })
    
  }
  
  list <- do.call(rbind, list)
  
  #temp design en dataset verwijderen uit globalEnv.
  rm(data_x, design_temp, envir = .GlobalEnv)
  
  # Filter en sorteer
  list <- list %>%
    filter(waarde %in% value) %>% # Filter de gegevens voor value eruit. Standaard is dit 1.
    arrange(desc(percentage)) # Sorteer op hoogte van estimate (percentage)
  
  # Print top
  if (toon_label) { # Wanneer label getoond moet worden
    
    if (length(var_inhoud) > 1) { # Bij meerdere indicatoren als input
      
      return(paste0(tolower(var_label(data[list$varcode[top]])), 
                    " (", 
                    ifelse(is.na(list$percentage[top]), "-", list$percentage[top]),
                    "%)"))
      
    } else { # Bij één indicator als input
      
      return(paste0(tolower(labelled_naar_character(list, var_inhoud))[top], 
                    " (", 
                    ifelse(is.na(list$percentage[top]), "-", list$percentage[top]),
                    "%)")) 
      
    }
    
  } else { # Wanneer geen label getoond moet worden
    
    return(paste0(ifelse(is.na(list$percentage[top]), "-", list$percentage[top]), "%")) 
    
  }
}

maak_percentage <- function(data, var_inhoud, value = 1, niveau = "regio",
                            huidig_jaar = 2024, var_jaar = "AGOJB401",
                            ongewogen = FALSE,
                            subset_var = NULL,
                            subset_val = NULL,
                            nvar = params$default_nvar,
                            ncel = params$default_ncel
                            
                            ) {
  
  # Input is één dichotome variabele met één niveau 
  if(length(niveau) > 1 | length(var_inhoud) > 1){
    
    stop(glue("
    Percentage kan niet berekend worden over meerdere niveaus en/of meerdere variabelen.
    Selecteer 1 niveau en 1 variabele.
    niveaus: {paste(niveau, collapse = ',')}
    var_inhoud: {paste(var_inhoud, collapse = ',')}"))
    
  }
  
  # Design en dataset bepalen o.b.v. niveau
  if(niveau == "nl"){
    
    design_x <- design_land
    subset_x <- data
    
  } else if(niveau == "regio"){
    
    design_x <- design_regio
    subset_x <- data %>% filter(GGDregio == params$regiocode)
    
  } else if (niveau == "gemeente"){
    
    design_x <- design_gem
    subset_x <- data %>% 
      filter(Gemeentecode == params$gemeentecode) %>%
      filter(!is.na(Standaardisatiefactor_gemeente))
    
  } else{
    
    stop(glue("niveau bestaat niet
              niveau: {niveau}"))
    
  }
  
  #Optie voor Inhoudelijke subset; tbv https://github.com/ggdatascience/gmjv-landelijk-format/issues/93
  if(!is.null(subset_var) & !is.null(subset_val)){
    
    design_x <- subset(design_x, get(subset_var) == subset_val) #design subsetten
    subset_x <- subset_x %>%
      filter(!!sym(subset_var) == subset_val) #data subsetten
  }
  
  
  #Ongewogen berekening
  if(ongewogen){
    # Filter op juiste jaar
    subset_x <- subset_x %>% 
      filter(!!sym(var_jaar) == huidig_jaar)
    
    tabel_percentage = subset_x %>% group_by(!!sym(var_inhoud)) %>% 
      summarise(aantal = n()) %>% 
      ungroup() %>% 
      filter(!is.na(!!sym(var_inhoud))) %>% #missing wissen
      mutate(totaal = sum(aantal),
             percentage = round(aantal / totaal * 100, 0)) #%>% 
      #filter(!!sym(var_inhoud) %in% value)
    
    # Checken of kleine & grote N is voldaan
    tabel_percentage$percentage <- 
      case_when(tabel_percentage$totaal < params$default_nvar 
                | any(tabel_percentage$aantal < params$default_ncel) ~ "-%",
                TRUE ~ paste0(tabel_percentage$percentage,"%"))
    
        
    # # Checken of er nog labels missen, dan zijn hebben deze groepen 0 respondenten en moeten dus niet getoond worden
    if (length(val_labels(monitor_df[[var_inhoud]])) != nrow(tabel_percentage)) {
      tabel_percentage$percentage <- "-%"
    }
    
    tabel_percentage <- tabel_percentage %>% filter(!!sym(var_inhoud) %in% value)
    
    #Checken of tabel leeg is: "-%"
    if (nrow(tabel_percentage) < 1) {
      return("-%")
    } else {
      return(tabel_percentage$percentage)
    }
  }
  
  # Standaard alleen laatste jaar overhouden
  # Subset maken van design
  design_temp <<- subset(design_x, get(var_jaar) == huidig_jaar)
  
  # Subset maken van data
  data_temp <<- subset_x %>% filter(!!sym(var_jaar) == huidig_jaar)
  
  # Bereken gewogen cijfers
  result <- bereken_kruistabel(data = data_temp,
                               survey_design = design_temp, 
                               variabele = var_inhoud,
                               min_observaties_per_vraag = nvar,
                               min_observaties_per_antwoord = ncel
                               ) %>%
    filter(waarde %in% value) #%>% # Filter de gegevens voor value eruit. Standaard is dit 1.
  
  #temp design verwijderen uit globalEnv.
  rm(data_temp, design_temp, envir = .GlobalEnv)
  
  # Output van functie maken
  return(paste0(ifelse(is.na(result$percentage), "-", result$percentage), "%"))
  
}

# table of contents voor pdf ----------------------------------------------
#Geleend van https://gist.github.com/gadenbuie/c83e078bf8c81b035e32c3fc0cf04ee8
#Pieter: kleine aanpassingen in gsub om met komma's om te gaan.
render_toc <- function(
    filename,
    toc_header_name = "Table of Contents",
    base_level = NULL,
    toc_depth = 3
) {
  x <- readLines(filename, warn = FALSE)
  x <- paste(x, collapse = "\n")
  x <- paste0("\n", x, "\n")
  for (i in 5:3) {
    regex_code_fence <- paste0("\n[`]{", i, "}.+?[`]{", i, "}\n")
    x <- gsub(regex_code_fence, "", x)
  }
  x <- strsplit(x, "\n")[[1]]
  x <- x[grepl("^#+", x)]
  if (!is.null(toc_header_name))
    x <- x[!grepl(paste0("^#+ ", toc_header_name), x)]
  if (is.null(base_level))
    base_level <- min(sapply(gsub("(#+).+", "\\1", x), nchar))
  start_at_base_level <- FALSE
  x <- sapply(x, function(h) {
    level <- nchar(gsub("(#+).+", "\\1", h)) - base_level
    if (level < 0) {
      stop("Cannot have negative header levels. Problematic header \"", h, '" ',
           "was considered level ", level, ". Please adjust `base_level`.")
    }
    if (level > toc_depth - 1) return("")
    if (!start_at_base_level && level == 0) start_at_base_level <<- TRUE
    if (!start_at_base_level) return("")
    if (grepl("\\{#.+\\}(\\s+)?$", h)) {
      # has special header slug
      header_text <- gsub("#+ (.+)\\s+?\\{.+$", "\\1", h)
      header_slug <- gsub(".+\\{\\s?#([-_.a-zA-Z]+).+", "\\1", h)
    } else {
      header_text <- gsub("#+\\s+?", "", h)
      header_text <- gsub("\\s+?\\{.+\\}\\s*$", "", header_text) # strip { .tabset ... }
      header_text <- gsub("^[^[:alpha:]]*\\s*", "", header_text) # remove up to first alpha char
      header_text <- gsub(",","", header_text) #remove ","
      header_slug <- paste(strsplit(header_text, " ")[[1]], collapse="-")
      header_slug <- tolower(header_slug)
    }
    paste0(strrep(" ", level * 4), "- [", header_text, "](#", header_slug, ")")
  })
  x <- x[x != ""]
  knitr::asis_output(paste(x, collapse = "\n"))
}



