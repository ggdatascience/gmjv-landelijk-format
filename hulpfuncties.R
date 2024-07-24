# Script met helpfuncties voor GMJV 2024.
# Packages ----------------------------------------------------------------

# Het script maakt gebruik van een aantal packages
# Deze moeten bij de eerste keer lokaal worden geinstalleerd. 
# Dat doe je met behulp van de functie: install.packages() 
# (Verwijder de # aan het begin van onderstaande regel om de code te runnen en de benodigde packages te installeren.)
# TODO hier nog iets van maken met een fucntie die checkt of deze package al is geinstalleerd, en anders installeren?
#install.packages(c('tidyverse', 'haven', 'labelled', 'survey'))
# TODO Pieter: Is het wel nodig om ziets te schrijven? moderne RStudio doet dat vanzelf voor je.


# TODO Alle grafieken automatisch van relevante alt text voorzien

# TODO code opschonen; dataverwerking voor grafiek in eigen functies stoppen om complexiteit / lengte van functies te verkleinen.


# Hieronder worden de benodige packages geladen
library(gt)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr) # Voor str_replace
library(labelled) # Package om te werken met datalabels, o.a. voor to_character()
library(survey) # Package om te werken met gewogen gemiddelds incl. betrouwbaarheidsintervallen
library(glue) #om strings aangenaam aan elkaar te plakken

# Standaard instellingen --------------------------------------------------

# Naar .qmd gehaald.

# # Standaard kleuren instellen
# default_kleuren_grafiek <- c("#012C17","#76B82A","#007E48")
# default_kleuren_responstabel <- c("header" = "#012C17",
#                                   "kleur_1" = "#007E48",
#                                   "kleur_2" = "#76B82A",
#                                   "kleur_tekst" = "#FFFFFF"
#                                   )
# 
# # Default minimum aantallen instellen
# default_Nvar = 100 # Minimum aantal invullers per vraag.
# default_Ncel = 10 # Minimum aantal invullers oper antwoordoptie.


# utility -----------------------------------------------------------------

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

#wordt in grafiekfuncties aangeroepen om automatisch alt_text te maken; 
#functie vertaald waarschijnlijk slecht naar andere contexten; verwacht df op specifieke volgorde &
#met specifiek vartypen
maak_alt_text <- function(df, doelgroep = "jongvolwassenen", type_grafiek = "staafdiagram",
                          is_vergelijking = F, is_dichotoom = T, is_gestapeld = F){
  
  #TODO Functie is onnodig complex. Oplossen door dataverwerking uniformer te maken; 
  #En/OF meer info over te selecteren variabelen naar de argumenten halen
  
  label_var_inhoud <- var_label(df[,1])
  
  #Als crossings naast elkaar gelegd worden worden de character variabelen onderdeel & groep gebruikt in
  #output df ipv de oorspronkelijke labelled+dbl
  #TODO gelijktrekken zodat er 1 alt-text functie kan komen zonder heel veel if-condities
  if(is_vergelijking){
    string_waarden <- df %>% 
      select(onderdeel, percentage) %>%
      rowwise() %>% 
      mutate(
        percentage = ifelse(is.na(percentage),
                            "percentage onbekend",
                            paste0(percentage,"%")),
        string = paste0(onderdeel,": ", percentage)) %>% 
      pull(string) %>% 
      paste0(collapse = ",")
    
    crossing_labels <- df$groep %>% unique() %>% paste0(collapse = " en ")
    
  }else if(is_gestapeld) {
    string_waarden <- df %>% 
      select(-c(n,totaal,te_weinig_n)) %>% 
      mutate(across(where(~ is.labelled(.) && is.double(.)),
                    ~ labelled_naar_character(df, cur_column()))) %>%
      rowwise() %>% 
      mutate(
        percentage = paste0(round(percentage,1),"%"),
        string = str_c(across(everything()), collapse = ": ")) %>% 
      pull(string) %>% 
      paste0(collapse = ", ")
    
    crossing_labels <- NULL
      
  } else if(is_dichotoom){
    string_waarden <- df[,-1] %>% 
      select(-c(aantal_antwoord,aantal_vraag,is_leeg,weggestreept)) %>%
      #labelled double naar character
      mutate(across(where(~ is.labelled(.) && is.double(.)),
                    ~ labelled_naar_character(df, cur_column()))) %>%
      #rowwise voor string_var
      rowwise() %>% 
      mutate(percentage = ifelse(is.na(percentage),
                                 "percentage onbekend",
                                 paste0(percentage,"%")
      ),
      cat = str_c(across(!percentage), collapse = " "),
      string = paste0(cat,": ", percentage)
      
      ) %>% 
      pull(string) %>% 
      paste0(collapse = ", ")
  
    crossing_labels <- var_label(df[,-1] %>% select(-c(aantal_antwoord,aantal_vraag,
                                                     is_leeg, weggestreept, percentage))) %>% 
    paste(collapse = " en ")
    
    }else {
    df$leeg <- NULL
    
    string_waarden <- df %>% 
      select(-c(aantal_antwoord,aantal_vraag,is_leeg,weggestreept)) %>%
      #labelled double naar character
      mutate(across(where(~ is.labelled(.) && is.double(.)),
                    ~ labelled_naar_character(df, cur_column()))) %>%
      #rowwise voor string_var
      rowwise() %>% 
      mutate(percentage = ifelse(is.na(percentage),
                                 "percentage onbekend",
                                 paste0(percentage,"%")
      ),
      cat = str_c(across(!percentage), collapse = " "),
      string = paste0(cat,": ", percentage)
      
      ) %>% 
      pull(string) %>% 
      paste0(collapse = ", ")
    
    crossing_labels <- var_label(df[,-1] %>% select(-c(aantal_antwoord,aantal_vraag,
                                                       is_leeg, weggestreept, percentage))) %>% 
      paste(collapse = " en ")
      
    }



  if(is.null(crossing_labels)){
    #Grafiek zonder crossings:
    glue("{type_grafiek} met percentages voor de indicator '{label_var_inhoud}' bij {doelgroep}: {string_waarden}")
  } else{
    glue("{type_grafiek} met percentages voor de indicator '{label_var_inhoud}' bij {doelgroep} per {crossing_labels}: {string_waarden}")  
  }
  
  
}


# Tabelfuncties -------------------------------------------------------
maak_responstabel <- function(df, crossings, missing_label = "Onbekend",
                              kleuren = default_kleuren_responstabel){

  #TODO Hoe om te gaan met missing waarden? Meetellen als 'onbekend' of niet weergeven?
  #In laatste geval kloppen de totalen van crossings onderling niet. Kan prima zijn
  #Voorlopige keuze: Missings weergeven als "onebekend"
  
  
  aantallen_per_crossing <- lapply(crossings, function(x){
    
    if(is.null(df[[x]])){
      
      warning(glue("De variabele {x} bestaat niet in de data. Typefout?"))
      return(NULL)
      
    }
    #Variabelen naar character omzetten
    df[[x]] <- labelled_naar_character(df, x)
    
    #Aantallen uitrekenen. Missing labelen als missing_label
    aantallen_df = df %>% 
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
    } else{
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
    
    df[[x]] %>% unique() %>% length()    
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
maak_staafdiagram_dubbele_uitsplitsing <- function(df, var_inhoud, var_crossing_groep, var_crossing_kleur, titel = "",
                                                   kleuren = default_kleuren_grafiek,
                                                   nvar = default_Nvar, ncel = default_Ncel,
                                                   alt_text = NULL
                                                   ){
  
  if(!labelled::is.labelled(df[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  if(val_labels(df[[var_inhoud]]) %>% length() > 2){
    warning(
      glue("{var_inhoud} is geen dichotome variabele. Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
    
    #TODO met Sjanne / Willeke overleggen hoe we foute invoer willen afhandelen.
  }
  
  #DIT IS EEN NAIEVE VERWERKING VH DATAFRAME MET ABSOLUTE AANTALLEN
  #TODO Vervangen met gewogen aantallen.
  df_plot <- df %>% 
    filter(!is.na(!!sym(var_inhoud)),
           !is.na(!!sym(var_crossing_groep)),
           !is.na(!!sym(var_crossing_kleur))       
           ) %>% 
    select(!!sym(var_inhoud),!!sym(var_crossing_groep),!!sym(var_crossing_kleur)) %>% 
    group_by(!!sym(var_inhoud),!!sym(var_crossing_groep),!!sym(var_crossing_kleur)) %>% 
    summarise(aantal_antwoord = n()) %>% 
    ungroup() %>% 
    group_by(!!sym(var_crossing_groep),!!sym(var_crossing_kleur)) %>% 
    mutate(aantal_vraag = sum(aantal_antwoord))%>%
    ungroup() %>%
    #ungroup om % en n_antwoord uit te rekenen
    mutate(percentage = round((aantal_antwoord/aantal_vraag)*100),
           
           #Bij te lage aantallen -> Kolom als leeg weergeven net een sterretje in de kolom
           percentage = case_when(aantal_vraag < nvar ~ NA,
                               aantal_antwoord < ncel ~ NA,
                               TRUE ~ percentage)) %>% 
    #voor de kleine N voorwaarde moet de hele vraag weggestreept worden als er niet
    #aan de voorwaarde wordt voldaan; groeperen op crossings:
    group_by(!!sym(var_crossing_groep),!!sym(var_crossing_kleur)) %>% 
    mutate(is_leeg = any(is.na(percentage))) %>% #is_leeg als één percentage van een vraag NA is.
    ungroup() %>% 
    mutate(percentage = ifelse(is_leeg, NA, percentage), #zet alle vragen met tenminste 1 NA antwoord op NA
           weggestreept = ifelse(is_leeg, 10, NA) %>% as.numeric()) %>% #maak een vector met val 30 waar een antwoord ontbreekt (voor missing sterretjes in diagram) 
    filter(!!sym(var_inhoud) == 1) #dichtome var; ja overhouden.
  
  
  #alt text aanmaken voor grafiek als deze niet handmatig is opgegeven
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(df_plot)
    
  }
  

  #variabelen naar factor omzetten zodat volgorde intact blijft in ggplot
  df_plot[[var_crossing_groep]] <- factor(df_plot[[var_crossing_groep]], 
                                          levels = val_labels(df_plot[[var_crossing_groep]]),
                                          labels = names(val_labels(df_plot[[var_crossing_groep]])))
  
  
  df_plot[[var_crossing_kleur]] <- factor(df_plot[[var_crossing_kleur]], 
                                          levels = val_labels(df_plot[[var_crossing_kleur]]),
                                          labels = names(val_labels(df_plot[[var_crossing_kleur]])))
  
  
  namen_kleuren <- levels(df_plot[[var_crossing_kleur]])
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  ggplot(df_plot) +
    geom_col(aes(x = !!sym(var_crossing_groep), y = percentage, fill = !!sym(var_crossing_kleur)),
             position = position_dodge(width = 0.8), width = 0.8,
             na.rm = T
             ) +
    
    geom_text(aes(x = !!sym(var_crossing_groep),
                  y = percentage,
                  label = paste(percentage,"%"),
                  vjust = -1),
              position = position_dodge2(width = 0.8),
              size = 5,
              na.rm = T
    ) +
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = !!sym(var_crossing_groep), y = weggestreept, color = !!sym(var_crossing_kleur)),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
               ) +
    #TODO toelichting op sterretje in grafiek?
    ggtitle(titel) +
    #Hier worden de kleuren en volgorde bepaald.
    #Voor de fill van de balken
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE,
                                           label.position = "right", title.position = "top")) +
    #voor de kleur van de sterretjes
    scale_color_manual(values= kleuren) +
    
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, by = 10),
                       labels = paste(seq(0,100, by = 10),"%"),
                       expand = expansion(mult = c(0, 0.05))
                       ) +
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1,"cm"),
          legend.position = "bottom",
          plot.title = element_text(hjust = .5),
          axis.line.x.bottom = element_line(linewidth = 1),
          axis.line.y.left = element_line(linewidth = 1,)
    ) + 
    labs(
      alt = alt_text
    )
}


maak_staafdiagram_vergelijking <- function(df, var_inhoud, var_crossings, titel = "",
                                           kleuren = default_kleuren_grafiek,
                                           nvar = default_Nvar, ncel = default_Ncel,
                                           alt_text = NULL
                                             ){
  
  if(!labelled::is.labelled(df[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  if(val_labels(df[[var_inhoud]]) %>% length() > 2){
    warning(
      glue("{var_inhoud} is geen dichotome variabele. Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
    
    #TODO met Sjanne / Willeke overleggen hoe we foute invoer willen afhandelen.
    #Verwachting is dat uitgebreide errorhandling nodig is. misschien naar aparte functie halen
  }
  
   df_plot <- lapply(var_crossings, function(crossing){

     df_crossing = df %>% 
       filter(!is.na(!!sym(var_inhoud)),
              !is.na(!!sym(crossing))) %>%
       select(!!sym(var_inhoud),!!sym(crossing)) %>% 
       group_by(!!sym(var_inhoud),!!sym(crossing)) %>% 
       summarise(aantal_antwoord = n()) %>% 
       ungroup() %>% 
       group_by(!!sym(crossing)) %>% 
       mutate(aantal_vraag = sum(aantal_antwoord)) %>% 
       ungroup() %>% 
       mutate(percentage = round((aantal_antwoord/aantal_vraag)*100),
              groep = var_label(!!sym(crossing)),
              #Bij te lage aantallen -> Kolom als leeg weergeven net een sterretje in de kolom
              percentage = case_when(aantal_vraag < nvar ~ NA,
                                     aantal_antwoord < ncel ~ NA,
                                     TRUE ~ percentage)) %>% 
       #voor de kleine N voorwaarde moet de hele vraag weggestreept worden als er niet
       #aan de voorwaarde wordt voldaan; groeperen op crossings:
       group_by(!!sym(crossing)) %>% 
       mutate(is_leeg = any(is.na(percentage))) %>% #is_leeg als één percentage van een vraag NA is.
       ungroup() %>% 
       mutate(percentage = ifelse(is_leeg, NA, percentage), #zet alle vragen met tenminste 1 NA antwoord op NA
              weggestreept = ifelse(is_leeg, 10, NA) %>% as.numeric()) %>% #maak een vector met val 30 waar een antwoord ontbreekt (voor missing sterretjes in diagram) 
       filter(!!sym(var_inhoud) == 1) %>%  #dichtome var; ja overhouden.
       rename("onderdeel" = 2) %>%
       mutate(onderdeel = val_labels(onderdeel) %>% names())
     
     
     df_crossing$kleuren <- kleuren[1:nrow(df_crossing)]
      
     df_crossing
       
   }) %>% do.call(rbind,.)

   
   if(is.null(alt_text)){
     
     alt_text = maak_alt_text(df_plot, is_vergelijking = T)
     
   } 
   
   #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
   df_plot$groep <- factor(df_plot$groep)
   #volgorde onderdeel vastzetten o.b.v dataframe
   onderdeel_levels <- df_plot$onderdeel %>% unique()
   df_plot$onderdeel <- factor(df_plot$onderdeel, levels = onderdeel_levels)

   
   kleuren <- df_plot$kleuren
   names(kleuren) <- onderdeel_levels


   ggplot(df_plot) +
     geom_col(aes(x = groep, y = percentage, fill = onderdeel),
              position = position_dodge(width = 0.8), width = 0.8,
              na.rm = T) +
     geom_text(aes(x = groep,
                   y = percentage,
                   label = paste(percentage,"%"),
                   vjust = -1),
               position = position_dodge2(width = 0.8),
               size = 5,
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
     ggtitle(titel) +
     #Hier worden de kleuren en volgorde bepaald.
     scale_fill_manual(values= kleuren,
                       guide = guide_legend(nrow = 1, byrow = TRUE,
                                            label.position = "right", title.position = "top")) +
     #kleuren voor sterretje
     scale_color_manual(values= kleuren) + 
     
     scale_y_continuous(limits = c(0,100),
                        breaks = seq(0,100, by = 10),
                        labels = paste(seq(0,100, by = 10),"%"),
                        expand = expansion(mult = c(0, 0.05))
     ) +
     coord_cartesian(ylim = c(0,100))+
     theme(axis.title = element_blank(),
           panel.background = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           legend.title = element_blank(),
           legend.spacing.x = unit(.1, 'cm'),
           legend.position = "bottom",
           plot.title = element_text(hjust = .5),
           axis.line.x.bottom = element_line(linewidth = 1, colour = "black"),
           axis.line.y.left = element_line(linewidth = 1)
     ) +
     labs(
       alt = alt_text
     )
 }

maak_staafdiagram_meerdere_staven <- function(df, var_inhoud,var_crossing = NULL, 
                                              titel = "",
                                              kleuren = default_kleuren_grafiek,
                                              flip = FALSE, nvar = default_Nvar, ncel = default_Ncel,
                                              alt_text = NULL
                                              
){
  
  
  if(!labelled::is.labelled(df[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  
  
  remove_legend = F

  v_just_text = ifelse(flip,0.5,-1)
  h_just_text = ifelse(flip,-1,0.5)

  if(is.null(var_crossing)){
    df$leeg = labelled(c(1),c("%" = 1))
    var_crossing = "leeg"
    remove_legend = T
  }
  
  df_plot <- df %>% 
    filter(!is.na(!!sym(var_inhoud)),
           !is.na(!!sym(var_crossing))      
    ) %>% 
    select(!!sym(var_inhoud),!!sym(var_crossing)) %>% 
    group_by(!!sym(var_inhoud),!!sym(var_crossing)) %>% 
    summarise(aantal_antwoord = n()) %>% 
    ungroup() %>% 
    group_by(!!sym(var_crossing)) %>% 
    mutate(aantal_vraag = sum(aantal_antwoord)) %>% 
    ungroup() %>% 
    mutate(percentage = round((aantal_antwoord/aantal_vraag)*100),
           #Bij te lage aantallen -> Kolom als leeg weergeven net een sterretje in de kolom
           percentage = case_when(aantal_vraag < nvar ~ NA,
                                  aantal_antwoord < ncel ~ NA,
                                  TRUE ~ percentage)
           ) %>% 
    #voor de kleine N voorwaarde moet de hele vraag weggestreept worden als er niet
    #aan de voorwaarde wordt voldaan; groeperen op crossings:
    group_by(!!sym(var_crossing)) %>% 
    mutate(is_leeg = any(is.na(percentage))) %>% #is_leeg als één percentage van een vraag NA is.
    ungroup() %>% 
    mutate(percentage = ifelse(is_leeg, NA, percentage), #zet alle vragen met tenminste 1 NA antwoord op NA
           weggestreept = ifelse(is_leeg, 10, NA) %>% as.numeric()) #maak een vector met val 30 waar een antwoord ontbreekt (voor missing sterretjes in diagram) 

  #Alt text toevoegen o.b.v. data als er nog niks is ingevuld
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(df_plot, is_dichotoom = F)
    
  }
  
  #TODO hier een functie van maken: labelled_dbl_to_factor
  df_plot[[var_crossing]] <- factor(df_plot[[var_crossing]], 
                                          levels = val_labels(df_plot[[var_crossing]]),
                                          labels = names(val_labels(df_plot[[var_crossing]])))
  
  df_plot[[var_inhoud]] <- factor(df_plot[[var_inhoud]], 
                                    levels = val_labels(df_plot[[var_inhoud]]),
                                    labels = names(val_labels(df_plot[[var_inhoud]])))
  
  namen_kleuren <- levels(df_plot[[var_crossing]])
  
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  plot = ggplot(df_plot) +
    geom_col(aes(x = !!sym(var_inhoud), y = percentage, fill = !!sym(var_crossing)),
             position = position_dodge(width = 0.8), width = 0.8,
             na.rm = T) +
    geom_text(aes(x = !!sym(var_inhoud),
                  y = percentage,
                  label = paste(percentage,"%"),
                  vjust = v_just_text,
                  hjust = h_just_text),
              position = position_dodge2(width = 0.8),
              size = 5,
              na.rm = T) +
    
    #sterretje invoegen bij weggestreepte data omdat nvar of ncel niet gehaald wordt
    geom_point(aes(x = !!sym(var_inhoud), y = weggestreept, color = !!sym(var_crossing)),
               position = position_dodge(width = .8),
               shape = 8,
               size = 5,
               stroke = 2,
               show.legend = FALSE,
               na.rm = T
    ) +
    
    ggtitle(titel) +
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "right", title.position = "top")
    ) +
    
    #kleuren voor sterretje
    scale_color_manual(values= kleuren) + 
    
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, by = 10),
                       labels = paste(seq(0,100, by = 10),"%"),
                       expand = expansion(mult = c(0, 0.05))
    ) +
    #lange labels opsplitsen
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
    
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1,"cm"),
          legend.position = "bottom",
          plot.title = element_text(hjust = .5),
          axis.line.x.bottom = element_line(linewidth = 1),
          axis.line.y.left = element_line(linewidth = 1,)
    ) +
    labs(
      alt = alt_text
    )
  
  if(remove_legend){
    plot <- plot + theme(legend.position = "none")
  }
  
  if(flip){
    plot <- plot +
      
      coord_flip()
  }
  
  plot
 
}




#TODO lijntjes tussen uitsplitsingen
#TODO namen uitsplitsingen onder x-as labels
#TODO stoppen met uitsplitsing en crossing door elkaar gebruiken
#TODO Overal chekc inbouwen of een var wel een lbl+dbl is. Of niet afh. van maak_kruistabel() output.
#TODO dezelfde dfbewerkingen uit plotfuncties halen & naar eigen functies halen

maak_staafdiagram_uitsplitsing_naast_elkaar <- function(df, var_inhoud, var_crossings, titel = "",
                                                        kleuren = default_kleuren_grafiek,
                                                        kleuren_per_crossing = F, fade_kleuren = F,
                                                        flip = FALSE, nvar = default_Nvar, ncel = default_Ncel,
                                                        alt_text = NULL){
  
  if(!labelled::is.labelled(df[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  if(val_labels(df[[var_inhoud]]) %>% length() > 2){
    warning(
      glue("{var_inhoud} is geen dichotome variabele. Kies een ander grafiektype of een andere var_inhoud"))
    return(NULL)
    
    #TODO met Sjanne / Willeke overleggen hoe we foute invoer willen afhandelen.
  }
  
  #o.b.v. de orientatie van de grafiek (flip = horizontaal)
  #De correctie van de geom_text aanpassen  zodat deze netjes in het midden v.e. balk komt 
  v_just_text = ifelse(flip,0.5,1.5)
  h_just_text = ifelse(flip,1.5,0.5)

  #% voor iedere crossing appart uitrekenen.
  df_plot <- lapply(var_crossings, function(crossing){
    
    df_crossing = df %>% 
      filter(!is.na(!!sym(var_inhoud)),
             !is.na(!!sym(crossing))) %>%
      select(!!sym(var_inhoud),!!sym(crossing)) %>% 
      group_by(!!sym(var_inhoud),!!sym(crossing)) %>% 
      summarise(aantal_antwoord = n()) %>% 
      ungroup() %>% 
      group_by(!!sym(crossing)) %>% 
      mutate(aantal_vraag = sum(aantal_antwoord)) %>% 
      ungroup() %>% 
      mutate(percentage = round((aantal_antwoord/aantal_vraag)*100),
             groep = var_label(!!sym(crossing)),
             #Bij te lage aantallen -> Kolom als leeg weergeven net een sterretje in de kolom
             percentage = case_when(aantal_vraag < nvar ~ NA,
                                    aantal_antwoord < ncel ~ NA,
                                    TRUE ~ percentage)
      ) %>% 
      #voor de kleine N voorwaarde moet de hele vraag weggestreept worden als er niet
      #aan de voorwaarde wordt voldaan; groeperen op crossings:
      group_by(!!sym(crossing)) %>% 
      mutate(is_leeg = any(is.na(percentage))) %>% #is_leeg als één percentage van een vraag NA is.
      ungroup() %>% 
      mutate(percentage = ifelse(is_leeg, NA, percentage), #zet alle vragen met tenminste 1 NA antwoord op NA
             weggestreept = ifelse(is_leeg, 10, NA) %>% as.numeric()) %>%  #maak een vector met val 30 waar een antwoord ontbreekt (voor missing sterretjes in diagram) 
      rename("onderdeel" = 2) %>% 
      filter(!!sym(var_inhoud) == 1) %>% 
      mutate(onderdeel = val_labels(onderdeel) %>% names())
    
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
  
  #Alt text maken o.b.v. data als geen eigen tekst is ingegeven
  if(is.null(alt_text)){
    
    alt_text = maak_alt_text(df_plot, is_vergelijking = T)
    
  } 
  
  
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
  
  plot <- ggplot(df_plot) +
    geom_bar(aes(x = onderdeel, y = percentage, fill = onderdeel),
             stat = "identity", width = 0.8,
             na.rm = T
             ) +
    geom_text(aes(x = onderdeel,
                  y = percentage,
                  label = paste(percentage,"%"),
                  vjust = v_just_text,
                  hjust = h_just_text),
              color = "white",
              position = position_dodge2(width = 0.8),
              size = 5,
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
    
    ggtitle(titel) +
    theme(panel.background = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = .5),
          axis.line.y.left = element_line(linewidth = 1)
    ) +
    labs(
      alt = alt_text
    )
  
  if(flip){
    plot <- plot + 
      scale_y_continuous(limits = c(0,100),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 20)) +
      coord_flip() +
      theme(axis.title = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x =  element_blank(),
            )
  } else{
    plot <- plot + 
      theme(axis.line.x.bottom = (element_line(linewidth = 1))) +
      scale_y_continuous(limits = c(0,100),
                         expand = expansion(mult = c(0, 0))) +
      scale_x_discrete(labels = function(x) str_wrap(x,width = 20)) +
      theme(
#        axis.text.x = element_text(angle = 90, hjust = .95, vjust = .2),
        strip.background = element_blank(),
        
      ) + xlab("")
      
  }

  return(plot)

}

#horizontaal gestapeld staafdiagram
maak_staafdiagram_gestapeld <- function(df, var_inhoud, titel = "",
                                        kleuren = default_kleuren_grafiek, x_label = "",
                                        nvar = default_Nvar, ncel = default_Ncel,
                                        alt_text = NULL){
  
  if(!labelled::is.labelled(df[[var_inhoud]])){
    warning(glue("variabele {var_inhoud} is geen gelabelde SPSS variabele"))
  }
  
  df_plot <- df %>%
    select(!!sym(var_inhoud)) %>%
    filter(!is.na(!!sym(var_inhoud))) %>% 
    group_by(!!sym(var_inhoud)) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(totaal = sum(n),
           percentage = n / totaal * 100,
           te_weinig_n = n < ncel | totaal < nvar
           
           ) 
  
  #Als er een te lage N is: plot kan niet gemaakt worden.
  if(any(df_plot$te_weinig_n)){
    warning(glue("Plot kan niet gemaakt worden! Te weinig observaties voor {var_inhoud}. De instellingen zijn: nvar = {nvar} en ncel = {ncel}"))
    return(NULL)
    
  }
  
  if(is.null(alt_text)){
    
    alt_text <- maak_alt_text(df_plot, is_gestapeld = T)
    
  }
  
  df_plot[[var_inhoud]] <- factor(df_plot[[var_inhoud]], 
                                  levels = rev(val_labels(df_plot[[var_inhoud]])),
                                  labels = rev(names(val_labels(df_plot[[var_inhoud]]))))
  
  
  
  namen_kleuren <- levels(df_plot[[var_inhoud]])
  
  kleuren <- kleuren[1:length(namen_kleuren)]
  
  ggplot(df_plot, aes(x = percentage, y = x_label, fill = !!sym(var_inhoud))) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage),"%")),
            color = "#FFFFFF",
            position = position_stack(vjust = 0.5),
            size = 3) +
    
    scale_fill_manual(values= kleuren,
                      labels = function(x) str_wrap(x, width = 40)
                      ) +
    scale_x_continuous(
      limits = c(0,100),
      breaks = seq(0,100, by = 10),
      labels = paste(seq(0,100, by = 10),"%"),
      expand = expansion(mult = c(0, 0.05)))+
    ggtitle(titel) + 
    ylab(x_label) + 
      theme(
        axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1,"cm"),
          legend.position = "bottom",
          plot.title = element_text(hjust = .5),
          axis.line.x.bottom = element_line(linewidth = 1),
          axis.line.y.left = element_line(linewidth = 1,)
        ) +
    guides(fill = guide_legend(reverse = TRUE))  +
    labs(alt = alt_text)
  
}

#TODO percentage in tekst
#TODO Cirkeldiagram
  #TODO Donut


# rekenfuncties -----------------------------------------------------------

# Functie maken om cijfers te berekenen.
# Gebaseerd op functie uitrapportage monitor GMJ 2023.
# Zie https://github.com/ggdatascience/rapportage_monitor_gmj 

bereken_cijfers <- function(data, indicator, waarde, omschrijving, niveau_indicator, niveau_waarde, 
                            niveau_naam, jaar_indicator, jaar, uitsplitsing = NA, groepering = NA, 
                            weegfactor_indicator = "geen", Nvar = default_Nvar, Ncel = default_Ncel) {
  data %>%
    filter(.[niveau_indicator] == niveau_waarde & .[jaar_indicator] == jaar) %>%
    select(all_of(setdiff(c(jaar_indicator, indicator, uitsplitsing, groepering, weegfactor_indicator), NA))) %>%
    group_by(across(all_of(setdiff(c(jaar_indicator, indicator, uitsplitsing, groepering), NA)))) %>%
    {if(weegfactor_indicator != "geen") rename_at(., vars(matches(weegfactor_indicator)), ~ str_replace(., weegfactor_indicator, 'weegfactor')) else .} %>%
    summarise(n_cel = n(), 
              n_cel_gewogen = ifelse(weegfactor_indicator == 'geen', NA, sum(weegfactor)),
              .groups = 'drop') %>%
    drop_na(-n_cel_gewogen) %>%
    group_by(across(all_of(setdiff(c(jaar_indicator, uitsplitsing, groepering), NA)))) %>%
    mutate(n_totaal = sum(n_cel),
           n_totaal_gewogen = sum(n_cel_gewogen),
           n_min = n_totaal-n_cel,
           p = ifelse(test = n_totaal < Nvar | n_cel < Ncel | n_min < Ncel | n_cel == n_totaal,
                      yes = NA,
                      no = if(weegfactor_indicator == 'geen') n_cel/n_totaal else n_cel_gewogen/n_totaal_gewogen) %>% as.numeric(),
           indicator = indicator,
           omschrijving = omschrijving,
           niveau = niveau_naam) %>%
    {if(!is.na(waarde)) filter_at(., vars(all_of(indicator)), function(x) x %in% waarde) else .} %>%
    ungroup() %>%
    rename(jaar = 1, aslabel = 2) %>%
    {if(!is.na(omschrijving)) mutate(., aslabel = omschrijving) else .} %>% # TODO deze kan misschien weg?
    {if(!is.na(uitsplitsing)) rename_at(., vars(matches(uitsplitsing)), ~ str_replace(., uitsplitsing, 'uitsplitsing')) else .} %>%
    {if(!is.na(groepering)) rename_at(., vars(matches(groepering)), ~ str_replace(., groepering, 'groepering')) else .} %>%
    mutate(across(!n_cel & !n_cel_gewogen & !n_totaal & !n_totaal_gewogen & !n_min & !p & !jaar, to_character)) %>%
    select(indicator, jaar, niveau, aslabel, everything()) 
  
}

## Voorbeeld:
# bereken_cijfers(data = monitor_df,
#                 indicator = 'GZGGA402',
#                 waarde = 1,
#                 omschrijving = 'Ervaren Gezondheid',
#                 niveau_indicator = 'Gemeentecode',
#                 niveau_waarde = 4,
#                 niveau_naam = 'Gemeente D',
#                 jaar_indicator = 'AGOJB401',
#                 jaar = 2022,
#                 #uitsplitsing = 'AGGSA402',
#                 #groepering = 'AGLFA401',
#                 weegfactor_indicator = "Standaardisatiefactor")

# TODO: functie maken om verschil tussen groepen (en/of jaren?) vast te stellen en tekst hierover te printen
# Mogelijk integreren in bereken cijfers functie?

maak_adaptieve_tekst <- function(data, indicator, waarde, omschrijving, niveau_indicator, niveau_waarde, 
                                 niveau_naam, jaar_indicator, jaar, uitsplitsing, groepering = NA, 
                                 weegfactor_indicator = "geen") {
  # Bereken gewogen cijfers
  bereken_cijfers(data = data,
                  indicator = indicator,
                  waarde = waarde,
                  omschrijving = omschrijving,
                  niveau_indicator = niveau_indicator,
                  niveau_waarde = niveau_waarde,
                  niveau_naam = niveau_naam,
                  jaar_indicator = jaar_indicator,
                  jaar = jaar,
                  uitsplitsing = uitsplitsing,
                  groepering = groepering,
                  weegfactor_indicator = weegfactor_indicator) -> result

  # Gewogen cijfers vergelijken en op of groep 1 afwijkt van groep 2
  # Vergelijk 2 groepen:
  if (nrow(result == 2)) {
    
    # Groepen vergelijken
    # TODO zoiets obv Confidence intervals
    # TODo nog testen, wat gebeurd er met NAs? en kan ik dit robuster maken + werkend voor meer uitsplitsingen?
    vergelijking_tekst <- case_when(result$p[1] == result$p[2] ~ "gelijk aan ",
                                    result$p[1] < result$p[2] ~ "lager dan ",
                                    result$p[1] > result$p[2] ~ "hoger dan ",)
    
    # Tekst printen
    print(paste0("Het percentage ", 
                 result$uitsplitsing[1], 
                 " dat ", 
                 result$omschrijving[1], 
                 " is ", 
                 vergelijking_tekst, 
                 " het percentage ", 
                 result$uitsplitsing[2]))

  }

  
}

# Test tekst
# maak_adaptieve_tekst(data = monitor_df,
#                 indicator = 'GZGGA402',
#                 waarde = 1,
#                 omschrijving = 'de gezondheid als (zeer) goed ervaart',
#                 niveau_indicator = 'Gemeentecode',
#                 niveau_waarde = 4,
#                 niveau_naam = 'Gemeente D',
#                 jaar_indicator = 'AGOJB401',
#                 jaar = 2022,
#                 uitsplitsing = 'AGGSA402',
#                 weegfactor_indicator = "Standaardisatiefactor")


##### Survey functies uit oud tabellenboekscript ####

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

#Deze functie wordt niet direct gebruikt in het script, maar wordt binnen de functie kruistabel_maken gebruikt.

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
      ci <- suppressWarnings(attr(svyciprop(eval(parse(text = tekst_formule)),survey_design, method='xlogit', na.rm=TRUE),"ci"))
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


#kruistabel_maken()
#functie die een kruistabel maaakt per variabele. Kan zonder of met crossing.
#Maakt een kruistabel
#Deze functie wordt niet direct gebruikt in het script, maar wordt binnen de functie tabellen_naar_csv gebruikt.
kruistabel_maken <- function(data, variabele = NULL, crossing = NULL, survey_design = NULL,
                             min_observaties_per_vraag = default_Nvar){
  
  #Niet-valide invoer afvangen & warning geven
  if(is.null(data[[variabele]])) {
    
    warning(paste("variabele",variabele, "bestaat niet in dataset. Wordt overgeslagen"))
    return(NULL)
    
  }else{
    
    antwoorden <-  attr(data[[variabele]], "labels") # value labels
    
    #Formule maken voor in svytable
    formule_tekst <- paste0("~ ", substitute(data), "[['", variabele,"']]")
    
    tb <- svytable(formula = eval(parse(text = formule_tekst)), design = survey_design) 
    
    #Als het aantal waarden in tb meer is dan het aantal antwoorden (obv value labels)
    #Betekend dit dat er een ongelabelde waarde is. Dat kan betekenen dat het een Missing is die niet zo is vastgelegd in spss
    #Of het kan betekenen dat een valide waarde ongelabeld is. vanwege deze onduidelijkheid. Harde error met melding
    if(length(antwoorden) < length(tb)){
      
      stop(paste("Er zijn ongelabelde waarden. Controleer variabele", variabele, "in .sav bestand"))
      
    }
    
    
    #svytable slaat antwoorden zonder respondenten over; aanvullen.
    #Als er iets in tb zit; maar het is niet even lang als het aantal antwoorden
    if(length(tb) > 0 & length(tb) < length(antwoorden)){
      
      #verschil lengte tb met lengte antwoorden 
      n_ontbrekende_antwoorden <- length(antwoorden) - length(tb) 
      
      lege_antwoorden <- rep(0,n_ontbrekende_antwoorden)
      names(lege_antwoorden) <- unname(antwoorden)[!unname(antwoorden)%in% names(tb)]
      
      tb <- c(tb, lege_antwoorden) 
      
      #Volgorde van NAMEN tb matchen aan antwoorden.
      tb <- tb[order(match(names(tb),antwoorden))]
      
    }
    ct <- prop.table(tb)*100 # ct bevat estimates als percentages
    
    #Als er geen crossings zijn
    if(is.null(crossing)){
      
      #reken voor ieder antwoord de ci uit
      confidence_intervals <- t(sapply(unname(antwoorden), function(x){prop_ci_berekenen(data = data, 
                                                                                         variabele = variabele,
                                                                                         nummer = x, 
                                                                                         survey_design = survey_design)}))
      
      #Confidence intervals worden gebruikt t.b.v. statistische toetsen. Deze toetsen moeten alleen uitgevoerd worden
      #wanneer het aantal observaties op een vraag minstens de waarde heeft van min_observaties.
      
      #Als er minder dan min_observaties zijn bij een vraag; maak CI's NA.
      if(sum(table(data[[variabele]])) < min_observaties_per_vraag){
        confidence_intervals <- confidence_intervals %>% replace(values = NA)
      }
      
      #Het kan voorkomen dat een variabele alleen maar missing kent voor een bepaalde subset
      #Toch willen we  rijen hebben die alle antwoord-niveuas vastleggen.
      #Als je direct in cbind een lege vector aanroept wordt deze kolom simpelweg niet toegevoegd, dit zorgt voor problemen bij het maken van dataframes met
      #rbind; daarvoor moet er een gelijk aantal kolommen zijn.
      
      #Daarom  expliciet NA toewijzen als  ontbrekende levels op een variabele in  een subset
      #resulteren in vectors die leeg zijn. Geld hier alleen voor ct/estimate. 
      estimate <- ct
      if(length(estimate) == 0){estimate <- NA}
      
      
      n_weighted <- tb
      if(length(n_weighted) == 0){n_weighted <- NA}
      
      kruistabel <- cbind("varcode" = variabele,
                          "waarde" = as.numeric(antwoorden),
                          "label" = names(antwoorden),
                          "n_weighted" = n_weighted,
                          "estimate" = estimate,
                          "ci_upper" = confidence_intervals[,2],
                          "ci_lower" = confidence_intervals[,1],
                          "n_unweighted" = table(factor(data[[variabele]], levels = antwoorden))
      )
      
    #Als er wel crossings zijn
    }else{
      
      #Warning als crossing niet bestaat
      if(is.null(data[[crossing]])){
        stop(paste("crossing",crossing, "bestaat niet in dataset. Kijk configuratie en dataset na"))
        return(NULL)
      }else{
        
        #Als er missings zijn op de crossing. Warning verplaatst naar tabellen_naar_csv
        if(any(is.na(data[[crossing]]))){
          warning(paste("Missing data gevonden bij crossing",crossing))
        }
        
        #CI uitrekenen
        confidence_intervals <- lapply(unname(antwoorden), function(x){prop_ci_berekenen(data = data,
                                                                                         variabele = variabele,
                                                                                         nummer = x, 
                                                                                         crossing = crossing, 
                                                                                         survey_design = survey_design)})
        
        #CI van verschillende antwoordmogelijkheden samenvoegen
        confidence_intervals <- do.call(rbind, confidence_intervals)
        
        
        #Gewogen Pop count uitrekenen 
        population_count <- lapply(unname(antwoorden), function(x){
          
          #Als var. in subset alleen maar NA heeft; lege tabel voor pop.count maken
          if(all(is.na(data[[variabele]]))){
            levels_crossing <- unname(val_labels(data[[crossing]]))
            
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
        crossing_levels <- attr(data[[crossing]],"labels")
        
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
          #Voor confidence intervals  geld ook dat ze  op NA willen zetten als het aantal obs op een vraag per level v.e. crossing
          #lager is dan de opgegeven min_obs. (een makkelijke manier om de vraag te excluderen v.  toetsing sign.)
          te_weinig_obs <- sum(table(factor(data[[variabele]][data[[crossing]] == x], levels = antwoorden))) < min_observaties_per_vraag
          
          ci_upper <- confidence_intervals[confidence_intervals$crossing_var == x,4]
          
          if(te_weinig_obs | length(ci_upper) == 0){
            ci_upper <- NA
          }
          
          ci_lower <- confidence_intervals[confidence_intervals$crossing_var == x,3]
          
          if(te_weinig_obs | length(ci_lower) == 0){
            ci_lower <- NA
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
                "n_unweighted" =  table(factor(data[[variabele]][data[[crossing]] == x], levels = antwoorden)))
          
          
        })
        
        
        kruistabel <- do.call(rbind, kruistabel)
        
      }
    }
    return(data.frame(kruistabel))
    
  }
  
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
## Voorbeeld:
# data = monitor_df
# variabele = GZGGA402
# crossing = AGLFA401
# design = design

# kruistabel_maken(data = monitor_df, variabele = 'GZGGA402', crossing = 'AGLFA401', survey_design = design)
# kruistabel_maken(data = monitor_df, variabele = 'GZGGA402', survey_design = design)