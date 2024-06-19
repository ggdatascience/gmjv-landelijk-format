# Script met helpfuncties voor GMJV 2024.
# Packages ----------------------------------------------------------------

# Het script maakt gebruik van een aantal packages
# Deze moeten bij de eerste keer lokaal worden geinstalleerd. 
# Dat doe je met behulp van de functie: install.packages() 
# (Verwijder de # aan het begin van onderstaande regel om de code te runnen en de benodigde packages te installeren.)
# TODO hier nog iets van maken met een fucntie die checkt of deze package al is geinstalleerd, en anders installeren?
# install.packages(c('tidyverse', 'haven', 'labelled'))

# Hieronder worden de benodige packages geladen
library(gt)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr) # Voor str_replace
library(labelled) # Package om te werken met datalabels, o.a. voor to_character()


# Standaard instellingen --------------------------------------------------

# Standaard kleuren instellen
default_kleuren <- c("#009898","#91caca","#009428")

# Default minimum aantallen instellen
default_Nvar = 100 # Minimum aantal invullers per vraag.
default_Ncel = 10 # Minimum aantal invullers oper antwoordoptie.

##### Maak responstabel ####
maak_responstabel <- function(df, crossings = NULL){
  
  
  
  
  rijnamen <- c("Totaal", "Klas 2", "Klas 4", "Vmbo", "Havo/Vwo", "Jongen","Meisje")
  waarden <- sample(1:300, 7)
  waarden[1] <- sum(waarden[2:3])
  waarden[6:7] <- waarden[1]/2
  
  nepdata <- data.frame("Groep" =  rijnamen, "Aantal ingevulde vragenlijsten" =  waarden)
  
  nepdata %>% 
    gt() %>% 
    # Bovenste rij roze kleur
    tab_style(style = cell_fill(color = "#e8525f"), locations = cells_column_labels()) %>% 
    # Totaal en gender donkergroen
    tab_style(style = cell_fill(color = "#009898"), locations = cells_body(rows = c(1, 4, 5))) %>% 
    # Klas lichtgroen
    tab_style(style = cell_fill(color = "#91caca"), locations = cells_body(rows = c(2, 3,6,7))) %>% 
    # Wit lettertype
    tab_style(style = cell_text(color = "#FFFFFF"), locations = cells_column_labels()) %>% 
    tab_style(style = cell_text(color = "#FFFFFF"), locations = cells_body()) %>% 
    # Kolomnaam Aantal vetgedrukt
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>% 
    # Verwijder kolomnaam boven eerste kolom
    cols_label(matches("Groep") ~ "") %>% 
    # Pas kolomnaam Aantal aan
    cols_label(matches("Aantal") ~ "Aantal ingevulde vragenlijsten") %>% 
    # Per default wordt de tabel gecentreerd op de pagina. Zet deze volledig naar links.
    tab_options(table.margin.left = 0,
                table.margin.right = 0) 
  
}

maak_staafdiagram_dubbele_uitsplitsing <- function(df, var_inhoud, var_crossing_groep, var_crossing_kleur, titel = "",
                                                   kleuren_grafiek = default_kleuren){
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
    mutate(aantal_vraag = sum(aantal_antwoord)) %>% 
    ungroup() %>% 
    mutate(percentage = round((aantal_antwoord/aantal_vraag)*100)) %>% 
    filter(!!sym(var_inhoud) == 1)
  
  #TODO; Bij te lage aantallen -> Kolom als leeg weergeven net een sterretje in de kolom

  
  df_plot[[var_crossing_groep]] <- factor(df_plot[[var_crossing_groep]], 
                                          levels = val_labels(df_plot[[var_crossing_groep]]),
                                          labels = names(val_labels(df_plot[[var_crossing_groep]])))
  
  
  df_plot[[var_crossing_kleur]] <- factor(df_plot[[var_crossing_kleur]], 
                                          levels = val_labels(df_plot[[var_crossing_kleur]]),
                                          labels = names(val_labels(df_plot[[var_crossing_kleur]])))
  
  
  namen_kleuren <- levels(df_plot[[var_crossing_kleur]])
  kleuren <- kleuren_grafiek[1:length(namen_kleuren)]
  
  ggplot(df_plot) +
    geom_col(aes(x = !!sym(var_crossing_groep), y = percentage, fill = !!sym(var_crossing_kleur)),
             position = position_dodge(width = 0.8), width = 0.8) +
    geom_text(aes(x = !!sym(var_crossing_groep),
                  y = percentage,
                  label = paste(percentage,"%"),
                  vjust = -1),
              position = position_dodge2(width = 0.8),
              size = 5,
    ) +
    ggtitle(titel) +
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "right", title.position = "top")
    ) +
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
    )
}

maak_staafdiagram_vergelijking <- function(df, var_inhoud, var_crossings, titel = "",
                                           kleuren_grafiek = default_kleuren
                                             ){

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
              ) %>% 
       rename("onderdeel" = 2) %>% 
       filter(!!sym(var_inhoud) == 1) %>% 
       mutate(onderdeel = val_labels(onderdeel) %>% names())
     
     
     df_crossing$kleuren <- kleuren_grafiek[1:nrow(df_crossing)]
      
     df_crossing
       
   }) %>% do.call(rbind,.)
   
    
   
   #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
   df_plot$groep <- factor(df_plot$groep)
   #volgorde onderdeel vastzetten o.b.v dataframe 
   df_plot$onderdeel <- factor(df_plot$onderdeel, levels = df_plot$onderdeel)

   
   kleuren <- df_plot$kleuren
   names(kleuren) <- df_plot$onderdeel


   ggplot(df_plot) +
     geom_col(aes(x = groep, y = percentage, fill = onderdeel),
              position = position_dodge(width = 0.8), width = 0.8) +
     geom_text(aes(x = groep,
                   y = percentage,
                   label = paste(percentage,"%"),
                   vjust = -1),
               position = position_dodge2(width = 0.8),
               size = 5,
     ) +
     ggtitle(titel) +
     #Hier worden de kleuren en volgorde bepaald.
     scale_fill_manual(values= kleuren,
                       guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "right", title.position = "top")
                       ) +
     scale_y_continuous(limits = c(0,100),
                        breaks = seq(0,100, by = 10),
                        labels = paste(seq(0,100, by = 10),"%"),
                        expand = expansion(mult = c(0, 0.05))
     ) +
     coord_cartesian(ylim = c(0,100))+
     #scale_x_discrete(values = var_crossings) +
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
     ) 
 }

maak_staafdiagram_meerdere_staven <- function(df, var_inhoud,var_crossing = NULL, 
                                              titel = "",
                                              kleuren_grafiek = default_kleuren,
                                              flip = FALSE
){
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
    mutate(percentage = round((aantal_antwoord/aantal_vraag)*100))
  
  
  #TODO hier een functie van maken: labelled_dbl_to_factor
  df_plot[[var_crossing]] <- factor(df_plot[[var_crossing]], 
                                          levels = val_labels(df_plot[[var_crossing]]),
                                          labels = names(val_labels(df_plot[[var_crossing]])))
  
  df_plot[[var_inhoud]] <- factor(df_plot[[var_inhoud]], 
                                    levels = val_labels(df_plot[[var_inhoud]]),
                                    labels = names(val_labels(df_plot[[var_inhoud]])))
  
  namen_kleuren <- levels(df_plot[[var_crossing]])
  
  kleuren <- kleuren_grafiek[1:length(namen_kleuren)]
  
  plot = ggplot(df_plot) +
    geom_col(aes(x = !!sym(var_inhoud), y = percentage, fill = !!sym(var_crossing)),
             position = position_dodge(width = 0.8), width = 0.8) +
    geom_text(aes(x = !!sym(var_inhoud),
                  y = percentage,
                  label = paste(percentage,"%"),
                  vjust = v_just_text,
                  hjust = h_just_text),
              position = position_dodge2(width = 0.8),
              size = 5,
    ) +
    ggtitle(titel) +
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "right", title.position = "top")
    ) +
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


# Functie maken om cijfers te berekenen.
# Gebaseerd op functie uitrapportage monitor GMJ 2023.
# Zie https://github.com/ggdatascience/rapportage_monitor_gmj 

bereken_cijfers <- function(data, indicator, waarde, omschrijving, niveau_indicator, niveau_waarde, 
                            niveau_naam, jaar_indicator, jaar, uitsplitsing, groepering, 
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

#Pieter: Test/Toepassing  functie tijdelijk uitgezet.  
# # Testen functie
# bereken_cijfers(data = monitor_df,
#                 indicator = 'GZGGA402',
#                 waarde = 1,
#                 omschrijving = 'Ervaren Gezondheid',
#                 niveau_indicator = 'Gemeentecode',
#                 niveau_waarde = 4,
#                 niveau_naam = 'Gemeente D',
#                 jaar_indicator = 'AGOJB401',
#                 jaar = 2022,
#                 uitsplitsing = 'AGGSA402',
#                 groepering = 'AGLFA401',
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
