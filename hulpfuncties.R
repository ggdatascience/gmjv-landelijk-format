library(gt)
library(dplyr)
library(ggplot2)
library(tidyr)


maak_staafdiagram_dubbele_uitsplitsing <- function(df, var_inhoud, var_crossing_groep, var_crossing_kleur, titel = "",
                                                   kleuren_grafiek = c("#009898","#91caca","#009428")){
  
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
                                           kleuren_grafiek = c("#009898","#91caca","#009428")
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


#TODO
#TEST MET EN ZONDER CROSSING
maak_staafdiagram_meerdere_staven <- function(df, var_inhoud,var_crossing = NULL, 
                                              titel = "",
                                              kleuren_grafiek = c("#009898","#91caca","#009428"),
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
# TODO verder gaan, dit is pas het begin van de code.

bereken_cijfers <- function(data, indicator, niveau_indicator, niveau_waarde, 
                            jaar_indicator, jaar, uitsplitsing, groepering, 
                            weegfactor_indicator) {
  data %>%
    filter(.[niveau_indicator] == niveau_waarde & .[jaar_indicator] == jaar) %>%
    select(all_of(setdiff(c(jaar_indicator, indicator, uitsplitsing, groepering, weegfactor_indicator), NA)))
}

# Testen functie
bereken_cijfers(data = monitor_df, 
                indicator = 'GZGGA402',
                niveau_indicator = 'Gemeentecode',
                niveau_waarde = 2,
                jaar_indicator = 'AGOJB401',
                jaar = 2022,
                uitsplitsing = 'AGGSA402', 
                groepering = 'AGLFA401', 
                weegfactor_indicator = "Standaardisatiefactor")

