library(gt)
library(dplyr)
library(ggplot2)
library(tidyr)




maak_staafdiagram_dubbele_uitsplitsing <- function(df, var_inhoud, var_crossing_groep, var_crossing_kleur, titel = "",
                                                   kleuren_grafiek = c("#009898","#91caca","#009428")){
  
  df_plot <-   df %>% filter(variabele == var_inhoud) %>% 
    group_by(!!sym(var_crossing_groep),!!sym(var_crossing_kleur)) %>% 
    summarise(n_observaties = sum(n_observaties),
              ja = sum(ja)) %>% 
    rowwise() %>% 
    mutate(percentage = (ja / n_observaties * 100) %>% round())
  
  namen_kleuren <- df_plot[[var_crossing_kleur]] %>% unique()
  kleuren <- kleuren_grafiek[1:length(namen_kleuren)]
  
  names(kleuren) <- namen_kleuren
  
  
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
                      guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "right", title.position = "top",
                                           # keywidth = 1,
                                           # default.unit = "inch"
                                           )
    ) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, by = 10),
                       labels = paste(seq(0,100, by = 10),"%")
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
                                             #TODO  naar andere plek halen zodat makkelijk in te stellen is?
                                             kleuren_grafiek = c("#009898","#91caca","#009428")
                                             ){

   #TODO default titel het vraaglabel maken

   df_plot <- lapply(var_crossings, function(crossing){

     #Per crossing % uitrekenen & kleuren toewijzen
     df_crossing <- df %>%
       filter(variabele == var_inhoud) %>%
       group_by_at(crossing) %>%
       summarise(n_observaties = sum(n_observaties),
                 ja = sum(ja)) %>%
       ungroup() %>%
       rowwise() %>%
       mutate(percentage = (ja / n_observaties * 100) %>% round(),
              groep = crossing
              ) %>%
       rename("onderdeel" = 1)

     df_crossing$kleuren <- kleuren_grafiek[1:nrow(df_crossing)]

     df_crossing


   }) %>% do.call(rbind,.)
   
   #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
   df_plot$groep <- factor(df_plot$groep, levels = var_crossings)
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
                        labels = paste(seq(0,100, by = 10),"%")
     ) +
     #scale_x_discrete(values = var_crossings) +
     theme(axis.title = element_blank(),
           panel.background = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           legend.title = element_blank(),
           legend.spacing.x = unit(.1, 'cm'),
           legend.position = "bottom",
           plot.title = element_text(hjust = .5),
           axis.line.x.bottom = element_line(linewidth = 1),
           axis.line.y.left = element_line(linewidth = 1)
     )
 }



maak_staafdiagram_meerdere_staven <- function(df, var_inhoud, var_crossings, titel = "",
                                              #TODO  naar andere plek halen zodat makkelijk in te stellen is?
                                              kleuren_grafiek = c("#009898","#91caca","#009428")
){
  
  #TODO default titel het vraaglabel maken
  
  df_plot <- lapply(var_crossings, function(crossing){
    
    #Per crossing % uitrekenen & kleuren toewijzen
    df_crossing <- df %>%
      filter(variabele == var_inhoud) %>%
      group_by_at(crossing) %>%
      summarise(n_observaties = sum(n_observaties),
                ja = sum(ja)) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(percentage = (ja / n_observaties * 100) %>% round(),
             groep = crossing
      ) %>%
      rename("onderdeel" = 1)
    
    df_crossing$kleuren <- kleuren_grafiek[1:nrow(df_crossing)]
    
    df_crossing
    
    
  }) %>% do.call(rbind,.)
  
  #volgorde groepen op x-as vastzetten o.b.v. volgorde variabelen door er een factor vna te maken
  df_plot$groep <- factor(df_plot$groep, levels = var_crossings)
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
                       labels = paste(seq(0,100, by = 10),"%")
    ) +
    #scale_x_discrete(values = var_crossings) +
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(.1, 'cm'),
          legend.position = "bottom",
          plot.title = element_text(hjust = .5),
          axis.line.x.bottom = element_line(linewidth = 1),
          axis.line.y.left = element_line(linewidth = 1)
    )
}