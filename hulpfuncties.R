#Testonzin
maak_staafdiagram_schoolprofiel <- function(df, var_inhoud, var_crossings, titel = "",
                                            #TODO  naar andere plek halen zodat makkelijk in te stellen is?
                                            kleuren_grafiek = c("#009898","#91caca","#009428")
                                            ){
  
  #TODO default titel het vraaglabel maken
  
  df_plot <- lapply(var_crossings, function(crossing){
    
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
    ylim(0,100) +
    ggtitle(titel) +
    #Hier worden de kleuren en volgorde bepaald.
    scale_fill_manual(values= kleuren,
                      breaks = df_plot$onderdeel,
                      guide = guide_legend(nrow = 1, byrow = TRUE, label.position = "top", title.position = "top")
                      ) +
    theme(axis.title = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.x = unit(5, 'cm'),
          legend.position = "bottom",
          plot.title = element_text(hjust = .5),
          axis.line.x.bottom = element_line(linewidth = 1),
          axis.line.y.left = element_line(linewidth = 1)
    )
}
