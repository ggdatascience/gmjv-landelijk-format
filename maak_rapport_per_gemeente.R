#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

#bestandsnaam
file_path = "nep testdata GMJV - Regionaal trendbestand 2022-2024.sav"
regiocode = 23
#lees SPSS bestand & converteer 'user-missing' naar missing in R (NA)
monitor_df = haven::read_spss(file_path,user_na =T) %>%
  labelled::user_na_to_na()

gemeentecodes_in_regio = monitor_df$Gemeentecode[monitor_df$GGDregio == regiocode] %>% 
  unique() %>% 
  as.numeric() %>% 
  sort()

#print codes
gemeentecodes_in_regio

# HTML uitdraai -----------------------------------------------------------
for(gemeentecode in gemeentecodes_in_regio){

  quarto::quarto_render(
    input = "voorbeeld_rapportage.qmd",
    output_format = "html",
    output_file = glue::glue("gemeenterapport_{gemeentecode}.html"),
    execute_params = list(gemeentecode = gemeentecode)
    )
  
}
