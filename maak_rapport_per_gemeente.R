#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)


#Hier regiocode invoeren
regiocode <- 23

#vector met numerieke waarden gemeentecodes in regio
gemeentecodes_in_regio = c(2,3,4,5,6) #codes nepgemeenten voorbeeldrapportage

# HTML uitdraai per gemeente -----------------------------------------------------------
for(gemeentecode in gemeentecodes_in_regio){

  quarto::quarto_render(
    input = "voorbeeld_rapportage.qmd", # Wijzig dit naar "Rapportage.qmd" als je deze voor meerdere gemeentes wil uitdraaien
    output_format = "html",
    output_file = glue::glue("gemeenterapport_{gemeentecode}.html"),
    execute_params = list(
      gemeentecode = gemeentecode,
      regiocode = regiocode)
    )
  
}