#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#fictieve gemeenten die gemeentecodes 1 t/m 6 hebben
alle_gemeentecodes <- 1:6

#Output als HTML
for(gemeentecode in alle_gemeentecodes){
  
  quarto::quarto_render(
    input = "voorbeeld_rapportage.qmd",
    output_format = "html",
    output_file = glue::glue("gemeenterapport_{gemeentecode}.html"),
    execute_params = list(
      gemeentecode = gemeentecode
    )
    )
  
}

#TEST PDF. Via chrome_print() om .css te bewaren
#TODO pagebreaks via css class regelen doen
#TODO flexbox / width & alignment instellingen goed krijgen voor landscape pdf
# pagedown::chrome_print(
#   input = glue("gemeenterapport_{gemeentecode}.html"),
#   
#   options = list(#preferCSSPageSize = FALSE,
#                  printBackground = TRUE,
#                  landscape = TRUE))
  