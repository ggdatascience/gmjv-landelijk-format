#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#fictieve gemeenten die gemeentecodes 1 t/m 6 hebben
alle_gemeentecodes <- 1:6

#Output als HTML
for(gemeentecode in alle_gemeentecodes){
  
  quarto::quarto_render(
    input = "voorbeeld_landscape_html.qmd",
    output_file = glue::glue("gemeenterapport_{gemeentecode}.html"),
    execute_params = list(
      gemeentecode = gemeentecode
    )
    )
  
}


#Output als pdf
#NB het kan zijn dat je het e.e.a moet updaten aan 'TinyTex'
#om pdf uit te draaien;
# klik op: View -> Move focus to terminal.
# Er opent nu linksonder een Terminal;
#hier type je: 'quarto install tinytex --update-path'

for(gemeentecode in alle_gemeentecodes){
  
  quarto::quarto_render(
    input = "voorbeeld_landscape_html.qmd",
    output_format = "pdf",
    output_file = glue::glue("gemeenterapport_{gemeentecode}.pdf"),
    execute_params = list(
      gemeentecode = gemeentecode
    )
  )
  
}
