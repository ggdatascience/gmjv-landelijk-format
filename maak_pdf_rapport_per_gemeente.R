#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

# Hier bestandsnaam invullen ----------------------------------------------
file_path = "nep testdata GMJV - Regionaal trendbestand 2022-2024.sav"
# Hier code regio invullen ------------------------------------------------
regiocode = 23
# Hier naam variabele gemeente invullen -----------------------------------
var_gemeente = "Gemeentecode"

#SPSS data inlezen
#lees SPSS bestand & converteer 'user-missing' naar missing in R (NA)
monitor_df = haven::read_spss(file_path,user_na =T) %>%
  labelled::user_na_to_na()



gemeentecodes_in_regio = monitor_df[[var_gemeente]][monitor_df$GGDregio == regiocode] %>% 
  unique() %>% 
  as.numeric() %>% 
  sort()

# PDF uitdraai ------------------------------------------------------------

for(gemeentecode in alle_gemeentecodes){
  
  html_file =  glue::glue("gemeenterapport_{gemeentecode}.html")
  
  #eerst een html maken waarbij expliciet een TOC wordt gegenereerd met render_toc() 
  quarto::quarto_render(
    input = "voorbeeld_rapportage.qmd",
    output_format = "html",
    output_file = html_file,
    execute_params = list(
      gemeentecode = gemeentecode,
      regiocode = regiocode,
      is_pdf = TRUE
    ),
    #Metadata aanpassen zodat inhoudsopgave naar body verplaatst wordt
    #Verdwijnt bij chrome_print() maar nodig omdat er anders een vlak leeg wordt gehouden
    #aan de zijkant van de rapportage
    metadata = list(`toc-location` = "body",
                    `toc-title` = "Inhoudsopgave",
                    `toc-float` = "false")
  )
  #Daarna met chrome_print omzetten naar pdf. Deze methode zorgt ervoor dat we
  #CSS niet genegeerd bij renderen naar pdf
  
  #Resulteert standaard in favicon error; fixen door html te injecteren.
  #zie: https://appwrk.com/resolving-favicon-ico-404-errors
  #zie: https://stackoverflow.com/questions/1321878/how-to-prevent-favicon-ico-requests/13416784#13416784
  
  #fix voor favicon error in html bestand toevoegen
  html_lines <- readLines(html_file, warn = F)
  html_lines <- sub('</head>',
                    '<link rel="icon" href="data:,"> \n</head>',
                    html_lines)
  writeLines(html_lines, html_file)
  
  #html omzetten naar pdf
  pagedown::chrome_print(
    input = html_file,
    options = list(preferCSSPageSize = FALSE,
                   printBackground = TRUE,
                   scale = .9,
                   landscape = TRUE,
                   margin = list(
                     top = "0.5in",
                     bottom = "0.5in",
                     left = "0.5in",
                     right = "0.5in"
                   )))
  
  file.remove(glue::glue("gemeenterapport_{gemeentecode}.html"))
  
}

#Issue met foute verwijzing links hoeven we zelf niet aan te werken. wordt gefixt in update julie v. google chrome :)
#https://stackoverflow.com/questions/78750596/pagedownchrome-print-links-go-to-wrong-page

#Is inmiddels opgelost bij chrome versie 128 + 


