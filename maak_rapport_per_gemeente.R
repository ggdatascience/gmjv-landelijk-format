#maak_rapport_gemeenten
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

#SPSS data inlezen
#bestandsnaam
file_path = "nep testdata GMJV - Regionaal trendbestand 2022-2024.sav"
#lees SPSS bestand & converteer 'user-missing' naar missing in R (NA)
monitor_df = haven::read_spss(file_path,user_na =T) %>%
  labelled::user_na_to_na()


# Hier naam variabele gemeente invullen -----------------------------------
#names(monitor_df)

var_gemeente = "Gemeentecode"

alle_gemeentecodes = monitor_df[[var_gemeente]] %>% unique() %>% as.numeric() %>% sort()

alle_gemeentecodes


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

#hoe pdf nu (nog niet helemaal) werkt
# Layout is goed. Schaling v. tekst nog fixen & table of contents.
# TODO oplossen;  URLS uit render_toc() zijn klikbaar maar verwijzen niet naar de juiste plek.
# TODO paginanummers in TOCH
# TODO schaling lettergrote.

# render eerst een html bestand & dat vertalen naar pdf met chrome_print
# TOC aan de zijkant werkt niet voor PDF; dus TOC moet in body
  #TOC-location in metadata meegeven is onvoldoende; want is bovenaan (& lelijk)
  #custom functie render_toc van github; aangepast zodat ook commas verwerkt kunnen worden.


#Dit in qmd zetten: (zie render_toc() hieronder)
#YAML
#voor_pdf: false

# ```{r, echo=F}
# if(params$voor_pdf){
#   render_toc(
#     toc_header_name = "Inhoudsopgave",
#     "voorbeeld_rapportage.qmd")
# }
# 
# ```

#Test pdf
# gemeentecode = 1
# 
# quarto::quarto_render(
#   input = "voorbeeld_rapportage.qmd",
#   output_format = "html",
#   output_file = glue::glue("gemeenterapport_{gemeentecode}.html"),
#   execute_params = list(
#     gemeentecode = gemeentecode,
#     voor_pdf = TRUE
#   ),
#   metadata = list(`toc-location` = "body",
#                   `toc-title` = "Inhoudsopgave")
#   )
# #TEST PDF. Via chrome_print() om .css te bewaren
# #TODO pagebreaks via css class regelen doen
# #TODO flexbox / width & alignment instellingen goed krijgen voor landscape pdf
# pagedown::chrome_print(
#   input = glue::glue("gemeenterapport_{gemeentecode}.html"),
# 
#   options = list(#preferCSSPageSize = FALSE,
#                  printBackground = TRUE,
#                  landscape = TRUE))


#RENDER TOC
#https://gist.github.com/gadenbuie/c83e078bf8c81b035e32c3fc0cf04ee8
#' Render Table of Contents
#' 
#' A simple function to extract headers from an RMarkdown or Markdown document
#' and build a table of contents. Returns a markdown list with links to the 
#' headers using 
#' [pandoc header identifiers](http://pandoc.org/MANUAL.html#header-identifiers).
#' 
#' WARNING: This function only works with hash-tag headers.
#' 
#' Because this function returns only the markdown list, the header for the
#' Table of Contents itself must be manually included in the text. Use
#' `toc_header_name` to exclude the table of contents header from the TOC, or
#' set to `NULL` for it to be included.
#' 
#' @section Usage:
#' Just drop in a chunk where you want the toc to appear (set `echo=FALSE`):
#' 
#'     # Table of Contents
#' 
#'     ```{r echo=FALSE}
#'     render_toc("/path/to/the/file.Rmd")
#'     ```
#' 
#' @param filename Name of RMarkdown or Markdown document
#' @param toc_header_name The table of contents header name. If specified, any
#'   header with this format will not be included in the TOC. Set to `NULL` to
#'   include the TOC itself in the TOC (but why?).
#' @param base_level Starting level of the lowest header level. Any headers 
#'   prior to the first header at the base_level are dropped silently.
#' @param toc_depth Maximum depth for TOC, relative to base_level. Default is
#'   `toc_depth = 3`, which results in a TOC of at most 3 levels.
# render_toc <- function(
#     filename, 
#     toc_header_name = "Table of Contents",
#     base_level = NULL,
#     toc_depth = 3
# ) {
#   x <- readLines(filename, warn = FALSE)
#   x <- paste(x, collapse = "\n")
#   x <- paste0("\n", x, "\n")
#   for (i in 5:3) {
#     regex_code_fence <- paste0("\n[`]{", i, "}.+?[`]{", i, "}\n")
#     x <- gsub(regex_code_fence, "", x)
#   }
#   x <- strsplit(x, "\n")[[1]]
#   x <- x[grepl("^#+", x)]
#   if (!is.null(toc_header_name)) 
#     x <- x[!grepl(paste0("^#+ ", toc_header_name), x)]
#   if (is.null(base_level))
#     base_level <- min(sapply(gsub("(#+).+", "\\1", x), nchar))
#   start_at_base_level <- FALSE
#   x <- sapply(x, function(h) {
#     level <- nchar(gsub("(#+).+", "\\1", h)) - base_level
#     if (level < 0) {
#       stop("Cannot have negative header levels. Problematic header \"", h, '" ',
#            "was considered level ", level, ". Please adjust `base_level`.")
#     }
#     if (level > toc_depth - 1) return("")
#     if (!start_at_base_level && level == 0) start_at_base_level <<- TRUE
#     if (!start_at_base_level) return("")
#     if (grepl("\\{#.+\\}(\\s+)?$", h)) {
#       # has special header slug
#       header_text <- gsub("#+ (.+)\\s+?\\{.+$", "\\1", h)
#       header_slug <- gsub(".+\\{\\s?#([-_.a-zA-Z]+).+", "\\1", h)
#     } else {
#       header_text <- gsub("#+\\s+?", "", h)
#       header_text <- gsub("\\s+?\\{.+\\}\\s*$", "", header_text) # strip { .tabset ... }
#       header_text <- gsub("^[^[:alpha:]]*\\s*", "", header_text) # remove up to first alpha char
#       header_text <- gsub(",","", header_text) #remove ","
#       header_slug <- paste(strsplit(header_text, " ")[[1]], collapse="-")
#       header_slug <- tolower(header_slug)
#     }
#     paste0(strrep(" ", level * 4), "- [", header_text, "](#", header_slug, ")")
#   })
#   x <- x[x != ""]
#   knitr::asis_output(paste(x, collapse = "\n"))
# }
# 
# 

  