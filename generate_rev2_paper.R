
if(!file.exists(here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R"))){
source(here::here("R/purl_with_logs"))
purl_with_logs(here::here("Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd"),
               output = here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R"),
               documentation = 2)
}

if(!dir.exists(here::here("outputs"))){
  dir.create(here::here("outputs"))
}
if(!dir.exists(here::here("outputs/figs_final"))){
  dir.create(here::here("outputs/figs_final"))
}
if(!dir.exists(here::here("outputs/figs"))){
  dir.create(here::here("outputs/figs"))
}

source(here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R")) # loading the data, mapping, filtering and creating all the outptus needed for the paper.

bookdown::render_book(output_format = "bookdown::pdf_document2", output_dir = getwd(), 
                      output_file = here::here("Paper_WPDCS_COMP_FSJ_IOTC_Rev2"), envir = new.env())

rmarkdown::render(here::here("WPDCS-oral-presentation-.Rmd"))
