
if(!file.exists(here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R"))){
source(here::here("R/purl_with_logs"))
purl_with_logs(here::here("Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd"),
               output = here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R"),
               documentation = 2)
}
source(here::here("Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R")) # loading the data, mapping, filtering and creating all the outptus needed for the paper.

bookdown::render_book(output_format = "bookdown::pdf_document2", output_dir = getwd(), 
                      output_file = "Paper_WPDCS_COMP_FSJ_IOTC_Rev2", envir = new.env())

rmarkdown::render("WPDCS-oral-presentation-.Rmd")
