# Run the R scripts
# Load and save library environment
source("initialisation/90_LIBS.R")
setwd("./initialisation/")

source("00_CORE.R")
setwd("..")
source("capture_quantity.R")

# DOCX
# render(here("rmd/00_ALL.Rmd"), 
#        output_dir    = "outputs/", 
#        output_file   = "GTA_Nominal_Catch_Data_Paper.docx"
# )

rmarkdown::render("Comp_FSJ_GTA.Rmd", 
                  output_format = "bookdown::pdf_document2", output_dir = getwd(), output_file = "Recap_compFSJ_GTA", envir = .GlobalEnv)


# rmarkdown::render("Comp_FSJ_GTA.Rmd", 
#                   output_format = "bookdown::word_document2", output_dir = getwd(), output_file = "Recap_compFSJ_GTA", envir = .GlobalEnv)




# rmarkdown::render("Comp_FSJ_GTA_IOTC_only.Rmd", 
#                   output_format = "bookdown::html_document2", output_dir = getwd(),
#                   output_file = "Recap_compFSJ_GTA_IOTC", envir = new.env())

rmarkdown::render("Comp_FSJ_GTA_IOTC_only.Rmd", 
                  output_format = "bookdown::pdf_document2", output_dir = getwd(), 
                  output_file = "Recap_compFSJ_GTA_IOTC", envir = new.env())


bookdown::render_book("Paper_WPDCS_Comp_FSJ_IOTC.Rmd", 
                  output_format = "bookdown::word_document2", output_dir = getwd(),
                  output_file = "Paper_WPDCS_Comp_FSJ_IOTC", envir = new.env())

unlink("_main.Rmd")
bookdown::render_book("Paper_WPDCS_Comp_FSJ_IOTC.Rmd", 
                  output_format = "bookdown::html_document2", output_dir = getwd(),
                  output_file = "Paper_WPDCS_Comp_FS_GTA", envir = new.env())

unlink("_main.Rmd")

bookdown::render_book("Paper_WPDCS_Comp_FSJ_IOTC.Rmd", 
                  output_format = "bookdown::pdf_document2", output_dir = getwd(), 
                  output_file = "Paper_WPDCS_Comp_FS_GTA", envir = new.env())

bookdown::render_book("Paper_WPDCS_Comp_FSJ_IOTC.Rmd", 
                      output_format = "bookdown::word_document2", output_dir = getwd(),
                      output_file = "Paper_WPDCS_Comp_FS_GTA", envir = new.env())


rmarkdown::render("WPDCS-oral-presentation-.Rmd")
rmarkdown::render("WPDCS-oral-presentation-.Rmd", output_format = "powerpoint_presentation")


