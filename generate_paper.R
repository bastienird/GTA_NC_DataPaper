source("initialisation/90_LIBS.R")
setwd("./initialisation/")
# 
source("00_CORE.R")
setwd("..")
source("capture_quantity2.R")

source("Paper_WPDCS_Comp_FSJ_IOTC_Rev1.R")

unlink("_main.Rmd")

bookdown::render_book(output_format = "bookdown::pdf_document2", output_dir = getwd(), 
                      output_file = "Paper_WPDCS_Comp_FSJ_IOTC_Rev1", envir = new.env())



rmarkdown::render("WPDCS-oral-presentation-.Rmd")
# fro djibouti all donnée YFT are now in TUN --> 