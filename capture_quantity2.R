source("initialisation/90_LIBS.R")
setwd("./initialisation/")
# 
source("00_CORE.R")
setwd("..")
# 1) lecture des données
setwd(here::here())
map <- read_csv("mapping.csv")


CAPTURED <- as.data.frame(CAPTURE)
NCD <- as.data.frame(NC)
NCD <- NCD %>% dplyr::left_join(as.data.frame(MAPPING_FLEET_FSJ_COUNTRY %>% dplyr::filter(code != "GHA")), by = c("country_code" = "code")) %>% 
  dplyr::mutate(fsj_country_code = ifelse(is.na(fsj_country_code), country_code, fsj_country_code))


Mapping_code_country_code <- CAPTURED %>% dplyr::select(COUNTRY, COUNTRY_CODE) %>% dplyr::distinct()

NCD <- NCD %>% dplyr::left_join(Mapping_code_country_code, by = c("fsj_country_code" = "COUNTRY_CODE"))%>% 
  dplyr::mutate(fishing_fleet_label = ifelse(!is.na(COUNTRY), COUNTRY, country )) %>% 
  dplyr::mutate(fishing_fleet_label = ifelse(is.na(fishing_fleet_label), "Other nei", fishing_fleet_label))


# 2) renommage automatique
names(CAPTURED)[ match(map$source, names(CAPTURED)) ] <- map$target



# Mapping_ocean and source_autority ---------------------------------------

CAPTURED$Ocean <- recode(CAPTURED$ocean_basin,
                         `America, South - Inland waters` = "Western Pacific Ocean",
                         `Atlantic, Northwest` = "Western Atlantic Ocean",
                         `Atlantic, Northeast` = "Eastern Atlantic Ocean",
                         `Atlantic, Western Central` = "Western Atlantic Ocean",
                         `Atlantic, Eastern Central` = "Eastern Atlantic Ocean",
                         `Mediterranean and Black Sea` = "Mediterranean and Black Sea",
                         `Atlantic, Southwest` = "Western Atlantic Ocean",
                         `Atlantic, Southeast` = "Eastern Atlantic Ocean",
                         `Atlantic, Antarctic` = "Atlantic Antarctic",
                         `Indian Ocean, Western` = "Western Indian Ocean",
                         `Indian Ocean, Eastern` = "Eastern Indian Ocean",
                         `Indian Ocean, Antarctic` = "Indian Antarctic",
                         `Pacific, Northwest` = "Western Pacific Ocean",
                         `Pacific, Northeast` = "Eastern Pacific Ocean",
                         `Pacific, Western Central` = "Western Pacific Ocean",
                         `Pacific, Eastern Central` = "Eastern Pacific Ocean",
                         `Pacific, Southwest` = "Western Pacific Ocean",
                         `Pacific, Southeast` = "Eastern Pacific Ocean",
                         `Pacific, Antarctic` = "Pacific Antarctic")

# Supprime la clé actuelle (ici fishing_fleet) et remet la table en mode "sans clé"
mapping_ocean_source_authority <- NCD %>% dplyr::select(source_authority, Ocean)%>% distinct() %>% 
  dplyr::mutate(source_authority = ifelse (Ocean == "Eastern Pacific Ocean", NA, source_authority)) %>% 
  dplyr::mutate(source_authority = ifelse (Ocean == "Pacific Antarctic", NA, source_authority)) %>% 
  dplyr::mutate(source_authority = ifelse (Ocean == "Indian Antarctic", NA, source_authority)) %>% 
  dplyr::mutate(source_authority = ifelse (Ocean == "Atlantic Antarctic", NA, source_authority)) %>% 
  dplyr::distinct() 
CAPTURED <- as.data.frame(CAPTURED) %>% dplyr::left_join(as.data.frame(mapping_ocean_source_authority), by = "Ocean")

CAPTURED$measurement_unit <- "t"
class(NCD$measurement_unit) <- "character"
NCD$measurement_unit <- "t"

FS_not_NC_label <- setdiff(CAPTURED$fishing_fleet_label, NCD$fishing_fleet_label)
NC_not_FS_label <- setdiff(NCD$fishing_fleet_label, CAPTURED$fishing_fleet_label)

mapping_from_FSJ_to_NC <- read_delim("inputs/mappings/Mapping_from_fsj_to_nc.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


CAPTURED_test <- CAPTURED %>% dplyr::filter(fishing_fleet_label%in%FS_not_NC_label)%>% dplyr::left_join(mapping_from_FSJ_to_NC %>% dplyr::select(FSJ, NC), 
                                                                                                        by = c("fishing_fleet_label"="FSJ"))%>% 
  dplyr::mutate(fishing_fleet_label = ifelse(!is.na(NC), NC, fishing_fleet_label)) %>% dplyr::select(-NC)

CAPTURED <- rbind(CAPTURED_test, CAPTURED %>% dplyr::filter(!fishing_fleet_label%in%FS_not_NC_label))


CAPTURED$species_name <- recode(CAPTURED$species_name,
                                `True tunas NEI` = "True tunas nei",
                                `Tunas NEI` = "Tunas nei")



require(CWP.dataset)

CAPTURED$year <- paste0(CAPTURED$year, "-01-01")
NCD$year <- paste0(NCD$year, "-01-01")

CAPTURED$species_name <- gsub("NEI", "nei", CAPTURED$species_name)

qs::qsave(CAPTURED, "inputs/data/FSJ/FS_MAPPED.qs")
qs::qsave(NCD, "inputs/data/GTA/NCD_MAPPED.qs")

## ALL MAPPING DONE

# Next groupping when no mapping possible

# source("for_each_country.R")


species_intersect <- c(intersect(unique(CAPTURED$species), unique(NCD$species)), "FRZ")

CAPTURED_filtered <- CAPTURED %>% dplyr::filter(species %in% species_intersect) %>% 
  dplyr::filter(species != "SBF")

NCD_filtered <- NCD%>% dplyr::filter(species %in% species_intersect) %>% 
  dplyr::filter(species != "SBF")

CAPTURED_filtered <- CAPTURED_filtered %>%
  dplyr::mutate(ocean_simple = case_when(
    Ocean %in% c("Western Atlantic Ocean", "Eastern Atlantic Ocean","Atlantic Antarctic") ~ "Atlantic Ocean",
    Ocean %in% c("Western Indian Ocean", "Eastern Indian Ocean","Indian Antarctic")   ~ "Indian Ocean",
    Ocean %in% c("Western Pacific Ocean", "Eastern Pacific Ocean","Pacific Antarctic") ~ "Pacific Ocean",
    Ocean == "Mediterranean and Black Sea"                          ~ "Mediterranean and Black Sea",
    TRUE ~ NA_character_
  ))

NCD_filtered <- NCD_filtered %>%
  dplyr::mutate(ocean_simple = case_when(
    Ocean %in% c("Western Atlantic Ocean", "Eastern Atlantic Ocean","Atlantic Antarctic") ~ "Atlantic Ocean",
    Ocean %in% c("Western Indian Ocean", "Eastern Indian Ocean","Indian Antarctic")   ~ "Indian Ocean",
    Ocean %in% c("Western Pacific Ocean", "Eastern Pacific Ocean","Pacific Antarctic") ~ "Pacific Ocean",
    Ocean == "Mediterranean and Black Sea"                          ~ "Mediterranean and Black Sea",
    TRUE ~ NA_character_
  ))

species_intersect <- NCD_filtered %>% dplyr::filter(source_authority == "IOTC") %>% dplyr::select(species) %>% dplyr::distinct()%>% dplyr::pull(species)

species_intersect <- c(species_intersect, "FRZ")

CAPTURED_filtered <- CAPTURED_filtered %>% dplyr::filter(species %in% species_intersect) %>% 
  dplyr::filter(species != "SBF")

NCD_filtered <- NCD_filtered%>% dplyr::filter(species %in% species_intersect) %>% 
  dplyr::filter(species != "SBF")

CAPTURED_filtered$measurement_processing_level <- CAPTURED_filtered$STATUS
CAPTURED_filtered$gear_label <- "Gear not known"

NCD$ocean_basin <- NCD$Ocean

onlyminortable <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED, 
                                                                    parameter_final = NCD,
                                                                    parameter_time_dimension = c("year"), 
                                                                    print_map = FALSE, 
                                                                    parameter_diff_value_or_percent = "Difference in value",
                                                                    parameter_colnames_to_keep = c("species_name", 
                                                                                                   "fishing_fleet_label", "measurement_unit", 
                                                                                                   "measurement_processing_level", "gear_label", 
                                                                                                   "year", "measurement_value", "Ocean", "source_authority", "ocean_basin"),
                                                                    parameter_titre_dataset_1 = "FishStatJ", 
                                                                    parameter_titre_dataset_2 = "GTA")$compare_strata_differences_list$number_init_column_final_column 

onlyminortabletest <- onlyminortable %>%
  dplyr::filter(
    !(` ` %in% c(
      "Number of source_authority",
      "Number of Ocean",
      "Number of gridtype",
      "Number of measurement_unit"
    ))
  )

# only_couple_species_ocean -----------------------------------------------

couple_species_ocean <- NCD_filtered %>% dplyr::select(ocean_simple, species) %>% dplyr::distinct() %>% 
  dplyr::inner_join(CAPTURED_filtered%>% dplyr::select(ocean_simple, species)%>% dplyr::distinct(), 
                    by = c("ocean_simple", "species"))


NCD_filtered_much <- NCD_filtered %>% dplyr::inner_join(couple_species_ocean)
CAPTURED_filtered_much <- CAPTURED_filtered %>% dplyr::inner_join(couple_species_ocean)

CAPTURED_filtered$gear_type <- "UNK"
cl_species_level0 <- read_csv("inputs/codelists/cl_species_level0.csv")

if(!dir.exists("outputs")){
  dir.create("outputs")
}
if(!dir.exists("outputs/figs_final")){
  dir.create("outputs/figs_final")
}
if(!dir.exists("outputs/figs")){
  dir.create("outputs/figs")
}


oceancaptured <- CAPTURED
oceancaptured$Ocean <- oceancaptured$ocean_basin
oceancaptured$gear_type <- "UNK"
minortable <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = oceancaptured,
                                                                parameter_final = NCD,
                                                                parameter_time_dimension = c("year"),
                                                                print_map = FALSE,
                                                                parameter_diff_value_or_percent = "Difference in value",
                                                                parameter_colnames_to_keep = c("species_name",
                                                                                               "fishing_fleet_label", "measurement_unit",
                                                                                               "year", "measurement_value", "Ocean", "gear_type"),
                                                                parameter_titre_dataset_1 = "FishStat",
                                                                parameter_titre_dataset_2 = "GTA")
minortabl <- minortable$compare_strata_differences_list$number_init_column_final_column
minortabl <- dplyr::filter(minortabl, !!names(minortabl)[1] != "Number of gridtype")


table_recap <- readr::read_csv(here::here("diff_FSJ_GTA_recap.csv"))
require(flextable)
onlyminortabletest <- onlyminortabletest[-nrow(onlyminortabletest), ]


species_intersect <- c(unique((NCD %>% dplyr::filter(source_authority == "IOTC"))$species), "FRZ")

percent_non_GTA <- 100 - (sum( (CAPTURED %>% dplyr::filter(species %in% species_intersect) )$measurement_value) * 100 ) /sum(CAPTURED$measurement_value)

captured <- CAPTURED_filtered %>% dplyr::filter(species %in% species_intersect)
ncd <- NCD_filtered %>% dplyr::filter(species %in% species_intersect)


