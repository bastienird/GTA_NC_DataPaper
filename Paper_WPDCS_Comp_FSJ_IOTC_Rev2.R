# Generated with purl_with_logs() from 'Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd'
# Source Rmd: /home/jovyan/GTA_NC_DataPaperBRepo/Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd
# Generated on: 2025-12-09 07:08:28

#' ---
#' title: "Following the tuna trail: Contrasting global catch estimates from FAO and RFMOs"
#' csl: dmk-format.csl
#' link-citations: true
#' author:
#'   - "Bastien Grasset\\textsuperscript{1,2}"
#'   - "Emmanuel Chassot\\textsuperscript{3}"
#'   - "Julien Barde\\textsuperscript{1,2}"
#'   - "James Geehan\\textsuperscript{4}"
#'   - "Fabio Fiorellato\\textsuperscript{4}"
#' output:
#'   bookdown::html_document2:
#'     toc: false
#'     toc_depth: 3
#'     toc_float: true
#'     number_sections: false
#'   bookdown::word_document2:
#'     toc: false
#'     toc_depth: 2
#'   bookdown::pdf_document2:
#'     toc: false
#'     toc_depth: 2
#'     latex_engine: pdflatex
#'     keep_tex: true
#'     number_sections: false
#'     lot: true
#'     includes:
#'       in_header: preamble.tex
#'     extra_dependencies: ["placeins", "pdflscape"]
#' encoding: UTF-8
#' bibliography: references.bib
#' abstract: > 
#'   This study compares annual catch statistics from the Global Tuna Atlas (GTA) and FAO FishStat (FS) marine capture datasets, with a focus on tuna and tuna-like species. The analysis first describes the differences between both dataset structures, then applies a harmonized mapping and filtering procedure to enable consistent inter-comparison. At the global scale, total catches from both datasets are highly consistent (differences < 1%), yet this apparent agreement conceals substantial variations at finer levels, particularly by species and fishing fleet. These discrepancies often compensate each other across years, producing an illusion of equivalence in aggregated time series. A regional focus on the Indian Ocean Tuna Commission (IOTC) management area confirms this pattern: while temporal trends are parallel overall, differences emerge for some species such as bigeye tuna and Albacore, often linked to specific fleets. In recent years (post-2014), several species show nearly identical values in both datasets, reflecting cases where one source adopts figures from the other when deemed more reliable. However, differences persist for certain taxa and, in some cases, where the underlying data flows or integration processes differ between the two datasets. This analysis highlights both the complementarity and the limitations of GTA and FS: GTA provides detailed fishing gear and fishing mode, while FS offers finer spatial resolution and broader taxonomic coverage. Understanding these structural and procedural differences is essential for ensuring the comparability and reproducibility of global fisheries statistics.
#' ---
#' 
#' \noindent\textbf{Keywords:} Global Tuna Atlas; FishStat; FAO; Fisheries statistics; Data harmonization; Open data; CWP standards; Tuna fisheries
#' 
#' \clearpage
#' 
#' # Introduction
#' 
#' Reliable and comprehensive catch statistics are fundamental for monitoring fisheries, assessing stocks, and supporting international policy and management decisions. However, uncertainties in catch data remain a major challenge in global fisheries statistics. These uncertainties arise from a range of factors, including incomplete or inconsistent data collection, limited monitoring capacity, reporting errors, differences in national data compilation and submission procedures, and the varying levels of aggregation and validation applied during data processing. At the global scale, fishery catches are generally considered to be widely underestimated [@pauly2016catch; @agnew2009estimating; @clarke2006global], although the extent of this underestimation remains difficult to quantify [@ye2017fao].
#' 
#' The Food and Agriculture Organization of the United Nations (FAO) and the tuna Regional Fisheries Management Organizations (t-RFMOs) represent two of the main institutional frameworks responsible for compiling and disseminating global and regional fishery statistics. Both rely on data originally collected by national authorities, yet they follow distinct workflows for validation, standardization, and dissemination. Comparing their outputs provides an opportunity to better understand how these processes influence the resulting estimates of catch, and to assess the magnitude and nature of uncertainties embedded in global fishery statistics.
#' 
#' In this study, we compare the FAO Global Capture Production Database and the nominal catch dataset compiled through the FIRMS Global Tuna, with a particular focus on tuna and tuna-like species caught in the Indian Ocean. By examining differences between these two datasetseach compiled through independent but interrelated reporting mechanismswe aim to highlight the potential sources of divergence and provide insights into the consistency and reliability of global tuna catch statistics.
#' 
#' # Materials
#' 
#' Both FishStat and the GTA have been developed by FAO to disseminate fisheries data, but they differ greatly in scope and design. Each encompasses multiple datasets and analytical tools, and both contribute to FAO's global fisheries data governance in complementary ways.
#' 
#' ## The FAO Global Capture Production Database
#' 
#' The FAO maintains the Global Capture Production Database [@FAO2025_Captures], which provides harmonized statistics on marine and inland capture fisheries production at the global level. The database is compiled annually by the FAO Statistics Division from information submitted by Member States and regional fishery bodies through the National Statistical Questionnaire 1 (NS1) survey. Reported data include annual catch quantities (in tonnes live weight) by country or territory, species (FAO ASFIS list), major fishing area (FAO statistical areas), and production source (marine or inland waters). Following submission, FAO performs quality control, validation, and standardization procedures to ensure internal consistency and comparability across countries and years.
#' 
#' As of today, the FAO's FishStat data, including the global and regional capture production data, are disseminated through a desktop client application (FishStatJ)^[Available at: https://www.fao.org/fishery/en/topic/166235/en] as well as through a CSV dataset accessible from the FAO Fisheries and Aquaculture Statistics online portal as a zipped file^[Available at: https://www.fao.org/fishery/static/Data/Capture_2025.1.0.zip]. FS constitutes the official global reference for capture fishery production statistics. The version used in this study corresponds to the dataset available from March 2025.
#' 
#' The FS dataset is compact, with only seven columns (Table \@ref(tab:fishStatfirstlines)). It focuses on annual national statistics of capture production. In FS, information on the type of measurement is implicit: all records correspond to nominal catches ("catch"), without discards, and are expressed in metric tonnes ("t") - the same unit used in the GTA dataset - and in number for a reduced set of species. In GTA, these details are explicitly defined through the columns *measurement*, *measurement_type*, and *measurement_unit*. For some groups, such as tunas and billfishes, FAO integrates best scientific estimates provided by regional tuna commissions.
#' 
#' ## The FIRMS Global Tuna Atlas (GTA)
#' 
#' The GTA is a global repository of harmonized nominal and geo-referenced catch datasets from tuna and tuna-like fisheries, developed under the auspices of the Fisheries and Resources Monitoring System (FIRMS) of the FAO [@GTA2025_GlobalNominal]. The GTA compiles, standardizes, and disseminates public-domain catch data submitted by the five tuna Regional Fisheries Management Organizations (t-RFMOs) the Commission for the Conservation of Southern Bluefin Tuna (CCSBT), the Inter-American Tropical Tuna Commission (IATTC), the International Commission for the Conservation of Atlantic Tunas (ICCAT), the Indian Ocean Tuna Commission (IOTC), and the Western and Central Pacific Fisheries Commission (WCPFC, in collaboration with the Pacific Community, SPC).
#' 
#' The GTA was developed through a coordinated process between the FIRMS Secretariat and the t-RFMOs to establish a common data exchange format consistent with the standards of the Coordinating Working Party on Fishery Statistics (CWP) [@FAO_CWP]. This process enables the systematic integration and annual update of harmonized nominal catch data from all t-RFMOs into a single global dataset.
#' 
#' The GTA currently disseminates catch data for tuna and tuna-like species, including principal market tunas, billfishes, coastal or neritic tunas, and associated species such as bonitos, mackerels, and pelagic sharks. All GTA datasets are openly accessible on Zenodo with complete metadata and transparent versioning, and are outputs of an R data generation workflow and inputs for Shiny applications for visualization and analysis^[Accessible at: https://tunaatlaspiemapinseetuto.lab.dive.edito.eu/]. The nominal catch dataset component of the GTA, covering the period 1918-2023, is publicly available from the Zenodo repository^[DOI: https://doi.org/10.5281/zenodo.17707455]. The full processing workflow is also openly available, with a DOI for the source code^[DOI: https://zenodo.org/records/15312151].
#' 
#' The GTA is an open, reproducible data system focused on tuna and tuna-like species. The GTA dataset follows the CWP standards. The data structure is made of 16 columns categorizing catches by using multiple dimensions: species, fleet, gear, fishing_mode, area, and time (Table \@ref(tab:gtafirstlines)). Among these, four temporal columns  *time_start*, *year*, *month*, and *quarter*  deliberately provide redundant but complementary time references which facilitate filtering and aggregation at multiple temporal scales. The exchange format is available as JSON file from the Fisheries Data Interoperability Working Group GitHub Repository^[Available at: https://github.com/fdiwg/fdi-formats/blob/main/cwp_rh_generic_gta_taskI.json].
#' 
#' # Methods
#' 
#' ## Scope and datasets
#' 
#' In this study, we focus only on one specific dataset type shared by both systems, the annual global capture data, to compare their structure, coverage, and consistency. Specifically, we use the datasets *FishStat - Global Capture Production.* [@FAO2025_Captures] and *Global Tuna Atlas - Global nominal catches* [@GTA2025_GlobalNominal], corresponding respectively to FAO's global capture production statistics and the harmonized nominal catch data compiled from t-RFMOs. Only marine capture data were retained (excluding inland records), and the analysis focuses on ISSCAAP groups 36 (Tunas and tuna-like species) and 38 (Sharks, rays, chimaeras). All measurement_value entries represent nominal catches in metric tonnes (t).
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: setup\n")
knitr::opts_chunk$set(
  echo    = FALSE,   # pas de code
  message = FALSE,   # pas de messages
  warning = FALSE,   # pas d'avertissements
  include = TRUE,    
  fig.show = "asis"
)
options(scipen = 9999)

knitr::knit_hooks$set( #options for curl if needed to put in .R 
  before = function(options) {
    # called before the chunk
    sprintf('cat("---- Beginning of the  chunk: %s ----\\n")\n', options$label)
  },
  after = function(options) {
    # called after the chunk
    sprintf('cat("---- End of the chunk: %s ----\\n")\n', options$label)
  }
)


#' 
#' 
cat("---- End of the chunk: setup\n")
## ----runnning-CAPTURE-QUANTITY-REV3, include=FALSE-------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: runnning-CAPTURE-QUANTITY-REV3\n")
lapply(
  list.files(here::here("R"), full.names = TRUE),
  source
)

source("CAPTURE_QUANTITY_REV3.R") # for the mapping first


#' 
#' 
#' \clearpage
#' 
#' ## Global comparison of available dimensions
#' 
#' The GTA dataset explicitly includes several descriptive dimensions such as gear_type, fishing_fleet, fishing_mode, and source_authority, consistent with CWP standards. These fields make it possible to group, filter, or compare data by gear, fleet, or reporting authority.
#' 
#' In contrast, FS provides a more aggregated representation: it does not include specific gear or fleet categories, and the reporting entity corresponds to the national authority. However, some missing dimensions can be inferred indirectly - for example, the tRFMO can generally be deduced from the geographic area code (i.e., FAO Major Fishing area). This correspondence is not always unique in the Pacific Ocean, where RFMO boundaries overlap. Table \@ref(tab:tabrecap) summarizes the correspondence between key dimensions in the two datasets.
#' 
#' ```{=latex}
#' \FloatBarrier
#' ```
#' 
#' \renewcommand{\arraystretch}{1.5}
#' \small
#' 
#' Table: (\#tab:tabrecap) Summary of key structural differences between FS and GTA datasets
#' 
#' 
#' | **Dimension** | **FS** | **GTA** | **Key differences** |
#' |----------------|------------------|----------|------------------------------|
#' | **Species and group of species** | 337 | 65 | FS has broader taxonomic coverage; GTA focuses on tuna and tuna-like species. |
#' | **Fishing Fleet** | 169 (country/territory level) | 158 (aggregated categories) | FS distinguishes individual territories; GTA merges some under national or RFMO entities. |
#' | **Gear Type** | Not specified | Reported | Enables stratification by fishing practice. |
#' | **Fishing Mode** | Not specified | Reported | Adds contextual information (e.g. free school vs associated school). |
#' | **Source Authority** | National authority | RFMO or source agency | Improves traceability and attribution of data. |
#' | **Area / Region** | 17 FAO subregions | 10 RFMO-based management areas | Partial overlap between spatial frameworks (see Table \@ref(tab:geo)). |
#' | **Measurement Type** | Nominal Landings (NL) only | Nominal Catch + NL (+ few discards) | Combined as equivalent for analysis. |
#' | **Temporal Range** | 1950-2023 | 1918-2023 | Analyses restricted to 1950-2023, the common period covered by both datasets. |
#' 
#' \normalsize
#' \renewcommand{\arraystretch}{1.0}
#' 
#' ### Taxonomic coverage
#' 
#' FishStat includes 337 species, whereas the GTA dataset lists only 65 (Table: \@ref(tab:tabrecap)). GTA deliberately focuses on a restricted set of 32 key species, considered the most relevant for tuna and tuna-like fisheries. For the remainder of this analysis, only this 32 species included in both FS and GTA are considered, with specific focus given to the main tuna species in the following sections. (See Appendix \@ref(tab:speciesmain)). Catches of species not present in the GTA, but present in FS, represent 28% of total catches, for over 250 additional species.
#' 
cat("---- End of the chunk: runnning-CAPTURE-QUANTITY-REV3\n")
## ----filteringoniotcspeciesandintersect, include=FALSE---------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: filteringoniotcspeciesandintersect\n")
species_intersect <- c(unique((NCD %>% dplyr::filter(source_authority == "IOTC"))$species), "FRZ")

percent_non_GTA <- 100 - (sum( (CAPTURED %>% dplyr::filter(species %in% species_intersect) )$measurement_value) * 100 ) /sum(CAPTURED$measurement_value)

CAPTURED_INTERSECT <- CAPTURED %>% dplyr::filter(species %in% species_intersect)
NCD_INTERSECT <- NCD %>% dplyr::filter(species %in% species_intersect)


#' 
#' ### Fleet coverage
#' 
#' Fishing fleet categories are slightly more numerous in FishStat (169) than in GTA (158), reflecting a slightly higher reporting granularity (Table: \@ref(tab:tabrecap)). FishStat distinguishes individual countries and territories, while GTA aggregates certain entities under broader national or regional categories. This difference mainly reflects variations in national reporting practices and data compilation rules.
#' 
#' ### Geographic coverage
#' 
#' The GTA defines 10 RFMO-based management regions, while FS subdivides the world into 17 FAO Major Fishing Areas and subregions.  GTA thus relies on large, RFMO-oriented areas or aggregated FAO regions, whereas FS follows a finer spatial framework defined by FAO statistical divisions. As a result, spatial identifiers between the two datasets only partially overlap. Table \@ref(tab:geo), in appendix summarizes the correspondence between GTA management regions and FAO fishing areas by ocean basin, illustrating the partial overlap between the two spatial frameworks.
#' 
#' Exact correspondences exist only for the Mediterranean and Black Sea (MD) and the Indian Ocean (IOTC_WEST and IOTC_EAST).
#' In contrast, the Atlantic and Pacific basins are divided into multiple FAO subregions in FS, making direct mapping with GTA's broader RFMO areas more complex.
#' Antarctic areas, which have no direct equivalent in GTA, include marginal species in FS (e.g. *Porbeagle*, *Rays*, *Mantas nei*), while in GTA, only Southern Bluefin Tuna (SBF) partially overlaps these regions under the CCSBT mandate.
#' 
#' ### Measurement type
#' 
#' In the GTA dataset, the variable measurement_type includes both Nominal Catch (NC) and Nominal Landings (NL), with a few occasional records of Dead Discards (DD) and Discarded Live (DL) following CWP standards [@FAO_CWP_CatchLandings; @CWP_CodeList_CatchConcepts]. In contrast, FishStat reports only Nominal Landings (NL). According to the CWP Handbook of Fishery Statistical Standards, Nominal Catch represents "nominal landings plus the component of the catch discarded dead, and post-release mortality of fish discarded alive", approximating the total biomass removed.
#' 
#' Despite this conceptual difference, the sum of Nominal Catch and Nominal Landings in GTA are generally of the same order of magnitude as Nominal Landings in FS. Although Nominal Catch theoretically includes dead discards and post-release mortalities, it is not possible to quantify how much these components contribute in practice.
#' 
#' For this reason, both NC and NL values are combined in the global analysis and compared directly with FS NL data, as a pragmatic approximation pending more detailed metadata on discard estimates.
#' 
#' ### Temporal coverage
#' 
#' The time series of FS catch data start in 1950, whereas GTA catch data begin in 1918, thus providing 32 additional years of historical coverage (Table \@ref(tab:tabrecap)). Note that dates prior to 1950 are not available for all ocean basins in GTA.
#' 
#' ## Harmonization and mapping process
#' 
#' ### Mapping of fishing fleets
#' 
#' Establishing a sound correspondence between the categories fishing_fleet, country, etc. is a complex task: FS offers a much finer level of detail in terms of capture production quantities at country or territorial entity level, and does not aggregate certain entities (e.g., Jersey, or Zanzibar), unlike GTA. In addition, geopolitical developments over the past years have made mapping difficult. We therefore propose a provisional mapping (see Table \@ref(tab:mappingFStonc)) for not specific countries/territories, that enables us to analyze certain trends between the data sets, without claiming to be exhaustive for each country.
#' 
#' ### Mapping of geographic areas
#' 
#' Given the differences highlighted earlier, spatial comparisons between GTA and FS are conducted at the major ocean-basin level (Atlantic, Indian, and Pacific), rather than by FAO subregion. Antarctic areas, which concern only marginal species (Porbeagle and Rays, stingrays, mantas nei in FS, and Southern bluefin tuna in GTA, absent from FS), were excluded to maintain analytical consistency. The goal of this harmonization is not to reproduce fine-scale geographic details but to identify broad inter-basin patterns in reported catches.
#' 
#' ## Filtering of comparable strata
#' 
cat("---- End of the chunk: filteringoniotcspeciesandintersect\n")
## ----presenceabsencecreation, include=FALSE--------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: presenceabsencecreation\n")
species_intersect_32 <- c(unique((NCD %>% dplyr::filter(source_authority == "IOTC"))$species), "FRZ")

NCD_presence_absence <- NCD %>% dplyr::filter(species%in%species_intersect_32)
CAPTURED_presence_absence <- CAPTURED %>% dplyr::filter(species%in%species_intersect_32)

df_presence <- bind_rows(
  NCD_presence_absence       %>% dplyr::select(species_name, ocean_simple) %>% dplyr::mutate(source = "NCD_presence_absence"),
  CAPTURED_presence_absence  %>% dplyr::select(species_name, ocean_simple) %>% dplyr::mutate(source = "CAPTURED_presence_absence")
)
df_values <- dplyr::bind_rows(
  NCD_presence_absence      %>% dplyr::select(species_name, ocean_simple, measurement_value) %>% dplyr::mutate(source = "GTA"),
  CAPTURED_presence_absence %>% dplyr::select(species_name, ocean_simple, measurement_value) %>% dplyr::mutate(source = "FS")
) %>%
  dplyr::group_by(species_name, ocean_simple, source) %>%
  dplyr::summarise(
    total_value = sum(measurement_value, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Construction du tableau complet (presence + valeurs)
all_species <- c(union(NCD_presence_absence$species_name, CAPTURED_presence_absence$species_name), "Frigate and bullet tunas")
all_oceans  <- base::union(NCD_presence_absence$ocean_simple, CAPTURED_presence_absence$ocean_simple)

presence_df <- base::expand.grid(
  species_name = all_species,
  ocean_simple  = all_oceans,
  stringsAsFactors = FALSE
) %>%
  dplyr::as_tibble() %>%
  dplyr::left_join(
    df_values %>%
      tidyr::pivot_wider(
        names_from  = source,
        values_from = total_value,
        values_fill = list(total_value = 0),
        names_prefix = "val_"
      ),
    by = c("species_name", "ocean_simple")
  ) %>%
  dplyr::mutate(
    has_GTA = (val_GTA > 0),
    has_FS = (val_FS > 0),
    presence = dplyr::case_when(
      has_GTA & has_FS & val_GTA  > val_FS  ~ "Both: GTA > FS",
      has_GTA & has_FS & val_GTA  < val_FS  ~ "Both: FS > GTA",
      has_GTA & has_FS & val_GTA == val_FS  ~ "Both: equal",
      has_GTA                                ~ "GTA only",
      has_FS                                ~ "FS only",
      TRUE                                   ~ "None"
    )
  )

presence_df_complete <- expand.grid(
  species_name = all_species,
  ocean_simple  = all_oceans,
  stringsAsFactors = FALSE
) %>%
  dplyr::left_join(
    df_presence %>%
      distinct(species_name, ocean_simple, source) %>%
      dplyr::mutate(present = 1) %>%
      pivot_wider(
        names_from  = source,
        values_from = present,
        values_fill = list(present = 0)
      ) %>%
      dplyr::mutate(
        presence = case_when(
          NCD_presence_absence == 1 & CAPTURED_presence_absence == 1 ~ "Both",
          NCD_presence_absence == 1               ~ "GTA",
          CAPTURED_presence_absence == 1          ~ "FSJ",
          TRUE                   ~ "None"
        )
      ) %>%
      dplyr::select(species_name, ocean_simple, presence),
    by = c("species_name", "ocean_simple")
  )

# 3. DÃ©tection des espÃ¨ces toujours en "Both" ou toujours en "None"
always_both_or_none <- presence_df_complete %>%
  dplyr::group_by(species_name) %>%
  dplyr::filter(all(presence %in% c("Both", NA))) %>%
  dplyr::pull(species_name) %>%
  unique()

presence_df_complete <- presence_df_complete %>%
  dplyr::mutate(
    flag = case_when(
      species_name %in% always_both_or_none ~ "Common in both datasets",
      TRUE                           ~ "Other"
    )
  )

presence_df <- presence_df %>%
  dplyr::left_join(
    presence_df_complete %>% dplyr::select(ocean_simple, species_name, flag),
    by = c("ocean_simple", "species_name")
  )


plot_pres_abs <- ggplot2::ggplot(
  presence_df,
  ggplot2::aes(x = ocean_simple, y = species_name, fill = presence)
) +
  ggplot2::geom_tile(color = "white") +

  ggplot2::scale_fill_manual(
    values = c(
      "Both: GTA > FS" = "#1B9E77",
      "GTA only"        = "#66C2A5",
      "Both: FS > GTA" = "#D95F02",
      "FS only"        = "#FC8D62",
      "None"            = "#BDBDBD"
    )
  ) +

  ggplot2::labs(
    x    = "Ocean",
    y    = "Species",
    fill = "Presence & dominance"
  ) +

  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = 20),
    legend.text = ggplot2::element_text(size = 14),
    axis.text.y      = ggplot2::element_text(size = 16),
    axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 16),
    axis.title.x = ggplot2::element_text(size = 20),
    axis.title.y = ggplot2::element_text(size = 20),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 13)
  ) +
  ggplot2::labs(fill = "Dataset presence") +
  ggplot2::facet_wrap(~ flag, scales = "free_y", ncol = 1)

plot_pres_abs_for_pwt <- plot_pres_abs +
  ggplot2::theme(
    axis.text.y      = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(
  filename = here::here("outputs/presenceabsencepwt.pdf"),
  plot     = plot_pres_abs_for_pwt,
  width    = 12,
  height   = 8
)

ggplot2::ggsave(
  filename = here::here("outputs/presenceabsence.pdf"),
  plot     = plot_pres_abs,
  width    = 18,
  height   = 18
)

rm(NCD_presence_absence)
rm(CAPTURED_presence_absence)


#' 
#' 
#' 
cat("---- End of the chunk: presenceabsencecreation\n")
## ----presenceabsence, fig.cap="Presence and absence of the 32 retain species and group of species for each ocean", fig.width=16, fig.height=16, out.width='100%'----

cat("---- Beginning of the chunk: presenceabsence\n")
knitr::include_graphics(here::here("outputs/presenceabsence.pdf"))


#' 
#' We compared the presence and absence of the 32 retained species across all ocean basins. This comparison reveals that only two species are present in the GTA dataset but absent from FishStat in one or more basins, whereas the opposite situation is much more frequent: 21 species reported by FishStat are not represented in GTA for several basin-species combinations (Figure: \@ref(fig:presenceabsence)). To ensure consistent coverage across basins, only species-ocean pairs present in both datasets were retained for the subsequent analyses.
#' 
#' However, this assessment should not be interpreted as a direct measure of completeness. Differences in taxonomic mapping, aggregation, or naming conventions between the two datasets, particularly for sharks and other elasmobranchs, can explain apparent absences. In many cases, catches may simply be recorded under broader nei or synonym categories rather than truly missing.
#' 
#' As an illustration, a specific case of data aggregation was encountered with Frigate and Bullet tunas, which were reported jointly for many years in FishStat under the combined FAO code 'FRZ'. We made an exception to retain this aggregated category in our comparison given its relevance for major tuna species and its direct equivalency in the GTA dataset. However, for the 'Mediterranean and Black Sea' basin, data for both individual species and the 'FRZ' group were removed, as only bullet tuna is represented in GTA dataset for that area.
#' 
cat("---- End of the chunk: presenceabsence\n")
## ----aggregatedspecies-----------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: aggregatedspecies-----------------------------------------------------------------------------------------------------------------------\n")
aggregated_species <- c(
  "Various sharks nei",
  "Tunas nei", 
  "Thresher sharks nei",
  "Marlins,sailfishes,etc. nei",
  "Mantas, devil rays nei",
  "Requiem sharks nei",
  "Hammerhead sharks nei",
  "Hammerhead sharks, etc. nei",
  "True tunas nei",
  "Seerfishes nei",
  "Bonitos nei",
  "Rays, stingrays, mantas nei",
  "Marlins nei",
  "Thresher" ,
  "Tuna-like fishes nei",
  "Sharks, rays, skates, etc. nei",
  "Mantas, devil rays, etc. nei",
  "Mako sharks"
)

NCD_groupped <- NCD %>% dplyr::filter(species_name %in% aggregated_species) %>% dplyr::mutate(species_name = ifelse(species_name == "Hammerhead sharks, etc. nei", "Hammerhead sharks nei", species_name)) %>% dplyr::mutate(species = ifelse(species_name %in% c("Hammerhead sharks, etc. nei", "Hammerhead sharks nei"),"SPN ; SPY", species)) %>% dplyr::group_by(species_name, species, year, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value))

CAPTURED_groupped <- CAPTURED%>% dplyr::filter(species_name %in% aggregated_species)  %>% dplyr::mutate(species = ifelse(species_name %in% c("Hammerhead sharks, etc. nei", "Hammerhead sharks nei"),"SPN ; SPY", species))%>% dplyr::group_by(species_name, species, year, measurement_unit)%>% dplyr::summarise(measurement_value = sum(measurement_value))


Groupped_nei_data <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_groupped,
                                                          parameter_final = NCD_groupped,
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c("species_name",
                                                                                         "year", "measurement_value", "measurement_unit"),
                                                          parameter_titre_dataset_1 = "FishStat",
                                                          parameter_titre_dataset_2 = "GTA")


table_groupped_nei <- CWP.dataset::compare_dimension_differences(Groupped_nei_data$groupping_differences_list$Groupped_all, "species_name", parameter_diff_value_or_percent = "Difference in value", topn = 18)$Groupped_all_not_disap_or_app_to_dysplay %>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`)%>% dplyr::rename(`Per species` = Precision ) %>% dplyr::ungroup()

totbyspecies_groupped_nei <- table_groupped_nei%>% dplyr::ungroup()%>% dplyr::select(-c(measurement_unit, Dimension))

totbyspecies_groupped_nei <- CWP.dataset::qflextable2(totbyspecies_groupped_nei, grouped_data = c( "Loss / Gain"), columns_to_color = c("Difference (in %)"))

image <- save_as_image(totbyspecies_groupped_nei, path = "outputs/totbyspecies_groupped_nei.png")
qs <- qs::qsave(totbyspecies_groupped_nei, "outputs/totbyspecies_groupped_nei.qs")


#' 
#' For other broadly aggregated taxa such as "Bonitos nei", "Tunas nei", or "Rays, stingrays, mantas nei", the corresponding catches were excluded to ensure consistency at the species level. However, it is important to note that these excluded categories represent substantial catch volumes for tunas and sharks, as illustrated in Table \@ref(tab:aggregatedcatches). 
#' 
#' The goal of this comparison is to illustrate the broader coverage of FishStat and to justify the application of a harmonized filtering by area and species in the subsequent analyses, rather than to infer which dataset is superior. We choose not to include the fishing_fleet dimension in this filtering step, because it is not always possible to verify that fleets are not reported under 'Other nei' (Not Elsewhere Included) in one of the datasets. For species, we therefore remove all 'UNK' categories to avoid ambiguities.
#' 
#' 
#' # Global comparison of captures
#' 
#' In the following subsections we detail the results obtained by comparing the remaining rows of the initial FS and GTA datasets, after applying the previous filters (see explanations in previous section). 
#' 
cat("---- End of the chunk: aggregatedspecies-----------------------------------------------------------------------------------------------------------------------\n")
## ----joiningfilteringoncommonspeciesocean----------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: joiningfilteringoncommonspeciesocean----------------------------------------------------------------------------------------------------\n")
couple_species_ocean <- rbind(NCD %>% dplyr::select(ocean_simple, species) %>% dplyr::distinct() %>% 
  dplyr::inner_join(CAPTURED%>% dplyr::select(ocean_simple, species)%>% dplyr::distinct(), 
                    by = c("ocean_simple", "species")), c("Indian Ocean", "FRZ"), c("Indian Ocean", "FRZ")) %>% dplyr::filter(!(ocean_simple == "Mediterranean and Black Sea" & species == "BLT"))


NCD_filtered_COUPLE_SPECIES_OCEAN_TIME <- NCD%>% dplyr::filter(species %in% species_intersect)%>% dplyr::inner_join(couple_species_ocean) %>% dplyr::filter(year >1949)
CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME <- CAPTURED%>% dplyr::filter(species %in% species_intersect) %>% dplyr::inner_join(couple_species_ocean)%>% dplyr::filter(year >1949)

# join <- inner_join(CAPTURED_filtered %>% dplyr::select(ocean_simple, species) %>% dplyr::distinct(), NCD_filtered %>% dplyr::select(ocean_simple, species)%>% dplyr::distinct())

# much_much_FS <- CAPTURED_filtered%>% dplyr::inner_join(join) %>% dplyr::filter(year >1949)
# much_much_nominal <- NCD_filtered%>% dplyr::inner_join(join)%>% dplyr::filter(year >1949)


#' 
cat("---- End of the chunk: joiningfilteringoncommonspeciesocean----------------------------------------------------------------------------------------------------\n")
## ----comparisonfiltereddata------------------------------------------------------------------------------------------------------------------
# adding one line with NEI FRZ to be able to print FRZ in table where there is loss not disappearing data
cat("---- Beginning of the chunk: comparisonfiltereddata------------------------------------------------------------------------------------------------------------------\n")
COMP_COUPLE_SPECIES_OCEAN_TIME <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME,
                                                          parameter_final = rbind(head(NCD_filtered_COUPLE_SPECIES_OCEAN_TIME,1) %>% dplyr::mutate(species = "FRZ", measurement_value = 0.001, fishing_fleet_label = "NEI", species_name = "Frigate and bullet tunas"),NCD_filtered_COUPLE_SPECIES_OCEAN_TIME),
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c("species_name",
                                                                                         "fishing_fleet_label", "measurement_unit",
                                                                                         "year", "measurement_value", "ocean_simple", "source_authority"),
                                                          parameter_titre_dataset_1 = "FishStat",
                                                          parameter_titre_dataset_2 = "GTA")

#' 
#' ## Overall differences and total values
#' 
#' 
#' 
cat("---- End of the chunk: comparisonfiltereddata------------------------------------------------------------------------------------------------------------------\n")
## ----totaldiffcreation-----------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: totaldiffcreation-----------------------------------------------------------------------------------------------------------------------\n")
df_totaldiff <- COMP_COUPLE_SPECIES_OCEAN_TIME$summary_of_differences %>%
  dplyr::select(-measurement_unit)


df_totaldiff <- CWP.dataset::qflextable2(df_totaldiff)
# Save for reuse later in oral presentation

image <- save_as_image(df_totaldiff, path = "outputs/totaldiff.png")
qs::qsave(df_totaldiff,"outputs/totaldiff.qs")


#' 
#' 
cat("---- End of the chunk: totaldiffcreation-----------------------------------------------------------------------------------------------------------------------\n")
## ----totaldiff, tab.cap="Total catch (t) for each dataset and relative differences", echo=FALSE----------------------------------------------

cat("---- Beginning of the chunk: totaldiff\n")
df_totaldiff <- qs::qread("outputs/totaldiff.qs")
df_totaldiff


#' 
#' <br>
#' 
#' When considering total global captures over the entire time series from 1950 to 2023, the two datasets show remarkably close aggregated values, with less than 1% difference between them. (Table \@ref(tab:totaldiff)). Such apparent similarity suggests that, at a broad scale, both FishStat and GTA provide coherent global estimates of total catches over the studied period. However, this global agreement can mask substantial differences when exploring specific dimensions, such as species composition or fishing fleet contributions, which may compensate each other when aggregated.
#' 
#' \clearpage
#' 
#' ## Temporal evolution of global captures
#' 
cat("---- End of the chunk: totaldiff\n")
## ----timeseriestotcreation-------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: timeseriestotcreation-------------------------------------------------------------------------------------------------------------------\n")
timeseriestot <- clean_time_series_plot(COMP_COUPLE_SPECIES_OCEAN_TIME$time_coverage_analysis_list$plots[[1]])
unlink("outputs/timeseriestot.pdf")
ggplot2::ggsave(
  filename = "outputs/timeseriestot.pdf",
  plot     = timeseriestot,
)

#' 
cat("---- End of the chunk: timeseriestotcreation-------------------------------------------------------------------------------------------------------------------\n")
## ----timeseriestot,  fig.cap = "Comparison of annual time series of catch (t) of tuna and tuna-like species in the Indian Ocean between for FishStat and GTA datasets for the period 1950-2023", echo=FALSE----

cat("---- Beginning of the chunk: timeseriestot\n")
knitr::include_graphics(here::here("outputs/timeseriestot.pdf"))


#' 
#' The temporal trends in total catches (Figure: \@ref(fig:timeseriestot)) confirm this overall consistency. Both datasets exhibit parallel trajectories, however, this apparent alignment does not imply full equivalence. In some years, FishStat reports higher values, while in others GTA exceeds it, suggesting that small interannual compensations between datasets smooth out when aggregated. These offsetting variations contribute to the impression of overall agreement, even though substantial compositional differences may persist beneath the global totals.
#' 
#' \clearpage
#' 
#' ## Breakdown by species and fleet
#' 
#' \footnotesize
#' 
cat("---- End of the chunk: timeseriestot\n")
## ----totbyspeciescreation--------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: totbyspeciescreation--------------------------------------------------------------------------------------------------------------------\n")
table_COMP_COUPLE_SPECIES_OCEAN_TIME <- CWP.dataset::compare_dimension_differences(COMP_COUPLE_SPECIES_OCEAN_TIME$groupping_differences_list$Groupped_all, "species_name", parameter_diff_value_or_percent = "Difference in value", topn = 18)$Groupped_all_not_disap_or_app_to_dysplay %>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`)%>% dplyr::rename(`Per species` = Precision ) %>% dplyr::ungroup()

totbyspecies <- table_COMP_COUPLE_SPECIES_OCEAN_TIME%>% dplyr::ungroup()%>% dplyr::select(-c(measurement_unit, Dimension))

totbyspecies <- CWP.dataset::qflextable2(totbyspecies, grouped_data = c( "Loss / Gain"), columns_to_color = c("Difference (in %)"))


## tres grand tableau, plusieurs lignes 
# Pour les 18 premiÃ¨res lignes
totbyspecies_part1 <- table_COMP_COUPLE_SPECIES_OCEAN_TIME %>%dplyr::slice(1:18)  %>% 
  dplyr::select(-c(measurement_unit, Dimension)) %>%
  CWP.dataset::qflextable2(grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

# Pour les lignes restantes
totbyspecies_part2 <- table_COMP_COUPLE_SPECIES_OCEAN_TIME %>%dplyr::slice(19:n()) %>% 
  dplyr::select(-c(measurement_unit, Dimension)) %>%
  CWP.dataset::qflextable2(grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))
image <- save_as_image(totbyspecies, path = "outputs/totbyspecies.png")

qs <- qs::qsave(totbyspecies,"outputs/totbyspecies.qs")

qs <- qs::qsave(totbyspecies_part1,"outputs/totbyspecies_part1.qs")
qs <- qs::qsave(totbyspecies_part2,"outputs/totbyspecies_part2.qs")

totbyspecies_part1 <- table_COMP_COUPLE_SPECIES_OCEAN_TIME %>%dplyr::slice(1:18)  %>% 
  dplyr::select(-c(measurement_unit, Dimension,"Loss / Gain")) %>%
  CWP.dataset::qflextable2(columns_to_color = c("Difference (in %)"))
totbyspecies_part2 <- table_COMP_COUPLE_SPECIES_OCEAN_TIME  %>%dplyr::slice(19:n())  %>% 
  dplyr::select(-c(measurement_unit, Dimension,"Loss / Gain")) %>%
  CWP.dataset::qflextable2(columns_to_color = c("Difference (in %)"))

image <- save_as_image(totbyspecies_part1, path = "outputs/totbyspecies_part1.png")
image <- save_as_image(totbyspecies_part2, path = "outputs/totbyspecies_part2.png")


#' 
#' 
cat("---- End of the chunk: totbyspeciescreation--------------------------------------------------------------------------------------------------------------------\n")
## ----totbyspecies, tab.cap="Comparison of total catch (t) by taxon between the Global Tuna Atlas and Fishstat database for the 32 retained species and one group of species", echo=FALSE, out.height='85%'----

cat("---- Beginning of the chunk: totbyspecies\n")
totbyspecies <- qs::qread("outputs/totbyspecies.qs")
totbyspecies



#' 
#' 
#' \normalsize 
#' 
#' A more detailed examination by species reveals significant discrepancies between the two datasets (Table \@ref(tab:totbyspecies)). The analysis shows marked deficits in GTA for certain tunas and mackerels, with kawakawa (-10.1%) and Indo-Pacific sailfish (-26.4%) displaying substantially lower volumes compared to FishStat. These differences are largely explained by our removal of generic aggregated categories such as "Bonitos nei" and "Tunas nei", where catches reported under these groupings in FishStat are not fully redistributed at the species level in GTA  (Table \@ref(tab:aggregatedcatches)).
#' 
#' As noted earlier, the Frigate and Bullet tunas case illustrates a complementary mechanism: the aggregated category appears only in FishStat, while GTA reports these catches at the species level. However, the significant difference between the speciated GTA total (~4.1 million tonnes) and the FishStat aggregate (5.3 million tonnes) reveals an unreconciled gap of ~1.2 million tonnes, indicating persistent differences in classification or allocation methods between the datasets.
#' 
#' Furthermore, an opposite trend is observed for sharks and rays, which are consistently highly reported in GTA. Smooth hammerhead (+204%) and scalloped hammerhead sharks (+69%), along with the giant manta (+1,424%), show higher catches in GTA, indicating successful disaggregation from broader FishStat categories such as "Rays, stingrays, mantas nei". This pattern highlights the value of GTA for detailed elasmobranch catch analysis, while FishStat may provide better coverage for certain aggregated tuna categories.
#' 
#' Finally, the highest percentage differences occur for species with the lowest catch volumes (e.g., devil ray, great hammerhead), highlighting greater uncertainty in their reporting.
#' 
#' In summary, the observed discrepancies are primarily structural and linked to the level of taxonomic resolution adopted by each database. The choice of dataset directly influences catch estimates depending on whether species-level detail or the retention of historical aggregated categories is prioritized.
#' 
cat("---- End of the chunk: totbyspecies\n")
## ----totbyoceancreation----------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: totbyoceancreation----------------------------------------------------------------------------------------------------------------------\n")
table_totbyocean <- CWP.dataset::compare_dimension_differences(COMP_COUPLE_SPECIES_OCEAN_TIME$groupping_differences_list$Groupped_all, "ocean_simple", parameter_diff_value_or_percent = "Difference in value", topn = 5)$Groupped_all_not_disap_or_app_to_dysplay %>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(Area = Precision)

table_totbyocean_tidied <- CWP.dataset::qflextable2(table_totbyocean%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::mutate(Dimension = "Ocean") %>% dplyr::select(-Dimension), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

qs::qsave(table_totbyocean_tidied, "outputs/table_totbyocean_tidied.qs")


#' 
#' 
cat("---- End of the chunk: totbyoceancreation----------------------------------------------------------------------------------------------------------------------\n")
## ----totbyocean, tab.cap="Comparison of total catch (t) by Area between the Global Tuna Atlas and Fishstat database for a set of selected taxa"----

cat("---- Beginning of the chunk: totbyocean\n")
table_totbyocean_tidied <- qs::qread("outputs/table_totbyocean_tidied.qs")
table_totbyocean_tidied


#' 
#' \scriptsize
#' *Note: See Table \@ref(tab:speciesmain) for the list of selected species.*
#' \normalsize
#' 
#' When aggregated by ocean basin, the overall distribution of catches shows moderate differences between the two datasets (Table \@ref(tab:totbyocean)). The Atlantic Ocean, along with the Mediterranean and Black Sea, shows slightly higher totals in the GTA ; by 3.4% and 2.7% respectively. In contrast, the Pacific and Indian Oceans show marginally higher totals in FishStat, with differences of -1.8% and -0.5% relative to the GTA. It is noteworthy that the Pacific Ocean, being the largest contributor to global catches, accounts for the greatest absolute difference. 
#' 
cat("---- End of the chunk: totbyocean\n")
## ----totbyfleetcreation----------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: totbyfleetcreation----------------------------------------------------------------------------------------------------------------------\n")
table_totbyfleet <- CWP.dataset::compare_dimension_differences(COMP_COUPLE_SPECIES_OCEAN_TIME$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 5)$Groupped_all_not_disap_or_app_to_dysplay %>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per fishing fleet` = Precision ) %>% dplyr::ungroup()

totbyfleet_tidied <- CWP.dataset::qflextable2(table_totbyfleet%>% dplyr::ungroup()%>% dplyr::select(-c(measurement_unit, Dimension)), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

qs::qsave(totbyfleet_tidied, "outputs/totbyfleet_tidied.qs")



#' 
#' 
cat("---- End of the chunk: totbyfleetcreation----------------------------------------------------------------------------------------------------------------------\n")
## ----totbyfleet, tab.cap="Comparison of total catch (t) by fishing fleet between the Global Tuna Atlas and Fishstat database for a set of selected taxa"----

cat("---- Beginning of the chunk: totbyfleet\n")
totbyfleet_tidied <- qs::qread("outputs/totbyfleet_tidied.qs")
totbyfleet_tidied


#' 
#' \scriptsize
#' *Note: See Table \@ref(tab:speciesmain) for the list of selected species.*
#' \normalsize
#' 
#' Similarly, when comparing the distribution of catches across fishing fleets, the detected differences are substantial (Table \@ref(tab:totbyfleet)). While some fleets, such as those of the United States, and Oman, report higher catches in GTA, others, notably Indonesia, Japan, and Peru, show marked decreases. A notable example concerns the Indonesian fleet, whose total catches are about 10% lower in GTA than in FishStat. This difference appears to stem from historical revisions to Indonesia's Indian Ocean catch data, which have been integrated into the GTA dataset but are not yet reflected in FishStat. 
#' 
#' Such temporal lags in data harmonization between sources can explain part of the residual discrepancies observed at the fleet level. This highlights the dynamic nature of reporting workflows between national authorities, RFMOs, and FAO, and suggests that discrepancies may often arise from asynchronous updates rather than conflicting information.
#' 
#' Finally, catches attributed to the 'Other nei' fishing fleet stratum have a substantial impact, representing the largest source of discrepancy between the two datasets at over 3 million tonnes.
#' 
#' ## Discussion
#' 
#' At the global level, total catches appear broadly consistent between the two datasets, with differences smaller than 1%. However, this apparent agreement conceals significant internal discrepancies across species, fleets, and regions. These differences likely result from the distinct data integration processes and levels of aggregation used by each system, as well as from variations in data provenance, whether the figures derive directly from national submissions, regional estimates, or FAO harmonized compilations.
#' 
#' In FishStat, data originate from official national submissions reviewed and adjusted by FAO when necessary, sometimes incorporating "best scientific estimates" from regional bodies to improve global consistency. In contrast, the Global Tuna Atlas (GTA) directly integrates the datasets produced by tuna RFMOs, which already represent the best validated scientific estimates endorsed by their respective scientific committees. GTA then harmonizes these data structurally but does not modify their quantitative content.
#' 
#' Given these methodological differences, identifying a single authoritative dataset is neither feasible nor desirable. Instead, a case-by-case comparison, by country, species, and ocean, is required to understand where and why divergences occur. Each system has complementary strengths:
#' 
#' 1. FishStat offers the most comprehensive and institutionally harmonized view of global fisheries, with broader taxonomic coverage;
#' 
#' 2. GTA provides finer thematic and methodological consistency within tuna fisheries, with full transparency and reproducibility of processing steps.
#' 
#' Understanding how these complementary data flows interact will be essential for future harmonization work between FAO and the RFMOs.
#' 
#' Finally, the preliminary filtering analysis (Figure: \@ref(fig:presenceabsence)) showed that several species recorded in FishStat are absent or only partially represented in GTA. This incomplete taxonomic coverage reflects differences in reporting and aggregation rules. Explicitly documenting, within metadata or as an additional field, the oceanic coverage and potential gaps for each species in the GTA would greatly enhance comparability, interoperability and help users better interpret spatial completeness.
#' 
#' # Regional Focus: IOTC Area (FAO 51 & 57)
#' 
#' After assessing global patterns, this section focuses on the Indian Ocean Tuna Commission (IOTC) area to examine whether the general trends observed at the global scale also hold regionally ; namely, a broad similarity between aggregated datasets but significant differences when disaggregated by specific dimensions such as species or fleet.
#' 
#' ## Definition of the harmonized comparison scope
#' 
#' This regional focus on the Indian Ocean is particularly relevant because both datasets share approximately the same geographical coverage (FAO areas 51 and 57) and the same temporal extent (1950-2023), which allows for a straightforward spatial and temporal comparison. Both datasets also report the same measurement type, corresponding to Nominal Landings, ensuring conceptual consistency in the comparison of catch values. The analysis therefore uses the same subset of 32 common species (and one group of species) identified in the global comparison (See Appendix \@ref(tab:speciesmain)), ensuring consistency in taxonomic scope across scales.
#' 
cat("---- End of the chunk: totbyfleet\n")
## ----filteringIOTCdata-----------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: filteringIOTCdata-----------------------------------------------------------------------------------------------------------------------\n")
NC_RAW <- NC_RAW%>% dplyr::filter(source_authority == "IOTC")
CAPTURE_RAW <- CAPTURE_RAW%>% dplyr::filter(AREA.CODE %in% c("57", "58", "51"))

NCD_IOTC <- NCD%>% dplyr::filter(source_authority == "IOTC")
CAPTURED_IOTC <- CAPTURED%>% dplyr::filter(source_authority == "IOTC")

NCD_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC <- NCD_filtered_COUPLE_SPECIES_OCEAN_TIME%>% dplyr::filter(source_authority == "IOTC")
CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC <- CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME%>% dplyr::filter(source_authority == "IOTC")%>% dplyr::mutate(ocean_iotc = case_when(ocean_basin == "Indian Ocean, Western" ~ "Western Indian Ocean",
                                                                                       ocean_basin == "Indian Ocean, Eastern" ~ "Eastern Indian Ocean",
                                                              TRUE ~ ocean_basin))




#' 
cat("---- End of the chunk: filteringIOTCdata-----------------------------------------------------------------------------------------------------------------------\n")
## ----onlyminortableiotc----------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: onlyminortableiotc----------------------------------------------------------------------------------------------------------------------\n")
onlyminortableiotc <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC,
                                                                    parameter_final = NCD_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC,
                                                                    parameter_time_dimension = c("year"),
                                                                    print_map = FALSE,
                                                                    parameter_diff_value_or_percent = "Difference in value",
                                                                    parameter_colnames_to_keep = c("species_name",
                                                                                                   "fishing_fleet_label", "measurement_unit",
                                                                                                   "measurement_processing_level", "gear_label",
                                                                                                   "year", "measurement_value", "Ocean", "source_authority", "ocean_basin"),
                                                                    parameter_titre_dataset_1 = "FishStat",
                                                                    parameter_titre_dataset_2 = "GTA")$compare_strata_differences_list$number_init_column_final_column

onlyminortableiotc <- onlyminortableiotc %>%
  dplyr::filter(
    !(` ` %in% c(
      "Number of source_authority",
      "Number of Ocean",
      "Number of gridtype",
      "Number of measurement_unit"
    ))
  )

qs::qsave(onlyminortableiotc, "outputs/onlyminortableiotc.qs")


#' 
#' Before applying any filtering, it is worth noting that, when restricted to FAO areas 51 and 57, the FishStat dataset still includes a greater number of species and fleets than GTA (Appendix: Table \@ref(tab:nmberdimIOTCannexe)). This observation is consistent with the global comparison presented earlier and confirms that the broader taxonomic scope of FishStat persists even within a single regional focus. For the subsequent analyses, this difference is acknowledged but not further detailed, as it primarily reflects the structural design of the two datasets rather than a regional anomaly.
#' 
#' ## Nominal landings values comparison
#' 
cat("---- End of the chunk: onlyminortableiotc----------------------------------------------------------------------------------------------------------------------\n")
## ----iotcdatacomparison----------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: iotcdatacomparison----------------------------------------------------------------------------------------------------------------------\n")
species_intersect <- c(intersect(unique(NCD_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC$species), unique(CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC$species)), "FRZ")

CAPTURED_IOTC_intersect <- CAPTURED_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC %>% dplyr::filter(species %in% species_intersect)
NCD_IOTC_intersect <- NCD_filtered_COUPLE_SPECIES_OCEAN_TIME_IOTC %>% dplyr::filter(species %in% species_intersect)

rm(CAPTURED_IOTC)
rm(NCD_IOTC)

COMP_IOTC_INTERSECT <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init =  CAPTURED_IOTC_intersect,
                                                          parameter_final = rbind(head(NCD_IOTC_intersect,1) %>% dplyr::mutate(species = "FRZ", measurement_value = 0.001, fishing_fleet_label = "NEI", species_name = "Frigate and bullet tunas"),NCD_IOTC_intersect),
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c("species_name",
                                                                                         "fishing_fleet_label", "measurement_unit",
                                                                                         "year", "measurement_value", "ocean_simple", "source_authority"),
                                                          parameter_titre_dataset_1 = "FishStat",
                                                          parameter_titre_dataset_2 = "GTA")

table_COMP_IOTC_INTERSECT_tidied <- CWP.dataset::qflextable2(COMP_IOTC_INTERSECT$summary_of_differences%>% dplyr::select(-measurement_unit))

qs::qsave(table_COMP_IOTC_INTERSECT_tidied, "outputs/table_COMP_IOTC_INTERSECT_tidied.qs")


#' 
cat("---- End of the chunk: iotcdatacomparison----------------------------------------------------------------------------------------------------------------------\n")
## ----summarydiffiotc, tab.cap="Total captures for each dataset and relative differences", eval=FALSE-----------------------------------------
## 
## table_COMP_IOTC_INTERSECT_tidied <- qs::qread("outputs/table_COMP_IOTC_INTERSECT_tidied.qs")
## table_COMP_IOTC_INTERSECT_tidied
## 

#' 
#' At the aggregated level, the total catches recorded are slightly higher in FishStat than those in GTA, with a difference of about 0.5% (Table \@ref(tab:totbyocean)). This discrepancy, although moderate, indicates that the datasets are broadly consistent in magnitude. However, such global differences may conceal higher variations when disaggregated by species, fleet, or year.
#' 
## ----timecovindianoceancreation--------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: timecovindianoceancreation--------------------------------------------------------------------------------------------------------------\n")
time_cov_indian_ocean <- clean_time_series_plot(COMP_IOTC_INTERSECT$time_coverage_analysis_list$plots[[1]])

unlink("outputs/time_cov_indian_ocean.pdf")

ggplot2::ggsave(
  filename = "outputs/time_cov_indian_ocean.pdf",
  plot     = time_cov_indian_ocean,
)


#' 
#' 
cat("---- End of the chunk: timecovindianoceancreation--------------------------------------------------------------------------------------------------------------\n")
## ----timecovindianocean, fig.cap = "Comparison of the annual time series of catch (t) of tuna and tuna-like species in the Indian Ocean between for FishStat and GTA datasets for the period 1950-2023"----

cat("---- Beginning of the chunk: timecovindianocean\n")
knitr::include_graphics(here::here("outputs/time_cov_indian_ocean.pdf"))


#' 
#' The temporal evolution of total catches in both datasets shows very similar trajectories throughout the period 1950-2023, with parallel growth patterns reflecting the progressive expansion of tuna fisheries in the Indian Ocean. Overall, GTA values are slightly higher than those of FishStat for most years, except for years between 2004 and 2017, when the two curves reverse.
#' 
#' This pattern indicates that the higher total catches observed in FishStat do not result from a systematic annual difference but rather from small cumulative deviations spread across the time series. In other words, although the overall magnitude is greater in GTA, both datasets reproduce nearly identical interannual trends, suggesting that they are largely driven by the same underlying dynamics.
#' 
#' To better understand the origin of these differences, the analysis is further disaggregated by species in the following section, in order to determine whether the observed discrepancies are evenly distributed across taxa or concentrated in specific groups.
#' 
#' ## Species-level analysis
#' 
cat("---- End of the chunk: timecovindianocean\n")
## ----iotcspecieslevelanalysiscreation--------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: iotcspecieslevelanalysiscreation--------------------------------------------------------------------------------------------------------\n")
table <- CWP.dataset::compare_dimension_differences(COMP_IOTC_INTERSECT$groupping_differences_list$Groupped_all, "species_name", parameter_diff_value_or_percent = "Difference in value", topn = 14)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per species` = Precision ) %>% dplyr::ungroup()

table_reduced <- CWP.dataset::compare_dimension_differences(COMP_IOTC_INTERSECT$groupping_differences_list$Groupped_all, "species_name", parameter_diff_value_or_percent = "Difference in value", topn = 3)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per species` = Precision ) %>% dplyr::ungroup()


species_level_analysis <- table%>% dplyr::ungroup()%>% dplyr::select(-c(measurement_unit, Dimension))
species_level_analysis <- CWP.dataset::qflextable2(species_level_analysis, grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

image <- save_as_image(species_level_analysis, "outputs/species_level_analysis.png")

species_level_analysis_reduced <- table_reduced%>% dplyr::ungroup()%>% dplyr::select(-c(measurement_unit, Dimension))
species_level_analysis_reduced <- CWP.dataset::qflextable2(species_level_analysis_reduced, grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

image <- save_as_image(species_level_analysis, "outputs/species_level_analysis.png")
image_reduced <- save_as_image(species_level_analysis_reduced, "outputs/species_level_analysis_reduced.png")

qs::qsave(species_level_analysis, "outputs/species_level_analysis.qs")

#' 
#' 
cat("---- End of the chunk: iotcspecieslevelanalysiscreation--------------------------------------------------------------------------------------------------------\n")
## ----iotcspecieslevelanalysis, tab.cap="Comparison of total catch (t) by species between the Global Tuna Atlas and Fishstat database for a set of selected taxa", echo=FALSE----


cat("---- Beginning of the chunk: iotcspecieslevelanalysis\n")
species_level_analysis <- qs::qread("outputs/species_level_analysis.qs")
species_level_analysis


#' 
#' \scriptsize
#' *Note: See Table \@ref(tab:speciesmain) for the list of selected species.*
#' \normalsize
#' 
#' <br>
#' 
#' At the species level, discrepancies between GTA and FishStat are not evenly distributed.
#' When focusing on the 16 IOTC "primary species" [@iotc2013cmm1303], most show comparable or slightly higher total catches in GTA compared to FishStat.
#' This group includes the main tuna species (yellowfin, skipjack, albacore, bigeye), as well as swordfish, marlin, and mackerel species, which together account for the majority of catches in the Indian Ocean. Among them, kawakawa (-10.1%) and, to a lesser extent, bigeye tuna (-0.9%) and "Indo-Paciï¬?c king mackerel" exhibit slightly lower totals in GTA, while albacore shows a more notable increase (+16.8%). The "Frigate and bullet tunas" is also, in total, higher in Fishstat. Yellowfin and skipjack tunas display close agreement between datasets, with slightly higher catches in GTA, further confirming the strong consistency observed for the main target species in the Indian Ocean.
#' In contrast, the largest discrepancies are observed for non-priority taxa such as blue shark (-46.5%) and thresher sharks nei (-93.2%), which likely reflect differences in the treatment of bycatch or taxonomic aggregation. These patterns suggest that while the reporting of primary target species is well harmonized between GTA and FishStat, secondary or bycatch groups remain less consistent and more sensitive to methodological or structural differences between datasets.
#' 
#' Notably, some of the discrepancies highlighted previously at the global scale are almost entirely explained by the IOTC region. For instance, the case of Kawakawa largely accounts for the global deficit observed for this species, as the Indian Ocean is the only basin for which complete data are available.
#' 
#' ## Focus on major tuna and tuna-like species
#' 
#' To facilitate a clearer comparison of temporal trends across countries and ocean basins, the analysis now focuses on the major tuna and tuna-like species - albacore, bigeye, skipjack, yellowfin, and swordfish. These species are among the most consistently reported across all oceans and constitute the core taxa of the Global Tuna Atlas. Focusing on this subset allows a more detailed examination of inter-dataset consistency, both spatially and temporally, while minimizing noise from irregularly documented species.
#' 
cat("---- End of the chunk: iotcspecieslevelanalysis\n")
## ----majoruntasinit--------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: majoruntasinit--------------------------------------------------------------------------------------------------------------------------\n")
NCD_filtered_much_much <- NCD_IOTC_intersect

CAPTURED_filtered_much_much <- CAPTURED_IOTC_intersect

#' 
cat("---- End of the chunk: majoruntasinit--------------------------------------------------------------------------------------------------------------------------\n")
## ----majortunascomp--------------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: majortunascomp--------------------------------------------------------------------------------------------------------------------------\n")
CAPTURED_filtered_much_much <- CAPTURED_filtered_much_much %>% dplyr::mutate(ocean_iotc = case_when(ocean_basin == "Indian Ocean, Western" ~ "Western Indian Ocean",
                                                                                       ocean_basin == "Indian Ocean, Eastern" ~ "Eastern Indian Ocean",
                                                                                       TRUE~ocean_iotc))

NCD_filtered_much_much <- NCD_filtered_much_much %>% dplyr::rename(ocean_iotc = Ocean)

major_tunas <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much,
                                                          parameter_final = NCD_filtered_much_much,
                                                          parameter_filtering = list(species_name = c(
                                                            "Skipjack tuna"     ,
                          "Yellowfin tuna"    ,
                          "Albacore"            ,
                          "Bigeye tuna" ,
                          # ,
                          # "Silky shark",
                          # "Porbeagle", "Devil fish",
                          # "Blue shark",
                          # "Blue marlin",
                          "Swordfish"

                          )),
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c("species_name",
                                                                                         "fishing_fleet_label", "measurement_unit",
                                                                                         "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                          parameter_titre_dataset_1 = "FishStat",
                                                          parameter_titre_dataset_2 = "GTA")

#' 
cat("---- End of the chunk: majortunascomp--------------------------------------------------------------------------------------------------------------------------\n")
## ----majortunascomptablecreation-------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: majortunascomptablecreation-------------------------------------------------------------------------------------------------------------\n")
major_tunas_summmary_diff <- major_tunas$summary_of_differences %>% dplyr::select(-measurement_unit)

major_tunas_summmary_diff <- CWP.dataset::qflextable2(major_tunas_summmary_diff)

image <- save_as_image(major_tunas_summmary_diff, "outputs/major_tunas_summmary_diff.png")
qs <- qs::qsave(major_tunas_summmary_diff, "outputs/major_tunas_summmary_diff.qs")


#' 
#' 
cat("---- End of the chunk: majortunascomptablecreation-------------------------------------------------------------------------------------------------------------\n")
## ----majortunascomptable, tab.cap="Total captures (t) for each dataset and relative differences, for datasets filtered on major species", echo=FALSE----

cat("---- Beginning of the chunk: majortunascomptable\n")
major_tunas_summmary_diff <- qs::qread("outputs/major_tunas_summmary_diff.qs")
major_tunas_summmary_diff


#' 
#' <!-- ```{r} -->
#' <!-- major_tunas$groupping_differences_list$Groupped_all %>% dplyr::filter(Dimension == "ocean_iotc") -->
#' <!-- ``` -->
#' 
#' ### Precision and temporal dynamics
#' 
cat("---- End of the chunk: majortunascomptable\n")
## ----imagepermajortunas----------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: imagepermajortunas----------------------------------------------------------------------------------------------------------------------\n")
  skipjack <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much ,
                                                                        parameter_final = NCD_filtered_much_much,
                                                                        parameter_filtering = list(species_name = "Skipjack tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)


  yellowfin <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much ,
                                                                        parameter_final = NCD_filtered_much_much,
                                                                        parameter_filtering = list(species_name = "Yellowfin tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)
    bigeye <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much ,
                                                                        parameter_final = NCD_filtered_much_much,
                                                                        parameter_filtering = list(species_name = "Bigeye tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)
      Albacore <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much ,
                                                                        parameter_final = NCD_filtered_much_much,
                                                                        parameter_filtering = list(species_name = "Albacore"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

        swordfish <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much ,
                                                                        parameter_final = NCD_filtered_much_much,
                                                                        parameter_filtering = list(species_name = "Swordfish"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

#' 
cat("---- End of the chunk: imagepermajortunas----------------------------------------------------------------------------------------------------------------------\n")
## ----figtunadifferences, fig.cap="Temporal evolution of catches in tons for skipjack (A), albacore (B), bigeye (C), yellowfin (D) and swordfish (E) from both FS and GTA datasets."----

cat("---- Beginning of the chunk: figtunadifferences\n")
g1 <- clean_time_series_plot(skipjack$time_coverage_analysis_list$plots[[1]])
g2 <- clean_time_series_plot(Albacore$time_coverage_analysis_list$plots[[1]])
g3 <- clean_time_series_plot(bigeye$time_coverage_analysis_list$plots[[1]])
g4 <- clean_time_series_plot(yellowfin$time_coverage_analysis_list$plots[[1]])
g5 <- clean_time_series_plot(swordfish$time_coverage_analysis_list$plots[[1]])

# cowplot::plot_grid(
#   g1, g2, g3, g4,
#   labels = c("A) skipjack", "B) Albacore", "C) bigeye", "D) yellowfin"),
#   ncol = 2,
#   label_size = 12
# )

#' 
cat("---- End of the chunk: figtunadifferences\n")
## ----tabtunadifferencesfirst, fig.cap="Summary of catches in tons for major tunas from FS and GTA datasets."---------------------------------
cat("---- Beginning of the chunk: tabtunadifferencesfirst\n")
library(gridExtra)
# g1 <- tableGrob(skipjack$summary_of_differences%>% dplyr::select(-measurement_unit) %>% dplyr::mutate_if(is.numeric, round))
# g2 <- tableGrob(Albacore$summary_of_differences%>% dplyr::select(-measurement_unit) %>% dplyr::mutate_if(is.numeric, round))
# g3 <- tableGrob(bigeye$summary_of_differences %>% dplyr::select(-measurement_unit)%>% dplyr::mutate_if(is.numeric, round))
# g4 <- tableGrob(yellowfin$summary_of_differences%>% dplyr::select(-measurement_unit) %>% dplyr::mutate_if(is.numeric, round))


skipjackg1 <- CWP.dataset::qflextable2(CWP.dataset::compare_dimension_differences(skipjack$groupping_differences_list$Groupped_all, "ocean_iotc", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::select(-Dimension) %>% dplyr::arrange(Precision)%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per Area` = Precision ) %>% dplyr::ungroup(), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

Albacorekg1 <- CWP.dataset::qflextable2(CWP.dataset::compare_dimension_differences(Albacore$groupping_differences_list$Groupped_all, "ocean_iotc", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::select(-Dimension) %>% dplyr::arrange(Precision)%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per Area` = Precision ) %>% dplyr::ungroup(), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

bigeyeg1 <- CWP.dataset::qflextable2(CWP.dataset::compare_dimension_differences(bigeye$groupping_differences_list$Groupped_all, "ocean_iotc", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`)%>% dplyr::ungroup()%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::arrange(Precision) %>% dplyr::select(-Dimension) %>% dplyr::rename(`Per Area` = Precision ) , grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

yellowfing1 <- CWP.dataset::qflextable2(CWP.dataset::compare_dimension_differences(yellowfin$groupping_differences_list$Groupped_all, "ocean_iotc", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::select(-Dimension) %>% dplyr::arrange(Precision)%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per Area` = Precision ) %>% dplyr::ungroup(), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))

swordfish1 <- CWP.dataset::qflextable2(CWP.dataset::compare_dimension_differences(swordfish$groupping_differences_list$Groupped_all, "ocean_iotc", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::select(-Dimension) %>% dplyr::arrange(Precision)%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per Area` = Precision ) %>% dplyr::ungroup(), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))


#' 
cat("---- End of the chunk: tabtunadifferencesfirst\n")
## ----tabtunadifferences, fig.cap="Summary of catches in tons for major tunas from FS and GTA datasets."--------------------------------------

cat("---- Beginning of the chunk: tabtunadifferences\n")
library(grid)
gg1 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(skipjackg1 %>%
  as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
gg2 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(Albacorekg1 %>%
  as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
gg3 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(bigeyeg1 %>%
  as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
gg4 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(yellowfing1 %>%
  as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

gg5 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(swordfish1 %>%
  as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#' 
cat("---- End of the chunk: tabtunadifferences\n")
## ----tabmajortunas---------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: tabmajortunas---------------------------------------------------------------------------------------------------------------------------\n")
library(grid)
library(gridExtra)

tab_skipjack <- rasterGrob(skipjackg1 %>% as_raster())

# 2. on ajoute un titre AU-DESSUS du tableau, aligne ÃƒÂ  gauche
tab_skipjack_labeled <- gridExtra::arrangeGrob(
  tab_skipjack,
  top = grid::textGrob(
    "A) Skipjack tuna",
    x = 0,             # 0 = completement ÃƒÂ  gauche
    hjust = 0,
    gp = grid::gpar(fontface = "bold")
  )
)

# 3. on pourra mettre ÃƒÂ§a dans patchwork avec wrap_elements()
gg1 <- patchwork::wrap_elements(tab_skipjack_labeled)
tab_alb <- rasterGrob(Albacorekg1 %>% as_raster())
gg2 <- patchwork::wrap_elements(
  arrangeGrob(
    tab_alb,
    top = textGrob("B) Albacore", x = 0, hjust = 0, gp = gpar(fontface = "bold"))
  )
)

tab_bet <- rasterGrob(bigeyeg1 %>% as_raster())
gg3 <- patchwork::wrap_elements(
  arrangeGrob(
    tab_bet,
    top = textGrob("C) Bigeye tuna", x = 0, hjust = 0, gp = gpar(fontface = "bold"))
  )
)

tab_yft <- rasterGrob(yellowfing1 %>% as_raster())
gg4 <- patchwork::wrap_elements(
  arrangeGrob(
    tab_yft,
    top = textGrob("D) Yellowfin tuna", x = 0, hjust = 0, gp = gpar(fontface = "bold"))
  )
)

tab_swo <- rasterGrob(swordfish1 %>% as_raster())
gg5 <- patchwork::wrap_elements(
  arrangeGrob(
    tab_swo,
    top = textGrob("E) Swordfish", x = 0, hjust = 0, gp = gpar(fontface = "bold"))
  )
)


#' 
#' 
cat("---- End of the chunk: tabmajortunas---------------------------------------------------------------------------------------------------------------------------\n")
## ----finalplotcreation-----------------------------------------------------------------------------------------------------------------------
# 1. Chargez cowplot (et non ggpubr pour get_legend avec return_all)
cat("---- Beginning of the chunk: finalplotcreation-----------------------------------------------------------------------------------------------------------------------\n")
library(cowplot)
library(patchwork)

row1 <- gg1 | (g1 + theme(legend.position = "none")) + plot_layout(widths = c(5, 1))
row2 <- gg2 | (g2 + theme(legend.position = "none")) + plot_layout(widths = c(5, 1))
row3 <- gg3 | (g3 + theme(legend.position = "none")) + plot_layout(widths = c(5, 1))
row4 <- gg4 | (g4 + theme(legend.position = "none")) + plot_layout(widths = c(5, 1))
row5 <- gg5 | (g5 + theme(legend.position = "none")) + plot_layout(widths = c(5, 1))

final_plot <- (row1 / row2 / row3 / row4 / row5) +
  patchwork::plot_layout(guides = "collect")

save_plot <- function(plot_obj, name, width = 10, height = 6) {
  filename <- paste0("outputs/figs_final/", name, ".pdf")
  if (file.exists(filename)) file.remove(filename)
  ggplot2::ggsave(filename, plot = plot_obj, width = width, height = height)
}
rows_list <- list(skipjack = row1, albacore = row2, bigeye = row3, yellowfin = row4, swordfish = row5)

purrr::walk2(
  .x = rows_list,
  .y = names(rows_list),
  .f = ~ save_plot(.x, .y, width = 10, height = 8)
)

two_species <- row2 / row3 +
  patchwork::plot_layout(guides = "collect")

# 2. Groupe Skipjack + Yellowfin + Swordfish (3 lignes empilÃ©es)
other_three <- row1 / row4 / row5 +
  patchwork::plot_layout(guides = "collect")

ggplot2::ggsave(
  filename = "outputs/other_three.pdf",
  plot     = other_three,
  width    = 10,
  height   = 6
)

ggplot2::ggsave(
  filename = "outputs/albacore_bigeye.pdf",
  plot     = two_species,
  width    = 10,
  height   = 6
)

ggplot2::ggsave(
  filename = "outputs/final_plot.pdf",
  plot     = final_plot,
  width    = 10,
  height   = 6
)

qs::qsave(final_plot,"outputs/final_plot.qs")


#' 
#' 
cat("---- End of the chunk: finalplotcreation-----------------------------------------------------------------------------------------------------------------------\n")
## ----finalplot,    echo=FALSE,   warning=FALSE,   message=FALSE,  fig.width=16,   fig.height=16,out.height='100%',   out.width='100%', fig.cap="Evolutions of values for the dimension year and differences by ocean for 5 major species between FS (red) and GTA (Blue)"----

cat("---- Beginning of the chunk: finalplot\n")
final_plot <- qs::qread("outputs/final_plot.qs")
final_plot




#' 
#' \clearpage
#' 
#' Pronounced discrepancies are observed for albacore and bigeye tuna, particularly in the western Indian Ocean, where GTA reports systematically higher values (see Figure: \@ref(fig:finalplot)). For bigeye tuna, these differences are especially pronounced at the ocean-basin level, suggesting that variations may stem from differences in data integration or national submissions rather than from temporal inconsistency.
#' 
#' For yellowfin and skipjack tuna, the two datasets display almost identical trajectories over time, indicating a strong coherence in the reporting of the dominant commercial species. In contrast, swordfish shows moderate discrepancies, mostly during the 1990s and early 2000s, which may correspond to periods of partial data revision or differing treatment of gear-specific catches.
#' 
#' Overall, while the general trends remain comparable across datasets, the magnitude and timing of reported catches for Albacore and bigeye highlight potential inconsistencies in how these species are aggregated or reported by ocean basin.
#' 
#' 
#' ### Focus on bigeye tuna
#' 
#' To better understand the origin of the discrepancies observed at the aggregate level, we focus on bigeye tuna, an illustrative example where the direction of the differences between GTA and FishStat reverses between ocean areas. While not the species with the largest discrepancies, it provides a relevant case study to explore patterns that could similarly be examined for other species.
#' 
#' For bigeye tuna, the largest discrepancies occur in the Eastern Indian Ocean, where GTA reports substantially higher total catches than FishStat for several decades (Figure: \@ref(fig:compbigeye)). The fleet breakdown in appendix (Table \@ref(tab:fleetbreakdownbigeyetotal)) shows that these differences are mainly associated with catches attributed to Indonesia, which are markedly lower in FishStat. This difference is mainly associated with the Eastern Indian Ocean which shows significantly high differences in percentage Table \@ref(tab:fleetbreakdownbigeyeeastern)). While part of this divergence could stem from the way catches are grouped (for example, through the use of the NEI category in GTA), such reclassification alone cannot explain the overall difference, since it would not affect total basin-level values.
#' 
#' In contrast, in the Western Indian Ocean, the two time series show a much closer alignment, with consistent magnitudes and parallel temporal patterns across most years. However, when comparing the two basins, the situation appears more complex: the dataset that reports higher values changes depending on the ocean, and in the Eastern basin, the interannual dynamics also diverge markedly, especially after the early 2000s.
#' 
#' As discussed previously, the case of Indonesia remains the most significant: its lower totals in GTA likely reflect historical revisions to Indian Ocean catch data that have been integrated into GTA but are not yet reflected in FishStat.
#' 
cat("---- End of the chunk: finalplot\n")
## ----bigeyeperocean--------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: bigeyeperocean--------------------------------------------------------------------------------------------------------------------------\n")
bigeyewestern <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much %>% dplyr::filter(ocean_iotc == "Western Indian Ocean") ,
                                                                        parameter_final = NCD_filtered_much_much %>% dplyr::filter(ocean_iotc == "Western Indian Ocean") ,
                                                                        parameter_filtering = list(species_name = "Bigeye tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

    bigeyeeastern <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much %>% dplyr::filter(ocean_iotc == "Eastern Indian Ocean") ,
                                                                        parameter_final = NCD_filtered_much_much %>% dplyr::filter(ocean_iotc == "Eastern Indian Ocean") ,
                                                                        parameter_filtering = list(species_name = "Bigeye tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

#' 
cat("---- End of the chunk: bigeyeperocean--------------------------------------------------------------------------------------------------------------------------\n")
## ----compbigeyecreation----------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: compbigeyecreation----------------------------------------------------------------------------------------------------------------------\n")
library(cowplot)

p1 <- clean_time_series_plot(bigeyewestern$time_coverage_analysis_list$plots[[1]])
p2 <- clean_time_series_plot(bigeyeeastern$time_coverage_analysis_list$plots[[1]])

compbigeyeplot <- cowplot::plot_grid(
  p1, p2,
  labels   = c("a) Western Indian Ocean", "b) Eastern Indian Ocean"),
  ncol     = 2,
  label_x  = c(0, 0),
  label_y  = c(1.05, 1.05),
  hjust    = 0
)

qs::qsave(compbigeyeplot, "outputs/compbigeyeplot.qs")


#' 
#' 
cat("---- End of the chunk: compbigeyecreation----------------------------------------------------------------------------------------------------------------------\n")
## ----compbigeye, fig.cap="Comparison of Western and Eastern Indian Ocean for bigeye tuna catches", fig.width=12, fig.height=6----------------

cat("---- Beginning of the chunk: compbigeye\n")
compbigeyeplot <- qs::qread("outputs/compbigeyeplot.qs")
compbigeyeplot


#' 
cat("---- End of the chunk: compbigeye\n")
## ----fleetbreakdownbigeyeeasterncreation-----------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: fleetbreakdownbigeyeeasterncreation-----------------------------------------------------------------------------------------------------\n")
tablebigeye <- CWP.dataset::compare_dimension_differences(bigeyeeastern$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 5)$Groupped_all_not_disap_or_app_to_dysplay%>% dplyr::rename(`Values dataset 1 (FS)` = `Values dataset 1`) %>% dplyr::rename(`Values dataset 2 (GTA)` = `Values dataset 2`) %>% dplyr::rename(`Per fishing fleet` = Precision ) %>% dplyr::ungroup()

bigeytable_tidy <- CWP.dataset::qflextable2(tablebigeye%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit) %>% dplyr::select(-Dimension), grouped_data = c("Loss / Gain"), columns_to_color = c("Difference (in %)"))


image <- save_as_image(bigeytable_tidy, "outputs/bigeytable_tidy.png")
qs <- qs::qsave(bigeytable_tidy, "outputs/bigeytable_tidy.qs")



#' 
#' 
cat("---- End of the chunk: fleetbreakdownbigeyeeasterncreation-----------------------------------------------------------------------------------------------------\n")
## ----fleetbreakdownbigeyeeastern, tab.cap="Major differences break down by fishing_fleet_label between FS and GTA datasets, for bigeye tuna catches in Eastern Indian Ocean"----

cat("---- Beginning of the chunk: fleetbreakdownbigeyeeastern\n")
bigeytable_tidy <- qs::qread("outputs/bigeytable_tidy.qs")
bigeytable_tidy


#' 
#' To better assess the alignment between datasets, a new comparison was performed excluding the Indonesian data, as the associated differences are well understood and explained by historical reporting revisions.
#' These discrepancies are expected to disappear in the next FishStat update; excluding them therefore allows a clearer examination of the remaining differences between the two datasets.
#' 
#' ### Focus on bigeye tuna without Indonesia data
#' 
cat("---- End of the chunk: fleetbreakdownbigeyeeastern\n")
## ----bigeyewithoutindo-----------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: bigeyewithoutindo-----------------------------------------------------------------------------------------------------------------------\n")
bigeyewesternwithoutindo <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much %>% dplyr::filter(fishing_fleet_label != "Indonesia") %>% dplyr::filter(ocean_iotc == "Western Indian Ocean") ,
                                                                        parameter_final = NCD_filtered_much_much %>% dplyr::filter(fishing_fleet_label != "Indonesia")%>% dplyr::filter(ocean_iotc == "Western Indian Ocean") ,
                                                                        parameter_filtering = list(species_name = "Bigeye tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

    bigeyeeasternwithoutindo <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = CAPTURED_filtered_much_much%>% dplyr::filter(fishing_fleet_label != "Indonesia") %>% dplyr::filter(ocean_iotc == "Eastern Indian Ocean") ,
                                                                        parameter_final = NCD_filtered_much_much%>% dplyr::filter(fishing_fleet_label != "Indonesia") %>% dplyr::filter(ocean_iotc == "Eastern Indian Ocean") ,
                                                                        parameter_filtering = list(species_name = "Bigeye tuna"),
                                                                        parameter_time_dimension = c("year"),
                                                                        parameter_diff_value_or_percent = "Difference in value",
                                                                        parameter_colnames_to_keep = c("species_name",
                                                                                                       "fishing_fleet_label", "measurement_unit",
                                                                                                       "year", "measurement_value", "ocean_iotc", "source_authority"),
                                                                        print_map = FALSE,
                                                                        parameter_titre_dataset_1 = "FishStat",
                                                                        parameter_titre_dataset_2 = "GTA", topnumber = 10)

#' 
cat("---- End of the chunk: bigeyewithoutindo-----------------------------------------------------------------------------------------------------------------------\n")
## ----compbigeyenoindcreation-----------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: compbigeyenoindcreation-----------------------------------------------------------------------------------------------------------------\n")
library(cowplot)

p1 <- clean_time_series_plot(bigeyewesternwithoutindo$time_coverage_analysis_list$plots[[1]])
p2 <- clean_time_series_plot(bigeyeeasternwithoutindo$time_coverage_analysis_list$plots[[1]])

withoutindo <- cowplot::plot_grid(
  p1, p2,
  labels   = c("a) Western Indian Ocean", "b) Eastern Indian Ocean"),
  ncol     = 2,
  label_x  = c(0, 0),
  label_y  = c(1.05, 1.05),
  hjust    = 0
)

ggplot2::ggsave(
  filename = "outputs/figs/compbigeyenoind.pdf",
  plot     = withoutindo,
  width    = 10,
  height   = 6
)

qs::qsave(withoutindo, "outputs/withoutindo.qs")


#' 
#' 
cat("---- End of the chunk: compbigeyenoindcreation-----------------------------------------------------------------------------------------------------------------\n")
## ----compbigeyenoind, fig.cap="Comparison of Western and Eastern Indian Ocean for bigeye tuna catches, without Indonesian data", fig.width=12, fig.height=6----

cat("---- Beginning of the chunk: compbigeyenoind\n")
withoutindo <- qs::qread("outputs/withoutindo.qs")
withoutindo


#' 
#' 
#' When excluding the Indonesian data from the bigeye tuna series, the overall alignment between GTA and FishStat substantially improves, confirming that the observed discrepancy was largely driven by this fleet. Nevertheless, noticeable differences persist in the Western Indian Ocean, suggesting that additional factors, such as spatial aggregation, reporting updates and use of 'Other nei' data may still play a role (Figure: \@ref(fig:compbigeyenoind)). 
#' 
#' 
#' The following results also suggest a progressive convergence between the two datasets in recent years. For several major tuna and tuna-like species -such as albacore, bigeye, and to some extent skipjack -the (see Figure: \@ref(fig:finalplot))) values reported by FishStat and GTA become nearly identical after 2014, indicating that both datasets may increasingly rely on similar or shared data sources.
#' 
#' To verify whether this convergence is systematic or species-specific, the following section focuses on the most recent years of the time series. By comparing the post-2014 period across all major tuna species, we aim to determine whether the observed alignment reflects a broader harmonization of data flows or remains limited to certain taxa or ocean basins.
#' 
#' ## Post-2014 alignment of major species
#' 
#' We restrict the analysis to data from 2014 onwards, since this is where the two series visually converge for all major species. On top of that, for all species the differences are lower from this year (See Appendix: Figure: \@ref(fig:generaldiffbyyear))
#' 
cat("---- End of the chunk: compbigeyenoind\n")
## ----fsj2014---------------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: fsj2014---------------------------------------------------------------------------------------------------------------------------------\n")
FS2014 <- CAPTURED_filtered_much_much%>% dplyr::filter(year > 2000)
NCD2014 <- NCD_filtered_much_much %>% dplyr::filter(year > 2000)

FS2014$year <- as.numeric(format(as.Date(FS2014$year), "%Y"))
NCD2014$year <- as.numeric(format(as.Date(NCD2014$year), "%Y"))


a <- FS2014 %>% dplyr::group_by(year, species_name) %>% dplyr::summarise(sum = sum(measurement_value))

b <- NCD2014 %>% dplyr::group_by(year, species_name) %>% dplyr::summarise(sum = sum(measurement_value))

c <- inner_join(a, b, by = c("year", "species_name")) %>% dplyr::mutate(diff = abs(sum.x-sum.y)) %>% dplyr::mutate(equal = ifelse(diff < 0.1*pmin(sum.x, sum.y), TRUE, FALSE))


#' 
cat("---- End of the chunk: fsj2014---------------------------------------------------------------------------------------------------------------------------------\n")
## ----similarities2014------------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: similarities2014------------------------------------------------------------------------------------------------------------------------\n")
c <- c %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc_diff = 100 * abs(sum.x - sum.y) / pmax(abs(sum.x), abs(sum.y)),
    diff_category = dplyr::case_when(
      sum.x == sum.y ~ "Exact",
      perc_diff < 0.1 ~ "<0.1%",
      perc_diff < 1   ~ "<1%",
      TRUE            ~ ">1%"
    )
  )

#' 
#' Many similarities are observed between 2014 and 2020. We now investigate, for the species-year combinations with small discrepancies, whether differences still remain at the fishing_fleet_label level.
#' 
#' ### Country-specific behaviour for Indian Ocean
#' 
cat("---- End of the chunk: similarities2014------------------------------------------------------------------------------------------------------------------------\n")
## ----countryspec-----------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: countryspec-----------------------------------------------------------------------------------------------------------------------------\n")
res <- make_diff_table(
  data = c,
  row_var = "species_name",
  col_var = "year",
  value_var = "diff_category"
)

# res$long   # pour ggplot

#' 
cat("---- End of the chunk: countryspec-----------------------------------------------------------------------------------------------------------------------------\n")
## ----comparison------------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: comparison------------------------------------------------------------------------------------------------------------------------------\n")
c <- make_comparison_df(
  df1 = FS2014,
  df2 = NCD2014,
  by = c("year", "species_name")
)

c3d <- make_comparison_df(
  df1 = FS2014,
  df2 = NCD2014,
  by = c("year", "species_name", "fishing_fleet_label")
)


#' 
cat("---- End of the chunk: comparison------------------------------------------------------------------------------------------------------------------------------\n")
## ----difftable-------------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: difftable-------------------------------------------------------------------------------------------------------------------------------\n")
tab3d <- make_diff_table_3d(
  data  = c3d,
  dim1  = "species_name",
  dim2  = "year",
  dim3  = "fishing_fleet_label",
  value_var = "diff_category"
)


#' 
cat("---- End of the chunk: difftable-------------------------------------------------------------------------------------------------------------------------------\n")
## ----countryspecificdiffcreation-------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: countryspecificdiffcreation-------------------------------------------------------------------------------------------------------------\n")
`%notin%` <- Negate(`%in%`)
library(grid)   # pour unit()

countryspecificdiff <- tab3d %>%
  rename(species = species_name) %>%
  filter(
    species %in% c("Yellowfin tuna", "Albacore", "Skipjack tuna", "Bigeye tuna", "Swordfish"),
    year > 2011
  ) %>%
  rename(Category = diff_category) %>%
  ggplot(aes(x = year, y = species, fill = Category)) +
  geom_tile() +
  facet_wrap(~ fishing_fleet_label, ncol = 5) +
  scale_fill_manual(
    values = c(
      "Exact"  = "#1b7837",
      "<0.1%"  = "#a6dba0",
      "<1%"    = "#fdb863",
      ">1%"    = "#d7191c"
    )
  ) +
  theme_minimal() +
  labs(
    # title = "Country-specific differences in reported catches (GTA vs FishStat, 2012-2023)",
    x = NULL,  # enleve le label "year"
    y = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14, margin = margin(b = 3, t = 3)),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.margin = margin(5, 5, 10, 5)
  )

countryspecificdiff_for_pwt <-  countryspecificdiff +
  ggplot2::theme(
    axis.text.y = element_text(size = 10)
  )
ggplot2::ggsave(
  filename = "outputs/countryspecificdiff_for_pwt.pdf",
  plot     = countryspecificdiff_for_pwt,
  width    = 12,
  height   = 8
)

ggplot2::ggsave(
  filename = "outputs/countryspecificdiff.pdf",
  plot     = countryspecificdiff,
  width    = 16,
  height   = 16
)

qs::qsave(countryspecificdiff, "outputs/countryspecificdiff.qs")

#' 
#' 
cat("---- End of the chunk: countryspecificdiffcreation-------------------------------------------------------------------------------------------------------------\n")
## ----countryspecificdiff, fig.width=16, fig.height=14, out.width='100%', fig.align='center', fig.cap="Country-specific differences in reported catches (GTA vs FishStat, 2012-2023)"----

cat("---- Beginning of the chunk: countryspecificdiff\n")
countryspecificdiff <- qs::qread("outputs/countryspecificdiff.qs")
countryspecificdiff


#' 
#' Although yellowfin tuna and swordfish do not appear to show a clear convergence between FishStat and GTA in the most recent years, a closer examination reveals that for many countries, the reported values are in fact very similar, sometimes even identical, as they are for the other species (see Figure: \@ref(fig:countryspecificdiff)). 
#' 
#' At the country level, three broad patterns can be distinguished.
#' 
#' 1.  Countries with stable agreement: for several reporting States (e.g. Republic of Korea, Mauritius, or Madagascar), the correspondence between the two datasets remains strong and constant over time.
#' 
#' 2.  Countries with persistent differences: some countries, such as South Africa or Jordan, show systematic deviations between datasets, suggesting enduring discrepancies in reporting, conversion, or aggregation practices.
#' 
#' 3.  Countries with variable alignment: in a few cases (e.g. France, Spain, Great Britain), the relationship between datasets changes from year to year, showing periods of agreement followed by sharp divergences. These alternating patterns are the most challenging to interpret, as they may result from changes in data structuring, aggregation rules, estimation procedures, or even shifts in the data source used by one of the systems.
#' 
#' 4. Non-Contracting Parties with data integration lags: The case of Djibouti exemplifies how asynchronous update cycles and data reallocation can create significant temporary discrepancies. While its data is ultimately sourced from FAO by the IOTC, a recent update in FishStat (where some of the catch previously reported as Yellowfin Tuna (YFT) was reallocated to the generic "Tunas nei" (TUN) category) had not yet been reflected in the GTA at the time of analysis. This highlights how taxonomic reclassifications in one dataset can create apparent discrepancies before synchronization occurs. In contrast, data for other non-Contracting Parties like Egypt shows near-perfect alignment, demonstrating that synchronization is maintained when no recent updates have been made to the underlying records in either dataset.
#' 
#' Overall, this analysis confirms that while convergence between GTA and FishStat is evident for many country-species combinations, differences remain and are not uniformly distributed. Understanding for each, whether these variations stem from harmonization updates, national resubmissions, or methodological differences in data integration will require a detailed comparison of the underlying reporting flows.
#' 
cat("---- End of the chunk: countryspecificdiff\n")
## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------
## 
## View(c)
## 

#' 
#' ### Verification example: identical strata
#' 
#' To conclude, a final verification was carried out on a single year-species pair to confirm whether identical values between datasets correspond to complete equivalence across all dimensions. We selected albacore in 2015, a representative case where total catches are identical in FishStat and GTA.
#' 
#' 
## ----alb2015---------------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: alb2015---------------------------------------------------------------------------------------------------------------------------------\n")
ALBFS2015 <- FS2014 %>% dplyr::filter(year == "2015" & species_name == "Albacore")
ALBNCD2015 <- NCD2014 %>% dplyr::filter(year == "2015" & species_name == "Albacore")


ALBACORE_2015 <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = ALBFS2015 %>% dplyr::mutate(year = "2015-01-01"),
                                                          parameter_final = ALBNCD2015%>% dplyr::mutate(year = "2015-01-01"),
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c(
                                                                                         "fishing_fleet_label", "measurement_unit",
                                                                                         "year", "measurement_value"),
                                                          parameter_titre_dataset_1 = "FishStat",
                                                          parameter_titre_dataset_2 = "GTA")


#' 
#' For this year and species, the data are fully identical between FishStat and GTA, differing only by rounding errors (Appendix: Table \@ref(tab:verifalb2015)). This confirms that in certain strata, both datasets rely on exactly the same source data and transformations, reinforcing the assumption of partial convergence observed in the most recent years.
#' 
#' # Synthesis and discussion
#' 
#' Historical analyses have already highlighted persistent inconsistencies between FAO and RFMO tuna statistics. Garibaldi and Kebe [@garibaldi2005mediterranean] were the first to document discrepancies between FAO and ICCAT tuna catch statistics in the Mediterranean. These differences were later confirmed at a broader scale by Justel-Rubio et al. [@justelrubio2016comparative], who compared FAO and tuna RFMO datasets globally and showed that such inconsistencies persisted across regions and species despite ongoing harmonization efforts.
#' 
#' Although the overall difference between FAO and RFMO datasets was estimated at less than 1% globally, differences exceeding 10% were found for several species or ocean areas. The main causes identified included variations in spatial delineation, flag attribution, the reporting of some fisheries to FAO only or present only in tRFMOs data. These findings mirror the patterns observed in the present study, where discrepancies between FishStat and the Global Tuna Atlas are generally small in aggregate but can reach higher levels when broken down by specific taxa or basins. Together, these analyses reinforce that such divergences largely stem from structural and procedural differences in reporting and harmonization, rather than from contradictory underlying data.
#' 
#' It is important to note that the FAO FishStat dataset does not represent a fully independent source from the RFMOs. A substantial part of the data originates from the same regional reporting systems (IOTC, ICCAT, WCPFC, IATTC, CCSBT), complemented by national submissions and FAO adjustments to fill gaps or ensure consistency. The opposite is also true: in some cases, RFMO statistics may directly draw on FishStat estimates for non-reporting members. This interdependence offers a promising pathway for enhancing the GTA. By systematically integrating new FishStat data as soon as it is published, the GTA could achieve more frequent updates and more comprehensive coverage, particularly for non-contracting parties. This approach could help bridge gaps caused by the delayed integration of national data into RFMO databases, or by the inclusion of catches from non-tuna fisheries. Therefore, many differences between the GTA and FishStat reflect not fundamental inconsistencies, but rather different timelines within the same data harmonization process.
#' 
#' The FAO performs additional aggregation, validation, and estimation steps that may correct for late reporting or fill missing values, leading to slightly higher totals in some cases. This aligns with findings by Heidrich et al. (2023) ([@heidrich2023reconstructing]), who demonstrated that IOTC data under-represent total pelagic catches by about 30 %, suggesting that part of the discrepancies observed between GTA and FishStat could stem from incomplete reporting at the RFMO level.
#' 
#' Interestingly, this pattern appears to vary by taxonomic group:
#' for major species, the GTA values are mainly higher than those in FishStat, reflecting the fact that RFMOs like IOTC tend to maintain more up-to-date and comprehensive statistics for their primary target stocks.
#' Conversely, secondary taxa and bycatch groups are often higher in FishStat, possibly due to FAO-level adjustments or reconstructions compensating for the limited coverage of these species in RFMO datasets.  This may also reflect the inclusion of catches from non-tuna fisheries reported under broader categories.
#' 
#' Overall, discrepancies between FishStat and RFMO-based products such as the GTA have tended to decrease since 2014, following FAO's efforts to enhance alignment with RFMO data.
#' However, historical differences persist, as countries seldom revise older submissions when updating their national reports to tRFMO. Residual mismatches are also partly explained by differences in species or fleet mappings between FAO and RFMO classification systems, and by the use of approximate or overlapping spatial areas that are not handled in a consistent way across datasets.
#' 
#' In addition, the conceptual distinction between nominal landing in FishStat and nominal catches in GTA may introduce minor biases when comparing aggregated totals, as the two variables are not strictly equivalent. This inconsistency cannot be fully resolved at present, but it highlights the need for continued clarification and harmonization of definitions and reporting practices across global tuna datasets.
#' 
#' # Acknowledgments
#' 
#' This work has received funding from the European Union's Horizon Europe research and innovation programme under the Blue-Cloud 2026 project (Grant agreement No 101094227).
#' 
#' \section*{How to cite this document}
#' 
#' \textbf{Grasset, B.}, \textbf{Chassot, E.}, \textbf{Barde, J.}, \textbf{Geehan, J.}, \textbf{Fiorellato, F.} (2025). \textit{Following the tuna trail: Contrasting global catch estimates from FAO and RFMOs}. IOTC-2025-WPDCS21-17\_Rev1.\\
#' \textbf{DOI:} \url{https://doi.org/10.5281/zenodo.17719667}
#' 
#' \clearpage
#' 
#' # Bibliography {-}
#' 
#' ::: {#refs}
#' :::
#' \clearpage
#' 
#' \listoffigures
#' \listoftables
#' 
#' \clearpage
#' 
#' # Appendix {-}
#' 
#' 
cat("---- End of the chunk: alb2015---------------------------------------------------------------------------------------------------------------------------------\n")
## ----gtafirstlinescreation-------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: gtafirstlinescreation-------------------------------------------------------------------------------------------------------------------\n")
headGTA <- head(NC_RAW %>% dplyr::select(-c(year, month, quarter)))
qs::qsave(headGTA, "outputs/headGTA.qs")

image <- save_as_image(flextable(headGTA), "outputs/headGTA.png")

#' 
#' 
cat("---- End of the chunk: gtafirstlinescreation-------------------------------------------------------------------------------------------------------------------\n")
## ----gtafirstlines, tab.cap="Extract of the Global Tuna Atlas dataset, with redundant temporal columns (year, month, quarter) omitted", results='asis', echo=FALSE----

cat("---- Beginning of the chunk: gtafirstlines\n")
headGTA <- qs::qread("outputs/headGTA.qs")

if (knitr::is_latex_output()) {
  cat("\\begin{landscape}\n")
}


knitr::kable(headGTA, booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

if (knitr::is_latex_output()) {
  cat("\\end{landscape}\n")
} else {
  cat('<div class="landscape">')

knitr::kable(headGTA, booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")
  cat('</div>')
}




#' 
cat("---- End of the chunk: gtafirstlines\n")
## ----fishStatfirstlines-prep, include=FALSE--------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: fishStatfirstlines-prep\n")
headfsj <- head(CAPTURE_RAW %>% dplyr::filter(AREA.CODE == "57"))
image <- save_as_image(flextable(headfsj), "outputs/headfsj.png")
qs::qsave(headfsj, "outputs/headfsj.qs")


#' 
cat("---- End of the chunk: fishStatfirstlines-prep\n")
## ----fishStatfirstlines, tab.cap="First lines of the FS dataset", echo=FALSE-----------------------------------------------------------------

cat("---- Beginning of the chunk: fishStatfirstlines\n")
headfsj <- qs::qread("outputs/headfsj.qs")

knitr::kable(headfsj, booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


#' 
cat("---- End of the chunk: fishStatfirstlines\n")
## ----speciesmaincreation---------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: speciesmaincreation---------------------------------------------------------------------------------------------------------------------\n")
speciesmain <- NCD_filtered_COUPLE_SPECIES_OCEAN_TIME %>% 
  dplyr::select(species_name, species) %>% 
  dplyr::distinct()

# On ajoute la nouvelle ligne
speciesmain <- speciesmain %>% 
  dplyr::add_row(
    species_name = "Frigate and bullet tunas",
    species = "FRZ"
  ) %>%
  dplyr::mutate(
    Group = ifelse(species == "FRZ", "Group of two species", "")
  ) %>%
  dplyr::mutate(n = dplyr::row_number()) %>%
  dplyr::select(n, "Species name" = species_name, "Species code" = species, Group) %>% dplyr::arrange(desc("Species name"))

qs::qsave(speciesmain, "outputs/speciesmain.qs")



#' 
#' 
cat("---- End of the chunk: speciesmaincreation---------------------------------------------------------------------------------------------------------------------\n")
## ----speciesmain, tab.cap="Species retained for the comparative analysis between FishStat and the Global Tuna Atlas (n = 32)"----------------

cat("---- Beginning of the chunk: speciesmain\n")
speciesmain <- qs::qread("outputs/speciesmain.qs")
knitr::kable(speciesmain, booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = "scale_down")



#' 
cat("---- End of the chunk: speciesmain\n")
## ----aggregatedcatches, tab.cap="Sample of aggregated species group removed for the comparative analysis between GTA and FS"-----------------

cat("---- Beginning of the chunk: aggregatedcatches\n")
totbyspecies_groupped_nei <-qs::qread("outputs/totbyspecies_groupped_nei.qs")

totbyspecies_groupped_nei


#' 
#' 
cat("---- End of the chunk: aggregatedcatches\n")
## ----Mapping-from-FS-to-nc-------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: Mapping-from-FS-to-nc-------------------------------------------------------------------------------------------------------------------\n")
library(readr)
Mapping_from_FS_to_nc <- read_delim("inputs/mappings/Mapping_from_fsj_to_nc.csv",
    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::select(-"...3")


#' 
#' \renewcommand{\arraystretch}{1.5}
#' \small
#' 
#' Table: (\#tab:geo) Geographic correspondence between GTA management areas and FishStat (FAO) subregions
#' 
#' | **Ocean Basin** | **GTA Regions** | **FishStat Subregions** | **Comments** |
#' |------------------|-----------------|-------------------------------|---------------|
#' | **Atlantic Ocean** | AT-NW, AT-NE, AT-SE, AT-SW | Atlantic Northwest, Northeast, Western Central, Eastern Central, Southwest, Southeast | FS subdivides the Atlantic more finely into six FAO areas, while GTA aggregates them into four broader RFMO regions. |
#' | **Indian Ocean** | IOTC_WEST, IOTC_EAST | Indian Ocean, Western; Indian Ocean, Eastern | Correspondence between GTA and FAO for both subregions. |
#' | **Pacific Ocean** | EPO, WCPO, WCPFC | Pacific Northwest, Northeast, Western Central, Eastern Central, Southwest, Southeast | FS distinguishes six FAO areas, whereas GTA uses three broad RFMO zones (EPO, WCPO, WCPFC) covering overlapping sectors. |
#' | **Mediterranean & Black Sea** | MD | Mediterranean and Black Sea | Correspondence between GTA and FAO. |
#' | **Southern Ocean (Antarctic)** | - | Atlantic Antarctic; Indian Ocean Antarctic; Pacific Antarctic | No direct equivalent in GTA. In FS, these zones contain marginal species (e.g. *Porbeagle*, *Rays*, *Mantas nei*). In GTA, only *Southern Bluefin Tuna (SBF)* may overlap under CCSBT, but reported at basin level (Indian or Atlantic). |
#' 
#' \normalsize
#' \renewcommand{\arraystretch}{1.0}
#' 
cat("---- End of the chunk: Mapping-from-FS-to-nc-------------------------------------------------------------------------------------------------------------------\n")
## ----mappingFStonc---------------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: mappingFStonc---------------------------------------------------------------------------------------------------------------------------\n")
knitr::kable(
  Mapping_from_FS_to_nc,
  booktabs = TRUE,
  caption = "Mapping from FishStat to GTA fishing\\_fleet\\_label"
)


#' 
#' 
cat("---- End of the chunk: mappingFStonc---------------------------------------------------------------------------------------------------------------------------\n")
## ----nmberdimIOTCannexe, tab.cap="Number of dimensions for each dataset filtered on FAO areas 51 and 57, after mapping of fishing_fleet for FS"----

cat("---- Beginning of the chunk: nmberdimIOTCannexe\n")
onlyminortableiotc <- qs::qread("outputs/onlyminortableiotc.qs")


qflextable(onlyminortableiotc)


#' 
#' 
cat("---- End of the chunk: nmberdimIOTCannexe\n")
## ----fleetbreakdownbigeyetotalcreation-------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: fleetbreakdownbigeyetotalcreation-------------------------------------------------------------------------------------------------------\n")
table_fleetbreakdownbigeyetotal <- CWP.dataset::compare_dimension_differences(bigeye$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay

table_fleetbreakdownbigeyetotal_tidied <- CWP.dataset::qflextable2(table_fleetbreakdownbigeyetotal%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit), grouped_data = c("Dimension", "Loss / Gain"), columns_to_color = c("Difference (in %)"))

qs::qsave(table_fleetbreakdownbigeyetotal_tidied, "outputs/table_fleetbreakdownbigeyetotal_tidied.qs")


#' 
#' 
cat("---- End of the chunk: fleetbreakdownbigeyetotalcreation-------------------------------------------------------------------------------------------------------\n")
## ----fleetbreakdownbigeyetotal, tab.cap="Major differences break down by fishing_fleet between FS and GTA datasets, filtered on species-oceans commons pairs", echo=FALSE----

cat("---- Beginning of the chunk: fleetbreakdownbigeyetotal\n")
table_fleetbreakdownbigeyetotal_tidied <- qs::qread("outputs/table_fleetbreakdownbigeyetotal_tidied.qs")
table_fleetbreakdownbigeyetotal_tidied


#' 
cat("---- End of the chunk: fleetbreakdownbigeyetotal\n")
## ----verifalb2015creation, tab.cap="Major differences break down by fishing_fleet between FishStat and GTA for albacore tuna data catches for year 2015"----
cat("---- Beginning of the chunk: verifalb2015creation\n")
table_verifalb2015 <- CWP.dataset::compare_dimension_differences(ALBACORE_2015$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 5)$Groupped_all_not_disap_or_app_to_dysplay

table_verifalb2015_tidied <- CWP.dataset::qflextable2(table_verifalb2015%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit), grouped_data = c("Dimension", "Loss / Gain"), columns_to_color = c("Difference (in %)"))

qs::qsave(table_verifalb2015_tidied, "outputs/table_verifalb2015_tidied.qs")

#' 
#' 
cat("---- End of the chunk: verifalb2015creation\n")
## ----verifalb2015, tab.cap="Major differences break down by fishing_fleet between FishStat and GTA for albacore tuna data catches for year 2015"----


cat("---- Beginning of the chunk: verifalb2015\n")
table_verifalb2015_tidied <- qs::qread("outputs/table_verifalb2015_tidied.qs")
table_verifalb2015_tidied


#' 
#' <!-- ## Western ocean bigeye -->
#' 
#' 
cat("---- End of the chunk: verifalb2015\n")
## ----bigeyewesterwithoutindonesiacreation----------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: bigeyewesterwithoutindonesiacreation----------------------------------------------------------------------------------------------------\n")
table_bigeyewesterwithoutindonesia <- CWP.dataset::compare_dimension_differences(bigeyewestern$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 10)$Groupped_all_not_disap_or_app_to_dysplay

table_bigeyewesterwithoutindonesia_tidied <- CWP.dataset::qflextable2(table_bigeyewesterwithoutindonesia%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit), grouped_data = c("Dimension", "Loss / Gain"), columns_to_color = c("Difference (in %)"))

qs::qsave(table_bigeyewesterwithoutindonesia_tidied, "outputs/table_bigeyewesterwithoutindonesia_tidied.qs")


#' 
#' 
cat("---- End of the chunk: bigeyewesterwithoutindonesiacreation----------------------------------------------------------------------------------------------------\n")
## ----bigeyewesterwithoutindonesia, tab.cap="Major differences break down by fishing_fleet between FishStat and GTA for bigeye tuna in western indian ocean"----

cat("---- Beginning of the chunk: bigeyewesterwithoutindonesia\n")
table_bigeyewesterwithoutindonesia_tidied <- qs::qread("outputs/table_bigeyewesterwithoutindonesia_tidied.qs")
table_bigeyewesterwithoutindonesia_tidied


#' 
#' 
#' <!-- ## Diff general by year -->
#' 
cat("---- End of the chunk: bigeyewesterwithoutindonesia\n")
## ----generaldiffbyyearcreation---------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: generaldiffbyyearcreation---------------------------------------------------------------------------------------------------------------\n")
year_plot_data <- c %>%
  dplyr::group_by(year, diff_category) %>%
  dplyr::summarise(n = n(), .groups = "drop_last") %>%
  dplyr::mutate(percentage = 100 * n / sum(n)) %>%
  dplyr::ungroup()

ggplot_year_plot_data <- ggplot(year_plot_data, aes(x = year, y = percentage, fill = diff_category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("Exact" = "darkgreen",
                               "<0.1%" = "lightgreen",
                               "<1%" = "orange",
                               ">1%" = "red")) +
  labs(
    x = "Year",
    y = "Relative part (%) of year",
    fill = "Relative difference",
    title = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

qs::qsave(ggplot_year_plot_data, "outputs/ggplot_year_plot_data.qs")

#' 
#' 
cat("---- End of the chunk: generaldiffbyyearcreation---------------------------------------------------------------------------------------------------------------\n")
## ----generaldiffbyyear, fig.cap="Comparison of species-to-species differences between GTA and FishStat datasets"-----------------------------

cat("---- Beginning of the chunk: generaldiffbyyear\n")
ggplot_year_plot_data <- qs::qread("outputs/ggplot_year_plot_data.qs")
ggplot_year_plot_data


#' 
cat("---- End of the chunk: generaldiffbyyear\n")
## ----Dominant-difference-category-by-species-a-yearcreation----------------------------------------------------------------------------------
cat("---- Beginning of the chunk: Dominant-difference-category-by-species-a-yearcreation----------------------------------------------------------------------------------\n")
Dominant_difference_category_by_species_a_year <- ggplot(res$long , aes(x = year, y = species_name, fill = diff_category)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Exact" = "#1b7837",
      "<0.1%" = "#a6dba0",
      "<1%" = "#fdb863",
      ">1%" = "#d7191c"
    ),
    name = "Relative difference"
  ) +
  labs(
    x = "Year",
    y = "Species"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

qs::qsave(Dominant_difference_category_by_species_a_year, "outputs/Dominant_difference_category_by_species_a_year.qs")

#' 
#' 
cat("---- End of the chunk: Dominant-difference-category-by-species-a-yearcreation----------------------------------------------------------------------------------\n")
## ----Dominant-difference-category-by-species-a-year, fig.cap = "Dominant difference category by species a year"------------------------------

cat("---- Beginning of the chunk: Dominant-difference-category-by-species-a-year\n")
Dominant_difference_category_by_species_a_year <- qs::qread("outputs/Dominant_difference_category_by_species_a_year.qs")

Dominant_difference_category_by_species_a_year


#' 
#' 
cat("---- End of the chunk: Dominant-difference-category-by-species-a-year\n")
## ----species-codes---------------------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: species-codes---------------------------------------------------------------------------------------------------------------------------\n")
species_codes <- c(
  "ALB", "BET", "BLM", "BLT", "BUM", "COM", "FRI", "GUT", "KAW", "LOT",
  "MLS", "SBF", "SFA", "SKJ", "SWO", "YFT", "FRZ"
)

species_table <- data.frame(
  Code = species_codes,
  Common_name = c(
    "Albacore",
    "Bigeye tuna",
    "Black marlin",
    "Bullet tuna",
    "Blue marlin",
    "Narrow-barred Spanish mackerel",
    "Frigate tuna",
    "Indo-Pacific king mackerel",
    "Kawakawa",
    "Little tunny",
    "Striped marlin",
    "Southern bluefin tuna",
    "Indo-Pacific sailfish",
    "Skipjack tuna",
    "Swordfish",
    "Yellowfin tuna", "Frigate and bullet tuna"
  ),
  Scientific_name = c(
    "Thunnus alalunga",
    "Thunnus obesus",
    "Istiompax indica",
    "Auxis rochei",
    "Makaira nigricans",
    "Scomberomorus commerson",
    "Auxis thazard",
    "Scomberomorus guttatus",
    "Euthynnus affinis",
    "Euthynnus alletteratus",
    "Kajikia audax",
    "Thunnus maccoyii",
    "Istiophorus platypterus",
    "Katsuwonus pelamis",
    "Xiphias gladius",
    "Thunnus albacares", "Auxis thazard and Auxis rochei"
  )
)

knitr::kable(
  species_table,
  booktabs = TRUE,
  caption = "List of species codes, common names, and scientific names (IOTC focus)."
)

#' 
cat("---- End of the chunk: species-codes---------------------------------------------------------------------------------------------------------------------------\n")
## ----djiboutidata, include=FALSE-------------------------------------------------------------------------------------------------------------

cat("---- Beginning of the chunk: djiboutidata\n")
djibouti_FS <- CAPTURED_filtered_much_much %>% dplyr::filter(fishing_fleet_label== "Djibouti" & species_name == "Yellowfin tuna")
djibouti_GTA <- NCD_filtered_much_much %>% dplyr::filter(fishing_fleet_label== "Djibouti" & species_name == "Yellowfin tuna")


Djibouti <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = djibouti_FS,
                                                          parameter_final = djibouti_GTA,
                                                          parameter_time_dimension = c("year"),
                                                          print_map = FALSE,
                                                          parameter_diff_value_or_percent = "Difference in value",
                                                          parameter_colnames_to_keep = c(
                                                                                         "fishing_fleet_label", "measurement_unit", "species_name",
                                                                                         "year", "measurement_value"),
                                                          parameter_titre_dataset_1 = "FishStat-Djibouti",
                                                          parameter_titre_dataset_2 = "GTA-Djibouti")

Djibouti_time_cov <- Djibouti$time_coverage_analysis_list[[2]][[1]]

qs::qsave(Djibouti_time_cov, "outputs/Djibouti_time_cov.qs")


#' 
cat("---- End of the chunk: djiboutidata\n")
## ----djiboutitimecov, fig.cap="Comparison of annual time series of catch (t) of Yellowfin tuna for Djibouti fleet between for FishStat and GTA datasets for the period 1980-2023"----

cat("---- Beginning of the chunk: djiboutitimecov\n")
Djibouti_time_cov <- qs::qread("outputs/Djibouti_time_cov.qs")
Djibouti_time_cov


#' 
#' 
#' 
#' 
cat("---- End of the chunk: djiboutitimecov\n")
## --------------------------------------------------------------------------------------------------------------------------------------------
cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
knitr::knit_exit()

#' #' 
#' #' \clearpage
#' #' 
#' #' # Deprecated
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----tabrecap, results='asis', echo=FALSE, warning=FALSE, message=FALSE,tab.cap="Recap of matching dimensions for FS and GTA datasets"-------
#' cat("---- Beginning of the chunk: tabrecap\n")
#' library(flextable)
#' library(dplyr)
#' 
#' # construction du flextable reduit
#' ft_recap <-
#'   qflextable(table_recap) %>%
#'   flextable::autofit() %>%
#'   flextable::set_table_properties(
#'     layout = "autofit",
#'     width  = 0.9
#'   ) %>%
#'   flextable::fontsize(size = 7, part = "all")
#' 
#' ft_recap
#' 
#' #' 
#' #' 
#' cat("---- End of the chunk: tabrecap\n")
#' ## ----numberdim, tab.cap="Number of dimensions for each dataset, after mapping of fishing_fleet"----------------------------------------------
#' 
#' cat("---- Beginning of the chunk: numberdim\n")
#' qflextable(minortabl)
#' 
#' 
#' #' 
#' #' ## Diff general by year all species and continuous
#' #' 
#' cat("---- End of the chunk: numberdim\n")
#' ## --------------------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ggplot(tab3d %>% dplyr::filter(species_name%in%c("Yellowfin tuna", "Albacore", "Skipjack tuna", "Bigeye tuna", "Swordfish")), aes(x = year, y = species_name, fill = diff_category)) +
#'   geom_tile() +
#'   facet_wrap(~ fishing_fleet_label) +
#'   scale_fill_manual(
#'     values = c(
#'       "Exact" = "#1b7837",
#'       "<0.1%" = "#a6dba0",
#'       "<1%" = "#fdb863",
#'       ">1%" = "#d7191c"
#'     )
#'   ) +
#'   theme_minimal()
#' 
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----makecomcontinuous-----------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: makecomcontinuous-----------------------------------------------------------------------------------------------------------------------\n")
#' c <- make_comparison_df_continuous(
#'   df1 = FS2014,
#'   df2 = NCD2014,
#'   by = c("year", "species_name")
#' )
#' 
#' 
#' #' 
#' cat("---- End of the chunk: makecomcontinuous-----------------------------------------------------------------------------------------------------------------------\n")
#' ## ----relativediff----------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: relativediff----------------------------------------------------------------------------------------------------------------------------\n")
#' library(ggplot2)
#' 
#' ggplot(c, aes(x = year, y = species_name, fill = perc_diff)) +
#'   geom_tile(color = "white") +
#' scale_fill_gradientn(
#'   colours = c("#1b7837", "#b2df8a", "#fdb863", "#d7191c"),
#'   values  = scales::rescale(c(0, 5, 10, 100)),  # milieu vers 5-10%
#'   trans   = "log10",
#'   limits  = c(0.01, 100),
#'   na.value = "grey90",
#'   name = "Relative difference (%)"
#' )+
#'   labs(
#'     x = "Year",
#'     y = "Species",
#'     title = "Relative difference between sum.x and sum.y (continuous scale)"
#'   ) +
#'   theme_minimal(base_size = 13) +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' 
#' #' 
#' cat("---- End of the chunk: relativediff----------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----continuous------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: continuous------------------------------------------------------------------------------------------------------------------------------\n")
#' c3d <- make_comparison_df_continuous(
#'   df1 = FS2014,
#'   df2 = NCD2014,
#'   by = c("year", "species_name", "fishing_fleet_label")
#' )
#' 
#' c3d <- c3d %>%
#' dplyr::mutate(
#' perc_diff_clipped = dplyr::case_when(
#' perc_diff < 0.01 ~ 0.01, # 1e-2
#' perc_diff > 100 ~ 100,
#' TRUE ~ perc_diff
#' )
#' )
#' 
#' ggplot(c3d%>% dplyr::filter(species_name %in% c("Yellowfin tuna", "Albacore", "Skipjack tuna", "Bigeye tuna", "Swordfish")), aes(x = year, y = species_name, fill = perc_diff_clipped)) +
#'   geom_tile(color = "white") +
#'   facet_wrap(~ fishing_fleet_label) +
#' scale_fill_gradientn(
#'   colours = c("#1b7837", "#b2df8a", "#fdb863", "#d7191c"),
#'   values  = scales::rescale(c(0, 5, 10, 100)),  # milieu vers 5-10%
#'   trans   = "log10",
#'   limits  = c(0.01, 100),
#'   na.value = "grey90",
#'   name = "Relative difference (%)"
#' ) +
#'   labs(
#'     x = "Year",
#'     y = "Species",
#'     title = "Relative difference by species a year a fishing fleet"
#'   ) +
#'   theme_minimal(base_size = 12) +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' 
#' #' 
#' cat("---- End of the chunk: continuous------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----plottingcontinuous----------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: plottingcontinuous----------------------------------------------------------------------------------------------------------------------\n")
#' ggplot(c3d%>% dplyr::filter(species_name %notin% c("Yellowfin tuna", "Albacore", "Skipjack tuna", "Bigeye tuna", "Swordfish")), aes(x = year, y = species_name, fill = perc_diff_clipped)) +
#'   geom_tile(color = "white") +
#'   facet_wrap(~ fishing_fleet_label) +
#' scale_fill_gradientn(
#'   colours = c("#1b7837", "#b2df8a", "#fdb863", "#d7191c"),
#'   values  = scales::rescale(c(0, 5, 10, 100)),  # milieu vers 5-10%
#'   trans   = "log10",
#'   limits  = c(0.01, 100),
#'   na.value = "grey90",
#'   name = "Relative difference (%)"
#' ) +
#'   labs(
#'     x = "Year",
#'     y = "Species",
#'     title = "Relative difference by species a year a fishing fleet"
#'   ) +
#'   theme_minimal(base_size = 12) +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' 
#' #' 
#' #' ## Recap by year species diff
#' #' 
#' cat("---- End of the chunk: plottingcontinuous----------------------------------------------------------------------------------------------------------------------\n")
#' ## --------------------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' library(dplyr)
#' 
#' recap_global <- c %>%
#'   mutate(
#'     perc_diff = 100 * abs(sum.x - sum.y) / pmax(abs(sum.x), abs(sum.y))
#'   ) %>%
#'   summarise(
#'     exact_equal     = sum(sum.x == sum.y),
#'     diff_lt_0_1pct  = sum(perc_diff < 0.1 & sum.x != sum.y),
#'     diff_lt_1pct    = sum(perc_diff >= 0.1 & perc_diff < 1),
#'     diff_ge_1pct    = sum(perc_diff >= 1),
#'     total           = n()
#'   )
#' 
#' recap_by_species_name <- c %>%
#'   mutate(
#'     perc_diff = 100 * abs(sum.x - sum.y) / pmax(abs(sum.x), abs(sum.y))
#'   ) %>%
#'   group_by(species_name) %>%
#'   summarise(
#'     exact_equal     = sum(sum.x == sum.y),
#'     diff_lt_0_1pct  = sum(perc_diff < 0.1 & sum.x != sum.y),
#'     diff_lt_1pct    = sum(perc_diff >= 0.1 & perc_diff < 1),
#'     diff_ge_1pct    = sum(perc_diff >= 1),
#'     total           = n()
#'   ) %>%
#'   ungroup()
#' 
#' 
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------
#' ## recap_by_species_name %>% dplyr::arrange(diff_ge_1pct)
#' 
#' #' 
#' ## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------
#' ## recap_global %>% dplyr::arrange(diff_ge_1pct)
#' 
#' #' 
#' ## --------------------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ggplot(res$long %>% dplyr::filter(species_name%in%c("Yellowfin tuna", "Albacore", "Skipjack tuna", "Bigeye tuna", "Swordfish")), aes(x = year, y = species_name, fill = diff_category)) +
#'   geom_tile(color = "white") +
#'   scale_fill_manual(
#'     values = c(
#'       "Exact" = "#1b7837",
#'       "<0.1%" = "#a6dba0",
#'       "<1%" = "#fdb863",
#'       ">1%" = "#d7191c"
#'     ),
#'     name = "Relative difference"
#'   ) +
#'   labs(
#'     x = "Year",
#'     y = "Species",
#'     title = "Dominant difference category by species a year"
#'   ) +
#'   theme_minimal(base_size = 12) +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' #' 
#' #' ## Now we look at all species/year pairs that have less than 1% difference.
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## --------------------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' less_1_pt <- c %>% dplyr::filter(perc_diff < 1 )%>% dplyr::select(year, species_name) %>% dplyr::distinct()
#' 
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------
#' ## # 2) Donnees pour le barplot empile (proportions internes a chaque espece)
#' ## species_name_plot_data <- c %>%
#' ##   dplyr::count(species_name, diff_category, name = "n") %>%
#' ##   dplyr::group_by(species_name) %>%
#' ##   dplyr::mutate(percentage = 100 * n / sum(n)) %>%
#' ##   dplyr::ungroup()
#' ## 
#' ## # 3) ORDRE = proportion de lignes "rouges" (>1%) par espece
#' ## red_share <- c %>%
#' ##   dplyr::group_by(species_name) %>%
#' ##   dplyr::summarise(red_prop = mean(perc_diff >= 1, na.rm = TRUE), .groups = "drop")
#' ## 
#' ## # Avec coord_flip(), le 1er niveau est en BAS.
#' ## order_levels <- red_share %>%
#' ##   dplyr::arrange(dplyr::desc(red_prop), species_name) %>%  # du + rouge au - rouge
#' ##   dplyr::pull(species_name)
#' ## 
#' ## # 4) Appliquer l'ordre + figer l'ordre des categories
#' ## species_name_plot_data <- species_name_plot_data %>%
#' ##   dplyr::mutate(
#' ##     species_name = factor(species_name, levels = order_levels),
#' ##     diff_category = factor(diff_category, levels = c("Exact", "<0.1%", "<1%", ">1%" ))
#' ##   )
#' ## 
#' ## # 5) Plot
#' ## ggplot2::ggplot(species_name_plot_data, ggplot2::aes(x = species_name, y = percentage, fill = diff_category)) +
#' ##   ggplot2::geom_col() +
#' ##   ggplot2::coord_flip() +
#' ##   ggplot2::scale_fill_manual(
#' ##     values = c("Exact" = "#1b7837", "<0.1%" = "#a6dba0", "<1%" = "#fdb863", ">1%" = "#d7191c"),
#' ##     name = "Relative difference"
#' ##   ) +
#' ##   ggplot2::labs(
#' ##     x = "Species", y = "Share of records (%)",
#' ##     title = "Differences between sum.x and sum.y by species_name"
#' ##   ) +
#' ##   ggplot2::theme_minimal(base_size = 13) +
#' ##   ggplot2::theme(
#' ##     legend.position = "top",
#' ##     plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
#' ##   )
#' ## 
#' 
#' #' 
#' ## --------------------------------------------------------------------------------------------------------------------------------------------
#' cat("---- Beginning of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' FS_less_1_pct <- FS2014 %>% dplyr::inner_join(less_1_pt)
#' NCD_less_1_pct <- NCD2014 %>% dplyr::inner_join(less_1_pt)
#' 
#' 
#' COMP_LESS_1_PCT <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = FS_less_1_pct,
#'                                                           parameter_final = NCD_less_1_pct,
#'                                                           parameter_time_dimension = c("year"),
#'                                                           print_map = FALSE,
#'                                                           parameter_diff_value_or_percent = "Difference in value",
#'                                                           parameter_colnames_to_keep = c(
#'                                                                                          "fishing_fleet_label", "measurement_unit", "species_name",
#'                                                                                          "year", "measurement_value"),
#'                                                           parameter_titre_dataset_1 = "FishStat",
#'                                                           parameter_titre_dataset_2 = "GTA")
#' 
#' 
#' #' 
#' cat("---- End of the chunk: ----------------------------------------------------------------------------------------------------------------------------------------\n")
#' ## ----attentioncestunesomme-------------------------------------------------------------------------------------------------------------------
#' 
#' cat("---- Beginning of the chunk: attentioncestunesomme-------------------------------------------------------------------------------------------------------------------\n")
#' ALBACORE_2015$other_dimension_analysis_list
#' 
#' 
#' #' 
#' cat("---- End of the chunk: attentioncestunesomme-------------------------------------------------------------------------------------------------------------------\n")
#' ## ----albacoretable2015, tab.cap="Major differences break down by fishing_fleet_label between less_1_pct datasets"----------------------------
#' 
#' cat("---- Beginning of the chunk: albacoretable2015\n")
#' table <- CWP.dataset::compare_dimension_differences(ALBACORE_2015$groupping_differences_list$Groupped_all, "fishing_fleet_label", parameter_diff_value_or_percent = "Difference in value", topn = 5)$Groupped_all_not_disap_or_app_to_dysplay
#' 
#' CWP.dataset::qflextable2(table%>% dplyr::ungroup()%>% dplyr::select(-measurement_unit), grouped_data = c("Dimension", "Loss / Gain"), columns_to_color = c("Difference (in %)"))
#' 
#' 
#' #' 
#' cat("---- End of the chunk: albacoretable2015\n")
#' ## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------
#' ## year_plot_data <- c %>%
#' ##   group_by(year, diff_category) %>%
#' ##   summarise(n = n(), .groups = "drop_last") %>%
#' ##   mutate(percentage = 100 * n / sum(n)) %>%
#' ##   ungroup()
#' ## 
#' ## ggplot(year_plot_data, aes(x = year, y = percentage, fill = diff_category)) +
#' ##   geom_col(position = "fill") +
#' ##   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#' ##   scale_fill_manual(values = c("Exact" = "darkgreen",
#' ##                                "<0.1%" = "lightgreen",
#' ##                                "<1%" = "orange",
#' ##                                ">1%" = "red")) +
#' ##   labs(
#' ##     x = "Year",
#' ##     y = "Relative part (%) of year",
#' ##     fill = "Relative difference",
#' ##     title = "Comparison of differences between sum.x and sum.y by year"
#' ##   ) +
#' ##   theme_minimal(base_size = 13) +
#' ##   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' ## 
#' 
#' #' 
#' #' ## Pourcentage de strates completement equivalentes.
#' #' 
#' #' ### Dimensions temps/especes
#' #' 
#' #' ### Dimensions temps/especes/pavillons
#' #' 
#' #' ## Pour les donnees equivalentes en temps/especes, quels sont les differences de pavillons
