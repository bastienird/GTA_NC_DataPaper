base::options(knitr.duplicate.label = "allow")

# Liste des espèces majeures
species_name_major <- c("Bigeye tuna", "Albacore", "Swordfish", "Skipjack tuna", "Yellowfin tuna")

# Liste des pays à analyser
pays_a_analyser <- c("United Republic of Tanzania", "Indonesia", "France", "India")

# Créer le répertoire de sortie
if(!dir.exists("outputs/species_analysis")) {
  dir.create("outputs/species_analysis", recursive = TRUE)
}
CAPTURED_indian <- CAPTURED %>% 
  filter(grepl("Indian", Ocean, ignore.case = TRUE)) %>% dplyr::filter(fishing_fleet_label  == "France")

NCD_indian <- NCD %>% 
  filter(grepl("Indian", Ocean, ignore.case = TRUE))%>% dplyr::filter(fishing_fleet_label  == "France")
base::options(knitr.duplicate.label = "allow")


# Double boucle avec gestion d'erreurs
for(pays in pays_a_analyser) {
  cat("=== Analyse pour:", pays, "===\n")
  
  # Liste pour stocker les fichiers HTML de ce pays
  fichiers_html_pays <- character()
  
  for(i in seq_along(species_name_major)) {
    species <- species_name_major[i]
    cat("  espece:", species, "... ")
    
    tryCatch({
      result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
        parameter_init = CAPTURED_indian,
        parameter_final = NCD_indian,
        parameter_time_dimension = c("year"),
        print_map = FALSE,
        parameter_filtering = list(
          "fishing_fleet_label" = pays, 
          "species_name" = species
        ),
        parameter_diff_value_or_percent = "Difference in value",
        parameter_colnames_to_keep = c(
          "species_name", "year", "measurement_value", 
          "measurement_unit", "fishing_fleet_label", "Ocean"
        ),
        parameter_titre_dataset_1 = "FishStat",
        parameter_titre_dataset_2 = "GTA"
      )
      
      result$combined_summary_histogram <- NULL
      
      filename <- paste0("outputs/species_analysis/",
                         gsub(" ", "_", tolower(pays)), "_",
                         gsub(" ", "_", tolower(species)), 
                         ".rds")
      
      
      result$step_title_t_f <- FALSE
      result$child_header <- "#"
      result$main_title <- FALSE
      result$other_dimension_analysis_list$figures <- list(
        result$other_dimension_analysis_list$figures[[length(result$other_dimension_analysis_list$figures)]]
      )
      
      result$other_dimension_analysis_list$dimension_title_subfigures <- tail(
        result$other_dimension_analysis_list$dimension_title_subfigures, 1
      )
      result$other_dimension_analysis_list <- NULL
      result$groupping_differences_list$Groupped_all <- result$groupping_differences_list$Groupped_all %>%
        dplyr::filter(Dimension == "Ocean")
      result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay <- result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay %>%
        dplyr::filter(Dimension == "Ocean")
      
      # Préparer l'environnement pour le Rmd
      resultenv <- list2env(result, envir = new.env())
      
      # Générer le rapport individuel
      fichier_html <- sprintf("%s.html", paste0("outputs/species_analysis/",
                                                gsub(" ", "_", tolower(pays)), "_",
                                                gsub(" ", "_", tolower(species))))
      
      rmarkdown::render("tstes.Rmd",
                        envir = resultenv,
                        output_file = basename(fichier_html),
                        output_dir = dirname(fichier_html))
      unlink(filename)
      saveRDS(resultenv, filename)
      
      # Stocker le chemin du fichier HTML
      fichiers_html_pays <- c(fichiers_html_pays, fichier_html)
      
      cat("???\n")
      
    }, error = function(e) {
      cat("??? erreur:", e$message, "\n")
    })
  }
  
  # === À LA FIN DE CHAQUE PAYS : CRÉER LE RAPPORT COMPLET ===
  if(length(fichiers_html_pays) > 0) {
    cat("  Création du rapport complet pour", pays, "... ")
    
    tryCatch({
      # Fonction pour combiner les HTML
      combiner_rapports_html <- function(fichiers, fichier_sortie) {
        pieces <- character()
        
        for(fichier in fichiers) {
          if(file.exists(fichier)) {
            contenu <- readLines(fichier, warn = FALSE)
            pieces <- c(pieces, paste(contenu, collapse = "\n"))
          }
        }
        
        html_complet <- paste0(
          '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Complete rapport - ', pays, '</title>
  <style>
    .rapport-section { 
      margin-bottom: 50px; 
      border-bottom: 2px solid #ccc; 
      padding-bottom: 30px; 
    }
    .rapport-section:last-child { border-bottom: none; }
    h1 { color: #2c3e50; }
    h2 { color: #3498db; margin-top: 40px; }
  </style>
</head>
<body>
  <h1>Report - ', pays, '</h1>
  <h2>Analyse for each species in the following list "Bigeye tuna", "Albacore", "Swordfish", "Skipjack tuna", "Yellowfin tuna"</h2>
  ',
          paste0('<div class="rapport-section">', pieces, '</div>', collapse = "\n"),
          '
</body>
</html>'
        )
        
        writeLines(html_complet, fichier_sortie)
        return(fichier_sortie)
      }
      
      # Créer le rapport complet pour ce pays
      fichier_complet <- paste0("outputs/species_analysis/",
                                gsub(" ", "_", tolower(pays)), 
                                "_RAPPORT_COMPLET.html")
      
      combiner_rapports_html(fichiers_html_pays, fichier_complet)
      cat("??? Rapport complet :", fichier_complet, "\n")
      
    }, error = function(e) {
      cat("??? Erreur création rapport complet:", e$message, "\n")
    })
  }
  
  cat("--- Terminé pour", pays, "---\n\n")
}

cat("???? Tous les rapports ont été générés!\n")


# result$step_title_t_f <- FALSE
# result$child_header <- "#"
# result$main_title <- FALSE
# resultenv <- list2env(result, envir = new.env())
# rmarkdown::render("tstes.Rmd",
#                   envir = resultenv,
#                   output_file = "catch_no_effort.html",
#                   output_dir = file.path("test"))



# Liste des pays à analyser
pays_a_analyser <- c("France")

# Créer le répertoire de sortie
if(!dir.exists("outputs/species_analysis")) {
  dir.create("outputs/species_analysis", recursive = TRUE)
}
CAPTURED_indian_1980 <- CAPTURED_indian %>% 
  filter(grepl("Indian", Ocean, ignore.case = TRUE)) %>% dplyr::filter(year > 1980)

NCD_indian_1980 <- NCD_indian %>% 
  filter(grepl("Indian", Ocean, ignore.case = TRUE))%>% dplyr::filter(year > 1980)
base::options(knitr.duplicate.label = "allow")


# Double boucle avec gestion d'erreurs
for(pays in pays_a_analyser) {
  cat("=== Analyse pour:", pays, "===\n")
  
  # Liste pour stocker les fichiers HTML de ce pays
  fichiers_html_pays <- character()
  
  for(i in seq_along(species_name_major)) {
    species <- species_name_major[i]
    cat("  espece:", species, "... ")
    
    tryCatch({
      result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
        parameter_init = CAPTURED_indian_1980,
        parameter_final = NCD_indian_1980,
        parameter_time_dimension = c("year"),
        print_map = FALSE,
        parameter_filtering = list(
          "fishing_fleet_label" = pays, 
          "species_name" = species
        ),
        parameter_diff_value_or_percent = "Difference in value",
        parameter_colnames_to_keep = c(
          "species_name", "year", "measurement_value", 
          "measurement_unit", "fishing_fleet_label", "Ocean"
        ),
        parameter_titre_dataset_1 = "FishStat",
        parameter_titre_dataset_2 = "GTA"
      )
      
      result$combined_summary_histogram <- NULL
      
      filename <- paste0("outputs/species_analysis/",
                         gsub(" ", "_", tolower(pays)), "_",
                         gsub(" ", "_", tolower(species)), 
                         ".rds")
      
      
      result$step_title_t_f <- FALSE
      result$child_header <- "#"
      result$main_title <- FALSE
      result$other_dimension_analysis_list$figures <- list(
        result$other_dimension_analysis_list$figures[[length(result$other_dimension_analysis_list$figures)]]
      )
      
      result$other_dimension_analysis_list$dimension_title_subfigures <- tail(
        result$other_dimension_analysis_list$dimension_title_subfigures, 1
      )
      result$other_dimension_analysis_list <- NULL
      result$groupping_differences_list$Groupped_all <- result$groupping_differences_list$Groupped_all %>%
        dplyr::filter(Dimension == "Ocean")
      result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay <- result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay %>%
        dplyr::filter(Dimension == "Ocean")
      
      # Préparer l'environnement pour le Rmd
      resultenv <- list2env(result, envir = new.env())
      
      # Générer le rapport individuel
      fichier_html <- sprintf("%s.html", paste0("outputs/species_analysis/",
                                                gsub(" ", "_", tolower(pays)), "_",
                                                gsub(" ", "_", tolower(species))))
      
      rmarkdown::render("tstes.Rmd",
                        envir = resultenv,
                        output_file = basename(fichier_html),
                        output_dir = dirname(fichier_html))
      unlink(filename)
      saveRDS(resultenv, filename)
      
      # Stocker le chemin du fichier HTML
      fichiers_html_pays <- c(fichiers_html_pays, fichier_html)
      
      cat("???\n")
      
    }, error = function(e) {
      cat("??? erreur:", e$message, "\n")
    })
  }
  
  # === À LA FIN DE CHAQUE PAYS : CRÉER LE RAPPORT COMPLET ===
  if(length(fichiers_html_pays) > 0) {
    cat("  Création du rapport complet pour", pays, "... ")
    
    tryCatch({
      # Fonction pour combiner les HTML
      combiner_rapports_html <- function(fichiers, fichier_sortie) {
        pieces <- character()
        
        for(fichier in fichiers) {
          if(file.exists(fichier)) {
            contenu <- readLines(fichier, warn = FALSE)
            pieces <- c(pieces, paste(contenu, collapse = "\n"))
          }
        }
        
        html_complet <- paste0(
          '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Complete rapport - ', pays, '</title>
  <style>
    .rapport-section { 
      margin-bottom: 50px; 
      border-bottom: 2px solid #ccc; 
      padding-bottom: 30px; 
    }
    .rapport-section:last-child { border-bottom: none; }
    h1 { color: #2c3e50; }
    h2 { color: #3498db; margin-top: 40px; }
  </style>
</head>
<body>
  <h1>Report - ', pays, '</h1>
  <h2>Analyse for each species in the following list "Bigeye tuna", "Albacore", "Swordfish", "Skipjack tuna", "Yellowfin tuna"</h2>
  ',
          paste0('<div class="rapport-section">', pieces, '</div>', collapse = "\n"),
          '
</body>
</html>'
        )
        
        writeLines(html_complet, fichier_sortie)
        return(fichier_sortie)
      }
      
      # Créer le rapport complet pour ce pays
      fichier_complet <- paste0("outputs/species_analysis/",
                                gsub(" ", "_", tolower(pays)), 
                                "_RAPPORT_COMPLET.html")
      
      combiner_rapports_html(fichiers_html_pays, fichier_complet)
      cat("??? Rapport complet :", fichier_complet, "\n")
      
    }, error = function(e) {
      cat("??? Erreur création rapport complet:", e$message, "\n")
    })
  }
  
  cat("--- Terminé pour", pays, "---\n\n")
}

cat("???? Tous les rapports ont été générés!\n")
