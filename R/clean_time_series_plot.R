clean_time_series_plot <- function(p) {
  # eviter la notation scientifique partout
  options(scipen = 9999)
  
  p +
    # on affiche les valeurs en milliers
    scale_y_continuous(
      name = "Total catches (x 1,000 t)",
      labels = function(x) x / 1000
    ) +
    # pas de label sur l'axe des x
    labs(x = NULL) +
    # legend en bas
    theme(
      legend.position = "bottom",
      # on enleve le strip de droite (cas des facet_grid(..., switch = "y") ou strip.right)
      strip.text.y.right = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
}