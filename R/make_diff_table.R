make_diff_table <- function(data,
                            row_var,     # ex: "species_name"
                            col_var,     # ex: "year"
                            value_var = "diff_category",
                            normalize = c("none", "row", "col")) {
  normalize <- match.arg(normalize)
  
  tab <- data %>%
    dplyr::count(
      !!rlang::sym(row_var),
      !!rlang::sym(col_var),
      !!rlang::sym(value_var),
      name = "n"
    )
  
  # on garde une ligne par (row, col) avec la cat majoritaire
  tab_major <- tab %>%
    dplyr::group_by(!!rlang::sym(row_var), !!rlang::sym(col_var)) %>%
    dplyr::slice_max(n, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # normalisation optionnelle
  if (normalize == "row") {
    tab_major <- tab_major %>%
      dplyr::group_by(!!rlang::sym(row_var)) %>%
      dplyr::mutate(pct = 100 * n / sum(n)) %>%
      dplyr::ungroup()
  } else if (normalize == "col") {
    tab_major <- tab_major %>%
      dplyr::group_by(!!rlang::sym(col_var)) %>%
      dplyr::mutate(pct = 100 * n / sum(n)) %>%
      dplyr::ungroup()
  }
  
  # passer en large : lignes = row_var, colonnes = col_var, valeurs = cat
  wide <- tab_major %>%
    tidyr::pivot_wider(
      id_cols = !!rlang::sym(row_var),
      names_from = !!rlang::sym(col_var),
      values_from = !!rlang::sym(value_var)
    )
  
  return(list(
    long = tab_major,
    wide = wide
  ))
}