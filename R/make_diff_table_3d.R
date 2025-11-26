make_diff_table_3d <- function(data,
                               dim1 = "species_name",
                               dim2 = "year",
                               dim3 = "fishing_fleet_label",
                               value_var = "diff_category") {
  
  data %>%
    dplyr::count(
      !!rlang::sym(dim1),
      !!rlang::sym(dim2),
      !!rlang::sym(dim3),
      !!rlang::sym(value_var),
      name = "n"
    ) %>%
    dplyr::group_by(
      !!rlang::sym(dim1),
      !!rlang::sym(dim2),
      !!rlang::sym(dim3)
    ) %>%
    dplyr::slice_max(n, with_ties = FALSE) %>%
    dplyr::ungroup()
}