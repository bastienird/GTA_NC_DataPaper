make_comparison_df <- function(df1,
                               df2,
                               by,                     # ex: c("year", "species_name")
                               value_col = "measurement_value",
                               name1 = "sum.x",
                               name2 = "sum.y") {
  
  a <- df1 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(.val = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(!!name1 := .val)
  
  b <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(.val = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(!!name2 := .val)
  
  c <- dplyr::inner_join(a, b, by = by)
  
  # 3) indicateurs
  c <- c %>%
    dplyr::mutate(
      diff_abs  = abs(.data[[name1]] - .data[[name2]]),
      perc_diff = 100 * diff_abs / pmax(abs(.data[[name1]]), abs(.data[[name2]])),
      diff_category = dplyr::case_when(
        .data[[name1]] == .data[[name2]] ~ "Exact",
        perc_diff < 0.1 ~ "<0.1%",
        perc_diff < 1   ~ "<1%",
        TRUE            ~ ">1%"
      )
    )
  
  c
}