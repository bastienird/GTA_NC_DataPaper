make_comparison_df_continuous <- function(df1,
                                          df2,
                                          by,
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
  
  c <- dplyr::inner_join(a, b, by = by) %>%
    dplyr::mutate(
      diff_abs  = abs(.data[[name1]] - .data[[name2]]),
      perc_diff = 100 * diff_abs / pmax(abs(.data[[name1]]), abs(.data[[name2]]))
    )
  
  c$perc_diff <- pmin(c$perc_diff, 100)
  
  c
}