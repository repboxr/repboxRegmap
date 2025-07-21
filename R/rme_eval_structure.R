# R/rme_eval_structure.R

# This file implements structural checks on the table mappings.

#' A function to group all structural checks
#' @export
rme_steps_structure = function() {
  c("single_col_reg", "multicol_reg_plausibility", "overlapping_regs", "consistent_vertical_structure")
}

#' Check if regressions span multiple columns without justification.
#'
#' A regression should ideally be in a single column. An exception is when
#' standard errors (or t-stats/p-values) are placed in the column to the
#' right of the coefficient. This check flags regressions that span multiple
#' columns without such a horizontal "coef-se" structure.
#'
#' @param rme The rme object.
#' @return A data frame of regressions violating the single-column principle.
rme_ev_single_col_reg = function(rme) {
  restore.point("rme_ev_single_col_reg")

  # The `mc_df` should already contain `se_position` from the join in `rme_init`.
  # A redundant join, as in the previous implementation, created problematic
  # duplicate columns (e.g., se_position.x, se_position.y), causing the `summarise`
  # to fail because the column `se_position` was no longer available.
  # We now use `rme$mc_df` directly.
  df = rme$mc_df %>%
    group_by(map_version, tabid, runid) %>%
    summarise(
      num_cols = n_distinct(col),
      has_right_se = any(se_position == "right", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(num_cols > 1 & !has_right_se) %>%
    rme_df_descr("Regressions spanning multiple columns without horizontal coef-se pairs.", test_type = "map_version_specific")

  return(df)
}

#' Plausibility check for multi-column regressions.
#'
#' If a regression is correctly mapped to multiple columns, it's expected that
#' for at least one row, there are values from that regression in multiple columns.
#' This check flags cases where a regression spans multiple columns, but each row
#' only contains values in a single column, suggesting a "slip" in column
#' assignment across rows.
#'
#' @param rme The rme object.
#' @return A data frame of implausible multi-column regressions.
rme_ev_multicol_reg_plausibility = function(rme) {
  restore.point("rme_ev_multicol_reg_plausibility")

  # Identify regressions mapped to more than one column
  multicol_regs = rme$mc_df %>%
    group_by(map_version, tabid, runid) %>%
    filter(n_distinct(col) > 1) %>%
    ungroup() %>%
    distinct(map_version, tabid, runid)

  if (NROW(multicol_regs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No multi-column regressions to check.", test_type = "map_version_specific"))
  }

  issues = multicol_regs %>%
    inner_join(rme$mc_df, by = c("map_version", "tabid", "runid")) %>%
    filter(has_num) %>%
    group_by(map_version, tabid, runid, row) %>%
    summarise(cols_in_row = n_distinct(col), .groups = "drop_last") %>%
    summarise(max_cols_per_row = max(cols_in_row), .groups = "drop") %>%
    filter(max_cols_per_row == 1) %>%
    rme_df_descr("Multi-column regressions where no row has values in more than one column.", test_type = "map_version_specific")

  return(issues)
}


#' Check for overlapping coefficient cells.
#'
#' This check identifies cells that are heuristically classified as coefficients
#' and are mapped to more than one regression run (`runid`) within the same
#' table. This indicates an overlap, which is usually an error.
#'
#' @param rme The rme object.
#' @return A data frame of cells with overlapping coefficient mappings.
rme_ev_overlapping_regs = function(rme) {
  restore.point("rme_ev_overlapping_regs")

  df = rme$mc_df %>%
    filter(reg_role == "coef") %>%
    group_by(map_version, tabid, cellid) %>%
    summarise(
      n_runids = n_distinct(runid),
      runids = paste0(sort(unique(runid)), collapse=","),
      .groups = "drop"
    ) %>%
    filter(n_runids > 1) %>%
    rme_df_descr("Coefficient cells mapped to multiple regressions.", test_type = "map_version_specific")

  return(df)
}

#' Classify table rows based on stub column keywords.
#' @param cell_df The `cell_df` from an rme object.
#' @return The `cell_df` with an added `row_class` column.
rme_add_row_class = function(cell_df) {
  restore.point("rme_add_row_class")

  if ("row_class" %in% names(cell_df)) return(cell_df)

  stub_df = cell_df %>%
    filter(col == 1 | (col > 1 & lag(col, default=0) == 0) ) %>% # Stub is often col 1
    mutate(text_clean = tolower(stringi::stri_replace_all_regex(text, "[^a-z0-9]", "")))

  stub_df = stub_df %>%
    mutate(
      row_class = dplyr::case_when(
        stringi::stri_detect_regex(text_clean, "observ|nobs|sampsize|numofobs") ~ "nobs",
        stringi::stri_detect_regex(text_clean, "r2|rsquared|rsq") ~ "r2",
        stringi::stri_detect_regex(text_clean, "fstat") ~ "fstat",
        TRUE ~ NA_character_
      )
    ) %>%
    select(tabid, row, row_class) %>%
    filter(!is.na(row_class)) %>%
    distinct(tabid, row, .keep_all = TRUE)

  cell_df = cell_df %>%
    left_join(stub_df, by = c("tabid", "row"))

  return(cell_df)
}

#' Check for consistent vertical structure across regressions.
#'
#' This check verifies that rows for common statistics (like N. of observations,
#' R-squared) appear consistently at the same row index across a table. It first
#' classifies rows based on keywords in the stub column.
#'
#' @param rme The rme object.
#' @return A data frame of inconsistencies found.
rme_ev_consistent_vertical_structure = function(rme) {
  restore.point("rme_ev_consistent_vertical_structure")

  # Add row class if not already present
  if (!"row_class" %in% names(rme$cell_df)) {
    rme$cell_df = rme_add_row_class(rme$cell_df)
  }

  mc_df_ext = rme$mc_df %>%
    left_join(rme$cell_df %>% select(cellid, row_class), by = "cellid") %>%
    filter(!is.na(row_class))

  if (NROW(mc_df_ext) == 0) {
    return(rme_df_descr(tibble::tibble(), "No classified rows (e.g., nobs, r2) found to check for consistency.", test_type = "map_version_specific"))
  }

  issues = mc_df_ext %>%
    group_by(map_version, tabid, row_class) %>%
    summarise(
      n_diff_rows = n_distinct(row),
      rows_used = paste0(sort(unique(row)), collapse = ","),
      .groups = "drop"
    ) %>%
    filter(n_diff_rows > 1) %>%
    rme_df_descr("Inconsistent row indices for the same statistic type (e.g., 'nobs').", test_type = "map_version_specific")

  return(issues)
}
