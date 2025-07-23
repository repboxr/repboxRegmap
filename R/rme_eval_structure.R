# R/rme_eval_structure.R

# This file implements structural checks on the table mappings.

#' A function to group all structural checks
#' @export
rme_steps_structure = function() {
  c("single_col_reg", "multicol_reg_plausibility", "overlapping_regs", "consistent_vertical_structure", "missing_se_mapping")
}

#' Check if regressions span multiple columns without justification.
#'
#' A regression should ideally be in a single column. An exception is when
#' standard errors (or t-stats/p-values) are placed in the column to the
#' right of the coefficient. This check flags regressions that span multiple
#' columns without such a horizontal "coef-se" structure.
#'
#' @param rme The rme object.
#' @return A data frame of regressions violating the single-column principle,
#'   including a `cellids` column with all cells of the regression.
rme_ev_single_col_reg = function(rme) {
  restore.point("rme_ev_single_col_reg")

  df = rme$mc_df %>%
    group_by(map_version, tabid, reg_ind) %>%
    summarise(
      cellids = paste(sort(unique(cellid)), collapse=","),
      num_cols = n_distinct(col),
      has_right_se = any(se_position == "right", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(num_cols > 1 & !has_right_se) %>%
    rme_df_descr("Regressions spanning multiple columns without horizontal coef-se pairs.", test_type = "flag")

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
#' @return A data frame of implausible multi-column regressions, including a
#'   `cellids` column.
rme_ev_multicol_reg_plausibility = function(rme) {
  restore.point("rme_ev_multicol_reg_plausibility")

  # Identify regressions mapped to more than one column and get their cellids
  multicol_regs = rme$mc_df %>%
    group_by(map_version, tabid, reg_ind) %>%
    filter(has_num, n_distinct(col) > 1) %>%
    summarise(
      cellids = paste(sort(unique(cellid)), collapse=","),
      cols = paste(sort(unique(col)), collapse=","),
      rows = paste(sort(unique(row)), collapse=","),
      .groups="drop")

  if (NROW(multicol_regs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No multi-column regressions to check.", test_type = "flag"))
  }

  # Find regressions where no row has numbers in more than one column
  plausibility_check = rme$mc_df %>%
    semi_join(multicol_regs, by = c("map_version", "tabid", "reg_ind")) %>%
    filter(has_num) %>%
    group_by(map_version, reg_ind, tabid, row) %>%
    summarise(cols_in_row = n_distinct(col), .groups = "drop_last") %>%
    summarise(max_cols_per_row = max(cols_in_row), .groups = "drop") %>%
    filter(max_cols_per_row == 1)

  if (NROW(plausibility_check) == 0) {
    return(rme_df_descr(tibble::tibble(), "No implausible multi-column regressions found.", test_type = "flag"))
  }

  # Join the issues with the cellids
  issues = multicol_regs %>%
    inner_join(plausibility_check, by = c("map_version", "tabid", "reg_ind")) %>%
    rme_df_descr("Multi-column regressions where no row has values in more than one column.", test_type = "flag")

  return(issues)
}


#' Check for overlapping coefficient cells.
#'
#' This check identifies cells that are heuristically classified as coefficients
#' and are mapped to more than one regression `reg_ind` or multiple `runid`
#' within the same table. This indicates an overlap, which is usually an error.
#'
#' @param rme The rme object.
#' @return A data frame of cells with overlapping coefficient mappings.
rme_ev_overlapping_regs = function(rme) {
  restore.point("rme_ev_overlapping_regs")

  df = rme$mc_df %>%
    filter(reg_role == "coef") %>%
    group_by(map_version, tabid, cellid) %>%
    summarise(
      n_reginds = n_distinct(reg_ind),
      reg_inds = paste0(sort(unique(reg_ind)), collapse=","),
      n_runids = n_distinct(runid),
      runids = paste0(sort(unique(runid)), collapse=","),
      .groups = "drop"
    ) %>%
    filter(n_runids > 1 | n_reginds > 1) %>%
    rme_df_descr("Coefficient cells mapped to multiple regressions.", test_type = "flag")

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
#' @return A data frame of inconsistencies found, including a `cellids` column.
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
    return(rme_df_descr(tibble::tibble(), "No classified rows (e.g., nobs, r2) found to check for consistency.", test_type = "flag"))
  }

  issues = mc_df_ext %>%
    group_by(map_version, tabid, row_class) %>%
    summarise(
      n_diff_rows = n_distinct(row),
      rows_used = paste0(sort(unique(row)), collapse = ","),
      cellids = paste0(sort(unique(cellid)), collapse = ","),
      .groups = "drop"
    ) %>%
    filter(n_diff_rows > 1) %>%
    rme_df_descr("Inconsistent row indices for the same statistic type (e.g., 'nobs').", test_type = "flag")

  return(issues)
}

#' Check for unmapped standard errors.
#'
#' This check identifies mapped coefficients where their corresponding standard
#' error cell (heuristically identified as the cell in parentheses below or
#' to the right) has not been included in the same regression mapping.
#' It also verifies if the numeric value in the unmapped SE cell would have
#' matched the standard error, t-statistic, or p-value from the regression output.
#'
#' @param rme The rme object.
#' @return A data frame of missing SE mappings, with a `would_match` column
#'   indicating if the SE value is consistent with the regression output.
rme_ev_missing_se_mapping = function(rme) {
  restore.point("rme_ev_missing_se_mapping")

  # 1. Identify all heuristically found coef-se pairs from cell_df
  coef_se_pairs = rme$cell_df %>%
    filter(reg_role == "coef", !is.na(partner_cellid)) %>%
    select(coef_cellid = cellid, se_cellid = partner_cellid, tabid)

  if (NROW(coef_se_pairs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No coefficient-se pairs found in tables.", test_type = "flag"))
  }

  # 2. Identify all uniquely mapped cells (map_version, runid, cellid)
  mapped_cells = rme$mc_df %>%
    select(map_version, runid, cellid) %>%
    distinct()

  # 3. Join pairs with the mapped coefficients to find the runid for each coef
  mapped_coefs = coef_se_pairs %>%
    inner_join(mapped_cells, by = c("coef_cellid" = "cellid"), relationship = "many-to-many")

  # 4. Create a lookup key for all mapped cells to efficiently check for existence
  mapped_cells_lookup = mapped_cells %>%
    mutate(key = paste(map_version, runid, cellid, sep = "--")) %>%
    pull(key)

  # 5. Filter for cases where the SE cell is NOT mapped to the same runid
  missing_se_df = mapped_coefs %>%
    mutate(se_key = paste(map_version, runid, se_cellid, sep = "--")) %>%
    filter(!se_key %in% mapped_cells_lookup)

  if (NROW(missing_se_df) == 0) {
    return(rme_df_descr(tibble::tibble(), "No missing SE mappings found.", test_type = "flag"))
  }

  # 6. For the missing SEs, check if their value would have matched the regression output
  if (is.null(rme$parcels$regcoef$regcoef)) {
     warning("`regcoef` parcel not found. Cannot check if missing SEs would match.")
     # Return issues without the match check
     issues = missing_se_df %>%
       select(map_version, tabid, runid, coef_cellid, se_cellid) %>%
       mutate(would_match = NA)
  } else {
    # Get numeric values and decimal places for the unmapped SE cells
    se_cell_data = rme$cell_df %>%
        filter(cellid %in% missing_se_df$se_cellid) %>%
        select(se_cellid = cellid, paren_val = num, paren_deci = num_deci)

    # Get relevant regression output
    regcoef_df = rme$parcels$regcoef$regcoef %>%
        select(runid, cterm, reg_se = se, reg_t = t, reg_p = p)

    # Join missing SEs with their values and potential regression matches
    check_df = missing_se_df %>%
        left_join(se_cell_data, by = "se_cellid") %>%
        filter(!is.na(paren_val)) %>%
        left_join(regcoef_df, by = "runid", relationship = "many-to-many")

    # Perform the check
    match_results = check_df %>%
        filter(!is.na(reg_se) | !is.na(reg_t) | !is.na(reg_p)) %>%
        mutate(
            paren_deci = tidyr::replace_na(paren_deci, 0),
            is_perfect_se_match = (round(reg_se, paren_deci) == paren_val),
            is_perfect_t_match = (round(reg_t, paren_deci) == paren_val),
            is_perfect_p_match = (round(reg_p, paren_deci) == paren_val)
        ) %>%
        rowwise() %>%
        mutate(
            is_match = any(c(is_perfect_se_match, is_perfect_t_match, is_perfect_p_match), na.rm = TRUE)
        ) %>%
        ungroup()

    # Summarize results for each missing SE
    issue_summary = match_results %>%
        group_by(map_version, tabid, runid, coef_cellid, se_cellid) %>%
        summarise(would_match = any(is_match, na.rm = TRUE), .groups = "drop")

    # Join back with the full list of missing SEs in case some had no value to check
    issues = missing_se_df %>%
        select(map_version, tabid, runid, coef_cellid, se_cellid) %>%
        left_join(issue_summary, by = c("map_version", "tabid", "runid", "coef_cellid", "se_cellid")) %>%
        mutate(would_match = tidyr::replace_na(would_match, FALSE))
  }

  # 7. Add reg_ind for better reporting and finalize
  final_issues = issues %>%
    left_join(
      rme$mc_df %>% select(map_version, runid, coef_cellid = cellid, reg_ind) %>% distinct(),
      by = c("map_version", "runid", "coef_cellid")
    ) %>%
    select(map_version, tabid, reg_ind, runid, coef_cellid, se_cellid, would_match) %>%
    rme_df_descr("Mapped coefficients with unmapped standard errors.", test_type = "flag")

  return(final_issues)
}
