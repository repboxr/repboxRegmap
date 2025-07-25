# R/rme_eval_other.R

# This file implements additional checks that are more specific
# or cross-cutting than the standard integrity, value, and structure checks.

#' A function to group all "other" checks
#' @export
rme_steps_other = function() {
  c("consistent_reg_ind_in_col", "post_est_reg_match", "value_match_other_cmd", "mapped_to_line_not_run")
}

#' Check for consistent reg_ind in single-regression columns.
#'
#' In a column that maps to a single main regression, all mapped cells
#' (including post-estimation stats) should share the same `reg_ind`. This check
#' flags columns where cells are assigned to multiple different `reg_ind`s,
#' suggesting a mapping inconsistency.
#'
#' @param rme The rme object.
#' @return A data frame of identified issues.
rme_ev_consistent_reg_ind_in_col = function(rme) {
  restore.point("rme_ev_consistent_reg_ind_in_col")

  long_descr = "**Inconsistent `reg_ind` in Column.** In a column that maps to a single main regression, all mapped cells (including post-estimation stats) should share the same `reg_ind`. This check flags columns where cells are assigned to multiple different `reg_ind`s, suggesting a mapping inconsistency."

  mc_df = rme$mc_df

  # 1. Identify single-regression columns and their "correct" reg_ind
  single_reg_cols = mc_df %>%
    filter(is_reg == TRUE) %>%
    group_by(map_version, tabid, col) %>%
    summarise(
      main_reg_inds = list(sort(unique(reg_ind))),
      .groups = "drop"
    ) %>%
    filter(lengths(main_reg_inds) == 1) %>%
    mutate(correct_reg_ind = unlist(main_reg_inds)) %>%
    select(map_version, tabid, col, correct_reg_ind)

  if (NROW(single_reg_cols) == 0) {
    return(rme_df_descr(tibble::tibble(), "No single-regression columns found to check.", test_type = "flag", long_descr = long_descr))
  }

  # 2. Find all cells in these columns that have an inconsistent reg_ind
  issues = mc_df %>%
    inner_join(single_reg_cols, by = c("map_version", "tabid", "col")) %>%
    filter(reg_ind != correct_reg_ind) %>%
    group_by(map_version, tabid, col, correct_reg_ind, reg_ind) %>%
    summarise(
        cellids = paste(sort(unique(cellid)), collapse=","),
        .groups = "drop"
    ) %>%
    mutate(
        details = paste0("Has reg_ind ", reg_ind, " but should be ", correct_reg_ind)
    ) %>%
    select(map_version, tabid, col, reg_ind, cellids, details) %>%
    rme_df_descr("Inconsistent `reg_ind` in single-regression columns.", test_type = "flag", long_descr = long_descr)

  return(issues)
}

#' Check for consistent reg_runid for post-estimation commands.
#'
#' In a column corresponding to a single regression, any post-estimation
#' commands should be associated with that main regression. This check flags
#' cases where a post-estimation command's associated `reg_runid` does not
#' match the main regression `runid` for that column.
#'
#' @param rme The rme object.
#' @return A data frame of identified issues.
rme_ev_post_est_reg_match = function(rme) {
  restore.point("rme_ev_post_est_reg_match")

  long_descr = "**Mismatched Post-Estimation Command.** In a column corresponding to a single regression, any post-estimation commands (e.g., `test`, `summarize`) should be associated with that main regression. This check flags cases where a post-estimation command's associated `reg_runid` does not match the main regression's `runid` for that column."

  mc_df = rme$mc_df

  # 1. Identify single-regression columns and their main runid
  single_reg_cols = mc_df %>%
    filter(is_reg == TRUE) %>%
    group_by(map_version, tabid, col) %>%
    summarise(main_reg_runids = list(sort(unique(runid))), .groups = "drop") %>%
    filter(lengths(main_reg_runids) == 1) %>%
    mutate(expected_reg_runid = unlist(main_reg_runids)) %>%
    select(map_version, tabid, col, expected_reg_runid)

  if (NROW(single_reg_cols) == 0) {
    return(rme_df_descr(tibble::tibble(), "No single-regression columns found to check.", test_type = "flag", long_descr = long_descr))
  }

  # 2. Find non-regression commands in these columns and check their associated reg_runid
  issues = mc_df %>%
    filter(is_reg == FALSE, !is.na(reg_runid)) %>%
    inner_join(single_reg_cols, by = c("map_version", "tabid", "col")) %>%
    filter(reg_runid != expected_reg_runid) %>%
    select(
      map_version, tabid, col, cellid, runid, cmd,
      found_reg_runid = reg_runid,
      expected_reg_runid
    ) %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("Post-estimation command associated with the wrong regression.", test_type = "flag", long_descr = long_descr)

  return(issues)
}

#' Helper to extract all numbers from a string
extract_numbers = function(text) {
  text_no_comma = stringi::stri_replace_all_fixed(text, ",", "")
  num_pattern = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
  matches = stringi::stri_extract_all_regex(text_no_comma, num_pattern, omit_empty = TRUE)
  lapply(matches, function(m) as.numeric(m))
}

#' Check if values from non-regression commands match Stata output.
#'
#' Verifies if numeric values in cells mapped to non-regression commands
#' (e.g., `summarize`, `display`) can be found in the raw output of that command.
#' It rounds output values to the same number of decimal places as the table value.
#'
#' @param rme The rme object.
#' @return A data frame of identified issues.
rme_ev_value_match_other_cmd = function(rme) {
  restore.point("rme_ev_value_match_other_cmd")

  long_descr = "**Value Mismatch for Other Commands.** This check verifies if numeric values in cells mapped to non-regression commands (e.g., `summarize`, `display`) can be found in the raw output of that command. It checks for a match after rounding the output values to the same number of decimal places as the table value."

  if (is.null(rme$parcels$stata_run_log$stata_run_log)) {
    warning("`stata_run_log` parcel not found. Cannot run `rme_ev_value_match_other_cmd`.")
    return(rme_df_descr(tibble::tibble(), "`stata_run_log` parcel not found.", test_type = "flag", long_descr = long_descr))
  }
  output_df = rme$parcels$stata_run_log$stata_run_log %>% select(runid, output = logtxt)

  cells_to_check = rme$mc_df %>%
    filter(!cmd_type %in% c("reg","altreg")) %>%
    select(map_version, tabid, cellid, runid, cmd, table_val = num, table_deci = num_deci) %>%
    distinct()

  if (NROW(cells_to_check) == 0) {
    return(rme_df_descr(tibble::tibble(), "No numeric cells mapped to non-regression commands.", test_type = "flag", long_descr = long_descr))
  }

  cells_with_output = cells_to_check %>%
    left_join(output_df, by = "runid") %>%
    filter(!is.na(output) & output != "")

  results = cells_with_output %>%
    mutate(
      table_deci = tidyr::replace_na(table_deci, 0),
      output_numbers = extract_numbers(output)
    ) %>%
    rowwise() %>%
    mutate(
      is_match = any(round(output_numbers[[1]], table_deci) == table_val, na.rm = TRUE)
    ) %>%
    ungroup()

  mismatch_issues = results %>%
    filter(!is_match) %>%
    rowwise() %>%
    mutate(
      diffs = abs(output_numbers[[1]] - table_val),
      closest_val = if (length(diffs) > 0 && any(!is.na(diffs))) output_numbers[[1]][which.min(diffs)] else NA_real_,
      details = if (!is.na(closest_val)) {
        paste0("No match. Closest value in output: ", closest_val)
      } else {
        "No numeric values found in output."
      }
    ) %>%
    ungroup() %>%
    select(map_version, tabid, cellid, runid, cmd, table_val, details) %>%
    mutate(issue = "no_value_match")

  no_output_issues = cells_to_check %>%
    anti_join(cells_with_output, by = c("map_version", "tabid", "cellid", "runid")) %>%
     mutate(details = paste0("No Stata output found for runid ", runid), issue="no_output_found") %>%
     select(map_version, tabid, cellid, runid, cmd, table_val, details, issue)

  issues = dplyr::bind_rows(mismatch_issues, no_output_issues) %>%
     rme_df_descr("Numeric value in table not found in command output.", test_type = "flag", long_descr = long_descr)

  return(issues)
}

#' Check for cells mapped to a code line but not a runid.
#'
#' This check flags cells that are mapped to a specific line in a script but
#' are not associated with a `runid` (i.e., a specific execution output).
#'
#' @param rme The rme object.
#' @return A data frame of identified issues.
rme_ev_mapped_to_line_not_run = function(rme) {
  restore.point("rme_ev_mapped_to_line_not_run")

  long_descr = "**Mapped to Code Line but not a `runid`.** This flags cells that are mapped to a specific line in a script but are not associated with a `runid` (i.e., a specific execution output). While sometimes intentional for un-executed code, numeric cells in a results table should be mapped to a `runid`. If a numeric cell cannot be mapped to a specific `runid`, both `runid` and `code_line` should ideally be to indicate it belongs to the conceptual regression without a direct code link."

  issues = rme$map_reg_run %>%
    filter(is.na(runid) & !is.na(code_line)) %>%
    select(map_version = ver_id, tabid, reg_ind, code_line, script_num, cell_ids) %>%
    filter(!is.na(cell_ids) & cell_ids != "") %>%
    unnest_comma_string_col("cell_ids") %>%
    rename(cellid = cell_ids) %>%
    group_by(map_version, tabid, reg_ind, code_line, script_num) %>%
    summarise(cellids = paste(sort(unique(cellid)), collapse=","), .groups="drop") %>%
    rename(mapped_code_line = code_line, mapped_script_num=script_num) %>%
    rme_df_descr("Cells mapped to a code line but not a `runid`.", test_type = "flag", long_descr = long_descr)

  return(issues)
}
