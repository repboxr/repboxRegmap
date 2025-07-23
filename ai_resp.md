I will now apply the requested changes. The main goal is to move the long descriptions for each evaluation test from the `rme_make_report` function to the individual evaluation functions (`rme_ev_*`). This makes the `rme_make_report` function more generic and keeps the test-specific information together with the test's implementation.

### 1. Update `rme_df_descr` to handle long descriptions

First, I'll modify the `rme_df_descr` helper function in `rme_eval_tools.R` to accept and store a `long_descr` argument as a data frame attribute.

!MODIFICATION rme_df_descr in rme_eval_tools.R
scope = "function"
file = "R/rme_eval_tools.R"
function_name = "rme_df_descr"
description = '''Update `rme_df_descr` to accept and store a `long_descr` attribute. A new argument `long_descr` is added to store a detailed explanation of the test, which can be used in reports.'''
---
```r
rme_df_descr = function(df, descr, test_type = c("flag","discrepancy", "note_flag")[1], long_descr = NULL) {
  attr(df, "descr") = descr
  attr(df, "test_type") = test_type
  if (!is.null(long_descr)) {
    attr(df, "long_descr") = long_descr
  }
  df
}
```
!END_MODIFICATION rme_df_descr in rme_eval_tools.R

### 2. Update Integrity and Sanity Checks

Next, I'll update the evaluation functions in `rme_eval.R` to pass the long descriptions to `rme_df_descr`.

!MODIFICATION rme_eval.R
scope = "file"
file = "R/rme_eval.R"
is_new_file = false
description = '''Update all evaluation functions in this file to pass a `long_descr` argument to `rme_df_descr`. The descriptions are moved from `rme_make_report` to be co-located with their respective tests.'''
---
```r
# This file implements Module A: Integrity and Sanity Checks,
# as described in the project's plan.md.

example = function() {
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  #rme = rme_load(project_dir)
  rme = rme_init(project_dir)
  rme = rme_add_eval(rme, rme_steps_integrity())

  rme_print_ev_report(rme)
  df = rme_combine_ev_df(rme)
}

rme_steps_integrity = function() {
  c("runids_differ", "invalid_runids", "invalid_cellids", "non_reg_cmd")
}


#' Get all evaluation step names
#' @export
rme_steps_all = function() {
  c(
    rme_steps_integrity(),
    rme_steps_value(),
    rme_steps_structure()
  )
}

#' Run all evaluation steps
#'
#' A convenience wrapper to run all available evaluation checks.
#' @param rme The rme object.
#' @param ... Arguments passed on to the individual evaluation functions.
#' @return The rme object with all evaluation results.
#' @export
rme_eval_all = function(rme, ...) {
  restore.point("rme_eval_all")
  rme = rme_add_eval(rme, rme_steps_all(), ...)
  rme
}


# Find cellids where different map versions point to different runids
rme_ev_runids_differ = function(rme) {
  mc_df = rme$mc_df

  # we store a string of all corresponding runid for a cell-map_version
  df_runs = mc_df %>%
    dplyr::group_by(map_version, tabid, cellid) %>%
    dplyr::summarize(
      runids = paste0(sort(unique(runid)), collapse=","),
      .groups = "drop"
    ) %>%
    dplyr::arrange(tabid, cellid, map_version)

  # Find cellids where there is more than one unique runids string
  discrepancy_df = df_runs %>%
    dplyr::group_by(tabid, cellid) %>%
    dplyr::filter(dplyr::n_distinct(runids) > 1) %>%
    dplyr::ungroup()

  # The returned df has columns: map_version, tabid, cellid, runids
  # and contains all map versions for cells that have discrepancies.
  # The reporting function will use this to generate per-version reports.
  discrepancy_df %>%
    rme_df_descr("cellids where map versions map to different runids",
                 test_type = "discrepancy",
                 long_descr = "**Discrepancy Across Map Versions.** This test identifies cells that are mapped to *different* regression `runid`s by different AI mapping versions. This is a key indicator of disagreement between models and points to areas of uncertainty.")
}

rme_ev_invalid_cellids = function(rme) {
  restore.point("eval_check_invalid_cellids")
  mc_df = rme$mc_df
  cell_df = rme$cell_df

  inv_df = mc_df %>%
    anti_join(cell_df, by="cellid") %>%
    select(map_version, tabid, reg_ind, cellid)  %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("map versions with invalid cellids (cellids not in cell_df)", test_type = "flag",
                 long_descr = "**Invalid `cellid` Mapping.** This test flags mappings that reference a `cellid` that does not exist in the parsed table data (`cell_df`). This is a critical integrity error, indicating a hallucinated or malformed cell reference from the AI.")
  return(inv_df)
}

# Check for mappings to runids that do not exist
rme_ev_invalid_runids = function(rme) {
  restore.point("eval_check_invalid_runids")
  df = rme$mc_df %>%
    filter(!is.na(runid)) %>%
    filter(!runid %in% rme$run_df$runid) %>%
    select(map_version, tabid,reg_ind, cellid, runid=runid) %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("mapped to non-existent runid", test_type = "flag",
                 long_descr = "**Invalid `runid` Mapping.** This test flags mappings that point to a `runid` that does not exist in the project's execution log (`run_df`). This is a critical integrity error, as the mapped regression output cannot be found.")
}

# Check for mappings to commands that are not regressions
rme_ev_non_reg_cmd = function(rme) {
  restore.point("eval_check_non_reg_cmd")
  df = rme$mc_df %>%
    # is_reg is pre-computed and joined into mc_df
    filter(!is.true(is_reg)) %>%
    select(map_version, tabid, reg_ind, cellid, runid, cmd, cmd_type, reg_runid) %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("Mapped to a cmd that is not a regcmd. This is not neccessarily a wrong mapping but useful information, e.g. for post-estimation commands.",
                 test_type = "note_flag",
                 long_descr = "**Mapping to Non-Regression Command.** This test identifies cells mapped to a Stata command that is not a primary regression command (e.g., `test`, `margins`, `summarize`). This is not necessarily an error—post-estimation results are often included in tables—but serves as an important note. The report shows the command type and the `runid` of the last preceding regression.")
  df
}
```
!END_MODIFICATION rme_eval.R

### 3. Update Value-Based Checks

I will now update the value-based check in `rme_eval_coef_se.R` to include its long description.

!MODIFICATION rme_eval_coef_se.R
scope = "file"
file = "R/rme_eval_coef_se.R"
is_new_file = false
description = '''Update `rme_ev_coef_se_match` to pass a `long_descr` argument to `rme_df_descr`. The long description is now defined here.'''
---
```r
#' A function to group all value-based checks
#' @export
rme_steps_value = function() {
  c("coef_se_match")
}

#' Check for consistency between table coefficients/SEs and regcoef output
#'
#' This evaluation compares numeric values from cells identified as coefficients
#' and their associated standard errors (in parentheses) with the detailed
#' output from the `regcoef` parcel.
#'
#' It first finds the best combined match for each (coefficient, se) pair from the
#' table with a result from the corresponding regression output (`regcoef` parcel).
#' A match is considered "perfect" if the regression output value, when rounded to the
#' number of decimal places shown in the table, is identical to the table value.
#'
#' Based on the best match, it reports several types of issues:
#' - `no_coef_match`: The coefficient from the table does not match any coefficient
#'   in the corresponding `regcoef` output for that `runid` within a given tolerance.
#' - `no_match_perhaps_wrong_sign`: A coefficient match is only found if the sign of the
#'   table value is flipped. This suggests a potential transcription error (e.g., a missing minus sign).
#' - `no_paren_match`: The coefficient matched perfectly, but the value in parentheses
#'   does not match the corresponding standard error, t-statistic, or p-value.
#' - `rounding_error`: A match was found (i.e., not a perfect match after rounding), but the relative difference
#'   is smaller than a mismatch tolerance. This can point to minor transcription errors or unusual rounding.
#'
#' @param rme The rme object.
#' @param rel_tol_rounding The relative tolerance for what is considered a rounding error. (This is now superseded by decimal-based rounding but kept for legacy reasons).
#' @param rel_tol_mismatch The relative tolerance for what is considered a mismatch.
#' @return A data frame of identified issues.
rme_ev_coef_se_match = function(rme, rel_tol_rounding = 0.005, rel_tol_mismatch = 0.05) {
  restore.point("rme_ev_coef_se_match")

  long_descr = "**Value Mismatch between Table and Code.** This is a core value-based check. It compares numeric values from the table (identified as coefficient/standard error pairs) against the results from the mapped regression's `regcoef` output. A match is considered perfect if the code output, rounded to the number of decimal places shown in the table, equals the table value. Issues can be `no_coef_match` (table coef not in output within tolerance), `no_match_perhaps_wrong_sign` (a match is found if the sign is flipped), `no_paren_match` (SE/t-stat/p-val mismatch), or `rounding_error` (the values are very close but don't match exactly after rounding, suggesting a minor discrepancy)."

  # 1. Get all coef-se pairs from the tables, including tabid and decimal places
  cell_info_df = rme$cell_df %>% select(cellid, num, num_deci)

  pairs_df = rme$cell_df %>%
    filter(reg_role == "se", !is.na(partner_cellid)) %>%
    select(tabid, se_cellid = cellid, coef_cellid = partner_cellid) %>%
    # Join for SE info (paren value)
    left_join(cell_info_df, by = c("se_cellid" = "cellid")) %>%
    rename(paren_val = num, paren_deci = num_deci) %>%
    # Join for Coef info
    left_join(cell_info_df, by = c("coef_cellid" = "cellid")) %>%
    rename(coef_val = num, coef_deci = num_deci)

  # 2. Join with mappings to get runids
  mc_coef_df = rme$mc_df %>%
    filter(cellid %in% pairs_df$coef_cellid) %>%
    select(map_version, coef_cellid = cellid, runid) %>%
    distinct()

  # 3. Combine pairs with their mappings
  mapped_pairs = pairs_df %>%
    inner_join(mc_coef_df, by = "coef_cellid", relationship = "many-to-many") %>%
    filter(!is.na(coef_val), !is.na(paren_val))

  if (NROW(mapped_pairs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No coef/se pairs found to check.", test_type = "flag", long_descr = long_descr))
  }

  # 4. Get all relevant regcoef data
  if (is.null(rme$parcels$regcoef$regcoef)) {
     warning("`regcoef` parcel not found in rme object. Cannot run `rme_ev_coef_se_match`.")
     return(NULL)
  }
  regcoef_df = rme$parcels$regcoef$regcoef %>%
    select(runid, cterm, reg_coef = coef, reg_se = se, reg_t = t, reg_p = p) %>%
    filter(!is.na(runid))

  # 5. Create all potential matches between table pairs and regcoef entries
  all_potential_matches = mapped_pairs %>%
    left_join(regcoef_df, by = "runid", relationship = "many-to-many") %>%
    filter(!is.na(reg_coef))

  if (NROW(all_potential_matches) == 0) {
    return(rme_df_descr(tibble::tibble(), "No regcoef entries for mapped runids.", test_type = "flag", long_descr = long_descr))
  }

  # 6. Score each potential match
  matches_with_scores = all_potential_matches %>%
    mutate(
      # Default NA decimal places to 0 (for integers)
      coef_deci = tidyr::replace_na(coef_deci, 0),
      paren_deci = tidyr::replace_na(paren_deci, 0),

      # Check for perfect match based on rounding to table's decimal places
      is_perfect_coef_match = (round(reg_coef, coef_deci) == coef_val),
      is_perfect_se_match = (round(reg_se, paren_deci) == paren_val),
      is_perfect_t_match = (round(reg_t, paren_deci) == paren_val),
      is_perfect_p_match = (round(reg_p, paren_deci) == paren_val),

      # Relative distances are still needed for scoring imperfect matches
      rel_dist_coef = abs(reg_coef - coef_val) / pmax(abs(reg_coef), abs(coef_val), 1e-9),
      rel_dist_coef_sign_flip = abs(reg_coef - (-1 * coef_val)) / pmax(abs(reg_coef), abs(coef_val), 1e-9),
      rel_dist_se = abs(reg_se - paren_val) / pmax(abs(reg_se), abs(paren_val), 1e-9),
      rel_dist_t = abs(reg_t - paren_val) / pmax(abs(reg_t), abs(paren_val), 1e-9),
      rel_dist_p = abs(reg_p - paren_val) / pmax(abs(reg_p), abs(paren_val), 1e-9)
    ) %>%
    rowwise() %>%
    mutate(
      min_rel_dist_paren = min(c(rel_dist_se, rel_dist_t, rel_dist_p), na.rm = TRUE),
      is_perfect_paren_match = any(c(is_perfect_se_match, is_perfect_t_match, is_perfect_p_match), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      min_rel_dist_paren = ifelse(is.infinite(min_rel_dist_paren), NA, min_rel_dist_paren),
      # Score: 2 for perfect match (after rounding), 1 for close match (rounding error), 0 for mismatch
      coef_match_quality = case_when(
        is.na(rel_dist_coef) ~ 0,
        is_perfect_coef_match ~ 2,
        rel_dist_coef <= rel_tol_mismatch ~ 1,
        TRUE ~ 0
      ),
      paren_match_quality = case_when(
        is.na(min_rel_dist_paren) ~ 0,
        is_perfect_paren_match ~ 2,
        min_rel_dist_paren <= rel_tol_mismatch ~ 1,
        TRUE ~ 0
      ),
      match_score = coef_match_quality + paren_match_quality
    )

  # 7. For each table pair, find the best `regcoef` match
  best_matches = matches_with_scores %>%
    group_by(map_version, coef_cellid) %>%
    filter(match_score == max(match_score)) %>%
    mutate(combined_dist = rel_dist_coef + min_rel_dist_paren) %>%
    filter(combined_dist == min(combined_dist, na.rm=TRUE)) %>%
    slice(1) %>%
    ungroup()

  # 8. Generate issues based on the quality of the best match
  issues_from_best = best_matches %>%
    filter(match_score < 4 & match_score > 0) %>% # Imperfect but not total failure
    rowwise() %>%
    mutate(
      best_paren_type = c("se", "t", "p")[which.min(c(rel_dist_se, rel_dist_t, rel_dist_p))],
      best_paren_val = get(paste0("reg_", best_paren_type))
    ) %>%
    ungroup() %>%
    mutate(
      is_sign_flip_match = (coef_match_quality == 0) & (rel_dist_coef_sign_flip <= rel_tol_mismatch),
      issue = case_when(
        is_sign_flip_match ~ "no_match_perhaps_wrong_sign",
        coef_match_quality == 0 ~ "no_coef_match",
        coef_match_quality == 1 ~ "rounding_error",
        paren_match_quality == 0 ~ "no_paren_match",
        paren_match_quality == 1 ~ "rounding_error",
        TRUE ~ NA_character_
      ),
      issue_cellid = if_else(coef_match_quality < 2, coef_cellid, se_cellid),
      partner_cellid = if_else(issue_cellid == coef_cellid, se_cellid, coef_cellid),
      table_val = if_else(issue_cellid == coef_cellid, coef_val, paren_val),
      true_val_cand = if_else(issue_cellid == coef_cellid, reg_coef, best_paren_val),
      details = case_when(
        is_sign_flip_match ~ paste0("Potential sign error. Flipped rel diff ", round(rel_dist_coef_sign_flip,3), ". Paren match quality: ", paren_match_quality),
        issue_cellid == coef_cellid ~ paste0("Coef rel diff ", round(rel_dist_coef,3), ". Paren match quality: ", paren_match_quality),
        TRUE ~ paste0("Paren rel diff ", round(min_rel_dist_paren,3)," to ", best_paren_type, " ", round(best_paren_val, 4))
      )
    ) %>%
    select(map_version, tabid, runid, cellid = issue_cellid, partner_cellid, issue, table_val, true_val_cand, details)

  # 9. Identify pairs that had no match at all (max score was 0)
  all_pairs_to_check = mapped_pairs %>% select(map_version, coef_cellid) %>% distinct()
  pairs_with_any_match = best_matches %>% filter(match_score > 0) %>% select(map_version, coef_cellid) %>% distinct()
  no_match_pairs = all_pairs_to_check %>% anti_join(pairs_with_any_match, by = c("map_version", "coef_cellid"))

  no_match_issues = tibble::tibble()
  if(NROW(no_match_pairs) > 0) {
    closest_misses = no_match_pairs %>%
      inner_join(matches_with_scores, by = c("map_version", "coef_cellid")) %>%
      mutate(combined_dist = rel_dist_coef + min_rel_dist_paren) %>%
      group_by(map_version, coef_cellid) %>%
      filter(combined_dist == min(combined_dist, na.rm=TRUE)) %>%
      slice(1) %>%
      ungroup()

    no_match_issues = closest_misses %>%
      mutate(
        is_sign_flip_match = rel_dist_coef_sign_flip <= rel_tol_mismatch,
        issue = if_else(is_sign_flip_match, "no_match_perhaps_wrong_sign", "no_coef_match"),
        partner_cellid = se_cellid,
        table_val = coef_val,
        true_val_cand = reg_coef,
        details = if_else(is_sign_flip_match,
                         paste0("Potential sign error. Flipped rel diff ", round(rel_dist_coef_sign_flip,3),
                                ". Paren rel diff ", round(min_rel_dist_paren,3)),
                         paste0("No match found. Closest coef rel diff ", round(rel_dist_coef,3),
                                ", closest paren rel diff ", round(min_rel_dist_paren,3)))
      ) %>%
      select(map_version, tabid, runid, cellid = coef_cellid, partner_cellid, issue, table_val, true_val_cand, details)
  }

  issues = dplyr::bind_rows(issues_from_best, no_match_issues) %>%
    left_join(rme$mc_df %>% select(map_version, reg_ind, cellid, runid) %>% unique(), by= c("map_version","cellid","runid"))
  return(rme_df_descr(issues, "Issues from comparing table coef/se values with regcoef output.", test_type = "flag", long_descr = long_descr))
}
```
!END_MODIFICATION rme_eval_coef_se.R

### 4. Update Structural Checks

Similarly, I will update all structural checks in `rme_eval_structure.R`.

!MODIFICATION rme_eval_structure.R
scope = "file"
file = "R/rme_eval_structure.R"
is_new_file = false
description = '''Update all structural evaluation functions in this file to pass their respective `long_descr` to `rme_df_descr`.'''
---
```r
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

  long_descr = "**Regression Spans Multiple Columns.** Regressions are typically presented in a single column. This test flags regressions whose mapped cells span multiple columns without a clear structural reason (like having standard errors in an adjacent column). This often indicates that cells from different regressions have been incorrectly grouped together."

  df = rme$mc_df %>%
    group_by(map_version, tabid, reg_ind) %>%
    summarise(
      cellids = paste(sort(unique(cellid)), collapse=","),
      num_cols = n_distinct(col),
      has_right_se = any(se_position == "right", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(num_cols > 1 & !has_right_se) %>%
    rme_df_descr("Regressions spanning multiple columns without horizontal coef-se pairs.", test_type = "flag", long_descr = long_descr)

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
  
  long_descr = "**Implausible Multi-Column Structure.** For a regression that legitimately spans multiple columns, we expect to find rows with numbers in more than one of those columns. This test flags multi-column regressions where every row *only* has a value in one column, suggesting a 'slip' where different rows of the same conceptual regression were incorrectly assigned to different columns."

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
    return(rme_df_descr(tibble::tibble(), "No multi-column regressions to check.", test_type = "flag", long_descr = long_descr))
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
    return(rme_df_descr(tibble::tibble(), "No implausible multi-column regressions found.", test_type = "flag", long_descr = long_descr))
  }

  # Join the issues with the cellids
  issues = multicol_regs %>%
    inner_join(plausibility_check, by = c("map_version", "tabid", "reg_ind")) %>%
    rme_df_descr("Multi-column regressions where no row has values in more than one column.", test_type = "flag", long_descr = long_descr)

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

  long_descr = "**Overlapping Regression Mappings.** This test flags cells identified as coefficients that have been mapped to *more than one* regression within the *same* map version. This is almost always an error, as a single coefficient should belong to only one regression specification."

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
    rme_df_descr("Coefficient cells mapped to multiple regressions.", test_type = "flag", long_descr = long_descr)

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

  long_descr = "**Inconsistent Summary Stat Rows.** This test checks for consistent table structure. It identifies summary statistics (like 'Observations' or 'R-squared') by keywords and flags cases where the same statistic appears on different row numbers across the columns of a single table. This points to a potentially messy or inconsistent table layout or a mapping error."

  # Add row class if not already present
  if (!"row_class" %in% names(rme$cell_df)) {
    rme$cell_df = rme_add_row_class(rme$cell_df)
  }

  mc_df_ext = rme$mc_df %>%
    left_join(rme$cell_df %>% select(cellid, row_class), by = "cellid") %>%
    filter(!is.na(row_class))

  if (NROW(mc_df_ext) == 0) {
    return(rme_df_descr(tibble::tibble(), "No classified rows (e.g., nobs, r2) found to check for consistency.", test_type = "flag", long_descr = long_descr))
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
    rme_df_descr("Inconsistent row indices for the same statistic type (e.g., 'nobs').", test_type = "flag", long_descr = long_descr)

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

  long_descr = "**Unmapped Standard Error.** This test flags cases where a mapped coefficient cell has an associated standard error (a value in parentheses, typically below the coefficient) that was *not* included in the regression mapping. It also reports whether the numeric value of that unmapped SE would have been a correct match for the regression's output, helping to distinguish simple mapping omissions from more complex issues."

  # 1. Identify all heuristically found coef-se pairs from cell_df
  coef_se_pairs = rme$cell_df %>%
    filter(reg_role == "coef", !is.na(partner_cellid)) %>%
    select(coef_cellid = cellid, se_cellid = partner_cellid, tabid)

  if (NROW(coef_se_pairs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No coefficient-se pairs found in tables.", test_type = "flag", long_descr = long_descr))
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
    return(rme_df_descr(tibble::tibble(), "No missing SE mappings found.", test_type = "flag", long_descr = long_descr))
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
    rme_df_descr("Mapped coefficients with unmapped standard errors.", test_type = "flag", long_descr = long_descr)

  return(final_issues)
}
```
!END_MODIFICATION rme_eval_structure.R

### 5. Finalize by Updating the Report Function

Finally, I'll update the `rme_make_report` function in `rme_report.R` to remove the hardcoded `long_descriptions` list and instead retrieve the descriptions from the attributes of the evaluation data frames.

!MODIFICATION rme_make_report in rme_report.R
scope = "function"
file = "R/rme_report.R"
function_name = "rme_make_report"
description = '''Modify `rme_make_report` to remove the hardcoded `long_descriptions` list. The function now retrieves descriptions from the attributes of the evaluation data frames, making it more generic and centralizing the description text with the test logic.'''
---
```r
#' Create a markdown report of evaluation results
#'
#' This function generates a markdown-formatted report summarizing the issues
#' found by the various evaluation steps. The report is structured hierarchically:
#' first by map version, then by table ID, and finally by test.
#'
#' @param rme The `rme` object containing evaluation results in `rme$evals`.
#' @param map_version An optional character vector of map versions to report on.
#'   If `NULL`, results from all versions with issues are included.
#' @param tabid A character vector of table IDs to include in the report.
#'   If `NULL`, all tables with issues are included.
#' @param test_names A character vector of specific evaluation step names to include.
#'   If `NULL`, all completed evaluations are reported.
#' @param ignore_tests A character vector of evaluation step names to exclude.
#' @param long_descr A logical value. If `TRUE`, a detailed explanation for each
#'   test is included in the report to help interpret the results. If `FALSE`,
#'   a concise description is used.
#' @param outfile An optional file path. If provided, the report is written to
#'   this file.
#' @return A string containing the markdown report.
#' @export
rme_make_report = function(rme, map_version = NULL, tabid = NULL, test_names = NULL, ignore_tests = NULL, long_descr = TRUE, outfile = NULL) {
  restore.point("rme_make_report")

  # --- Helper Functions ---

  # Converts a data frame to a markdown table string
  df_to_markdown = function(df) {
    if (!is.data.frame(df) || NROW(df) == 0) return("")
    # Clean strings for markdown
    clean_string = function(s) {
      s = as.character(s)
      s[is.na(s)] = ""
      s = stringi::stri_replace_all_fixed(s, "|", "&#124;")
      s = stringi::stri_replace_all_regex(s, "[\r\n]+", " ")
      s
    }
    df_char = as.data.frame(lapply(df, clean_string), stringsAsFactors = FALSE)
    header = paste0("| ", paste(names(df_char), collapse = " | "), " |")
    separator = paste0("|", paste(rep("---", ncol(df_char)), collapse = "|"), "|")
    body_rows = apply(df_char, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
    paste(c(header, separator, body_rows), collapse = "\n")
  }

  # Chooses a display format (table or list) for a given issue data frame
  format_issues_md = function(df, test_name) {
    # For certain tests, a list is more readable than a wide table
    use_list_format = test_name %in% c("multicol_reg_plausibility", "invalid_cellids", "single_col_reg")

    if (use_list_format) {
      # Custom list format for specific tests
      if (test_name == "multicol_reg_plausibility" && all(c("reg_ind", "cellids", "cols") %in% names(df))) {
        items = purrr::pmap_chr(df, function(reg_ind, cellids, cols, ...) {
          paste0("* **Reg. ", reg_ind, "**: Implausible structure for columns `", cols, "`. (Cells: `", cellids, "`)")
        })
        return(paste(items, collapse = "\n"))
      }
      if (test_name %in% c("invalid_cellids", "single_col_reg") && all(c("reg_ind", "cellids") %in% names(df))) {
        items = purrr::map2_chr(df$reg_ind, df$cellids, ~paste0("* **Reg. ", .x, "**: Affects cells `", .y, "`"))
        return(paste(items, collapse = "\n"))
      }
    }
    # Default to a table for all other cases
    return(df_to_markdown(df))
  }

  # --- Main Logic ---

  # 1. Determine tests and get all evaluation data
  all_ev_dfs = rme$evals
  available_tests = names(all_ev_dfs)
  tests_to_report = if (is.null(test_names)) available_tests else intersect(available_tests, test_names)
  if (!is.null(ignore_tests)) tests_to_report = setdiff(tests_to_report, ignore_tests)

  if (length(tests_to_report) == 0 || length(all_ev_dfs) == 0) {
    msg = "No tests to report on (either none run, or all filtered out)."
    if (!is.null(outfile)) writeLines(msg, outfile)
    return(msg)
  }

  # Filter all DFs based on top-level filters (if any) and combine to find relevant scopes
  filtered_ev_list = lapply(all_ev_dfs[tests_to_report], function(df) {
    if (is.null(df) || NROW(df) == 0) return(NULL)
    # Ungroup to prevent dplyr warnings during filtering
    df = dplyr::ungroup(df)
    if (!is.null(map_version) && "map_version" %in% names(df)) df = dplyr::filter(df, .data$map_version %in% .env$map_version)
    if (!is.null(tabid) && "tabid" %in% names(df)) df = dplyr::filter(df, .data$tabid %in% .env$tabid)
    if (NROW(df) > 0) df else NULL
  })
  filtered_ev_list = purrr::compact(filtered_ev_list)
  if(length(filtered_ev_list) == 0) {
     msg = "No issues found for the specified filters."
     if (!is.null(outfile)) writeLines(msg, outfile)
     return(msg)
  }

  combined_issues = dplyr::bind_rows(filtered_ev_list, .id = "test")

  # 2. Build Report Header
  report_parts = list(
    "# Regression Mapping Evaluation Report",
    if (!is.null(rme$project_dir)) paste0("**Project**: `", rme$project_dir, "`"),
    "\n---"
  )

  # 3. Iterate and Build Report Body
  issues_by_version = split(combined_issues, combined_issues$map_version)

  for (mv in sort(names(issues_by_version))) {
    version_issues = issues_by_version[[mv]]
    version_parts = list(paste0("\n## Map Version: `", mv, "`"))

    issues_by_table = split(version_issues, version_issues$tabid)

    for (tid in sort(names(issues_by_table))) {
      table_issues = issues_by_table[[tid]]
      table_parts = list(paste0("\n### Table `", tid, "`"))

      tests_in_table = intersect(tests_to_report, unique(table_issues$test))

      for (test in tests_in_table) {
        current_issues = dplyr::filter(table_issues, .data$test == .env$test)

        table_parts = c(table_parts, paste0("\n#### Test: `", test, "`"))

        test_df = filtered_ev_list[[test]]
        descr = if (long_descr) attr(test_df, "long_descr") else NULL
        if (is.null(descr)) {
          descr = attr(test_df, "descr")
        }
        
        if (!is.null(descr)) {
            table_parts = c(table_parts, paste0("> ", descr))
        }

        table_parts = c(table_parts,
            paste0("\n**Issues Found**: ", NROW(current_issues)),
            format_issues_md(dplyr::select(current_issues, -any_of(c("map_version", "tabid", "test"))), test)
        )
      }
      table_parts = c(table_parts, "\n---")
      version_parts = c(version_parts, table_parts)
    }
    report_parts = c(report_parts, version_parts)
  }

  # 4. Finalize and Output
  final_report = paste(purrr::compact(unlist(report_parts)), collapse = "\n\n")
  final_report = stringi::stri_replace_all_regex(final_report, "(\n){3,}", "\n\n") # Clean up excess newlines

  if (!is.null(outfile)) {
    writeLines(final_report, outfile)
    cat(paste0("\nReport written to '", outfile, "'."))
  }

  invisible(final_report)
}
```
!END_MODIFICATION rme_make_report in rme_report.R
