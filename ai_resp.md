I will now refactor the reporting functionality as requested. The main changes involve:
1.  Renaming `rme_make_tab_report` to `rme_make_report_df`.
2.  Modifying the report data frames (`ttr_df`, `tr_df`) to be specific to each `map_version`.
3.  Introducing a `test_type` classification for each evaluation check (`map_version_specific` or `discrepancy`).
4.  Updating the report generation logic to handle these new classifications, providing detailed, version-specific descriptions for discrepancy tests.

Here are the required modifications:

!MODIFICATION rme_eval_tools.R
scope = "file"
file = "R/rme_eval_tools.R"
is_new_file = false
description = '''Rename 'rme_make_tab_report' to 'rme_make_report_df' and rewrite it to generate version-specific reports. Also update 'rme_df_descr' to support test classification.'''
---
```r
rme_add_eval = function(rme, eval_steps, ...) {
  if (length(eval_steps)>1) {
    restore.point("rme_add_eval_mult")
    for (eval_step in eval_steps) {
      rme = rme_add_eval(rme, eval_step)
    }
    return(rme)
  }

  restore.point("rme_add_eval")
  eval_step = eval_steps

  eval_fun_name = paste0("rme_ev_", eval_step)

  # Check if the evaluation function exists
  if (!exists(eval_fun_name, mode = "function")) {
    warning("Evaluation function '", eval_fun_name, "' not found.", call. = FALSE)
    return(rme)
  }

  # Call the evaluation function
  ev_df = do.call(eval_fun_name, list(rme = rme, ...))

  if (!is.null(ev_df)) {
    all_keys = c("map_version","prod_id","tabid","cellid","runid","script_line","code_line","script_num")
    keys = all_keys %in% names(ev_df)
    names(keys) = all_keys
    rme$eval_keys[[eval_step]] = keys
  }

  # Initialize the evals list in the rme object if it doesn't exist
  if (is.null(rme$evals)) {
    rme$evals = list()
  }

  # Store the resulting data frame
  rme$evals[[eval_step]] = ev_df

  # Provide feedback to the user
  num_issues = if (is.null(ev_df)) 0 else NROW(ev_df)
  if (num_issues > 0) {
    cat(paste0("\nFinished '", eval_step, "' check. Found ", num_issues, " issues."))
  } else {
    cat(paste0("\nFinished '", eval_step, "' check. OK."))
  }

  return(rme)
}

#' Print a summary report of evaluation results to the console.
#'
#' @param rme The `rme` object containing evaluation results.
#' @param eval_steps A character vector of specific evaluations to report.
#'   If `NULL` (the default), all available evaluations are reported.
#' @export
rme_print_ev_report = function(rme, eval_steps = NULL) {
  restore.point("rme_print_ev_report")

  if (is.null(rme$evals) || length(rme$evals) == 0) {
    cat("No evaluations have been run yet.\n")
    return(invisible(NULL))
  }

  if (is.null(eval_steps)) {
    eval_steps = names(rme$evals)
  }

  cat("\n--- Evaluation Report ---")

  for (eval_step in eval_steps) {
    if (!eval_step %in% names(rme$evals)) {
      cat(paste0("\n\n* Evaluation '", eval_step, "': Not found."))
      next
    }

    ev_df = rme$evals[[eval_step]]
    n_issues = if (is.null(ev_df)) 0 else NROW(ev_df)

    cat(paste0("\n\n* ", eval_step, ": ", n_issues, " issues found."))

    if (n_issues > 0) {
      cat("\n")
      if ("map_version" %in% names(ev_df)) {
        # The project uses dplyr extensively, so it's assumed to be available.
        summary_df = ev_df %>%
          dplyr::count(map_version, sort = TRUE) %>%
          dplyr::rename(issues = n)
        print(summary_df, n = 10)
      } else {
        # Fallback for checks not related to map_version
        print(utils::head(ev_df))
      }
    }
  }
  cat("\n--- End of Report ---\n")
  invisible(NULL)
}

#' Combine multiple evaluation result data frames into one.
#'
#' @param rme The `rme` object containing evaluation results.
#' @param eval_steps A character vector of specific evaluations to combine.
#'   If `NULL` (the default), all available evaluations are combined.
#' @return A single tibble containing all issues from the specified evaluations.
#' @export
rme_combine_ev_df = function(rme, eval_steps = NULL) {
  restore.point("rme_combine_ev_df")

  if (is.null(rme$evals) || length(rme$evals) == 0) {
    return(tibble::tibble())
  }

  if (is.null(eval_steps)) {
    eval_steps = names(rme$evals)
  }

  # Filter for existing evaluation results
  eval_steps = intersect(eval_steps, names(rme$evals))
  if (length(eval_steps) == 0) {
    return(tibble::tibble())
  }

  eval_list = rme$evals[eval_steps]

  # The evaluation functions are expected to return a tibble/data.frame.
  # We combine them into one big tibble.
  # dplyr::bind_rows is used as it handles differing columns by filling with NA,
  # and it's used throughout the project.
  combined_df = dplyr::bind_rows(eval_list)

  return(combined_df)
}

rme_df_descr = function(df, descr, test_type = "map_version_specific") {
  attr(df, "descr") = descr
  attr(df, "test_type") = test_type
  df
}

#' Generate data frames with evaluation issue reports.
#'
#' This function processes the evaluation results stored in `rme$evals`,
#' summarizes them by map version and table (`map_version`, `tabid`), and creates two new data frames in
#' the `rme` object:
#'
#' - `rme$ttr_df`: A "Table Test Report" data frame with one row for each
#'   test that failed for a given map version and table. It includes the test name, a
#'   description of the problem, and a comma-separated list of affected cell IDs.
#' - `rme$tr_df`: A "Table Report" data frame with one row per map version and table,
#'   containing a consolidated `problem_report` string that combines all
#'   issues for that combination.
#'
#' This report is intended to be used to generate prompts for AI to improve
#' mappings in subsequent runs.
#'
#' @param rme The `rme` object containing evaluation results.
#' @return The `rme` object, augmented with `ttr_df` and `tr_df`.
#' @export
rme_make_report_df = function(rme) {
  restore.point("rme_make_report_df")

  if (is.null(rme$evals) || length(rme$evals) == 0) {
    # Create empty data frames with correct structure
    all_tabs = if (!is.null(rme$cell_df)) unique(rme$cell_df$tabid) else character(0)
    all_maps = if (!is.null(rme$mc_df)) unique(rme$mc_df$map_version) else character(0)

    rme$ttr_df = tibble::tibble(map_version=character(), tabid=character(), test_name=character(), problem_descr=character(), cells=character())

    if (length(all_tabs) > 0 && length(all_maps) > 0) {
      all_combos = tidyr::crossing(map_version = all_maps, tabid = all_tabs)
      rme$tr_df = all_combos %>% dplyr::mutate(problem_report = "")
    } else {
      rme$tr_df = tibble::tibble(map_version=character(), tabid=character(), problem_report = "")
    }
    return(rme)
  }

  ttr_list = list()

  for (test_name in names(rme$evals)) {
    ev_df = rme$evals[[test_name]]
    if (is.null(ev_df) || NROW(ev_df) == 0) next

    descr = attr(ev_df, "descr")
    if (is.null(descr)) descr = paste0("Issues from ", test_name, ".")
    test_type = attr(ev_df, "test_type")
    if (is.null(test_type)) test_type = "map_version_specific"

    if (!"tabid" %in% names(ev_df) && "cellid" %in% names(ev_df)) {
        ev_df = ev_df %>% dplyr::left_join(rme$cell_df %>% dplyr::select(cellid, tabid) %>% dplyr::distinct(), by="cellid")
    }

    if (!"tabid" %in% names(ev_df)) {
      warning(paste0("Test '", test_name, "' has no 'tabid' column. Skipping in report generation."))
      next
    }

    if (test_type == "map_version_specific") {
      if (!"map_version" %in% names(ev_df)) {
        warning(paste0("Map version specific test '", test_name, "' has no 'map_version' column. Skipping in report generation."))
        next
      }

      tab_summary = ev_df %>%
        dplyr::filter(!is.na(tabid), !is.na(map_version)) %>%
        dplyr::group_by(map_version, tabid) %>%
        dplyr::summarise(
          subject_str = {
            subjects = list()
            data = cur_data()
            if ("cellid" %in% names(data)) {
              vals = stats::na.omit(data$cellid)
              if (length(vals) > 0) subjects$cells = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("runid" %in% names(data)) {
              vals = stats::na.omit(data$runid)
              if (length(vals) > 0) subjects$runids = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("runids" %in% names(data)) {
              vals = stats::na.omit(data$runids)
              if (length(vals) > 0) subjects$runids_list = paste(sort(unique(vals)), collapse = "; ")
            }
            if ("invalid_runid" %in% names(data)) {
              vals = stats::na.omit(data$invalid_runid)
              if (length(vals) > 0) subjects$invalid_runids = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("rows_used" %in% names(data)) {
              vals = stats::na.omit(data$rows_used)
              if (length(vals) > 0) subjects$rows = paste(sort(unique(vals)), collapse = ", ")
            }
            subject_str_parts = purrr::imap_chr(subjects, ~ paste0(.y, ": ", .x))
            if (length(subject_str_parts) > 0) paste0(" (", paste(subject_str_parts, collapse="; "), ")") else ""
          },
          cells = if("cellid" %in% names(cur_data())) {
            vals = stats::na.omit(cur_data()$cellid)
            if(length(vals)>0) paste(sort(unique(vals)), collapse = ",") else NA_character_
          } else NA_character_,
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          test_name = test_name,
          problem_descr = paste0(descr, subject_str)
        ) %>%
        dplyr::select(map_version, tabid, test_name, problem_descr, cells)
      ttr_list[[test_name]] = tab_summary

    } else if (test_type == "discrepancy") {
      # Handle runids_differ
      if (test_name == "runids_differ") {
        if(NROW(ev_df) == 0) next
        discrepancy_report = ev_df %>%
          dplyr::group_by(tabid, cellid) %>%
          dplyr::mutate(
            details = purrr::map_chr(map_version, function(current_mv) {
              my_runids = runids[map_version == current_mv]
              other_maps = cur_data() %>% dplyr::filter(map_version != current_mv) %>%
                dplyr::group_by(runids) %>%
                dplyr::summarise(mvs = paste(stringi::stri_replace_first_regex(map_version,"^[^:]+:",""), collapse=", "), .groups="drop")

              other_strs = paste0("runids '", other_maps$runids, "' (by ", other_maps$mvs, ")")
              paste0("This version maps to runids '", my_runids, "', while others map to: ", paste(other_strs, collapse="; "))
            })
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            test_name = test_name,
            problem_descr = paste0(descr, " for cell ", cellid, ". ", details),
            cells = cellid
          ) %>%
          dplyr::select(map_version, tabid, test_name, problem_descr, cells) %>%
          dplyr::group_by(map_version, tabid, test_name) %>%
          dplyr::summarise(
            problem_descr = paste(problem_descr, collapse = "\n"),
            cells = paste(sort(unique(cells)), collapse=","),
            .groups="drop"
          )
        ttr_list[[test_name]] = discrepancy_report
      } else {
        warning(paste0("Discrepancy test '", test_name, "' not implemented in report generation. Skipping."))
      }
    }
  }

  ttr_df = dplyr::bind_rows(ttr_list)

  if (NROW(ttr_df) > 0) {
    tr_df = ttr_df %>%
      dplyr::mutate(problem_report_part = paste0("--- Test: ", test_name, " ---\n", problem_descr)) %>%
      dplyr::group_by(map_version, tabid) %>%
      dplyr::summarise(problem_report = paste(problem_report_part, collapse = "\n\n"), .groups = "drop")
  } else {
    tr_df = tibble::tibble(map_version=character(), tabid=character(), problem_report=character())
  }

  all_tabs = if (!is.null(rme$cell_df)) unique(rme$cell_df$tabid) else character(0)
  all_maps = if (!is.null(rme$mc_df)) unique(rme$mc_df$map_version) else character(0)

  if (length(all_tabs) > 0 && length(all_maps) > 0) {
    all_combos = tidyr::crossing(map_version = all_maps, tabid = all_tabs)
    tr_df = tr_df %>%
      dplyr::full_join(all_combos, by = c("map_version", "tabid")) %>%
      dplyr::mutate(problem_report = ifelse(is.na(problem_report), "", problem_report))
  }

  rme$ttr_df = ttr_df
  rme$tr_df = tr_df

  return(rme)
}
```
!END_MODIFICATION rme_eval_tools.R
!MODIFICATION rme_eval.R
scope = "file"
file = "R/rme_eval.R"
is_new_file = false
description = '''Update evaluation checks to specify a 'test_type'. Rewrite 'rme_ev_runids_differ' to be a 'discrepancy' test providing detailed output for the new reporting function.'''
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
    dplyr::group_by(map_version, prod_id, tabid, cellid) %>%
    dplyr::summarize(
      runids = paste0(sort(unique(runid)), collapse=","),
      .groups = "drop"
    ) %>%
    dplyr::arrange(tabid, cellid, prod_id, map_version)

  # Find cellids where there is more than one unique runids string
  discrepancy_df = df_runs %>%
    dplyr::group_by(tabid, cellid) %>%
    dplyr::filter(dplyr::n_distinct(runids) > 1) %>%
    dplyr::ungroup()

  # The returned df has columns: map_version, prod_id, tabid, cellid, runids
  # and contains all map versions for cells that have discrepancies.
  # The reporting function will use this to generate per-version reports.
  discrepancy_df %>%
    rme_df_descr("cellids where map versions map to different runids",
                 test_type = "discrepancy")
}

rme_ev_invalid_cellids = function(rme) {
  restore.point("eval_check_invalid_cellids")
  mc_df = rme$mc_df
  cell_df = rme$cell_df

  inv_df = mc_df %>%
    anti_join(cell_df, by="cellid") %>%
    select(tabid, cellid, map_version, prod_id)  %>%
    rme_df_descr("map versions with invalid cellids (cellids not in cell_df)", test_type = "map_version_specific")
  return(inv_df)
}

# Check for mappings to runids that do not exist
rme_ev_invalid_runids = function(rme) {
  restore.point("eval_check_invalid_runids")
  df = rme$mc_df %>%
    filter(!is.na(runid)) %>%
    filter(!runid %in% rme$run_df$runid) %>%
    select(map_version, prod_id, tabid, cellid, invalid_runid=runid) %>%
    rme_df_descr("mapped to non-existent runid", test_type = "map_version_specific")
}

# Check for mappings to commands that are not regressions
rme_ev_non_reg_cmd = function(rme) {
  restore.point("eval_check_non_reg_cmd")
  df = rme$mc_df %>%
    # is_reg is pre-computed and joined into mc_df
    filter(!is.true(is_reg)) %>%
    select(map_version, prod_id, tabid, cellid, runid, cmd, cmd_type, reg_runid) %>%
    rme_df_descr("Mapped to a cmd that is not a regcmd. This is not neccessarily a wrong mapping but useful information, e.g. for post-estimation commands.", test_type = "map_version_specific")
  df
}
```
!END_MODIFICATION rme_eval.R
!MODIFICATION rme_eval_structure.R
scope = "file"
file = "R/rme_eval_structure.R"
is_new_file = false
description = '''Update all structural evaluation checks to specify their 'test_type' as 'map_version_specific'.'''
---
```r
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
```
!END_MODIFICATION rme_eval_structure.R
!MODIFICATION rme_ev_coef_se_match in rme_eval_coef_se.R
scope = "function"
file = "R/rme_eval_coef_se.R"
function_name = "rme_ev_coef_se_match"
description = '''Update 'rme_ev_coef_se_match' to specify its 'test_type' as 'map_version_specific'.'''
---
```r
rme_ev_coef_se_match = function(rme, rel_tol_rounding = 0.005, rel_tol_mismatch = 0.05) {
  restore.point("rme_ev_coef_se_match")

  # 1. Get all coef-se pairs from the tables, including tabid
  pairs_df = rme$cell_df %>%
    filter(reg_role == "se", !is.na(partner_cellid)) %>%
    select(tabid, se_cellid = cellid, coef_cellid = partner_cellid, paren_val = num) %>%
    left_join(rme$cell_df %>% select(coef_cellid=cellid, coef_val=num), by="coef_cellid")

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
    return(rme_df_descr(tibble::tibble(), "No coef/se pairs found to check.", test_type = "map_version_specific"))
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
    return(rme_df_descr(tibble::tibble(), "No regcoef entries for mapped runids.", test_type = "map_version_specific"))
  }

  # 6. Score each potential match
  matches_with_scores = all_potential_matches %>%
    mutate(
      rel_dist_coef = abs(reg_coef - coef_val) / pmax(abs(reg_coef), abs(coef_val), 1e-9),
      rel_dist_se = abs(reg_se - paren_val) / pmax(abs(reg_se), abs(paren_val), 1e-9),
      rel_dist_t = abs(reg_t - paren_val) / pmax(abs(reg_t), abs(paren_val), 1e-9),
      rel_dist_p = abs(reg_p - paren_val) / pmax(abs(reg_p), abs(paren_val), 1e-9)
    ) %>%
    rowwise() %>%
    mutate(min_rel_dist_paren = min(c(rel_dist_se, rel_dist_t, rel_dist_p), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      min_rel_dist_paren = ifelse(is.infinite(min_rel_dist_paren), NA, min_rel_dist_paren),
      coef_match_quality = case_when(
        is.na(rel_dist_coef) ~ 0,
        rel_dist_coef <= rel_tol_rounding ~ 2,
        rel_dist_coef <= rel_tol_mismatch ~ 1,
        TRUE ~ 0
      ),
      paren_match_quality = case_when(
        is.na(min_rel_dist_paren) ~ 0,
        min_rel_dist_paren <= rel_tol_rounding ~ 2,
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
      issue = case_when(
        coef_match_quality == 0 ~ "no_coef_match",
        coef_match_quality == 1 ~ "large_discrepancy",
        paren_match_quality == 0 ~ "no_paren_match",
        paren_match_quality == 1 ~ "large_discrepancy",
        TRUE ~ NA_character_
      ),
      issue_cellid = if_else(coef_match_quality < 2, coef_cellid, se_cellid),
      partner_cellid = if_else(issue_cellid == coef_cellid, se_cellid, coef_cellid),
      table_val = if_else(issue_cellid == coef_cellid, coef_val, paren_val),
      closest_reg_val = if_else(issue_cellid == coef_cellid, reg_coef, best_paren_val),
      details = if_else(issue_cellid == coef_cellid,
        paste0("Coef rel diff ", round(rel_dist_coef,3), ". Paren match quality: ", paren_match_quality),
        paste0("Paren rel diff ", round(min_rel_dist_paren,3)," to ", best_paren_type, " ", round(best_paren_val, 4))
      )
    ) %>%
    select(map_version, tabid, runid, cellid = issue_cellid, partner_cellid, issue, table_val, closest_reg_val, details)

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
        issue = "no_coef_match",
        partner_cellid = se_cellid,
        table_val = coef_val,
        closest_reg_val = reg_coef,
        details = paste0("No match found. Closest coef rel diff ", round(rel_dist_coef,3),
                         ", closest paren rel diff ", round(min_rel_dist_paren,3))
      ) %>%
      select(map_version, tabid, runid, cellid = coef_cellid, partner_cellid, issue, table_val, closest_reg_val, details)
  }

  issues = dplyr::bind_rows(issues_from_best, no_match_issues)
  return(rme_df_descr(issues, "Issues from comparing table coef/se values with regcoef output.", test_type = "map_version_specific"))
}
```
!END_MODIFICATION rme_ev_coef_se_match in rme_eval_coef_se.R
!MODIFICATION example in rme_init.R
scope = "function"
file = "R/rme_init.R"
function_name = "example"
description = '''Update the example function to call the renamed 'rme_make_report_df' function.'''
---
```r
example = function() {
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  rstudioapi::filesPaneNavigate(project_dir)
  rme = rme_init(project_dir)
  rme = rme_eval_all(rme)
  rme = rme_make_report_df(rme)
  rme_save(rme)
  # Explore the created rme object
  ls(rme)
  head(rme$mc_df)
  head(rme$wrong_numbers_df)


  lapply(rme$evals, function(df) {
    unique(df$tabid)
  })
}
```
!END_MODIFICATION example in rme_init.R
