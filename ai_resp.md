I will refactor your R package `repboxRegmap` to improve the structure and content of the evaluation reports.

Here are the planned changes:
1.  I will create a new file `R/rme_report.R` and move the reporting function `rme_make_report_df` into it.
2.  I will update `rme_make_report_df` to produce more structured output. The resulting `ttr_df` will have distinct `cellids` and `runids` columns, making the report data easier to process. The text-based `problem_report` in `tr_df` will be generated from these structured columns.
3.  I will remove `rme_make_report_df` from `R/rme_eval_tools.R` to reflect its move to the new file.
4.  I will update several evaluation functions in `R/rme_eval_structure.R` (`rme_ev_single_col_reg`, `rme_ev_multicol_reg_plausibility`, `rme_ev_consistent_vertical_structure`) to ensure they consistently report the `cellids` associated with any identified issues. This is crucial for the new reporting function to work correctly.

These changes will make the evaluation outputs more systematic and useful for downstream tasks, such as generating prompts for AI to fix mapping errors.

!MODIFICATION rme_report.R new file
scope = "file"
file = "R/rme_report.R"
is_new_file = true
description = '''Create a new file `R/rme_report.R` and move a refactored `rme_make_report_df` into it. The function now generates more structured `ttr_df` and `tr_df` reports with dedicated `cellids` and `runids` columns.'''
---
```r
#' Generate data frames with evaluation issue reports.
#'
#' This function processes the evaluation results stored in `rme$evals`,
#' summarizes them by map version and table (`map_version`, `tabid`), and creates two new data frames in
#' the `rme` object:
#'
#' - `rme$ttr_df`: A "Table Test Report" data frame with one row for each
#'   test that failed for a given map version and table. It includes the test name, a
#'   description of the problem, a comma-separated list of affected cell IDs, and a
#'   comma-separated list of relevant run IDs (if applicable).
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

    rme$ttr_df = tibble::tibble(map_version=character(), tabid=character(), test_name=character(), problem_descr=character(), cellids=character(), runids=character())

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
          cellids = {
            data = cur_data()
            all_cellids = c()
            if ("cellid" %in% names(data)) {
              all_cellids = c(all_cellids, stats::na.omit(data$cellid))
            }
            if ("cellids" %in% names(data)) {
              unlisted = unlist(stringi::stri_split_fixed(stats::na.omit(data$cellids), ","))
              all_cellids = c(all_cellids, unlisted[unlisted != ""])
            }
            if(length(all_cellids) > 0) paste(sort(unique(all_cellids)), collapse = ",") else NA_character_
          },
          runids = {
            data = cur_data()
            all_runids = c()
            if ("runid" %in% names(data)) all_runids = c(all_runids, stats::na.omit(data$runid))
            if ("runids" %in% names(data)) all_runids = c(all_runids, unlist(stringi::stri_split_fixed(stats::na.omit(data$runids), ",")))
            if ("invalid_runid" %in% names(data)) all_runids = c(all_runids, stats::na.omit(data$invalid_runid))

            all_runids = all_runids[all_runids != ""]
            if (length(all_runids) > 0) paste(sort(unique(as.character(all_runids))), collapse = ",") else NA_character_
          },
          n_issues = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          test_name = test_name,
          problem_descr = paste0(descr, " (", n_issues, " instances found).")
        ) %>%
        dplyr::select(map_version, tabid, test_name, problem_descr, cellids, runids)

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
            problem_descr = paste0(descr, " for cell ", cellid, ". ", details)
          ) %>%
          dplyr::group_by(map_version, tabid, test_name) %>%
          dplyr::summarise(
            problem_descr = paste(problem_descr, collapse = "\n"),
            cellids = paste(sort(unique(cellid)), collapse=","),
            runids = NA_character_, # Not applicable for aggregated discrepancy report
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
      dplyr::mutate(problem_report_part = paste0(
        "--- Test: ", test_name, " ---\n",
        problem_descr,
        dplyr::if_else(test_name != "runids_differ" & !is.na(cellids), paste0("\n-> Affected cellids: ", cellids), ""),
        dplyr::if_else(test_name != "runids_differ" & !is.na(runids), paste0("\n-> Affected runids: ", runids), "")
      )) %>%
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
!END_MODIFICATION rme_report.R new file
!MODIFICATION rme_eval_tools.R file
scope = "file"
file = "R/rme_eval_tools.R"
is_new_file = false
description = '''Remove the `rme_make_report_df` function, as it has been moved to its own file, `R/rme_report.R`.'''
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
```
!END_MODIFICATION rme_eval_tools.R file
!MODIFICATION rme_eval_structure.R file
scope = "file"
file = "R/rme_eval_structure.R"
is_new_file = false
description = '''Rewrite `rme_eval_structure.R` to ensure all evaluation functions return a `cellids` column for identified issues. This makes the evaluation output consistent and easier to use for reporting.'''
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
#' @return A data frame of regressions violating the single-column principle,
#'   including a `cellids` column with all cells of the regression.
rme_ev_single_col_reg = function(rme) {
  restore.point("rme_ev_single_col_reg")

  df = rme$mc_df %>%
    group_by(map_version, tabid, runid) %>%
    summarise(
      num_cols = n_distinct(col),
      has_right_se = any(se_position == "right", na.rm = TRUE),
      cellids = paste(sort(unique(cellid)), collapse=","),
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
#' @return A data frame of implausible multi-column regressions, including a
#'   `cellids` column.
rme_ev_multicol_reg_plausibility = function(rme) {
  restore.point("rme_ev_multicol_reg_plausibility")

  # Identify regressions mapped to more than one column and get their cellids
  multicol_regs = rme$mc_df %>%
    group_by(map_version, tabid, runid) %>%
    filter(n_distinct(col) > 1) %>%
    summarise(cellids = paste(sort(unique(cellid)), collapse=","), .groups="drop")

  if (NROW(multicol_regs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No multi-column regressions to check.", test_type = "map_version_specific"))
  }

  # Find regressions where no row has numbers in more than one column
  plausibility_check = rme$mc_df %>%
    semi_join(multicol_regs, by = c("map_version", "tabid", "runid")) %>%
    filter(has_num) %>%
    group_by(map_version, tabid, runid, row) %>%
    summarise(cols_in_row = n_distinct(col), .groups = "drop_last") %>%
    summarise(max_cols_per_row = max(cols_in_row), .groups = "drop") %>%
    filter(max_cols_per_row == 1)

  if (NROW(plausibility_check) == 0) {
    return(rme_df_descr(tibble::tibble(), "No implausible multi-column regressions found.", test_type = "map_version_specific"))
  }

  # Join the issues with the cellids
  issues = multicol_regs %>%
    inner_join(plausibility_check, by = c("map_version", "tabid", "runid")) %>%
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
    return(rme_df_descr(tibble::tibble(), "No classified rows (e.g., nobs, r2) found to check for consistency.", test_type = "map_version_specific"))
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
    rme_df_descr("Inconsistent row indices for the same statistic type (e.g., 'nobs').", test_type = "map_version_specific")

  return(issues)
}
```
!END_MODIFICATION rme_eval_structure.R file
