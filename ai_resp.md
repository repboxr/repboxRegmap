An excellent suggestion. Generating a structured report of evaluation failures for each table will be very useful for creating targeted prompts to improve the AI's performance. I will add the `rme_make_tab_report` function to `rme_eval_tools.R`.

This function will iterate through the evaluation results, summarize the issues for each table, and generate the `ttr_df` and `tr_df` data frames as you specified. These will then be added to the `rme` object for later use.

Here is the implementation:

!MODIFICATION rme_make_tab_report in rme_eval_tools.R
scope = "function"
file = "R/rme_eval_tools.R"
insert_bottom = true
description = '''Add a new function `rme_make_tab_report` to generate a summary of evaluation issues per table. This creates `ttr_df` and `tr_df` within the `rme` object, which can be used to generate new AI prompts.'''
---
```r

#' Generate a table-by-table report of evaluation issues.
#'
#' This function processes the evaluation results stored in `rme$evals`,
#' summarizes them by table (`tabid`), and creates two new data frames in
#' the `rme` object:
#'
#' - `rme$ttr_df`: A "Table Test Report" data frame with one row for each
#'   test that failed for a given table. It includes the test name, a
#'   description of the problem, and a comma-separated list of affected cell IDs.
#' - `rme$tr_df`: A "Table Report" data frame with one row per table,
#'   containing a consolidated `problem_report` string that combines all
#'   issues for that table. Tables with no issues will have an empty string.
#'
#' This report is intended to be used to generate prompts for AI to improve
#' mappings in subsequent runs.
#'
#' @param rme The `rme` object containing evaluation results.
#' @return The `rme` object, augmented with `ttr_df` and `tr_df`.
#' @export
rme_make_tab_report = function(rme) {
  restore.point("rme_make_tab_report")

  if (is.null(rme$evals) || length(rme$evals) == 0) {
    all_tabids = if (!is.null(rme$cell_df)) unique(rme$cell_df$tabid) else character(0)
    rme$ttr_df = tibble::tibble(tabid=character(), test_name=character(), problem_descr=character(), cells=character())
    rme$tr_df = tibble::tibble(tabid = all_tabids, problem_report = "")
    return(rme)
  }

  ttr_list = list()

  for (test_name in names(rme$evals)) {
    ev_df = rme$evals[[test_name]]
    if (is.null(ev_df) || NROW(ev_df) == 0) next

    descr = attr(ev_df, "descr")
    if (is.null(descr)) descr = paste0("Issues from ", test_name, ".")

    # Ensure tabid column exists, deriving it from cellid if necessary
    if (!"tabid" %in% names(ev_df)) {
      if ("cellid" %in% names(ev_df)) {
        ev_df$tabid = stringi::stri_extract_first_regex(ev_df$cellid, "^c([0-9]+)_", 1)
      } else {
        # This test is not table-specific. Skip.
        next
      }
    }

    # Summarize issues by table
    tab_summary = ev_df %>%
      filter(!is.na(tabid)) %>%
      group_by(tabid) %>%
      summarise(
        # Create a string of affected entities
        subject_str = {
          subjects = list()
          if ("cellid" %in% names(.)) {
            vals = stats::na.omit(cellid)
            if (length(vals) > 0) subjects$cells = paste(sort(unique(vals)), collapse = ", ")
          }
          if ("runid" %in% names(.)) {
            vals = stats::na.omit(runid)
            if (length(vals) > 0) subjects$runids = paste(sort(unique(vals)), collapse = ", ")
          }
          # Handle plural 'runids' column from checks like overlapping_regs
          if ("runids" %in% names(.)) {
            vals = stats::na.omit(runids)
            if (length(vals) > 0) subjects$runids_list = paste(sort(unique(vals)), collapse = "; ")
          }
          if ("invalid_runid" %in% names(.)) {
            vals = stats::na.omit(invalid_runid)
            if (length(vals) > 0) subjects$invalid_runids = paste(sort(unique(vals)), collapse = ", ")
          }
          if ("rows_used" %in% names(.)) {
            vals = stats::na.omit(rows_used)
            if (length(vals) > 0) subjects$rows = paste(sort(unique(vals)), collapse = ", ")
          }

          subject_str_parts = purrr::imap_chr(subjects, ~ paste0(.y, ": ", .x))
          if (length(subject_str_parts) > 0) {
            paste0(" (", paste(subject_str_parts, collapse="; "), ")")
          } else {
            ""
          }
        },
        cells = if("cellid" %in% names(.)) {
          vals = stats::na.omit(cellid)
          if(length(vals)>0) paste(sort(unique(vals)), collapse = ",") else NA_character_
        } else NA_character_,
        .groups = "drop"
      ) %>%
      mutate(
        test_name = test_name,
        problem_descr = paste0(descr, subject_str)
      ) %>%
      select(tabid, test_name, problem_descr, cells)

    ttr_list[[test_name]] = tab_summary
  }

  ttr_df = dplyr::bind_rows(ttr_list)

  # Create tr_df from ttr_df
  if (NROW(ttr_df) > 0) {
    tr_df = ttr_df %>%
      # For the combined report, add a structured header for each test
      mutate(problem_report_part = paste0("--- Test: ", test_name, " ---\n", problem_descr)) %>%
      group_by(tabid) %>%
      summarise(problem_report = paste(problem_report_part, collapse = "\n\n"), .groups = "drop")
  } else {
    tr_df = tibble::tibble(tabid=character(), problem_report=character())
  }

  # Ensure all tables are in tr_df, even those without issues
  all_tabids = if (!is.null(rme$cell_df)) unique(rme$cell_df$tabid) else character(0)
  if (length(all_tabids) > 0) {
    tr_df = tr_df %>%
      dplyr::full_join(tibble::tibble(tabid = all_tabids), by = "tabid") %>%
      mutate(problem_report = ifelse(is.na(problem_report), "", problem_report))
  }

  rme$ttr_df = ttr_df
  rme$tr_df = tr_df

  return(rme)
}
```
!END_MODIFICATION rme_make_tab_report in rme_eval_tools.R
