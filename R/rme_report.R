#' Create a markdown report of evaluation results
#'
#' This function generates a markdown-formatted report summarizing the issues
#' found by the various evaluation steps. The report is structured hierarchically:
#' first by map version, then by table ID, and finally by test.
#'
#' @param rme The `rme` object containing evaluation results in `rme$evals`.
#' @param map_version An optional character vector of map versions to report on.
#'   If `NULL`, results from all versions with issues are included.
#' @param tabids A character vector of table IDs to include in the report.
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
rme_make_report = function(rme, map_version = NULL, tabids = NULL, test_names = NULL, ignore_tests = NULL, long_descr = TRUE, outfile = NULL) {
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

  # Descriptions for tests when long_descr = TRUE
  long_descriptions = list(
    runids_differ = "**Discrepancy Across Map Versions.** This test identifies cells that are mapped to *different* regression `runid`s by different AI mapping versions. This is a key indicator of disagreement between models and points to areas of uncertainty.",
    invalid_runids = "**Invalid `runid` Mapping.** This test flags mappings that point to a `runid` that does not exist in the project's execution log (`run_df`). This is a critical integrity error, as the mapped regression output cannot be found.",
    invalid_cellids = "**Invalid `cellid` Mapping.** This test flags mappings that reference a `cellid` that does not exist in the parsed table data (`cell_df`). This is a critical integrity error, indicating a hallucinated or malformed cell reference from the AI.",
    non_reg_cmd = "**Mapping to Non-Regression Command.** This test identifies cells mapped to a Stata command that is not a primary regression command (e.g., `test`, `margins`, `summarize`). This is not necessarily an error—post-estimation results are often included in tables—but serves as an important note. The report shows the command type and the `runid` of the last preceding regression.",
    coef_se_match = "**Value Mismatch between Table and Code.** This is a core value-based check. It compares numeric values from the table (identified as coefficient/standard error pairs) against the results from the mapped regression's `regcoef` output. Issues can be `no_coef_match` (table coef not in output), `no_paren_match` (SE/t-stat/p-val mismatch), or `large_discrepancy` (match found, but difference exceeds rounding tolerance).",
    single_col_reg = "**Regression Spans Multiple Columns.** Regressions are typically presented in a single column. This test flags regressions whose mapped cells span multiple columns without a clear structural reason (like having standard errors in an adjacent column). This often indicates that cells from different regressions have been incorrectly grouped together.",
    multicol_reg_plausibility = "**Implausible Multi-Column Structure.** For a regression that legitimately spans multiple columns, we expect to find rows with numbers in more than one of those columns. This test flags multi-column regressions where every row *only* has a value in one column, suggesting a 'slip' where different rows of the same conceptual regression were incorrectly assigned to different columns.",
    overlapping_regs = "**Overlapping Regression Mappings.** This test flags cells identified as coefficients that have been mapped to *more than one* regression within the *same* map version. This is almost always an error, as a single coefficient should belong to only one regression specification.",
    consistent_vertical_structure = "**Inconsistent Summary Stat Rows.** This test checks for consistent table structure. It identifies summary statistics (like 'Observations' or 'R-squared') by keywords and flags cases where the same statistic appears on different row numbers across the columns of a single table. This points to a potentially messy or inconsistent table layout or a mapping error."
  )

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
    if (!is.null(map_version) && "map_version" %in% names(df)) df = dplyr::filter(df, .data$map_version %in% .env$map_version)
    if (!is.null(tabids) && "tabid" %in% names(df)) df = dplyr::filter(df, .data$tabid %in% .env$tabids)
    if (NROW(df) > 0) df else NULL
  })
  filtered_ev_list = purrr::compact(filtered_ev_list)
  if(length(filtered_ev_list) == 0) {
     msg = "No issues found for the specified filters."
     if (!is.null(outfile)) writeLines(msg, outfile)
     return(msg)
  }

  combined_issues = dplyr::bind_rows(filtered_ev_list)

  # Determine which map_versions and tabids to iterate through
  versions_to_report = sort(unique(combined_issues$map_version))

  # 2. Build Report Header
  report_parts = c("# Regression Mapping Evaluation Report")
  if (!is.null(rme$project_dir)) report_parts = c(report_parts, paste0("**Project**: `", rme$project_dir, "`"))
  report_parts = c(report_parts, "\n---")

  # 3. Iterate and Build Report Body
  for (mv in versions_to_report) {
    report_parts = c(report_parts, paste0("\n## Map Version: `", mv, "`"))

    version_issues = dplyr::filter(combined_issues, .data$map_version == .env$mv)
    tables_in_version = sort(unique(version_issues$tabid))

    for (tid in tables_in_version) {
      report_parts = c(report_parts, paste0("\n### Table `", tid, "`"))
      any_issue_in_table = FALSE

      for (test in tests_to_report) {
        if (!test %in% names(filtered_ev_list)) next

        test_df = filtered_ev_list[[test]]

        # Filter for current scope
        current_issues = test_df %>%
          dplyr::filter(.data$map_version == .env$mv, .data$tabid == .env$tid)

        if (NROW(current_issues) > 0) {
          any_issue_in_table = TRUE

          # Test subsection header
          report_parts = c(report_parts, paste0("\n#### Test: `", test, "`"))

          # Add description
          descr = if (long_descr) long_descriptions[[test]] else attr(test_df, "descr")
          if (!is.null(descr)) report_parts = c(report_parts, paste0("> ", descr))

          report_parts = c(report_parts, paste0("\n**Issues Found**: ", NROW(current_issues)))

          # Format and add results
          display_df = dplyr::select(current_issues, -any_of(c("map_version", "tabid")))
          report_parts = c(report_parts, format_issues_md(display_df, test))
        }
      }

      if (!any_issue_in_table) {
        report_parts = c(report_parts, "_No issues found for this table in this map version._")
      }
       report_parts = c(report_parts, "\n---")
    }
  }

  # 4. Finalize and Output
  final_report = paste(report_parts, collapse = "\n\n")
  final_report = stringi::stri_replace_all_regex(final_report, "(\n){3,}", "\n\n") # Clean up excess newlines

  if (!is.null(outfile)) {
    writeLines(final_report, outfile)
    cat(paste0("\nReport written to '", outfile, "'."))
  }

  invisible(final_report)
}
