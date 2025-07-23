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

  # Descriptions for tests when long_descr = TRUE
  long_descriptions = list(
    runids_differ = "**Discrepancy Across Map Versions.** This test identifies cells that are mapped to *different* regression `runid`s by different AI mapping versions. This is a key indicator of disagreement between models and points to areas of uncertainty.",
    invalid_runids = "**Invalid `runid` Mapping.** This test flags mappings that point to a `runid` that does not exist in the project's execution log (`run_df`). This is a critical integrity error, as the mapped regression output cannot be found.",
    invalid_cellids = "**Invalid `cellid` Mapping.** This test flags mappings that reference a `cellid` that does not exist in the parsed table data (`cell_df`). This is a critical integrity error, indicating a hallucinated or malformed cell reference from the AI.",
    non_reg_cmd = "**Mapping to Non-Regression Command.** This test identifies cells mapped to a Stata command that is not a primary regression command (e.g., `test`, `margins`, `summarize`). This is not necessarily an error—post-estimation results are often included in tables—but serves as an important note. The report shows the command type and the `runid` of the last preceding regression.",
    coef_se_match = "**Value Mismatch between Table and Code.** This is a core value-based check. It compares numeric values from the table (identified as coefficient/standard error pairs) against the results from the mapped regression's `regcoef` output. A match is considered perfect if the code output, rounded to the number of decimal places shown in the table, equals the table value. Issues can be `no_coef_match` (table coef not in output within tolerance), `no_match_perhaps_wrong_sign` (a match is found if the sign is flipped), `no_paren_match` (SE/t-stat/p-val mismatch), or `rounding_error` (the values are very close but don't match exactly after rounding, suggesting a minor discrepancy).",
    single_col_reg = "**Regression Spans Multiple Columns.** Regressions are typically presented in a single column. This test flags regressions whose mapped cells span multiple columns without a clear structural reason (like having standard errors in an adjacent column). This often indicates that cells from different regressions have been incorrectly grouped together.",
    multicol_reg_plausibility = "**Implausible Multi-Column Structure.** For a regression that legitimately spans multiple columns, we expect to find rows with numbers in more than one of those columns. This test flags multi-column regressions where every row *only* has a value in one column, suggesting a 'slip' where different rows of the same conceptual regression were incorrectly assigned to different columns.",
    overlapping_regs = "**Overlapping Regression Mappings.** This test flags cells identified as coefficients that have been mapped to *more than one* regression within the *same* map version. This is almost always an error, as a single coefficient should belong to only one regression specification.",
    consistent_vertical_structure = "**Inconsistent Summary Stat Rows.** This test checks for consistent table structure. It identifies summary statistics (like 'Observations' or 'R-squared') by keywords and flags cases where the same statistic appears on different row numbers across the columns of a single table. This points to a potentially messy or inconsistent table layout or a mapping error.",
    missing_se_mapping = "**Unmapped Standard Error.** This test flags cases where a mapped coefficient cell has an associated standard error (a value in parentheses, typically below the coefficient) that was *not* included in the regression mapping. It also reports whether the numeric value of that unmapped SE would have been a correct match for the regression's output, helping to distinguish simple mapping omissions from more complex issues."
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

        descr = if (long_descr) long_descriptions[[test]] else attr(filtered_ev_list[[test]], "descr")
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
