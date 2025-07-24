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
