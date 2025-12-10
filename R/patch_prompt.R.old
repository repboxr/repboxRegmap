example = function() {
  library(repboxRegmap)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  base_ver_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6/fp/prod_art/map_reg_run/g25f-mocr/v0"
  rstudioapi::filesPaneNavigate(project_dir)
  options(warn=1)
  rme = rme_load(project_dir)
  txt = patch_prompt_map_reg_run_base_results(base_ver_dir, rme)
  outfile = file.path(project_dir,"fp/eval_art/patch_map_reg_run.md")
  writeLines(txt, outfile)

  rme = rme_init(project_dir)

}


#' Create a string with base results and issues for a patch prompt
#'
#' This function generates a markdown-formatted string that summarizes
#' the previous mapping results and flagged issues for specific tables.
#' This string is intended to be used as part of a larger prompt for an
#' AI to "patch" or correct the initial mappings. The function is
#' structured with explicit blocks for each check to allow for easy
#' manual customization (e.g., changing descriptions or filtering checks).
#'
#' @param base_ver_dir The directory of the base mapping version to report on.
#' @param rme The rme object containing all data and evaluation results.
#' @return A character string with the formatted summary.
patch_prompt_map_reg_run_base_results = function(base_ver_dir, rme) {
  restore.point("patch_prompt_map_reg_run_base_results")

  map_version = fp_ver_dir_to_ver_id(base_ver_dir)

  # Filter for all issues related to the specified base version

  ignore_tests = c("runids_differ")

  all_issues = rme_combine_ev_df(rme) %>%
    dplyr::filter(map_version == .env$map_version, !test_name %in% ignore_tests)

  if (NROW(all_issues) == 0) {
    return("No issues found.")
  }

  # Consider only tabids that have issues
  tabids = repboxTableTools::sort_tabids(unique(all_issues$tabid))
  tab_str = sapply(tabids,patch_prompt_base_results_for_tab, rme=rme, map_version=map_version)
  main_str = paste0(tab_str, collapse="\n")

  return(main_str)
}
# This file contains functions to generate prompts for patching AI mappings.


#' Generates a markdown summary of issues for a single table
patch_prompt_base_results_for_tab = function(tabid, rme, map_version) {
  tid = tabid; ver_id = map_version
  restore.point("patch_prompt_base_results_for_tab")

  table_str = paste0("\n\n## Table ", tid)

  # 1. Show the previous mapping result for this table
  prev_map = rme$map_reg_run %>%
    dplyr::filter(ver_id == .env$map_version, tabid == tid) %>%
    dplyr::select(any_of(c("reg_ind", "runid", "script_file", "code_line", "cell_ids", "problem", "wrong_number_cases")))

  map_json = jsonlite::toJSON(prev_map, pretty = TRUE, auto_unbox = TRUE)

  table_str = paste0(table_str,
    "\n\n### Previous Mapping Result\n\n",
    "```json\n",
    map_json,
    "\n```\n",
    "\n\n### Flagged Potential Issues"
  )

  # Helper to get the issues for a specific test, filtered for the current table/version
  get_test_df = function(test_name) {
    if (!test_name %in% names(rme$evals)) return(tibble::tibble())
    df = rme$evals[[test_name]]
    if (is.null(df) || NROW(df) == 0) return(tibble::tibble())

    df = dplyr::ungroup(df)
    if ("map_version" %in% names(df)) df = dplyr::filter(df, map_version == ver_id)
    if ("tabid" %in% names(df)) df = dplyr::filter(df, tabid == tid)
    df
  }

  # Helper to append a formatted test result to the main string
  num_tests = 0

  note_test = function(test_name, descr, df) {
    cat("\nNoted: ", test_name, " for Table ", tid, " with ", NROW(df), " flagged rows.\n")
    num_tests <<- num_tests+1
    table_str <<- paste0(table_str,
      "\n\n#### Flagged Issue: `", test_name, "`\n\n",
      "", descr, "\n\n")
    if (NROW(df)==0) return(invisible(table_str))

    table_str <<- paste0(table_str,
      df_to_markdown(df %>% dplyr::select(-any_of(c("map_version", "tabid"))))
    )
    invisible(table_str)
  }

  # --- Manual blocks for each check ---

  # Ignore runids_differ, non_reg_cmd

  # Check: invalid_cellids
  test_name = "invalid_cellids"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Invalid `cellid` Mapping.** This test flags mappings that reference a `cellid` that does not exist in the parsed table data (`cell_df`). This is a critical integrity error, indicating a hallucinated or malformed cell reference from the AI.
    descr = "**Invalid `cell_id` Mapping.** This test flags mappings that reference a `cell_id` that does not exist in the parsed table data. This indicates a hallucinated or malformed cell reference from the AI."
    note_test(test_name, descr, df)
  }

  # Check: coef_se_match
  test_name = "coef_se_match"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # Get wrong numbers identified by the previous AI run for this table
    wn_df = rme$map_reg_run %>%
      dplyr::filter(ver_id == .env$ver_id, tabid == tid, !sapply(wrong_number_cases, is.null)) %>%
      dplyr::select(reg_ind, wrong_number_cases) %>%
      tidyr::unnest(wrong_number_cases)
    if (NROW(wn_df)==0) {
      wn_df$cellid = character(0)
    } else {
      wn_df = wn_df %>%
      dplyr::rename(cellid = cell_id) # Rename for consistent joining
    }

    # Compare check results with what the AI previously found
    ai_miss_df = df %>%
      dplyr::anti_join(wn_df, by = "cellid")

    ai_extra_df = wn_df %>%
      dplyr::anti_join(df, by = "cellid")
  }

  if (NROW(ai_miss_df) > 0 | NROW(ai_extra_df) > 0 ) {
    common_df = df %>%
      dplyr::inner_join(prev_wn_df, by = "cellid")
    common_str = "\n\n- There were no cells for which at the same time the previous AI run and our heuristic identified a problem."
    if (NROW(common_df)>0) {
      common_str =  paste0(
        "\n\n- The previous AI run and our heuristic at the same time identified likely transcription errors for the following cells:\n\n",
        df_to_markdown(mismatches_found_by_ai %>% dplyr::select(cellid, table_val, true_val_cand, issue, details))
      )
    }

    if (NROW(ai_miss_df)>0) {
      miss_str =  paste0("\n\n- Compared to the heuristic the previous AI run did not mention transcription errors for the cells below. Please verify these missing cells and, if there are indeed transcription errors, add them this time to the `wrong_number_cases` array:\n\n",
        df_to_markdown(mismatches_missed_by_ai %>% dplyr::select(cellid, table_val, true_val_cand, issue, details)))
    } else {
      miss_str = ""
    }

    if (NROW(ai_extra_df)>0) {
      extra_str =  paste0("\n\n- The previous AI mentioned possible transcription errors for cells where our heuristic did not catch any transcription errors. Please verify whether these are indeed transcription errors and if not don't include them this time in the `wrong_number_cases` array:\n\n",
        df_to_markdown(mismatches_missed_by_ai %>% dplyr::select(cellid, table_val, true_val_cand, issue, details)))
    } else {
      extra_str = ""
    }

    descr = paste0("The previous AI run and / or our heuristic checks suggested that some numeric values in the table do not match the Stata output. But the actually identified cells where this occurs are not the same.", common_str, miss_str, extra_str)
    note_test(test_name, descr, NULL)
  }

  # Check: single_col_reg
  test_name = "single_col_reg"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Regression Spans Multiple Columns.** Regressions are typically presented in a single column. This test flags regressions whose mapped cells span multiple columns without a clear structural reason (like having standard errors in an adjacent column). This often indicates that cells from different regressions have been incorrectly grouped together.
    descr = "**Regression Spans Multiple Columns.** A single regression has been mapped to cells in multiple columns, which is unusual but can happen in some cases. E.g.

  - SEs are in an adjacent column instead of below the coefficient.

  - If a balancing table is generated using regressions sometimes a regression spans one or two rows instead of a column.

  But sometimes having mapped cells from multiple columns for a regression can indicated an mistake in the mapping, so please check again."
    note_test(test_name, descr, df)
  }

  # Check: multicol_reg_plausibility
  test_name = "multicol_reg_plausibility"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Implausible Multi-Column Structure.** For a regression that legitimately spans multiple columns, we expect to find rows with numbers in more than one of those columns. This test flags multi-column regressions where every row *only* has a value in one column, suggesting a 'slip' where different rows of the same conceptual regression were incorrectly assigned to different columns.
    descr = "**Implausible Multi-Column Structure.** A regression spans multiple columns, but each row only has a value in one of those columns. This typcially suggests an inconsistent mapping across rows, but there might be exceptions."
    note_test(test_name, descr, df)
  }

  # Check: overlapping_regs
  test_name = "overlapping_regs"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    descr = "**Overlapping Regression Mappings.** A single cell (identified as a coefficient) has been mapped to more than one regression. This can well indicate a mapping error."
    note_test(test_name, descr, df)
  }

  # Check: missing_se_mapping
  test_name = "missing_se_mapping"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Unmapped Standard Error.** This test flags cases where a mapped coefficient cell has an associated standard error (a value in parentheses, typically below the coefficient) that was *not* included in the regression mapping. It also reports whether the numeric value of that unmapped SE would have been a correct match for the regression's output, helping to distinguish simple mapping omissions from more complex issues.
    descr = "**Unmapped Standard Error.** A coefficient was mapped, but its associated standard error (or t-value, p-value ..., we mean the value in parentheses) was not included in the mapping. Check if it should be added."
    note_test(test_name, descr, df)
  }

  # Check: consistent_reg_ind_in_col
  test_name = "consistent_reg_ind_in_col"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Inconsistent `reg_ind` in Column.** In a column that maps to a single main regression, all mapped cells (including post-estimation stats) should share the same `reg_ind`. This check flags columns where cells are assigned to multiple different `reg_ind`s, suggesting a mapping inconsistency.
    descr = "**Inconsistent `regid` in Column.** Within a single table column that seems to represent one regression, cells have been mapped to different regression IDs (`regid`). Check whether that was indeed correct, otherwise correct."
    note_test(test_name, descr, df)
  }

  # Check: post_est_reg_match
  test_name = "post_est_reg_match"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Mismatched Post-Estimation Command.** In a column corresponding to a single regression, any post-estimation commands (e.g., `test`, `summarize`) should be associated with that main regression. This check flags cases where a post-estimation command's associated `reg_runid` does not match the main regression's `runid` for that column.
    descr = "**Mismatched Post-Estimation Command.** A post-estimation command (like a test statistic) in a regression column appears to be linked to the wrong regression run. Our heuristic check which post-regression runid is closest to the runid of the mapped regression in the table column. But in particular if there are multiple regressions in a table column, the heuristic may be wrong. But take a look it."
    note_test(test_name, descr, df)
  }

  # Check: value_match_other_cmd
  test_name = "value_match_other_cmd"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Value Mismatch for Other Commands.** This check verifies if numeric values in cells mapped to non-regression commands (e.g., `summarize`, `display`) can be found in the raw output of that command. It checks for a match after rounding the output values to the same number of decimal places as the table value.
    descr = "**Value Mismatch for Other Commands.** A numeric value in a cell mapped to a non-regression command (e.g., `summarize`) could not be found in the raw Stata output of that command. This might be a transcription error. If yes, make sure the cellid is noted in  wrong_number_cases (it might already be there, we did not check). But it might also indicate a mapping error by the previous AI run. If yes, please correct."
    note_test(test_name, descr, df)
  }

  # Check: mapped_to_line_not_run
  test_name = "mapped_to_line_not_run"
  df = get_test_df(test_name)
  if (NROW(df) > 0) {
    # long_descr = **Mapped to Code Line but not a `runid`.** This flags cells that are mapped to a specific line in a script but are not associated with a `runid` (i.e., a specific execution output). While sometimes intentional for un-executed code, numeric cells in a results table should be mapped to a `runid`. If a numeric cell cannot be mapped to a specific `runid`, both `runid` and `code_line` should ideally be to indicate it belongs to the conceptual regression without a direct code link.
    descr = "**Mapped to Code Line but not a `runid`.** Cells are mapped to a line of code, but not to a specific output run (`runid`). Numeric results should generally be linked to a `runid` if the outut is shown. The previous AI run might have not yet seen the Stata output and runid, but now we have added every Stata output from the code lines listed below. Make sure to add for those cases the runid now, if you think the mapping is still valid after seeing the output. Otherwise modify the mapping."
    note_test(test_name, descr, df)
  }


  if (num_tests == 0) table_str = "-- No issues found --"

  # The check consistent_vertical_structure is omitted as it is less about a single
  # mapping's correctness and more about overall table consistency.

  return(table_str)
}
