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
    rme_steps_structure(),
    rme_steps_other()
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
