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
    select(map_version, tabid, reg_ind, cellid)  %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("map versions with invalid cellids (cellids not in cell_df)", test_type = "flag")
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
    rme_df_descr("mapped to non-existent runid", test_type = "flag")
}

# Check for mappings to commands that are not regressions
rme_ev_non_reg_cmd = function(rme) {
  restore.point("eval_check_non_reg_cmd")
  df = rme$mc_df %>%
    # is_reg is pre-computed and joined into mc_df
    filter(!is.true(is_reg)) %>%
    select(map_version, tabid, reg_ind, cellid, runid, cmd, cmd_type, reg_runid) %>%
    rme_df_merge_cellids() %>%
    rme_df_descr("Mapped to a cmd that is not a regcmd. This is not neccessarily a wrong mapping but useful information, e.g. for post-estimation commands.", test_type = "note_flag")
  df
}
