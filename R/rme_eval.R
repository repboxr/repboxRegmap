# R/rme_eval_a.R

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
  df = rme$mc_df

  # A cell like num_obs can be linked to multiple regressions
  # we store a string of all corresponding runid for a cell-map_version
  df = mc_df %>%
    group_by(map_version, prod_id, tabid, cellid) %>%
    summarize(
      runids = paste0(sort(unique(runid)), collapse=","),
      num_runids = n_distinct(runid),
      .groups = "drop"
    ) %>%
    arrange(tabid, cellid, prod_id, map_version)

  # Find differences across map versions
  df = df %>%
    group_by(tabid, cellid) %>%
    summarize(
      n_diff_runids = n_distinct(runids),
      .groups = "drop"
    ) %>%
    filter(n_diff_runids > 1) %>%
    rme_df_descr("cellids where not all map versions map to the same runids")
}

rme_ev_invalid_cellids = function(rme) {
  restore.point("eval_check_invalid_cellids")
  mc_df = rme$mc_df
  cell_df = rme$cell_df

  inv_df = mc_df %>%
    anti_join(cell_df, by="cellid") %>%
    select(tabid, cellid, map_version, prod_id)  %>%
    rme_df_descr("map versions with invalid cellids (cellids not in cell_df)")
  return(inv_df)
}

# Check for mappings to runids that do not exist
rme_ev_invalid_runids = function(rme) {
  restore.point("eval_check_invalid_runids")
  df = rme$mc_df %>%
    filter(!is.na(runid)) %>%
    filter(!runid %in% rme$run_df$runid) %>%
    select(map_version, prod_id, tabid, cellid, invalid_runid=runid) %>%
    rme_df_descr("mapped to non-existent runid")
}

# Check for mappings to commands that are not regressions
rme_ev_non_reg_cmd = function(rme) {
  restore.point("eval_check_non_reg_cmd")
  df = rme$mc_df %>%
    # is_reg is pre-computed and joined into mc_df
    filter(!is.true(is_reg)) %>%
    select(map_version, prod_id, tabid, cellid, runid, cmd, cmd_type, reg_runid) %>%
    rme_df_descr("Mapped to a cmd that is not a regcmd. This is not neccessarily a wrong mapping but useful information, e.g. for post-estimation commands.")
  df
}
