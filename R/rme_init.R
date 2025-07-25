# rme stands for repbox map evaluation
#
# We will use an rme object (just an R list)
# that will load and transform all data needed

example = function() {
  library(repboxRegmap)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  rstudioapi::filesPaneNavigate(project_dir)
  options(warn=2)

  rme = rme_init(project_dir)
  rme = rme_eval_all(rme)
  rme_save(rme)
  outfile = file.path(project_dir,"fp","eval_art/rme_report.md")
  str = rme_make_report(rme,outfile = outfile,long_descr = TRUE, map_version = "g25f-mocr--v0", tabid="2")
  rstudioapi::filesPaneNavigate(outfile)
  # Explore the created rme object
  ls(rme)
  head(rme$mc_df)
  head(rme$wrong_numbers_df)


  lapply(rme$evals, function(df) {
    unique(df$tabid)
  })
}


rme_init = function(project_dir, doc_type = "art") {
  restore.point("rme_init")

  # Load repdb parcels
  parcel_names = c("reg_core", "regcoef", "regscalar", "regstring", "regvar", "regxvar", "stata_run_log")
  parcels = repdb_load_parcels(project_dir, parcel_names = parcel_names)
  parcels = parcels_add_runid_step(parcels,map_df = parcels$reg_core$reg)


  # Load fp data
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  eval_dir = file.path(project_dir, "fp", paste0("eval_", doc_type))

  # table structure is best stored in cell_base
  cell_ver = repboxAI::rai_pick_tab_ver(fp_dir, "cell_base")
  cell_df = fp_load_prod_df(ver_dir = cell_ver$ver_dir,add_ids = FALSE)

  #cell_df = FuzzyProduction::fp_pick_and_load_prod_df(fp_dir,"cell_base",add_fp_dir = FALSE,add_ids = FALSE)

  rme = list(
    project_dir = project_dir,
    doc_type = doc_type,
    fp_dir = fp_dir,
    eval_dir = eval_dir,
    cell_verid = cell_ver$ver_id,
    cell_df = cell_df,
    parcels = parcels
  )
  # Add command and run info, including command type and associated regressions
  rme = rme_add_run_df_cmd_df(rme)
  rme = rme_add_cmd_info(rme)

  # Add cell heuristic info (coef, se, etc.)
  rme = rme_add_cell_reg_info(rme)


  # Load all mapping product versions for map_reg_run
  # This is now the only mapping we consider for building the "best" map
  map_reg_run = fp_load_all_prod_df(fp_dir, prod_id = "map_reg_run") %>%
    add_col_left(prod_id = "map_reg_run")

  # Add info from run_df
  map_reg_run = map_reg_run %>%
      left_join(rme$run_df %>% select(runid, cmd, cmd_type, reg_runid, is_reg), by="runid")


  wrong_numbers_df = map_reg_run %>%
    filter(!sapply(wrong_number_cases, is.null)) %>%
    select(ver_id, prod_id, tabid, reg_ind, runid, cmd, cmd_type,  wrong_number_cases) %>%
    tidyr::unnest(wrong_number_cases)


  # Unnest on a cellid level and add additional info
  mc_df = unnest_comma_string_col(map_reg_run, "cell_ids") %>%
    rename(cellid = cell_ids) %>%
    # Focus on mappings with a valid runid, which is our main key
    filter(!is.na(runid))

  mc_df = mc_df %>%
    mutate(map_version = ver_id) %>%
    filter(!is.na(cellid), cellid != "") %>%
    # Select and reorder a common set of columns for the final unified data frame.
    select(
      cellid, map_version, prod_id, ver_id, tabid, any_of("reg_ind"), runid,
      # Keep problem description if present
      any_of("problem")
    ) %>%
    # Enrich with run information (cmd, type, associated reg)
    left_join(rme$run_df %>% select(runid, cmd, cmd_type, reg_runid, is_reg), by="runid") %>%
    # Enrich with cell coordinates and content from the main cell_df
    left_join(rme$cell_df, by=c("tabid", "cellid"))


  # Aggregate on a map_version, cellid level
  map_cell_agg = mc_df %>%
    group_by(map_version, prod_id, tabid, cellid) %>%
    summarize(
      runids = paste0(sort(unique(runid)), collapse=","),
      num_runids = n_distinct(runid),
      .groups = "drop"
    ) %>%
    arrange(tabid, cellid, prod_id, map_version)

  # --- Create rme object ---
  rme = c(rme,list(
    mc_df = mc_df,
    wrong_numbers_df = wrong_numbers_df,
    map_cell_agg = map_cell_agg,
    map_reg_run = map_reg_run
  ))

  return(rme)
}

rme_save = function(rme) {
  if (is.null(rme$eval_dir)) {
    cat("\nrem$eval_dir not specified")
    return(NULL)
  }
  file = file.path(rme$eval_dir, "rme.Rds")
  if (!dir.exists(rme$eval_dir)) dir.create(rme$eval_dir)
  saveRDS(rme, file)
  invisible(rme)
}

rme_load = function(project_dir, doc_type = "art") {
  eval_dir = file.path(project_dir, "fp", paste0("eval_", doc_type))
  file = file.path(eval_dir, "rme.Rds")
  readRDS.or.null(file)
}


# Add missing step or runid column if the other exists
parcels_add_runid_step = function(parcels, map_df = parcels$reg_core$reg) {
  restore.point("parcels_map_runid_step")
  map_df = map_df[,c("runid","step")]


  # Add runid to all parcel data frames that have 'step'
  for (pname in names(parcels)) {
    if (is.list(parcels[[pname]])) {
       for (df_name in names(parcels[[pname]])) {
         df = parcels[[pname]][[df_name]]
         if ("step" %in% names(df) && !"runid" %in% names(df)) {
            parcels[[pname]][[df_name]] = left_join(df, map_df, by="step")
         } else if (!"step" %in% names(df) && "runid" %in% names(df)) {
            parcels[[pname]][[df_name]] = left_join(df, map_df, by="runid")
         }
       }
    }
  }
  parcels
}


# return a version of cmd_df and run_df that have script_num and code_line
# given by orgline
rme_add_run_df_cmd_df = function(rme, project_dir=rme$project_dir, parcels=rme$parcels) {
  restore.point("rme_load_run_df_cmd_df")
  # Load repdb parcels
  parcel_names = c("stata_run_cmd", "stata_cmd", "stata_file")
  parcels = repdb_load_parcels(project_dir, parcel_names = parcel_names,parcels=parcels)

  script_df = parcels$stata_file$script_file %>% select(file_path, script_num)
  cmd_df = parcels$stata_cmd$stata_cmd %>%
    left_join(script_df, by = c("file_path")) %>%
    select(artid, script_num, code_line=orgline, line=line, cmd=cmd, is_reg=is_reg) %>%
    mutate(script_line = paste0(script_num, "-", code_line))

  run_df = parcels$stata_run_cmd$stata_run_cmd %>%
    left_join(script_df, by = c("file_path")) %>%
    left_join(cmd_df %>% select(script_num, line, code_line, is_reg, script_line), by=c("script_num","line")) %>%
    select(artid, runid, script_num, code_line=code_line, script_line, cmd=cmd, is_reg, missing_data, errcode)

  rme$cmd_df = cmd_df
  rme$run_df = run_df

  rme
}
