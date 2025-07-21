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

#' Generate data frames with evaluation issue reports.
#'
#' This function processes the evaluation results stored in `rme$evals`,
#' summarizes them by map version and table (`map_version`, `tabid`), and creates two new data frames in
#' the `rme` object:
#'
#' - `rme$ttr_df`: A "Table Test Report" data frame with one row for each
#'   test that failed for a given map version and table. It includes the test name, a
#'   description of the problem, and a comma-separated list of affected cell IDs.
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

    rme$ttr_df = tibble::tibble(map_version=character(), tabid=character(), test_name=character(), problem_descr=character(), cells=character())

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
          subject_str = {
            subjects = list()
            data = cur_data()
            if ("cellid" %in% names(data)) {
              vals = stats::na.omit(data$cellid)
              if (length(vals) > 0) subjects$cells = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("runid" %in% names(data)) {
              vals = stats::na.omit(data$runid)
              if (length(vals) > 0) subjects$runids = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("runids" %in% names(data)) {
              vals = stats::na.omit(data$runids)
              if (length(vals) > 0) subjects$runids_list = paste(sort(unique(vals)), collapse = "; ")
            }
            if ("invalid_runid" %in% names(data)) {
              vals = stats::na.omit(data$invalid_runid)
              if (length(vals) > 0) subjects$invalid_runids = paste(sort(unique(vals)), collapse = ", ")
            }
            if ("rows_used" %in% names(data)) {
              vals = stats::na.omit(data$rows_used)
              if (length(vals) > 0) subjects$rows = paste(sort(unique(vals)), collapse = ", ")
            }
            subject_str_parts = purrr::imap_chr(subjects, ~ paste0(.y, ": ", .x))
            if (length(subject_str_parts) > 0) paste0(" (", paste(subject_str_parts, collapse="; "), ")") else ""
          },
          cells = if("cellid" %in% names(cur_data())) {
            vals = stats::na.omit(cur_data()$cellid)
            if(length(vals)>0) paste(sort(unique(vals)), collapse = ",") else NA_character_
          } else NA_character_,
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          test_name = test_name,
          problem_descr = paste0(descr, subject_str)
        ) %>%
        dplyr::select(map_version, tabid, test_name, problem_descr, cells)
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
            problem_descr = paste0(descr, " for cell ", cellid, ". ", details),
            cells = cellid
          ) %>%
          dplyr::select(map_version, tabid, test_name, problem_descr, cells) %>%
          dplyr::group_by(map_version, tabid, test_name) %>%
          dplyr::summarise(
            problem_descr = paste(problem_descr, collapse = "\n"),
            cells = paste(sort(unique(cells)), collapse=","),
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
      dplyr::mutate(problem_report_part = paste0("--- Test: ", test_name, " ---\n", problem_descr)) %>%
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
