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

rme_df_descr = function(df, descr) {
  attr(df,"descr") = descr
  df
}
