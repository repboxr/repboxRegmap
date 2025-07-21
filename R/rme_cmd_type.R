# R/rme_cmd_type.R

#' Get the type of a Stata command
#' @param cmd A character vector of Stata command names.
#' @return A character vector of command types.
rme_get_cmd_type = function(cmd) {
  type = rep("unknown", length(cmd))
  # Standard regression commands
  reg_cmds = c("reg", "areg", "ivreg", "ivreg2", "xtreg", "xtivreg", "probit", "logit", "ols")
  type[cmd %in% reg_cmds] = "reg"
  # Other estimation commands
  altreg_cmds = c("rd", "gmm")
  type[cmd %in% altreg_cmds] = "altreg"
  # Post-estimation / test commands
  test_cmds = c("test", "testparm", "lincom", "margins", "estat")
  type[cmd %in% test_cmds] = "test"
  # Descriptive statistics
  descr_cmds = c("sum", "summarize", "tab", "tabulate")
  type[cmd %in% descr_cmds] = "descr"
  # Output commands
  output_cmds = c("display", "putexcel")
  type[cmd %in% output_cmds] = "output"
  return(type)
}


#' Adds command type and associated regression runid to run_df
#' @param rme The rme object.
#' @return The rme object with augmented run_df and cmd_df.
rme_add_cmd_info = function(rme) {
  restore.point("rme_add_cmd_info")

  run_df = rme$run_df %>%
    arrange(runid)

  # Get command type
  run_df$cmd_type = rme_get_cmd_type(run_df$cmd)

  # For non-regression commands, find the runid of the last preceding regression
  run_df = run_df %>%
    mutate(
      prev_reg_runid = if_else(is_reg, runid, NA_integer_)
    ) %>%
    tidyr::fill(prev_reg_runid, .direction = "down") %>%
    mutate(
      # The associated regression is the previous one, but not the command itself if it's a regression
      reg_runid = if_else(is_reg, NA_integer_, prev_reg_runid)
    ) %>%
    select(-prev_reg_runid)

  rme$run_df = run_df

  # Also add info to cmd_df
  if (!is.null(rme$cmd_df)) {
    rme$cmd_df = rme$cmd_df %>%
      mutate(cmd_type = rme_get_cmd_type(cmd))
  }

  return(rme)
}
