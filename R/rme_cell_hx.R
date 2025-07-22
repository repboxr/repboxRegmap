first_or_na = function(x, na_val=NA_character_) {
  if (NROW(x)==0) return(na_val)
  return(first(x))
}

#' Add cell type info (coef, se) to cell_df
#'
#' This function applies heuristics to identify which cells in a table
#' likely represent coefficients and which represent their standard errors,
#' t-stats, or p-values, which are typically in parentheses.
#' @param rme The rme object.
#' @return The rme object with an augmented cell_df.
rme_add_cell_reg_info = function(rme) {
  restore.point("rme_add_cell_reg_info")
  cell_df = rme$cell_df

  # Initialize columns
  cell_df$reg_role = NA_character_
  cell_df$partner_cellid = NA_character_
  cell_df$se_position = NA_character_
  cell_df$is_paren = stringi::stri_detect_fixed(cell_df$text, "(") | stringi::stri_detect_fixed(cell_df$text, "[")

  # Heuristic 1: Value in parenthesis is below a non-parenthesis value
  # This suggests a coef-se pair stacked vertically.
  df = cell_df %>%
    filter(has_num) %>%
    select(tabid, cellid, row, col, is_paren) %>%
    group_by(tabid, col) %>%
    arrange(row) %>%
    mutate(
      is_se_below = is_paren & lag(row) == row - 1 & !lag(is_paren, default=FALSE),
      coef_cellid_below = if_else(is_se_below, lag(cellid), NA_character_)
    ) %>%
    ungroup()

  # Heuristic 2: Value in parenthesis is to the right of a non-parenthesis value
  # This suggests a coef-se pair side-by-side.
  df = df %>%
    group_by(tabid, row) %>%
    arrange(col) %>%
    mutate(
      is_se_right = is_paren & lag(col) == col - 1 & !lag(is_paren, default=FALSE),
      coef_cellid_right = if_else(is_se_right, lag(cellid), NA_character_)
    ) %>%
    ungroup()

  # Combine heuristics
  # Priority to "below" as it's a more common table format.
  res_df = df %>%
    mutate(
      partner_cellid = dplyr::coalesce(coef_cellid_below, coef_cellid_right),
      se_position = dplyr::case_when(
        !is.na(coef_cellid_below) ~ "below",
        !is.na(coef_cellid_right) ~ "right",
        TRUE ~ NA_character_
      ),
      reg_role = if_else(!is.na(partner_cellid), "se", NA_character_)
    ) %>%
    select(cellid, reg_role, partner_cellid, se_position) %>%
    filter(!is.na(reg_role))

  # Mark the corresponding coefficients
  coef_df = res_df %>%
    filter(!is.na(partner_cellid)) %>%
    select(cellid = partner_cellid) %>%
    distinct() %>%
    mutate(reg_role = "coef")

  # Join the identified roles back to the main cell_df
  roles_df = dplyr::bind_rows(res_df, coef_df) %>%
    # In case a cell is both a coef and an se (unlikely but possible), prioritize coef
    group_by(cellid) %>%
    summarise(
      reg_role = first_or_na(if_else("coef" %in% reg_role, "coef", "se")),
      partner_cellid = first_or_na(stats::na.omit(partner_cellid)),
      se_position = first_or_na(stats::na.omit(se_position))
    )

  cell_df = cell_df %>%
    select(-any_of(c("reg_role", "partner_cellid", "se_position"))) %>%
    left_join(roles_df, by = "cellid")

  # Fill other numeric cells
  cell_df = cell_df %>%
    mutate(reg_role = ifelse(has_num & is.na(reg_role), "other_num", reg_role))

  rme$cell_df = cell_df
  return(rme)
}
