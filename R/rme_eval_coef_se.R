# R/rme_eval_b.R

# This file implements Module B: Value-based Checks

#' A function to group all value-based checks
#' @export
rme_steps_value = function() {
  c("coef_se_match")
}

#' Check for consistency between table coefficients/SEs and regcoef output
#'
#' This evaluation compares numeric values from cells identified as coefficients
#' and their associated standard errors (in parentheses) with the detailed
#' output from the `regcoef` parcel.
#'
#' It first finds the best combined match for each (coefficient, se) pair from the
#' table with a result from the corresponding regression output (`regcoef` parcel).
#' A match is scored based on how well both the coefficient and the parenthesis value
#' (se, t-stat, or p-value) align.
#'
#' Based on the best match, it reports several types of issues:
#' - `no_coef_match`: The coefficient from the table does not match any coefficient
#'   in the corresponding `regcoef` output for that `runid` within a given tolerance.
#' - `no_paren_match`: The coefficient matched, but the value in parentheses
#'   does not match the corresponding standard error, t-statistic, or p-value.
#' - `large_discrepancy`: A match was found, but the relative difference
#'   is larger than a rounding tolerance but smaller than a mismatch tolerance. This can
#'   be reported for either the coefficient or the parenthesis value.
#'
#' @param rme The rme object.
#' @param rel_tol_rounding The relative tolerance for what is considered a rounding error.
#' @param rel_tol_mismatch The relative tolerance for what is considered a mismatch.
#' @return A data frame of identified issues.
rme_ev_coef_se_match = function(rme, rel_tol_rounding = 0.005, rel_tol_mismatch = 0.05) {
  restore.point("rme_ev_coef_se_match")

  # 1. Get all coef-se pairs from the tables, including tabid
  pairs_df = rme$cell_df %>%
    filter(reg_role == "se", !is.na(partner_cellid)) %>%
    select(tabid, se_cellid = cellid, coef_cellid = partner_cellid, paren_val = num) %>%
    left_join(rme$cell_df %>% select(coef_cellid=cellid, coef_val=num), by="coef_cellid")

  # 2. Join with mappings to get runids
  mc_coef_df = rme$mc_df %>%
    filter(cellid %in% pairs_df$coef_cellid) %>%
    select(map_version, coef_cellid = cellid, runid) %>%
    distinct()

  # 3. Combine pairs with their mappings
  mapped_pairs = pairs_df %>%
    inner_join(mc_coef_df, by = "coef_cellid", relationship = "many-to-many") %>%
    filter(!is.na(coef_val), !is.na(paren_val))

  if (NROW(mapped_pairs) == 0) {
    return(rme_df_descr(tibble::tibble(), "No coef/se pairs found to check."))
  }

  # 4. Get all relevant regcoef data
  if (is.null(rme$parcels$regcoef$regcoef)) {
     warning("`regcoef` parcel not found in rme object. Cannot run `rme_ev_coef_se_match`.")
     return(NULL)
  }
  regcoef_df = rme$parcels$regcoef$regcoef %>%
    select(runid, cterm, reg_coef = coef, reg_se = se, reg_t = t, reg_p = p) %>%
    filter(!is.na(runid))

  # 5. Create all potential matches between table pairs and regcoef entries
  all_potential_matches = mapped_pairs %>%
    left_join(regcoef_df, by = "runid", relationship = "many-to-many") %>%
    filter(!is.na(reg_coef))

  if (NROW(all_potential_matches) == 0) {
    return(rme_df_descr(tibble::tibble(), "No regcoef entries for mapped runids."))
  }

  # 6. Score each potential match
  matches_with_scores = all_potential_matches %>%
    mutate(
      rel_dist_coef = abs(reg_coef - coef_val) / pmax(abs(reg_coef), abs(coef_val), 1e-9),
      rel_dist_se = abs(reg_se - paren_val) / pmax(abs(reg_se), abs(paren_val), 1e-9),
      rel_dist_t = abs(reg_t - paren_val) / pmax(abs(reg_t), abs(paren_val), 1e-9),
      rel_dist_p = abs(reg_p - paren_val) / pmax(abs(reg_p), abs(paren_val), 1e-9)
    ) %>%
    rowwise() %>%
    mutate(min_rel_dist_paren = min(c(rel_dist_se, rel_dist_t, rel_dist_p), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      min_rel_dist_paren = ifelse(is.infinite(min_rel_dist_paren), NA, min_rel_dist_paren),
      coef_match_quality = case_when(
        is.na(rel_dist_coef) ~ 0,
        rel_dist_coef <= rel_tol_rounding ~ 2,
        rel_dist_coef <= rel_tol_mismatch ~ 1,
        TRUE ~ 0
      ),
      paren_match_quality = case_when(
        is.na(min_rel_dist_paren) ~ 0,
        min_rel_dist_paren <= rel_tol_rounding ~ 2,
        min_rel_dist_paren <= rel_tol_mismatch ~ 1,
        TRUE ~ 0
      ),
      match_score = coef_match_quality + paren_match_quality
    )

  # 7. For each table pair, find the best `regcoef` match
  best_matches = matches_with_scores %>%
    group_by(map_version, coef_cellid) %>%
    filter(match_score == max(match_score)) %>%
    mutate(combined_dist = rel_dist_coef + min_rel_dist_paren) %>%
    filter(combined_dist == min(combined_dist, na.rm=TRUE)) %>%
    slice(1) %>%
    ungroup()

  # 8. Generate issues based on the quality of the best match
  issues_from_best = best_matches %>%
    filter(match_score < 4 & match_score > 0) %>% # Imperfect but not total failure
    rowwise() %>%
    mutate(
      best_paren_type = c("se", "t", "p")[which.min(c(rel_dist_se, rel_dist_t, rel_dist_p))],
      best_paren_val = get(paste0("reg_", best_paren_type))
    ) %>%
    ungroup() %>%
    mutate(
      issue = case_when(
        coef_match_quality == 0 ~ "no_coef_match",
        coef_match_quality == 1 ~ "large_discrepancy",
        paren_match_quality == 0 ~ "no_paren_match",
        paren_match_quality == 1 ~ "large_discrepancy",
        TRUE ~ NA_character_
      ),
      issue_cellid = if_else(coef_match_quality < 2, coef_cellid, se_cellid),
      partner_cellid = if_else(issue_cellid == coef_cellid, se_cellid, coef_cellid),
      table_val = if_else(issue_cellid == coef_cellid, coef_val, paren_val),
      closest_reg_val = if_else(issue_cellid == coef_cellid, reg_coef, best_paren_val),
      details = if_else(issue_cellid == coef_cellid,
        paste0("Coef rel diff ", round(rel_dist_coef,3), ". Paren match quality: ", paren_match_quality),
        paste0("Paren rel diff ", round(min_rel_dist_paren,3)," to ", best_paren_type, " ", round(best_paren_val, 4))
      )
    ) %>%
    select(map_version, tabid, runid, cellid = issue_cellid, partner_cellid, issue, table_val, closest_reg_val, details)

  # 9. Identify pairs that had no match at all (max score was 0)
  all_pairs_to_check = mapped_pairs %>% select(map_version, coef_cellid) %>% distinct()
  pairs_with_any_match = best_matches %>% filter(match_score > 0) %>% select(map_version, coef_cellid) %>% distinct()
  no_match_pairs = all_pairs_to_check %>% anti_join(pairs_with_any_match, by = c("map_version", "coef_cellid"))

  no_match_issues = tibble::tibble()
  if(NROW(no_match_pairs) > 0) {
    closest_misses = no_match_pairs %>%
      inner_join(matches_with_scores, by = c("map_version", "coef_cellid")) %>%
      mutate(combined_dist = rel_dist_coef + min_rel_dist_paren) %>%
      group_by(map_version, coef_cellid) %>%
      filter(combined_dist == min(combined_dist, na.rm=TRUE)) %>%
      slice(1) %>%
      ungroup()

    no_match_issues = closest_misses %>%
      mutate(
        issue = "no_coef_match",
        partner_cellid = se_cellid,
        table_val = coef_val,
        closest_reg_val = reg_coef,
        details = paste0("No match found. Closest coef rel diff ", round(rel_dist_coef,3),
                         ", closest paren rel diff ", round(min_rel_dist_paren,3))
      ) %>%
      select(map_version, tabid, runid, cellid = coef_cellid, partner_cellid, issue, table_val, closest_reg_val, details)
  }

  issues = dplyr::bind_rows(issues_from_best, no_match_issues)
  return(rme_df_descr(issues, "Issues from comparing table coef/se values with regcoef output."))
}
