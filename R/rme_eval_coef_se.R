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
#' A match is considered "perfect" if the regression output value, when rounded to the
#' number of decimal places shown in the table, is identical to the table value.
#'
#' Based on the best match, it reports several types of issues:
#' - `no_coef_match`: The coefficient from the table does not match any coefficient
#'   in the corresponding `regcoef` output for that `runid` within a given tolerance.
#' - `no_match_perhaps_wrong_sign`: A coefficient match is only found if the sign of the
#'   table value is flipped. This suggests a potential transcription error (e.g., a missing minus sign).
#' - `no_paren_match`: The coefficient matched perfectly, but the value in parentheses
#'   does not match the corresponding standard error, t-statistic, or p-value.
#' - `rounding_error`: A match was found (i.e., not a perfect match after rounding), but the relative difference
#'   is smaller than a mismatch tolerance. This can point to minor transcription errors or unusual rounding.
#'
#' @param rme The rme object.
#' @param rel_tol_rounding The relative tolerance for what is considered a rounding error. (This is now superseded by decimal-based rounding but kept for legacy reasons).
#' @param rel_tol_mismatch The relative tolerance for what is considered a mismatch.
#' @return A data frame of identified issues.
rme_ev_coef_se_match = function(rme) {
  restore.point("rme_ev_coef_se_match")

  long_descr = "**Value Mismatch between Table and Code.** This is a core value-based check. It compares numeric values from the table (identified as coefficient/standard error pairs) against the results from the mapped regression's `regcoef` output. A match is considered perfect if the code output, rounded to the number of decimal places shown in the table, equals the table value. Issues can be `no_coef_match` (table coef not in output within tolerance), `no_match_perhaps_wrong_sign` (a match is found if the sign is flipped), `no_paren_match` (SE/t-stat/p-val mismatch), or `rounding_error` (the values are very close but don't match exactly after rounding, suggesting a minor discrepancy)."

  # 1. Get all coef-se pairs from the tables, including tabid and decimal places
  cell_info_df = rme$cell_df %>% select(cellid, num, num_deci)

  pairs_df = rme$cell_df %>%
    filter(reg_role == "se", !is.na(partner_cellid)) %>%
    select(tabid, se_cellid = cellid, coef_cellid = partner_cellid) %>%
    # Join for SE info (paren value)
    left_join(cell_info_df, by = c("se_cellid" = "cellid")) %>%
    rename(paren_val = num, paren_deci = num_deci) %>%
    # Join for Coef info
    left_join(cell_info_df, by = c("coef_cellid" = "cellid")) %>%
    rename(coef_val = num, coef_deci = num_deci)

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
    return(rme_df_descr(tibble::tibble(), "No coef/se pairs found to check.", test_type = "flag", long_descr = long_descr))
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
    return(rme_df_descr(tibble::tibble(), "No regcoef entries for mapped runids.", test_type = "flag", long_descr = long_descr))
  }

  # 6. Score each potential match
  matches_with_scores = all_potential_matches %>%
    mutate(
      # Default NA decimal places to 0 (for integers)
      coef_deci = tidyr::replace_na(coef_deci, 0),
      paren_deci = tidyr::replace_na(paren_deci, 0),

      # Check for perfect matches based on rounding to table's decimal places
      is_perfect_coef_match = (round(reg_coef, coef_deci) == coef_val),
      is_perfect_sign_flip_match = (round(reg_coef, coef_deci) == -coef_val),
      is_perfect_se_match = (round(reg_se, paren_deci) == paren_val),
      is_perfect_t_match = (round(reg_t, paren_deci) == paren_val),
      is_perfect_p_match = (round(reg_p, paren_deci) == paren_val),

      # Relative distances are still needed for reporting on misses
      rel_dist_coef = abs(reg_coef - coef_val) / pmax(abs(reg_coef), abs(coef_val), 1e-9),
      rel_dist_se = abs(reg_se - paren_val) / pmax(abs(reg_se), abs(paren_val), 1e-9),
      rel_dist_t = abs(reg_t - paren_val) / pmax(abs(reg_t), abs(paren_val), 1e-9),
      rel_dist_p = abs(reg_p - paren_val) / pmax(abs(reg_p), abs(paren_val), 1e-9)
    ) %>%
    rowwise() %>%
    mutate(
      min_rel_dist_paren = min(c(rel_dist_se, rel_dist_t, rel_dist_p), na.rm = TRUE),
      is_perfect_paren_match = any(c(is_perfect_se_match, is_perfect_t_match, is_perfect_p_match), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      min_rel_dist_paren = ifelse(is.infinite(min_rel_dist_paren), NA, min_rel_dist_paren),
      # Score: 2 for perfect match, 1 for close match (rounding error), 0 for mismatch.
      # A "rounding error" is a non-perfect match where the absolute difference
      # is within two units of the last decimal place.
      is_rounding_coef = !is_perfect_coef_match & (abs(reg_coef - coef_val) < (2 * 10^(-coef_deci))),

      coef_match_quality = case_when(
        is_perfect_coef_match ~ 2,
        is_rounding_coef ~ 1,
        TRUE ~ 0
      ),

      # For parenthesis value, check if any candidate (se, t, p) qualifies as a rounding error.
      is_rounding_paren = !is_perfect_paren_match & any(
          (abs(reg_se - paren_val) < (2 * 10^(-paren_deci))),
          (abs(reg_t - paren_val) < (2 * 10^(-paren_deci))),
          (abs(reg_p - paren_val) < (2 * 10^(-paren_deci))),
          na.rm = TRUE
      ),

      paren_match_quality = case_when(
        is.na(min_rel_dist_paren) ~ 0,
        is_perfect_paren_match ~ 2,
        is_rounding_paren ~ 1,
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
      is_sign_flip_match = (coef_match_quality == 0) & is_perfect_sign_flip_match,
      issue = case_when(
        is_sign_flip_match ~ "no_match_perhaps_wrong_sign",
        coef_match_quality == 0 ~ "no_coef_match",
        coef_match_quality == 1 ~ "rounding_error",
        paren_match_quality == 0 ~ "no_paren_match",
        paren_match_quality == 1 ~ "rounding_error",
        TRUE ~ NA_character_
      ),
      issue_cellid = if_else(coef_match_quality < 2, coef_cellid, se_cellid),
      partner_cellid = if_else(issue_cellid == coef_cellid, se_cellid, coef_cellid),
      table_val = if_else(issue_cellid == coef_cellid, coef_val, paren_val),
      true_val_cand = if_else(issue_cellid == coef_cellid, reg_coef, best_paren_val),
      details = case_when(
        is_sign_flip_match ~ paste0("Perfect match if sign is flipped. Paren match quality: ", paren_match_quality),
        issue_cellid == coef_cellid ~ paste0("Coef rel diff ", round(rel_dist_coef,3), ". Paren match quality: ", paren_match_quality),
        TRUE ~ paste0("Paren rel diff ", round(min_rel_dist_paren,3)," to ", best_paren_type, " ", round(best_paren_val, 4))
      )
    ) %>%
    select(map_version, tabid, runid, cellid = issue_cellid, partner_cellid, issue, table_val, true_val_cand, details)

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
        is_sign_flip_match = is_perfect_sign_flip_match,
        issue = if_else(is_sign_flip_match, "no_match_perhaps_wrong_sign", "no_coef_match"),
        partner_cellid = se_cellid,
        table_val = coef_val,
        true_val_cand = reg_coef,
        details = if_else(is_sign_flip_match,
                         paste0("Perfect match if sign is flipped. Paren rel diff ", round(min_rel_dist_paren,3)),
                         paste0("No match found. Closest coef rel diff ", round(rel_dist_coef,3),
                                ", closest paren rel diff ", round(min_rel_dist_paren,3)))
      ) %>%
      select(map_version, tabid, runid, cellid = coef_cellid, partner_cellid, issue, table_val, true_val_cand, details)
  }

  issues = dplyr::bind_rows(issues_from_best, no_match_issues) %>%
    left_join(rme$mc_df %>% select(map_version, reg_ind, cellid, runid) %>% unique(), by= c("map_version","cellid","runid"))
  return(rme_df_descr(issues, "Issues from comparing table coef/se values with regcoef output.", test_type = "flag", long_descr = long_descr))
}
