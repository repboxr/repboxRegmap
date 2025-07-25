add_missing_cols = function(df, ...) {
  vals = list(...)
  for (col in names(vals)) {
    if (!col %in% names(df)) {
      df[[col]] = rep(vals[[col]], NROW(df))
    }
  }
  df
}

add_missing_cols_and_na_val = function(df, ...) {
  vals = list(...)
  for (col in names(vals)) {
    if (!col %in% names(df)) {
      df[[col]] = rep(vals[[col]], NROW(df))
    } else {
      na_rows = which(is.na(df[[col]]))
      if (length(na_rows)>0) {
        df[[col]][na_rows] = vals[[col]]
      }
    }
  }
  df
}

unnest_comma_string_col = function(df, comma_string_col,sep=",") {
  restore.point("unnest_comma_string_col")

  # Handle case where column does not exist
  if (!comma_string_col %in% colnames(df)) {
    return(df)
  }

  col_vec = df[[comma_string_col]]
  # Treat NA as empty string for consistent processing
  col_vec[is.na(col_vec)] = ""

  # Split strings into a list of character vectors
  s_list = stringi::stri_split_fixed(col_vec, sep)

  # For each element in the list, trim whitespace from its components
  # and remove any resulting empty strings.
  # s_list = lapply(s_list, function(x) {
  #   x = stringi::stri_trim_both(x)
  #   x = x[x != ""]
  #   x
  # })

  # Get the number of non-empty components for each original row
  reps = vapply(s_list, length, integer(1))

  # Replicate the original data frame rows according to the number of elements
  res_df = df[rep(seq_len(nrow(df)), reps), , drop = FALSE]

  # Get the unlisted, non-empty values
  new_col = unlist(s_list)

  # Replace the original comma-separated column with the unnested values
  res_df[[comma_string_col]] = new_col

  # Reset row names for the new data frame
  rownames(res_df) = NULL

  res_df
}

# Helper to convert a data frame to a markdown table
df_to_markdown = function(df) {
  if (!is.data.frame(df) || NROW(df) == 0) return("")

  # Clean strings for markdown and handle list columns
  clean_val = function(v) {
    if (is.list(v)) return("[[...]]")
    s = as.character(v)
    s[is.na(s)] = ""
    s = stringi::stri_replace_all_fixed(s, "|", "|")
    s = stringi::stri_replace_all_regex(s, "[\r\n]+", " ")
    s
  }

  df_char = as.data.frame(lapply(df, function(col) sapply(col, clean_val)), stringsAsFactors = FALSE)
  names(df_char) = names(df)

  header = paste0("| ", paste(names(df_char), collapse = " | "), " |")
  separator = paste0("|", paste(rep("---", ncol(df_char)), collapse = "|"), "|")
  body_rows = apply(df_char, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  paste(c(header, separator, body_rows), collapse = "\n")
}
