replace_null_with_na <- function(x) {
  purrr::map(x, ~ if (is.null(.x)) NA else .x)
}

filter_null_columns <- function(x) {
  purrr::map(x, ~ .x[!purrr::map_lgl(.x, is.null)])
}

standardize_keys <- function(x) {
  all_keys <- unique(unlist(purrr::map(x, names)))
  purrr::map(x, function(.x) {
    purrr::map(all_keys, function(key) {
      if (key %in% names(.x)) {
        .x[[key]]
      } else {
        NA
      }
    }) |> rlang::set_names(all_keys)
  })
}

list_to_tibble <- function(x, keep_empty = TRUE) {
  if (rlang::is_list(x) && purrr::pluck_depth(x) == 2) {
    # Apply the appropriate function based on keep_empty argument
    x_processed <- if (keep_empty) replace_null_with_na(x) else filter_null_columns(x)
    x_standardized <- standardize_keys(x_processed)
    tibble::as_tibble(x_standardized)
  } else if (rlang::is_list(x) && purrr::pluck_depth(x) > 2) {
    # Apply the appropriate function to each sublist based on keep_empty argument
    x_processed <- purrr::map(x, function(.x) {
      if (rlang::is_list(.x)) {
        # Apply the appropriate function before converting to tibble
        .x_processed <- if (keep_empty) replace_null_with_na(.x) else filter_null_columns(.x)
        .x_standardized <- standardize_keys(.x_processed)
        tibble::as_tibble(.x_standardized)
      } else {
        # For non-list elements, return the element itself
        .x
      }
    })
    # Optionally, combine all sublists into a single tibble if structure allows
    dplyr::bind_rows(x_processed)
  } else {
    # Return the original value if not a list or doesn't meet the depth conditions
    x
  }
}
