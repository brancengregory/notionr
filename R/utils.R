list_to_tibble <- function(x) {
  if (rlang::is_list(x) && purrr::pluck_depth(x) == 2) {
    # Convert to tibble if it's a list of atomic vectors
    tibble::as_tibble(x)
  } else if (rlang::is_list(x) && purrr::pluck_depth(x) > 2) {
    # If it's a deeper list (lists of lists), attempt to convert each sublist
    purrr::map(x, function(.x) {
      if (rlang::is_list(.x)) {
        tibble::as_tibble(.x)
      } else {
        # For non-list elements, return the element itself
        .x
      }
    }) |>
      # Optionally, combine all sublists into a single tibble if structure allows
      dplyr::bind_rows()
  } else {
    # Return the original value if not a list or doesn't meet the depth conditions
    x
  }
}
