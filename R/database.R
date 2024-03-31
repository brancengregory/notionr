query_database <- function(id, filter_properties = NULL, filter = NULL, sorts = NULL, start_cursor = NULL, page_size = NULL) {
  res <- notion_request(
    endpoint = paste0("/databases/", id, "/query"),
    method = "POST"
  )

  new_database(res)
}

new_database <- function(x) {
  structure(x, class = "notion_database")
}

validate_database <- function(x) {
}

database_contents <- function(x) {
  temp <- x[[1]]

  temp |>
    purrr::discard(is.null) |>
    purrr::discard_at(c(
      "created_by",
      "properties",
      "parent",
      "last_edited_by"
    )) |>
    tibble::as_tibble()
}
