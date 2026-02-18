new_query_filter <- function(x) {
  structure(x, class = "notion_query_filter")
}

validate_page <- function(x) {
  stopifnot(
    inherits(x, "notion_query_filter")
  )

  return(x)
}
