new_database <- function(x) {
  structure(x, class = "notion_database")
}

validate_database <- function(x) {
  stopifnot(
    inherits(x, "notion_database"),
    x$object == "database",
    !is.null(x$id),
    is.character(x$id),
  )

  return(x)
}

new_database_property <- function(x) {
  structure(x, class = "notion_database_property")
}

validate_database_property <- function(x) {
  stopifnot(
    inherits(x, "notion_database_property"),
    !is.null(x$id),
    is.character(x$id),
  )

  if(!x$type %in% c(
    "checkbox",
    "created_by",
    "created_time",
    "date",
    "email",
    "files",
    "formula",
    "last_edited_by",
    "last_edited_time",
    "multi_select",
    "number",
    "people",
    "phone_number",
    "relation",
    "rich_text",
    "rollup",
    "select",
    "status",
    "title",
    "url"
  )) {
    stop("Invalid database property type")
  }

  return(x)
}

database_property_type <- function(x) {
  stopifnot(inherits(x, "notion_database_property"))

  return(x$type)
}

database_contents <- function(x) {
  temp <- x[[1]]

  temp |>
    purrr::discard(is.null) |>
    tibble::enframe()
}

query_database <- function(id, filter_properties = NULL, filter = NULL, sorts = NULL, start_cursor = NULL, page_size = NULL) {
  res <- notion_request(
    endpoint = paste0("/databases/", id, "/query"),
    method = "POST"
  )

  new_database(res)
}
