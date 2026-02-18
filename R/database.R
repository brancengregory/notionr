new_database <- function(x) {
  x$properties <- x$properties |>
    purrr::map(~ {
      new_database_property(
        id = .x$id,
        name = .x$name,
        description = .x$description,
        type = .x$type
      )
    })

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

database_contents <- function(x) {
  temp <- x[[1]]

  temp |>
    purrr::discard(is.null) |>
    tibble::enframe()
}

create_database <- function(parent, title = NULL, properties) {
  notion_request(
    endpoint = "/databases",
    method = "POST",
    body = list(
      parent = parent,
      title = title,
      properties = properties
    )
  )
}

query_database <- function(id, filters = NULL, sorts = NULL, start_cursor = NULL, page_size = NULL) {

  if (!is.null(filters)) {
    property_filter <- list(
      "property" = filter_properties,
      "text" = list(
        "contains" = filter
      )
    )
  }

  res <- notion_request(
    endpoint = paste0("/databases/", id, "/query"),
    method = "POST"
  )

  new_database(res)
}

get_database <- function(database_id) {
  res <- notion_request(
    endpoint = paste0("/databases/", database_id),
    method = "GET"
  )

  db <- new_database(res)

  return(db)
}

update_database <- function(database_id, title = NULL, description = NULL, properties = NULL) {
  db <- notion_request(
    endpoint = paste0("/databases/", database_id),
    method = "PATCH",
    body = list(
      title = title,
      description = description,
      properties = properties
    )
  )

  new_database(db)
}

#' @export
format.notion_database <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h1("{x$icon$emoji} {x$title[[1]]$plain_text}")
    cli::cli_text("{.href {x$url}}")
    cli::cli_h2("Information")
    cli::cli_dl(
      items = c(
        "ID" = x$id,
        "Created" = x$created_time,
        "Created by" = x$created_by$id,
        "Last edited" = x$last_edited_time,
        "Last edited by" = x$last_edited_by$id
      )
    )
    cli::cli_h2("Properties")
    cli::cli_dl(
      items = setNames(
        purrr::map_chr(db$properties, "type") |> unname(),
        purrr::map_chr(db$properties, "name") |> unname()
      )
    )
  })
}

#' @export
print.notion_database <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
