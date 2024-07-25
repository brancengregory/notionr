new_page <- function(x) {
  structure(x, class = "notion_page")
}

validate_page <- function(x) {
  stopifnot(
    inherits(x, "notion_page"),
    x$object == "page",
    !is.null(x$id),
    is.character(x$id),
  )

  return(x)
}

new_page_property <- function(x) {
  structure(x, class = "notion_page_property")
}

validate_page_property <- function(x) {
  stopifnot(
    inherits(x, "notion_page_property"),
    !is.character(x$id)
  )

  if (!x$type %in% c(
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
    "rollup",
    "rich_text",
    "select",
    "status",
    "title",
    "url",
    "unique_id",
    "verification"
  )) {
    stop("Invalid page property type")
  }

  return(x)
}

#' @export
get_page <- function(page_id) {
  p <- notion_request(
    endpoint = paste0("/pages/", page_id),
    method = "GET"
  )

  new_page(p)
}

get_page_property <- function(page_id, property_id) {
  pp <- notion_request(
    endpoint = paste0("/pages/", page_id, "/properties/", property_id)
  )

  new_page_property(pp)
}

create_page <- function(parent, properties, children, icon, cover) {
  if (is.null(parent) || is.null(properties)) {
    stop("`parent` and `properties` arguments must be supplied")
  }

  p <- notion_request(
    endpoint = "/pages",
    method = "POST",
    body = list(
      parent = parent,
      properties = properties,
      children = children,
      icon = icon,
      cover = cover
    )
  )

  new_page(p)
}

update_page_property <- function(page_id, properties = NULL, archived = NULL, icon = NULL, cover = NULL) {
  p <- notion_request(
    endpoint = paste0("/pages/", page_id)
  )

  new_page(p)
}

archive_page <- function(page_id) {
  p <- update_page_property(page_id)

  new_page(p)
}

#' @export
format.notion_page <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h1("{x$icon$emoji} {x$properties$Page$title[[1]]$text$content}")
    cli::cli_text("{.href {x$url}}")
    cli::cli_dl(
      items = c(
        "ID" = x$id,
        "Created Time" = x$created_time,
        "Last Edited Time" = x$last_edited_time,
        "Archived" = x$archived
      )
    )
  })
}

#' @export
print.notion_page <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
