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




