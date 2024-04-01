new_parent <- function(x = list(), type) {
  parent_type_class_name <- paste0("notion_parent_", type)
  structure(x, class = c(parent_type_class_name, "notion_parent"))
}

new_parent_database <- function(x = list()) {
  new_parent(x, "parent")
}

validate_parent_database <- function(x = list()) {

}

new_parent_page <- function(x = list()) {
  new_parent(x, "page")
}

validate_parent_page <- function(x = list()) {

}

new_parent_block <- function(x = list()) {
 new_parent(x, "block")
}

validate_parent_block <- function(x = list()) {

}

validate_parent <- function(x = list()) {
  stopifnot(
    inherits(x, "notion_parent"),
    !is.null(x$id),
    is.character(x$id),
  )

  pt <- parent_type(x)

  if (!pt %in% c(
    "database",
    "page",
    "workspace",
    "block"
  )) {
    stop("Invalid parent type")
  }

  switch(
    pt,
    database = validate_parent_database(x),
    page = validate_parent_page(x),
    block = validate_parent_block(x),
    workspace = NULL
  )

  return(x)
}

parent_type <- function(x = list()) {
  stopifnot(inherits(x, "notion_parent"))

  attr(x, "type", exact = TRUE)
}

parent_id <- function(x = list()) {
  stopifnot(inherits(x, "notion_parent"))

  pt <- parent_type(x)

  pid <- switch(
    pt,
    database = x$database_id,
    page = x$page_id,
    workspace = NULL,
    block = x$block_id
  )

  return(pid)
}
