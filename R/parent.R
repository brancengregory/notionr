new_parent <- function(x, type) {
  if(!type %in% c(
    "database",
    "page",
    "workspace",
    "block"
  )) {
    stop("Invalid parent type")
  }

  structure(x, class = "notion_parent", type = type)
}

validate_parent <- function(x) {
  stopifnot(
    inherits(x, "notion_parent"),
    !is.null(x$id),
    is.character(x$id),
  )

  pt <- parent_type(x)

  if(!pt %in% c(
    "database",
    "page",
    "workspace",
    "block"
  )) {
    stop("Invalid parent type")
  }

  if(pt == "database") {
    stopifnot(!is.null(x$database_id), is.character(x$database_id))
  } else if(pt == "page") {
    stopifnot(!is.null(x$page_id), is.character(x$page_id))
  } else if(pt == "workspace") {
    stopifnot(!is.null(x$workspace), isTRUE(x$workspace))
  } else if(pt == "block") {
    stopifnot(!is.null(x$block_id), is.character(x$block_id))
  }

  return(x)
}

parent_type <- function(x) {
  stopifnot(inherits(x, "notion_parent"))

  attr(x, "type", exact = TRUE)
}

parent_id <- function(x) {
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
