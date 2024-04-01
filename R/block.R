new_block <- function(x) {
  structure(x, class = "notion_block")
}

validate_block <- function(x) {
  stopifnot(
    inherits(x, "notion_block"),
    x$object == "block",
    !is.null(x$id),
    is.character(x$id),
  )

  return(x)
}

block_type <- function(x) {
  stopifnot(inherits(x, "notion_block"))

  bt <- x$type

  if (!bt %in% c(
    "bookmark",
    "breadcrumb",
    "bulleted_list_item",
    "callout",
    "child_database",
    "child_page",
    "column",
    "column_list",
    "divider",
    "embed",
    "equation",
    "file",
    "heading_1",
    "heading_2",
    "heading_3",
    "image",
    "link_preview",
    "link_to_page",
    "numbered_list_item",
    "paragraph",
    "pdf",
    "quote",
    "synced_block",
    "table",
    "table_of_contents",
    "table_row",
    "template",
    "to_do",
    "toggle",
    "unsupported",
    "video"
  )) {
    stop("Invalid block type")
  }

  return(bt)
}

append_block_children <- function(block_id, children, after = NULL) {
  ch <- notion_request(
    endpoint = paste0("/blocks/", block_id, "/children"),
    method = "PATCH"
  )

  ch
}

get_block <- function(block_id) {
  b <- notion_request(
    endpoint = paste0("/blocks/", block_id),
    method = "GET"
  )

  new_block(b)
}

get_block_children <- function(block_id) {
  notion_request(
    endpoint = paste0("/blocks/", block_id, "/children"),
    method = "GET"
  )
}

update_block <- function(block_id, archived, ...) {
  b <- notion_request(
    endpoint = paste0("/blocks/", block_id),
    method = "GET",
    body = list(
      archived = archived
    )
  )

  new_block(b)
}

delete_block <- function(block_id) {
  b <- notion_request(
    endpoint = paste0("/blocks/", block_id),
    method = "DELETE"
  )

  new_block(b)
}


