get_block <- function(id) {
  endpoint <- paste0("blocks/", id)

  res <- notion_request(
    endpoint = endpoint,
    method = "GET"
  )

  res
}

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





