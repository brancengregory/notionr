new_block <- function(x, ...) {
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

#' @export
format.notion_block <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h1("{x$type}")
    cli::cli_dl(
      items = c(
        "ID" = x$id,
        "Type" = x$type
      )
    )
  })
}

#' @export
print.notion_block <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

block_bookmark <- function(
    caption,
    url
) {}

block_breadcrumb <- function() {}

block_bulleted_list_item <- function(
    rich_text = NULL,
    color = NULL,
    children = NULL
) {}

block_callout <- function(
    rich_text = NULL,
    icon = NULL,
    color = NULL
) {}

block_child_database <- function(
    title
) {}

block_child_page <- function(
    title
) {}

block_code <- function(
    caption = NULL,
    rich_text = NULL,
    language = NULL
) {}

block_column_list <- function() {}

block_column <- function() {}

block_divider <- function() {}

block_embed <- function(
    url
) {}

block_equation <- function(
    expression
) {}

block_file <- function(
    caption = NULL,
    type = NULL,
    file = NULL,
    name = NULL
) {}

block_heading_1 <- function(
    rich_text = NULL,
    color = NULL,
    is_toggleable = NULL
) {}

block_heading_2 <- function(
    rich_text = NULL,
    color = NULL,
    is_toggleable = NULL
) {}

block_heading_3 <- function(
    rich_text = NULL,
    color = NULL,
    is_toggleable = NULL
) {}

block_image <- function(
    file = NULL,
    caption = NULL,
    width = NULL,
    height = NULL
) {}

block_link_preview <- function(
    url
) {}

block_mention <- function(
    type,
    ...
) {}

block_numbered_list_item <- function(
    rich_text = NULL,
    color = NULL,
    children = NULL
) {}

block_paragraph <- function(
    rich_text = NULL,
    color = NULL,
    children = NULL
) {}

block_pdf <- function(
    caption = NULL,
    type = NULL,
    ...
) {}

block_quote <- function(
    rich_text = NULL,
    color = NULL,
    children = NULL
) {}

block_synced_block <- function(
    synced_from = NULL
) {}

block_table <- function(
    table_width = NULL,
    has_column_header = NULL,
    has_row_header = NULL
) {}

block_table_row <- function(cells = NULL) {}

block_table_of_contents <- function(color = NULL) {}

block_to_do <- function(
    rich_text = NULL,
    checked = NULL,
    color = NULL,
    children = NULL
) {}

block_toggle <- function(
    rich_text = NULL,
    color = NULL,
    children = NULL
) {}

block_video <- function() {}


