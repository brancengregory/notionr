new_rich_text <- function(x) {
  structure(x, class = "notion_rich_text")
}

validate_rich_text <- function(x) {
  x
}
