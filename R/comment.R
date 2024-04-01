new_comment <- function(x) {
  structure(x, class = "notion_comment")
}

validate_comment <- function(x) {
  x
}

create_comment <- function(parent = NULL, discussion_id = NULL, rich_text) {
  ct <- notion_request(
    endpoint = "/comments",
    method = "POST",
    body = list(
      parent = parent,
      discussion_id = discussion_id,
      rich_text = rich_text
    )
  )

  new_comment(ct)
}

list_comments <- function() {
  notion_request(
    endpoint = "/comments",
    method = "GET"
  )
}


