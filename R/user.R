new_user <- function(x) {
  structure(x, class = "notion_user")
}

validate_user <- function(x) {
  x
}

list_users <- function() {
  u <- notion_request(
    endpoint = "/users",
    method = "GET"
  )

  return(u)
}

get_user <- function(user_id) {
  u <- notion_request(
    endpoint = paste0("/users/", user_id),
    method = "GET"
  )

  new_user(u)
}

get_token_user <- function() {
  get_user("me")
}

#' @export
format.notion_user <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h1("{x$name}")
    cli::cli_dl(
      items = c(
        "ID" = x$id,
        "Type" = x$type
      )
    )
  })
}

#' @export
print.notion_user <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
