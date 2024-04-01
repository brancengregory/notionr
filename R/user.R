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
