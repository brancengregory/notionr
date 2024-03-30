get_block <- function(id) {
  endpoint <- paste0("blocks/", id)

  res <- notion_request(
    endpoint = endpoint,
    method = "GET"
  )

  res
}
