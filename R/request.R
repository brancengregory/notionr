notion_request <- function(endpoint, method = "GET", body = NULL, ..., all_pages = FALSE, base_url = "https://api.notion.com/v1") {
  key <- cached_access_code()

  req_url <- paste0(base_url, endpoint)

  req <- httr2::request(req_url) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      `Authorization` = paste("Bearer", key),
      `Notion-Version` = "2022-06-28"
    )

  if (!is.null(body)) {
    req <- httr2::req_body_json(body)
  }

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  return(res)
}
