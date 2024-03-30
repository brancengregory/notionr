base_url <- "https://api.notion.com/v1"

notion_request <- function(endpoint, method = "GET", body = NULL, ...,  all_pages = FALSE, base_url = base_url) {
  key <- cached_access_code() # Assumes cached_access_code() implementation

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

  results <- list()
  cursor <- NULL

  repeat {
    if (!is.null(cursor)) {
      req <- req |> httr2::req_url_query("start_cursor" = cursor)
    }

    resp <- httr2::req_perform(req)
    resp_content <- httr2::resp_body_json(resp)

    results <- c(results, list(resp_content$results))
    if (!all_pages || !resp_content$has_more) break
    cursor <- resp_content$next_cursor
  }

  do.call("c", results)
}
