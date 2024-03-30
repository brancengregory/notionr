#' Get a Page
#'
#' Gets a Notion Page
#'
#'
#' @author Eduardo Flores
#' @return list of response
#'
#' @param secret Notion API token
#' @param id page id
#'
#'
#' @importFrom httr PATCH
#' @importFrom httr content
#' @importFrom httr content_type
#' @export
get_notion_page <- function(secret, id) {
  url <- paste0("https://api.notion.com/v1/pages/", id)

  res <- notion_request(url)
}




