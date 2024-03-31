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
get_notion_page <- function(id) {
  url <- paste0("/pages/", id)

  res <- notion_request(url)

  return(res)
}




