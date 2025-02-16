# Constructor for search_filter class
new_search_filter <- function(value, property) {
  stopifnot(property == "object",
            value %in% c("page", "database"))
  x <- list("value" = value,
            "property" = property)
  class(x) <- "notionr_search_filter"
  x
}

# Constructor for search_sort class
new_search_sort <- function(timestamp, direction) {
  stopifnot(timestamp == "last_edited_time",
            direction %in% c("ascending", "descending"))
  x <- list("direction" = direction,
            "timestamp" = timestamp)
  class(x) <- "notionr_search_sort"
  x
}

#' Search for databases and pages
#'
#' Search for databases and pages, using filters and sorts to limit which/how
#' pages and databases are returned.
#'
#' @param key Notion access key.
#' @param query A string which limits which pages are returned by comparing the
#'   query to the page title. If `NULL`, no limiting occurs.
#' @param sort A search sort object. If `NULL`, no sorting will occur.
#' @param filter A search filter object. If `NULL` no filtering will occur.
#' @return A list of pages and/or databases.
#' @seealso [search_filter()] and [search_sort()] to see details on how to
#'   correctly construct filter and sort objects.
#' @export
search_workspace <- function(
    query = NULL,
    sort = NULL,
    filter = NULL,
    n_results = 10,
    key = cached_access_code()
) {
  stopifnot(nzchar(query) || is.null(query),
            inherits(sort, "notionr_search_sort") || is.null(sort),
            inherits(filter, "notionr_search_filter") || is.null(filter))

  # Construct POST body
  body <- list()

  if (!is.null(query)) {
    body <- append(body, list("query" = query))
  }

  if (!is.null(sort)) {
    body <- append(body, list("sort" = unclass(sort)))
  }

  if (!is.null(filter)) {
    body <- append(body, list("filter" = unclass(filter)))
  }

  # If n_results < 100, change the paging
  # If equal to 100, no need to change the paging, but don't recurse
  # If greater than 100, recurse until n_results is reached
  if (n_results < 100) {
    body <- append(body, list("page_size" = n_results))
  } else if (n_results > 100) {
    body <- append(body, list("page_size" = 100))
  }

  # If body is empty, set to NULL
  if (identical(body, list())) body <- NULL

  res <- notion_request(
    endpoint = "/search",
    method = "POST",
    body = body,
    key = key
  )

  return(res$results)
}

#' Create a search filter
#'
#' Construct a search filter to filter results returned from the \code{\link{search_workspace}}
#' function. This filter is only meant for use within the \code{\link{search_workspace}}
#' function.
#'
#' @param value Which type of property to filter to. Currently, `"page"` and
#' `"database"` are the only valid options.
#' @param property Which property type to apply the filter to. Currently
#' `"object"` is the only valid option.
#' @return A search filter object.
#' @export
search_filter <- function(value = "page", property = "object") {
  new_search_filter(value = value, property = property)
}

#' Create a search sort
#'
#' Construct a search sort to sort results returned from the \code{\link{search_workspace}}
#' function. This sort is only meant for use within the \code{\link{search_workspace}}
#' function.
#'
#' @param timestamp Which property to sort by. Currently, `"last_edited_time"`
#'   is the only valid option.
#' @param direction Which direction to sort in. Must be either `"descending"` or
#'   `"ascending"`.
#' @return A search sort object.
#' @export
search_sort <- function(timestamp = "last_edited_time", direction = "descending") {
  new_search_sort(timestamp = timestamp, direction = direction)
}
