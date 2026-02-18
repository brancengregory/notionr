new_page_property <- function(
    id = NULL,
    name = NULL,
    description = NULL,
    type = NULL,
    ...
) {
  x <- list(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = type,
    ...
  )

  structure(
    x,
    class = c(
      paste0("notion_property_", x$type),
      "notion_page_property"
    )
  )
}

validate_page_property <- function(x) {
  stopifnot(
    inherits(x, "notion_page_property"),
    !is.null(x$id),
    !is.null(x$name),
    !is.null(x$description),
    !is.null(x$type),
    is.character(x$id),
    is.character(x$name),
    is.character(x$description),
    is.character(x$type)
  )

  if (!x$type %in% c(
    "checkbox",
    "created_by",
    "created_time",
    "date",
    "email",
    "files",
    "formula",
    "last_edited_by",
    "last_edited_time",
    "multi_select",
    "number",
    "people",
    "phone_number",
    "relation",
    "rich_text",
    "rollup",
    "select",
    "status",
    "title",
    "url"
  )) {
    stop("Invalid page property type")
  }

  return(x)
}

page_property_type <- function(x) {
  stopifnot(inherits(x, "notion_page_property"))

  return(x$type)
}

property_checkbox <- function(id, name, description) {
  new_page_property(
    list(
      "id" = id,
      "name" = name,
      "description" = description,
      "type" = "checkbox",
      "checkbox" = list()
    )
  )
}

property_created_by <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "created_by",
    "created_by" = list()
  )
}

property_created_time <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "created_time",
    "created_time" = list()
  )
}

property_date <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "date",
    "date" = list()
  )
}

property_email <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "email",
    "email" = list()
  )
}

property_files <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "files",
    "files" = list()
  )
}

property_formula <- function(id, name, description, expression) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "formula",
    "formula" = list(
      "expression" = expression
    )
  )
}

property_last_edited_by <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "last_edited_by",
    "last_edited_by" = list()
  )
}

property_last_edited_time <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "last_edited_time",
    "last_edited_time" = list()
  )
}

property_multi_select_option <- function(id, name, color = NULL) {
  stopifnot(is.character(name))

  x <- list(
    "id" = id,
    "name" = name,
    "color" = color
  )

  structure(x, class = "notion_multi_select_option")
}

property_multi_select <- function(id, name, description, options) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "multi_select",
    "multi_select" = list(
      "options" = options
    )
  )
}

property_number_formats <- c(
  "argentine_peso",
  "baht",
  "australian_dollar",
  "canadian_dollar",
  "chilean_peso",
  "colombian_peso",
  "danish_krone",
  "dirham",
  "dollar",
  "euro",
  "forint",
  "franc",
  "hong_kong_dollar",
  "koruna",
  "krona",
  "leu",
  "lira",
  "mexican_peso",
  "new_taiwan_dollar",
  "new_zealand_dollar",
  "norwegian_krone",
  "number",
  "number_with_commas",
  "percent",
  "philippine_peso",
  "pound",
  "peruvian_sol",
  "rand",
  "real",
  "ringgit",
  "riyal",
  "ruble",
  "rupee",
  "rupiah",
  "shekel",
  "singapore_dollar",
  "uruguayan_peso",
  "yen",
  "yuan",
  "won",
  "zloty"
)

property_number <- function(id, name, description, format) {
  stopifnot(format %in% property_number_formats)

  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "number",
    "number" = list(
      "format" = format
    )
  )
}

property_people <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "people",
    "people" = list()
  )
}

property_phone_number <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "phone_number",
    "phone_number" = list()
  )
}

property_relation <- function(
    id,
    name,
    description,
    database_id = NULL,
    synced_property_name = NULL,
    synced_property_id = NULL
) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "relation",
    "relation" = list(
      "database_id" = database_id,
      "synced_property_name" = synced_property_name,
      "synced_property_id" = synced_property_id
    )
  )
}

property_rich_text <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "rich_text",
    "rich_text" = list()
  )
}

property_rollup_functions <- c(
  "average",
  "checked",
  "count_per_group",
  "count",
  "count_values",
  "date_range",
  "earliest_date",
  "empty",
  "latest_date",
  "max",
  "median",
  "min",
  "not_empty",
  "percent_checked",
  "percent_empty",
  "percent_not_empty",
  "percent_per_group",
  "percent_unchecked",
  "range",
  "unchecked",
  "unique",
  "show_original",
  "show_unique",
  "sum"
)

property_rollup <- function(
    id,
    name,
    description,
    function_name = NULL,
    relation_property_id = NULL,
    relation_property_name = NULL,
    rollup_property_id = NULL,
    rollup_property_name = NULL
) {
  stopifnot(function_name %in% property_rollup_functions)

  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "rollup",
    "rollup" = list(
      "rollup_property_name" = rollup_property_name,
      "relation_property_name" = relation_property_name,
      "rollup_property_id" = rollup_property_id,
      "relation_property_id" = relation_property_id,
      "function" = function_name
    )
  )
}

property_select_option <- function(id, name, color = NULL) {
  stopifnot(color %in% notion_colors)

  x <- list(
    "id" = id,
    "name" = name,
    "color" = color
  )

  structure(x, class = "notion_select_option")
}

property_select <- function(id, name, description, options) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "select",
    "select" = list(
      "options" = options
    )
  )
}

property_status_option <- function(id, name, color = NULL) {
  stopifnot(color %in% notion_colors)

  x <- list(
    "id" = id,
    "name" = name,
    "color" = color
  )

  structure(x, class = "notion_status_option")
}

property_status_group <- function(id, name, color, option_ids) {
  x <- list(
    "name" = name,
    "status" = status_options
  )

  structure(x, class = "notion_status_group")
}

property_status <- function(id, name, description, color, options, groups) {
  stopifnot(color %in% notion_colors)

  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "status",
    "status" = list(
      "options" = options,
      "groups" = groups
    )
  )
}

property_title <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "title",
    "title" = list()
  )
}

property_url <- function(id, name, description) {
  new_page_property(
    "id" = id,
    "name" = name,
    "description" = description,
    "type" = "url",
    "url" = list()
  )
}

#' @export
format.notion_page_property <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_h3(x$name)
    cli::cli_dl(
      items = list(
        "Type" = x$type,
        "ID" = x$id,
        "Description" = x$description
      )
    )
  })
}

#' @export
print.notion_page_property <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
