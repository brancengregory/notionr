#' @export
is_empty_notion_database_export <- function(dataframe){
  if( (nrow(dataframe) == 1 & names(dataframe)[1] == "results" & dataframe[1,1] == "none") ){ TRUE }else{ FALSE }
}
