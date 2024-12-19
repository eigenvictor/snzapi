
setClass(
  "API_conn",
  representation(
    resourceId = "character",
    version = "character",
    metadata = "list",
    filters = "list",
    format = "character",
    url = "character"
  )
)

#' Set up connection to a data series in the ADE
#'
#' Create an S4 object to act as the basis of an API query
#'
#' @param resourceId the resourceId for a data series.
#' This can be found in the data explorer, or using the search_data_series() or get_all_data_series() functions.
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#'
#' @export
connect_to_ade <- function(resourceId, version = NULL, format = "csv") {

  if(!format %in% c("xml", "csv")) stop(sprintf("Format %s not currently accepted, please use xml or csv", format))

  conn <- new("API_conn", resourceId = resourceId, format = format)

  conn@metadata = get_ade_metadata(resourceId, version)

  conn@version = conn@metadata$series$version

  vars <- get_variables(conn)

  conn@filters = as.list(rep("", length(vars)))
  names(conn@filters) = vars

  conn@url = paste(c("https://api.data.stats.govt.nz/rest/data/STATSNZ", resourceId, conn@version), collapse = ",")

  return(conn)
}


#' Print
#'
#' Prints the name of the series of an API_conn object
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @export
print.API_conn <- function(conn) {
  print(conn@resourceId)
}

#' Remove filter from API query
#'
#' Removes a filter from API query to the ADE
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param var The name of a variable with a filter currently applied by add_filters()
#'
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' conn <- add_filters(conn, sex = c("SEX1", "SEX2"), age = c("AGE1539", "AGE4064"))
#' data <- collect(conn)
#' conn <- remove_filter(conn, var = "sex")
#'
#'
#' @export
remove_filter <- function(conn, var) {
  current_filters <- conn@filters

  current_filters[[var]] <- ""

  conn@filters <- current_filters

  return(conn)
}

#' Add filters to API query
#'
#' Filters API query to only return a subset of records
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param ... a named list of vectors, where the name is the variable the filter applies to, and the vector contains the filter values
#'
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' conn <- add_filters(conn, sex = c("SEX1", "SEX2"), age = c("AGE1539", "AGE4064"))
#'
#'
#' @export
add_filters <- function(conn, ...) {

  dots <- list(...)

  for(x in names(dots)) {
    conn <- add_filter(conn, x, dots[[x]])
  }

  return(conn)
}

#' Add filter to API query
#'
#' Filters API query to only return a subset of records
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param var The variable to apply value filters to
#' @param values A vector of values for var to take
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' conn <- add_filter(conn, var = "sex", values = c("SEX1", "SEX2"))
#'
#'
#' @export
add_filter <- function(conn, var, values) {
  if(!var %in% get_variables(conn)) stop(sprintf("%s not a valid variable in %s", var, conn))

  if(is.null(values)) {
    warning("NULL values provided. To remove a filter, use remove_filters()")
    remove_filter(conn, var)
  }

  current_filters <- conn@filters

  if(length(current_filters[[var]]) > 1 | current_filters[[var]][[1]] != "") {
    current_var_filter <- current_filters[[var]]
    warning(sprintf("Overwriting existing filter for %s, previous: %s", var, paste(values, collapse = ",")))
  }

  current_filters[[var]] <- values

  conn@filters <- current_filters

  return(conn)
}

#' Build filter string
#'
#' Turn given filters for an API query into a string to be used in the query URL
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#'
#' @export
build_filter_string <- function(conn) {
  coordinate_order <- conn@metadata$concepts$name
  coordinate_order <- coordinate_order[1:(length(coordinate_order)-1)]

  filters <- conn@filters[coordinate_order]

  filters <- sapply(X=filters, FUN = \(x) paste0(x, collapse = "+"))
  filter_string <- paste(filters, collapse = ".")

  return(filter_string)
}

#' Collect data from API
#'
#' Executes the API query generated for the series
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param add_labels Add text labels for variables?
#' @param drop_codes If adding text labels, remove the code version of variables?
#'
#' @returns A data.table object
#'
#' @export
collect.API_conn <- function(conn, add_labels = F, drop_codes = F) {
  url <- sprintf(
    "%s/%s?dimensionAtObservation=AllDimensions&format=%s",
    conn@url, build_filter_string(conn), conn@format
  )

  headers <- get_ade_query_header()

  data <- if(conn@format == "xml") {
    as.data.frame(rsdmx::readSDMX(url, headers = headers))
  } else if(conn@format == "csv") {
    res <- httr::content(
      httr::GET(url, httr::add_headers(headers)),
      as = "text"
    )
    data.table::fread(res, header = T)
  }

  names(data) <- clean_ade_names(
    names(data),
    resourceId = conn@resourceId
  )

  if(add_labels) data <- add_ade_value_labels(data, conn, drop_codes = drop_codes)

  data$dataflow <- NULL

  return(data)
}

#' Collect
#'
#' @export
collect <- function(x, ...) {
  UseMethod("collect")
}
