
.api_env <- rlang::env(
  api_key = NULL
)

#' Store API key for use in queries
#'
#' Keep your API key secret. In general, it is safest to not save API keys
#' directly in your code, rather store them in an external yaml file which is
#' listed in your .gitignore file.
#'
#' @param key an API key generated at https://portal.apis.stats.govt.nz/
#' @examples
#' set_api_key(config::get("api_key"))
#'
#' @export
set_api_key <- function(key) {
  assign("api_key", key, .api_env)
}

#' Get API key
#'
#' This is a prank function, and won't return your API key
#'
#' @export
get_api_key <- function() {
  warning("We can't share that, it's a secret 😉")
}

#' Get header for use in Aotearoa Data Explorer API queries
#'
#' @param format optional parameter to specify return format for queries, defaults to XML
#' @examples
#' header <- get_ade_query_header(format = "text/csv")
#'
#' @export
get_ade_query_header <- function(format = NULL){

  format = ifelse(format == "csv", "text/csv", "csv")

  if(is.null(.api_env$api_key)) {
    stop("Cannot find Stats NZ API key, generate one at https://portal.apis.stats.govt.nz/ and set using set_api_key()")
  }

  c(
    `Ocp-Apim-Subscription-Key` = .api_env$api_key,
    if(!is.null(format)) accept = format
  )
}
