
#' Search data series
#'
#' Searches the titles of all published datasets in the ADE and returns matches
#'
#' @param text substring to search for
#' @param perl use perl regex?
#' @examples
#' search_data_series(text="population")
#'
#' @export
search_data_series <- function(text, perl = F) {
  data_series <- get_all_data_series()

  data_series[grepl(text, data_series$name, ignore.case = T, perl = perl),]
}

#' Get all data series
#'
#' Searches the titles of all published datasets in the ADE and returns a data frame
#' containing textual name, and the flowRef and version required to query it
#'
#' @export
get_all_data_series <- function(){

  provider = rsdmx::findSDMXServiceProvider("STATSNZ")

  rsdmxAgent <- paste("rsdmx/", as.character(packageVersion("rsdmx")),
                      sep = "")

  requestParams <- rsdmx::SDMXRequestParams(
    regUrl = provider@builder@regUrl,
    repoUrl = provider@builder@repoUrl, accessKey = NULL,
    providerId = "STATSNZ", agencyId = "STATSNZ", resource = "dataflow",
    resourceId = NULL, version = NULL, flowRef = NULL,
    key = NULL, start = NULL, end = NULL, compliant = provider@builder@compliant
    )

  requestHandler <- provider@builder@handler
  requestFormatter <- provider@builder@formatter
  requestParams <- requestFormatter$dataflow(requestParams)

  file <- requestHandler$dataflow(requestParams)

  res <- httr::GET(
    file,
    httr::add_headers(
      Accept = "application/xml",
      `User-Agent` = rsdmxAgent,
      get_ade_query_header()
    )
  )

  content <- httr::content(res, "text", encoding = "UTF-8")

  status <- tryCatch({
    if ((attr(regexpr("<!DOCTYPE html>", content), "match.length") ==
         -1) && (attr(regexpr("<html>", content), "match.length") ==
                 -1)) {
      BOM <- "﻿"
      if (attr(regexpr(BOM, content), "match.length") !=
          -1) {
        content <- gsub(BOM, "", content)
      }
      content <- gsub("<!--.*?-->", "", content)
      content <- gsub("&ldquo;", "&quot;", content)
      content <- gsub("&rdquo;", "&quot;", content)
      content <- gsub("&lsquo;", "'", content)
      content <- gsub("&rsquo;", "'", content)
      xmlObj <- XML::xmlTreeParse(content, useInternalNodes = TRUE)
      status <- 1
    }
    else {
      stop("Invalid SDMX-ML file")
    }
  }, error = function(err) {
    print(err)
    status <- 0
    return(status)
  })

  ns <- rsdmx::namespaces.SDMX(xmlObj)

  struct_obj <- rsdmx::SDMXDataFlows(xmlObj, ns)

  flows <- struct_obj@dataflows

  data_series <- data.frame(
    flowRef = sapply(X = flows, FUN = \(x) slot(x, "id")),
    name = sapply(X = flows, FUN = \(x) slot(x, "Name")$en),
    version = sapply(X = flows, FUN = \(x) slot(x, "version"))
  )

  return(data_series)
}
