
#' Get metadata for resource
#'
#' Request certain useful metadata values from the structural metadata API
#'
#' @returns A list of data.frame objects containing:
#' - series - descriptors of the data set being queried
#' - concepts - metadata on the variables included in the data
#' - codelists - the values each variable can take, and a textual name for them
#'
#' @param resourceId an API key generated at https://portal.apis.stats.govt.nz/
#' @examples
#' set_api_key(config::get("api_key"))
#'
#' @export
get_ade_metadata <- function(resourceId, version) {
  sdmx.dsd <- rsdmx::readSDMX(
    resourceId = resourceId,
    version = version,
    providerId = "STATSNZ",
    resource = "datastructure",
    dsd = T,
    headers = get_ade_query_header()
  )

  series_metadata <- as.data.frame(sdmx.dsd@datastructures)

  concept_metadata <- as.data.frame(sdmx.dsd@concepts, conceptSchemeId = paste0("CONCEPT_", resourceId))
  concept_metadata$full_name = concept_metadata$name
  concept_metadata$name <- clean_ade_names(concept_metadata$id, resourceId = resourceId)

  codelists <- slot(slot(sdmx.dsd, "codelists"), "codelists")

  codelist_names <- sapply(codelists, function(x) slot(x, "id"))
  codelist_names <- clean_ade_names(codelist_names, resourceId = resourceId, var_type = "codelist")

  codelist_metadata <- lapply(
    X = codelists,
    FUN = as.data.frame
  )

  names(codelist_metadata) <- codelist_names

  metadata <- list(
    series = series_metadata,
    concepts = concept_metadata,
    codelists = codelist_metadata
  )

  return(metadata)
}

#' Get series metadata
#'
#' Get the name, description, and other info about the series for the connection object
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' series_info <- get_series_metadata(conn)
#' series_info$Name.en
#' > "STATSNZ Subnational population estimates (RC, SA2), by age and sex, at 30 June 1996-2024 (2024 boundaries)"
#'
#' @export
get_series_metadata <- function(conn) {
  slot(conn, "metadata")$series
}

#' Get code-label pairs for all values of a variable
#'
#' Get the codes and respective labels for values a variable can take
#'
#' @returns A data.frame
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param variable The name of a variable in the dataset for which the metadata applies
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' age_groups <- get_variable_labels(conn, variable = "age")
#' age_groups <- get_variable_values(conn, variable = "age")
#'
#' @export
get_variable_labels <- function(conn, variable) {
  slot(conn, "metadata")$codelists[[variable]]
}

#' Get code-label pairs for all values of a variable
#'
#' Get the codes and respective labels for values a variable can take
#'
#' @returns A data.frame
#'
#' @param conn A connection to the ADE API, obtained from connect_to_ade()
#' @param variable The name of a variable in the dataset for which the metadata applies
#' @examples
#' conn <- connect_to_ade("POPES_SUB_004")
#' age_groups <- get_variable_labels(conn, variable = "age")
#' age_groups <- get_variable_values(conn, variable = "age")
#'
#' @export
get_variable_values <- get_variable_labels

#' Get variables
#'
#' List variables available in a data series
#'
#' @returns A character vector of variable names
#'
#' @param metadata A metadata table obtained from the get_ade_metadata() function
#' @examples
#' metadata <- get_ade_metadata("POPES_SUB_004")
#' age_groups <- get_variables(metadata)
#'
#' @export
get_variables <- function(conn) {
  names(slot(conn, "metadata")$codelists)
}

#' Add labels to values
#'
#' By default the ADE API returns value codes for all variables, this function
#' replaces them with more meaningful labels
#'
#' @returns A data.frame enriched with labels for values
#'
#' @param .data Data obtained from the ADE API
#' @param conn The connection that produced the data
#' @param drop_codes If true, the labels will replace the code values. If false, codes are kept.
#'
#' @export
add_ade_value_labels <- function(.data, conn, drop_codes = F) {

  codelists <- conn@metadata$codelists

  vars <- names(codelists)

  for(var in vars) {

    labels <- codelists[[var]]

    names(labels) <- c(var, paste0(var, "_name"))

    type_var = typeof(.data[[var]])
    type_code = typeof(labels[[var]])

    if(type_var != type_code) labels[[var]] <- as(labels[[var]], type_var)

    .data <- merge(
      .data, labels,
      by = var,
      all.x = T,
      all.y = F
    )

    names_keep <- names(.data)

    if(drop_codes) {
      names_keep <- names_keep[names_keep != var]
      .data <- .data[,names_keep]

      names(.data) <- gsub("_name", "", names(.data))
    }

  }

  return(.data)
}

#' @export
as.data.frame.SDMXCodelist <- function (x, ...)
{
  out <- do.call("rbind", lapply(x@Code, function(cl) {
    names <- slot(cl, "name")
    clf.names <- NULL
    if (length(names) > 0) {
      clf.names <- as.data.frame(names, stringsAsFactors = FALSE)
      colnames(clf.names) <- "name"
    }
    clf <- data.frame(
      code = slot(cl, "id"),
      stringsAsFactors = FALSE
    )
    if (!is.null(clf.names)) {
      clf <- cbind(clf, clf.names, stringsAsFactors = FALSE)
    }
    return(clf)
  }))
  return(rsdmx:::encodeSDMXOutput(out))
}
