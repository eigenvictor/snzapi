

#' Clean API variable names
#'
#' The Aotearoa Data Explorer refers to all variable metadata with names of the form
#' <prefix>_<variable_name>_<series_name>. This function extracts that core <variable_name>
#' value and returns it in lower case.
#'
#' @param text a vector of strings to clean
#' @param var_type used to infer prefix, does it come from a concept, codelist, etc
#' @param resourceId resourceId for the variable
#' @examples
#' clean_ade_names("CL_AREA_POPES_SUB_004", var_type = "codelist", resourceId = "POPES_SUB_004")
#' > "area"
#'
#' @export
clean_ade_names <- function(text, resourceId, var_type = "id") {

  prefix = switch(
    var_type,
    id = "",
    concept = "CONCEPT_",
    codelist = "CL_","\\1",
    NULL = ""
  )

  name_pattern <- paste0(prefix, "(\\w+)_", resourceId)

  text <- gsub(name_pattern, "\\1", text, perl = T)
  text <- tolower(text)

  return(text)
}


get_area_type <- function(area) {
  sa1_pattern <- "^\\d{7}$"
  sa2_pattern <- "^\\d{6}($|\\s)"

  type <- if (grepl(sa1_pattern, area)) {
    "SA1"
  } else if (grepl(sa2_pattern, area)) {
    "SA2"
  } else if (grepl("region", area)) {
    "REGC"
  } else if (grepl("Island", area)) {
    "Island"
  } else if(grepl("New Zealand", area)) {
    "NZ"
  } else {
    "Unknown"
  }

  return(type)
}
