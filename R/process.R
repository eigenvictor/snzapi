

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

#' Get area type
#'
#' Some published series include multiple area types in the same "area" column.
#' This helper function aims to assign, as best as it can, the relevant area type.
#' If you are querying a table with only one area type, you are best to set the area
#' type manually.
#'
#' Known issues:
#' - Auckland is assigned TA, but is also the name of a DHB
#' - Totals and areas outside common geographies are often mislabeled
#' - Doesn't work for Urban-Rural areas
#'
#' @param area a character string containing a label/name for an area concept
#' @examples
#' get_area_type("Gisborne district")
#' > "TA"
#'
#' @export
get_area_type <- function(area) {

  .get_area_type <- function(x) {
    sa1_pattern <- "^\\d{7}$"
    sa2_pattern <- "^\\d{6}($|\\s)"
    lb_pattern <- "06\\d{3}($|\\s)"

    dhbs <- c(
      "Northland", "Waitemata", "Auckland", "Counties Manukau", "Waikato",
      "Lakes", "Bay of Plenty", "Tairawhiti", "Taranaki", "Hawke's Bay",
      "Whanganui", "MidCentral", "Hutt Valley", "Capital and Coast",
      "Wairarapa", "Nelson Marlborough", "West Coast", "Canterbury",
      "South Canterbury", "Southern", "2201 Otago constituency", "2202 Southland constituency"
    )

    type <- if (grepl(sa1_pattern, x)) {
      "SA1"
    } else if (grepl(sa2_pattern, x)) {
      "SA2"
    } else if (grepl("regio", x)) {
      "REGC"
    } else if (grepl("Island(\\s|$)", x)) {
      "Island"
    } else if (
      grepl("district|territorial authority|city|territory", x) &
      !grepl("Other|Inlets|Inland|health board", x) |
      x == "Auckland"
    ) {
      "TA"
    } else if (
      grepl("community|local board area", x) |
      grepl(lb_pattern, x)
    ) {
      "CB/LB"
    } else if (grepl("subdivis", x)) {
      "subdivision"
    } else if (x %in% dhbs | grepl("district health board|DHB", x)) {
      "DHB"
    } else if (grepl("constituen", x, ignore.case = T) & !grepl("district health board|DHB", x)) {
      "constituency"
    } else if (grepl("ward", x)) {
      "Ward"
    } else if(grepl("New Zealand", x)) {
      "NZ"
    } else {
      "Unknown"
    }

    return(type)
  }

  type <- sapply(X = area, FUN = .get_area_type)

  return(type)
}


#' Get area type
#'
#' Labels for some area types will include the area code in the label, this function removes the code.
#'
#' NOTE: If you wish to use the get_area_type() function, use it before removing area codes.
#' Area codes are used to help classify SA2s and Auckland Local Boards.
#'
#' @param area a character string containing a label/name for an area concept
#' @examples
#' remove_leading_area_code("100100 North Cape")
#' > "North Cape"
#'
#' @export
remove_leading_area_code <- function(area) {
  gsub("^\\d+\\s", "", area)
}
