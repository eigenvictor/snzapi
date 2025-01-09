

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
#' - If no code is provided, Auckland is assigned TA, although it may be a DHB
#' - SA2s need either their code included in the area_name, or need their code provided
#' - Totals and areas outside common geographies are often mislabeled
#' - Doesn't work for Urban-Rural areas
#'
#' @param area_name a character string containing a label/name for an area concept
#' @param area_code (optional) the numeric code for an area to help improve classification accuracy
#' @param options (optional) possible options of types for the area
#' @examples
#' get_area_type("Gisborne district")
#' > "TA"
#'
#' @export
get_area_type <- function(
    area_name,
    area_code = NULL,
    options = c("SA1", "SA2", "Region", "TA", "LB", "CB",
                "Subdivision", "DHB", "Constituency", "Ward")
) {

  area_name <- tolower(area_name)

  if(is.null(area_code)) area_code <- ""

  .get_area_type <- function(code, name) {

    matches <- mapply(FUN = .matches_geo, code = code, name = name, geo = options)

    if(sum(matches) > 0) return(options[matches][[1]])

    if (grepl("outside|total", name) & length(options) == 1) return(options)
    if (grepl("island(\\s|$)", name) | code %in% c("NIRC", "SIRC")) return("Island")
    if (grepl("new zealand", name)) return("NZ")
    return("Unknown")
  }

  code_name_pairs <- data.frame(name = area_name, code = area_code)

  unique_pairs <- unique(code_name_pairs)

  unique_pairs$type <- mapply(FUN = .get_area_type, code = unique_pairs$code, name = unique_pairs$name)

  code_name_types <- code_name_pairs

  data.table::setDT(code_name_types)[unique_pairs, on = c("code", "name"), type := i.type]

  return(code_name_types$type)
}

.matches_geo <- function(code, name, geo) {

  match_fun_call <- paste0(".matches_", tolower(geo), "(code, name)")

  match_fun_eval <- tryCatch(
    eval(parse(text=match_fun_call)),
    error = function(e) {stop("Specific geo option not recognised:", geo)}
  )

  return(match_fun_eval)
}

#' check if area is an SA2
#' @export
.matches_sa2 <- function(code, name){
  sa2_pattern <- "^\\d{6}($|\\s)"
  grepl(sa2_pattern, code) & nchar(code) == 6 | grepl(sa2_pattern, name)
}

#' check if area is an SA1
#' @export
.matches_sa1 <- function(code, name){
  sa1_pattern <- "^\\d{7}$"
  grepl(sa1_pattern, code) & nchar(code) == 7 | grepl(sa1_pattern, name)
}

#' check if area is a regional council
#' @export
.matches_region <- function(code, name){
  grepl("regio", name)
}

#' check if area is a LB
#' @export
.matches_lb <- function(code, name){
  lb_pattern <- "06\\d{3}($|\\s)"
  grepl("local board area", name) |
    grepl(lb_pattern, name)
}

#' check if area is a CB
#' @export
.matches_cb <- function(code, name){
  cb_pattern <- "\\d{5}($|\\s)"
  grepl("community", name) |
    grepl(cb_pattern, name)
}

#' check if area is a TA
#' @export
.matches_ta <- function(code, name){
  grepl("district|territorial authority|city|territory", name) &
    !grepl("Other|Inlets|Inland|health board", name) |
    name == "Auckland" & code != "03"
}

#' check if area is a TA
#' @export
.matches_dhb <- function(code, name){

  dhbs <- c(
    "Northland", "Waitemata", "Auckland", "Counties Manukau", "Waikato",
    "Lakes", "Bay of Plenty", "Tairawhiti", "Taranaki", "Hawke's Bay",
    "Whanganui", "MidCentral", "Hutt Valley", "Capital and Coast",
    "Wairarapa", "Nelson Marlborough", "West Coast", "Canterbury",
    "South Canterbury", "Southern", "2201 Otago constituency", "2202 Southland constituency"
  )

  name %in% dhbs | grepl("district health board|DHB", name)
}

#' check if area is a subdivision
#' @export
.matches_subdivision <- function(code, name){
  grepl("subdivis", name)
}

#' check if area is a constituency
#' @export
.matches_constituency <- function(code, name){
  grepl("constituen", name)
}

#' check if area is a ward
#' @export
.matches_ward <- function(code, name){
  grepl("ward", name)
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
