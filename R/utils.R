#' @title USPS Parameter Casing
#'
to_usps_case <- function(params) {
  names(params) <- snakecase::to_lower_camel_case(names(params))

  names(params) <- gsub("^ZipCode$", "ZIPCode", names(params))
  names(params) <- gsub("^ZipPlus4$", "ZIPPlus4", names(params))

  return(params)
}

from_usps_case <- function(params) {

}

replace_nulls_with_na <- function(x) {
  purrr::map(x, ~ if (is.null(.x)) NA else .x)
}
