#' @title USPS Address
#'
#' @export
#'
usps_address <- function(street_address, state, ...) {
  args <- list(
    streetAddress = street_address,
    state = state,
    ...
  )

  args <- to_usps_case(args)

  req <- usps_request(
    endpoint = "address",
    method = "GET",
    query = args
  )

  res <- usps_response(req)

  res <- replace_nulls_with_na(res)

  tibble(
    street_address = res$address$streetAddress,
    street_address_abbreviation = res$address$streetAddressAbbreviation,
    secondary_address = res$address$secondaryAddress,
    city = res$address$city,
    city_abbreviation = res$address$cityAbbreviation,
    state = res$address$state,
    postal_code = res$address$postalCode,
    zip_code = res$address$ZIPCode,
    zip_code_plus_4 = res$address$ZIPPlus4,
    urbanization = res$address$urbanization,
    carrier_route = res$additionalInfo$carrierRoute,
    dpv_confirmation = res$additionalInfo$DPVConfirmation,
    central_delivery_point = res$additionalInfo$centralDeliveryPoint
  )
}

#' @title USPS City/State
#'
#' @export
#'
usps_city_state <- function(zip_code) {
  req <- usps_request(
    endpoint = "city-state",
    method = "GET",
    query = list(
      ZIPCode = zip_code
    )
  )

  res <- usps_response(req)

  return(res)
}

#' @title USPS ZIP Code
#'
#' @export
#'
usps_zip_code <- function(street_address, city, state, ...) {
  args <- list(
    streetAddress = street_address,
    city = city,
    state = state,
    ...
  )

  args <- to_usps_case(args)

  req <- usps_request(
    endpoint = "zipcode",
    method = "GET",
    query = args
  )

  res <- usps_response(req)

  return(res)
}
