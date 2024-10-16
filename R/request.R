#' @title USPS API Endpoints
#'
usps_endpoints <- function() {
  c(
    "address",
    "city-state",
    "zipcode"
  )
}

#' @title USPS Authorization
#'
usps_auth <- function(request) {
  # Define the OAuth client with the correct credentials and token URL
  oauth_client <- httr2::oauth_client(
    id = Sys.getenv("USPS_CLIENT_ID"),         # Client ID from environment variables
    secret = Sys.getenv("USPS_CLIENT_SECRET"),  # Client secret from environment variables
    token_url = "https://api.usps.com/oauth2/v3/token"  # Token URL
  )

  # Use req_oauth_client_credentials to attach the token to the request
  request <- httr2::req_oauth_client_credentials(
    req = request,
    client = oauth_client,
    scope = "addresses"  # The scope required for the USPS API
  )

  return(request)
}

#' @title USPS Request
#'
usps_request <- function(endpoint, method = "GET", query, base_url = "https://api.usps.com/addresses/v3") {
  request <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_method(method = method) |>
    httr2::req_url_query(
      !!!query,
      .multi = "explode"
    ) |>
    usps_auth()

  return(request)
}

#' @title USPS Response
#'
usps_response <- function(request) {
  response <- httr2::req_perform(request)

  if (httr2::resp_status(response) >= 400) {
    stop("Error in the request: ", httr2::resp_body_string(response))
  }

  return(httr2::resp_body_json(response))
}
