usps_endpoints <- function() {
  c(
    "address",
    "city-state",
    "zipcode"
  )
}

usps_get_token <- function() {
  # Define the OAuth client with the correct token URL
  token_url <- "https://api.usps.com/oauth2/v3/token"

  # Construct the request manually with all necessary parameters
  request <- httr2::request(token_url) |>
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
    httr2::req_body_form(
      grant_type = "client_credentials",               # Grant type
      client_id = Sys.getenv("USPS_CLIENT_ID"),         # Client ID from env vars
      client_secret = Sys.getenv("USPS_CLIENT_SECRET"), # Client secret from env vars
      scope = "addresses"                              # Specify the scope
    )

  # Perform the token request
  token_response <- httr2::req_perform(request)

  # Check if the response was successful
  if (httr2::resp_status(token_response) != 200) {
    stop("Failed to obtain OAuth token: ", httr2::resp_body_string(token_response))
  }

  # Extract and return the access token
  token <- httr2::resp_body_json(token_response)$access_token
  return(token)
}

# Attach the OAuth token to the request
usps_auth <- function(request) {
  token <- usps_get_token()

  httr2::req_headers(request, Authorization = paste("Bearer", token))
}

# Function to make a request to USPS API
usps_request <- function(endpoint, method = "GET", base_url = "https://api.usps.com/addresses/v3") {
  request <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_method(method = method) |>
    usps_auth()  # Apply OAuth

  return(request)
}

# Perform the request and handle the response
usps_response <- function(request) {
  response <- httr2::req_perform(request)

  if (httr2::resp_status(response) >= 400) {
    stop("Error in the request: ", httr2::resp_body_string(response))
  }

  return(httr2::resp_body_json(response))
}

# Example API Call to get standardized address
usps_address <- function(street_address, state, ...) {
  args <- list(...)

  req <- usps_request(endpoint = "address") |>
    httr2::req_url_query(
      streetAddress = street_address,
      state = state,
      !!!args
    )

  res <- usps_response(req)

  return(res)
}
