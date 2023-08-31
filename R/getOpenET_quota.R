#' Quota and usage of an OpenET API account
#'
#' `getOpenET_quota` fetches the quotas and current usage of the user's API account
#'
#' OpenET API documentation for this endpoint:
#'
#' https://openet.gitbook.io/docs/reference/api-reference/account
#'
#' OpenET API testbed for this endpoint:
#'
#' https://openet-api.org/#/Manage%20Account%20Information/_account_status_get
#'
#' Description of subscription plan quotas:
#'
#' https://openet.gitbook.io/docs/additional-resources/quota
#'
#' @param api_key OpenET API key (string). Obtain from https://wave.etdata.org/settings/api
#'
#' @returns prints quota limits to the console and returns nothing. OpenET quotas reset on the 1st of each month.
#'
#' <Tier> Account subscription tier
#'
#' <Monthly Requests> Number of calls to the API
#'
#' <Max Field IDS> Maximum number of OpenET field ids that can be requested at once through the geodatabase/timeseries endpoint
#'
#' Quotas for raster endpoints (polygon and multipolygon) only:
#'
#' <Monthly Export EECU Seconds> Earth Engine computing units (amount of compute) used by raster calls
#'
#' <Max Area Acres> The maximum number of acres that can be requested in a single call to the polygon or multipolygon endpoitns
#'
#' <Max Polygons> The maximum number of polygons that can be requested in a single call to the multipolygon endpoint
#'
#' <Earth Engine Linking> Whether your OpenET account is linked to your Earth Engine account
#'
#' @examples getOpenET_quota(api_key = 'mykey')


getOpenET_quota <- function (api_key = '')

{
  httr::set_config(httr::config(ssl_verifypeer=0L))

  url <- 'https://openet-api.org/account/status'  # URL for the key expiration date endpoint

  response <- httr::GET(url, httr::add_headers(accept = "application/json", Authorization = api_key))

  if (httr::http_error(response)) {              # If the server returned an error
    cat('The API server returned an error:\n')
    cat(httr::http_status(response)$message, '\n') # print the server's error message
    helpful_error <- dplyr::case_when(             # print a helpful error message
      response$status_code == 401 ~ 'The API key may be invalid',
      response$status_code == 403 ~ 'The API key may be expired'
    )
    cat(helpful_error, '\n') }
  else {                                         # If successful
    cat(httr::http_status(response)$message)       # print the http success message
    response <- httr::content(response)
    if (is.null(response$`Cloud Project ID`)) { response$`Cloud Project ID` <- 'None' }  # replace NULL with 'None' so list will convert to df
    quota <- as.data.frame(response, check.names=F)  # read the quota into a data frame; avoid reading col name spaces as periods
    print(knitr::kable(quota))                     # pretty print the quotas to the console
    cat('\nAccount quotas reset on the 1st of every month. For a description of plan quotas, see:\nhttps://openet.gitbook.io/docs/additional-resources/quota', '\n')
    }
}
