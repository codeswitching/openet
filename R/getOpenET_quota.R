#' Quota and expiration date of an OpenET API key
#'
#' `getOpenET_quota` fetches the quota and expiration date of the user's API key
#'
#' OpenET API documentation with description of quotas:
#'
#' https://open-et.github.io/docs/build/html/faq.html
#'
#' OpenET API Testbed:
#'
#' https://openet.dri.edu/docs#/home/user_quotas_home_user_quotas_get
#'
#' @param api_key             OpenET API key (long string), obtain from https://auth.etdata.org
#'
#' @returns A data frame containing the various quota limits and data use to date, and a console message showing the expiration date. The OpenET quotas reset every 30 days.
#'
#' <acre_years>, <acre_months>, <acre_days> Total (acreage x timestep) quotas for the 30-day period
#'
#' Quotas for raster endpoints (polygon and multipolygon) only:
#'
#' <total_data_month>, <total_data_day> Total acreage that can be retrieved at daily or monthly timestep during the 30-day quota period
#'
#' <year_access_start>, <year_access_end> The earliest and latest years of data available
#'
#' <max_acres> The maximum number of acres that can be requested in a single call to the polygon or multipolygon endpoitns
#'
#' <max_polygons> The maximum number of polygons that can be requested in a single call to the multipolygon endpoint
#'
#' @examples getOpenET_quota(api_key = 'mykey')


getOpenET_quota <- function (api_key = '')

{

  library(httr)      # API tools for R

  httr::set_config(httr::config(ssl_verifypeer=0L))

  url1 <- 'https://openet.dri.edu/home/key_expiration'  # URL for the key expiration date endpoint
  url2 <- 'https://openet.dri.edu/home/user/quotas'     # URL for the quota endpoint

  response1 <- GET(url1, add_headers(accept = "application/json", Authorization = api_key))
  response2 <- GET(url2, add_headers(accept = "application/json", Authorization = api_key))

  if (http_error(response1)) {                     # if the server returned an error
    cat('The API server returned an error:\n')
    cat(http_status(response1)$message, '\n')      # print the server's error message
    helpful_error <- dplyr::case_when(             # print a helpful error message
      response1$status_code == 401 ~ 'The API key may be invalid',
      response1$status_code == 403 ~ 'The API key may be expired'
    )
    cat(helpful_error, '\n') }
  else {                                          # if successful
    cat(content(response1)$status)
    expirydate <- as.Date(content(response1)$`Expiration date`)  # read the expiration date
    cat('Your API key will expire on:\n')
    print(expirydate)

    quota <- as.data.frame(content(response2))  # read the quota into a data frame
    }

  return(quota)
}
