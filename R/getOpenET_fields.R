#' Timeseries of monthly ET for one or more OpenET fields
#'
#' Makes calls to the OpenET /timeseries/features/monthly API endpoint. Use this function when you
#' want to use OpenET's built-in field boundaries, and you know the field id's of the specific field(s). The field id's
#' can be found by clicking on fields in the OpenET web app and looking at the top of the resulting popup. Data is
#' returned as an R data frame.
#'
#' Note that all parameters are strings, esp. field id's which may have leading zeroes. Most parameters have default values
#' and can therefore be omitted if the defaults are acceptable.
#'
#' OpenET API Documentation:
#'
#' https://open-et.github.io/docs/build/html/geo_timeseries.html#timeseries-features-monthly
#'
#' OpenET API Testbed:
#'
#' https://openet.dri.edu/docs#/timeseries/feature_timeseries_annual_timeseries_features_monthly_post
#'
#' @param field_ids One or more OpenET field ids, as a string or vector of strings. To determine the field id, click on a field within the OpenET web explorer. Id is shown in the popup title.
#' @param start_date The start date as a string in 'yyyy-mm-dd' format. Defaults to start of current year.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to two weeks ago.
#' @param model A vector of one of more ET models to use: 'ensemble', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble'.
#' @param variable A vector of one or more variables to fetch: 'et', 'et_mad_min', 'et_mad_max', 'eto', 'etr', 'etof', 'ndvi', 'pr'. Defaults to 'et'.
#' @param units Units for ET; 'mm' or 'in' (inches). Defaults to 'in'.
#' @param interval Time interval: 'daily' or 'monthly'. Defaults to 'monthly'.
#' @param api_key Your personal OpenET API token as a string.
#'
#' @returns Returns a data frame with (months) x (fields) rows and 5 columns:
#'
#' <start_date>  Start date of the month, in 'yyyy-mm-dd' format
#'
#' <end_date>    End date of the month
#'
#' <month>       Numeric month, extracted from <start_date>
#'
#' <year>        Numeric year, extracted from <start_date>
#'
#' <field>       OpenET field id
#'
#' <et>          Total monthly ET (sum of all daily values) in either inches or mm, depending on chosen units
#'
#' <units>       'in' or 'mm'
#'
#' @examples getOpenET_fields(field_ids = c('06323746', '06435895'), '2020-01-01', '2021-12-31', 'ensemble', 'et', 'metric', mykey)
#' @examples getOpenET_fields(field_ids = '065746833', start_date = '2021-01-01', end_date = '2022-12-31', api_key = mykey)
#'
#' @export

getOpenET_fields <- function (field_ids = '06323746', start_date = paste0(year(Sys.Date()), '-01-01'), end_date = as.character(Sys.Date()-14),
                              model = 'ensemble', variable = 'et', interval = 'monthly', units = 'in', api_key = '')
{
  httr::set_config(httr::config(ssl_verifypeer=0L))  # turn off ssl_verify (for use behind firewall)

  url <- 'https://openet-api.org/geodatabase/timeseries'  # URL for the API's timeseries/features/monthly endpoint

  response <- httr::POST(url,
                         httr::add_headers(accept = 'application/zip',          # type of response to accept
                                           Authorization = api_key,             # API key
                                           content_type = 'application/json'),  # tells server how the body data is formatted
                         encode = 'json',                                       # tells POST how to encode the body list
                         body = list(field_ids     = as.list(field_ids),
                                     models        = as.list(model),
                                     variables     = as.list(variable),
                                     date_range    = list(start_date, end_date),
                                     interval      = interval,
                                     file_format   = 'csv'))

  if (httr::http_error(response)) {                # If the server returned an error...
    cat('The API server returned an error:\n')
    cat(httr::http_status(response)$message, '\n')      # print the server's error message
    cat(httr::content(response)$detail[[1]]$msg, '\n')  # print the server's detailed error message
    helpful_error <- dplyr::case_when(
      response$status_code == 401 ~ 'API key may be invalid, expired, or over quota',
      response$status_code == 403 ~ 'API key may be missing, invalid or over quota',
      response$status_code == 404 ~ 'Data may not be available for this date range [yet]',
      response$status_code == 422 ~ 'Malformed parameter data - check your parameter types and formatting'
    )
    cat(helpful_error, '\n')                          # print a more helpful error message
    return(NULL)                                      # and return a null data frame
    }
  else {                                           # Else if successful...
    cat('Server reports', httr::http_status(response)$message, '\n')        # print a success message
    response_data <- httr::content(response)          # extract the response data as a data frame

    etdata <- tryCatch({                              # Test to see if we have a non-null data frame
      response_data |>
        dplyr::rename(date  = time,                   # Rename some columns
                      field = field_id,
                      value = value_mm)               # We're going to pivot later so need this to be unit-neutral
    }, error = function(e) {                        # If unpacking returns as error
      cat('Malformed parameter data - check that your parameters are specified correctly\n')
      return(NULL)
    })

    if (units == 'in') {                            # If user wants inches, we have to convert from mm
      etdata <- dplyr::mutate(etdata, value = ifelse(stringr::str_detect(collection, 'et'), value / 25.4, value))  # Convert mm to inches
    }
    etdata$units <- units                           # Add a units columns
    etdata <- tidyr::pivot_wider(etdata, names_from = collection, values_from = value)   # for multiple vars, pivot to crosstab
    }

  # Extract date variables for month and year
  etdata <- dplyr::mutate(etdata, month = lubridate::month(start_date),
                                  year  = lubridate::year(start_date))

  return(etdata)  # return the data frame
}
