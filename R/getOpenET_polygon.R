#' Timeseries of monthly/daily ET for a custom polygon
#'
#' Makes calls to the OpenET /raster/timeseries/polygon API endpoint. Use this function when you want
#' to get data for a single user-defined polygon instead of using OpenET's built-in fields. Data is returned as an R data frame.
#'
#' If you don't know the lat-long coordinates of your desired polygon, there is an easy way to find it. From the OpenET web app,
#' click the orange Draw Custom Area button at lower right. Click one of the two polygon tools and draw the polygon on the map.
#' Click Run Timeseries, then click Copy Shape to Clipboard at the top of the popup. Now you can create a numeric vector containing
#' the pasted lat/long pairs, e.g. `geom <- c(-114.2, 33.5, -114.8, 33.7, -114.0, 33.0)`. Finally, pass the vector to the
#' `geometry` parameter of the function.
#'
#' #' Note that all parameters except `geometry` are strings. Most parameters have default values and can therefore be omitted,
#' if the defaults are acceptable.
#'
#' OpenET API Documentation for this endpoint:
#'
#' https://openet.gitbook.io/docs/reference/api-reference/raster#timeseries-polygon
#'
#' OpenET API Testbed:
#'
#' https://openet-api.org/#/Retrieve%20Raster%20Data/_raster_timeseries_polygon_post
#'
#' @param geometry A numeric vector containing longitude-latitude pairs for a single polygon. e.g. `c(-114.2, 33.5, -114.8, 33.7, -114.0, 33.0)`
#' @param start_date The start date as a string in 'yyyy-mm-dd' format.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to today's date.
#' @param model The ET model: 'ensemble', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble'.
#' @param variable Variable to fetch: 'et', 'et_mad_min', 'et_mad_max', 'eto', 'etr', 'etof', 'ndvi'. Defaults to 'et'.
#' @param units Units for ET; 'mm' or 'in' (inches). Defaults to 'in'.
#' @param reference_et Reference ET source, either 'cimis' (CA only) or 'gridmet' (all states). Defaults to 'cimis'.
#' @param interval Time interval: 'daily' or 'monthly'. Defaults to 'daily'.
#' @param reducer Pixel aggregation method for the polygon: 'mean', 'median', 'min', 'max', or 'sum'. Defaults to 'mean'.
#' @param api_key Your personal OpenET API token as a string.
#'
#' @returns Returns a data frame with one row per time interval and 3 columns:
#'
#' <date>    Date, in 'yyyy-mm-dd' format
#'
#' <year>    Numeric year, extracted from <date>
#'
#' <month>   Numeric month, extracted from <date>
#'
#' <julian>  Numeric julian day of year, extracted from <date>
#'
#' <et>      Daily or monthly ET, depending on chosen interval, in either inches or mm, depending on chosen units
#'
#' <model>   Name of the ET model
#'
#' <units>   'inches' or 'mm'
#'
#' @examples getOpenET_polygon(geometry = c(-114.2, 33.5, -114.8, 33.7, -114.0, 33.0), start_date = '2020-01-01', end_date = '2021-12-31', model = 'ensemble', units = 'mm', interval = 'daily', api_key = mykey)
#'
#' @export


getOpenET_polygon <- function (geometry, start_date = '2021-01-01', end_date = as.character(Sys.Date()),
                               model = 'ensemble', variable = 'et', units = 'in', reference_et = 'cimis',
                               interval = 'daily', reducer = 'mean', api_key = '')

{
  httr::set_config(httr::config(ssl_verifypeer=0L))         # turn off ssl_verify (for use behind firewall)

  url <- 'https://openet-api.org/raster/timeseries/polygon' # URL for the API's timeseries/features/monthly endpoint

  date_range <- c(start_date, end_date)

  response <- httr::POST(url,
                         httr::add_headers(accept = 'application/json',         # type of response to accept
                                           Authorization = api_key,             # API key
                                           content_type = 'application/json'),  # tells server how the body data is formatted
                         encode = 'json',                                       # tells POST how to encode the body list
                         body = list(geometry      = geometry,
                                     model         = model,
                                     variable      = variable,
                                     date_range    = date_range,
                                     units         = units,
                                     file_format   = 'csv',
                                     reducer       = reducer,
                                     reference_et  = reference_et,
                                     interval      = interval))

  if (httr::http_error(response)) {                 # If the server returned an error...
    cat('The API server returned the following error:\n')
    cat(httr::http_status(response)$message, '\n')    # print the server's error message
    cat(httr::content(response)$detail, '\n')         # print the server's detailed error message
    cat(httr::content(response)[[1]][[1]]$msg, '\n')    # print the server's detailed error message
    helpful_error <- dplyr::case_when(
      response$status_code == 401 ~ 'API key may be invalid, expired, or over quota',
      response$status_code == 403 ~ 'API key may be invalid or over quota',
      response$status_code == 404 ~ 'Data may not be available for this date range [yet]',
      response$status_code == 406 ~ 'Please try again with a shorter date range',
      response$status_code == 422 ~ 'Malformed parameter data - check your parameter data types and formatting'
    )
    cat(helpful_error, '\n')                         # print a more helpful error message
    return(NULL)
    }                                                # return a null data frame
  else {                                          # Else if successful...
    cat('Server reports', httr::http_status(response)$message, '\n')  # print a success message
    response_data <- httr::content(response)         # extract the returned data as a data frame
    etdata <- tryCatch ({                            # test whether the data frame contains meaningful data
      dplyr::rename(response_data, date = time)      # if it does,
    }, error = function(e) {                         # if unpacking returns an error
      cat('Malformed parameter data - check that your parameters are specified correctly\n')
      return(NULL)
    })
  }

  etdata <- dplyr::mutate(etdata, year   = lubridate::year(date),   # extract year from date and add year column
                                  month  = lubridate::month(date),  # extract month from date and add month column
                                  julian = lubridate::yday(date),   # add julian day of year column
                                  units  = ifelse(units == 'in', 'inches', 'mm'),  # add the ET units
                                  model  = model)                   # add the name of the ET model

  return(etdata)
}
