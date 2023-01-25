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
#' #' Note that all parameters except `gometry` are strings. Most parameters have default values and can therefore be omitted,
#' if the defaults are acceptable.
#'
#' OpenET API Documentation:
#'
#' https://open-et.github.io/docs/build/html/ras_timeseries.html#raster-timeseries-polygon
#'
#' OpenET API Testbed:
#'
#' https://openet.dri.edu/docs#/raster/raster_timeseries_polygon_raster_timeseries_polygon_post
#'
#' @param geometry A numeric vector containing lat-long pairs. e.g. `c(-114.2, 33.5, -114.8, 33.7, -114.0, 33.0)`
#' @param start_date The start date as a string in 'yyyy-mm-dd' format.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to today's date.
#' @param model The ET model: 'ensemble', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble_mean'.
#' @param variable Variable to fetch: 'et', 'ndvi', 'et_reference', 'et_fraction', 'count'. Defaults to 'et'.
#' @param units Units for ET; 'metric' will give mm and 'english' will give inches. Defaults to 'english'.
#' @param ref_et_source Reference ET source, either 'cimis' (CA only) or 'gridmet' (all states). Defaults to 'cimis'.
#' @param provisional Include most recent months of data, even if provisional? Defaults to 'true'.
#' @param interval Time interval: 'daily' or 'monthly'. Defaults to 'daily'.
#' @param moving_average Average the data by x moving time intervals. '0', '1', '2', '3', etc. Defaults to '0'.
#' @param best_effort Use non-pixel-based interpolation for faster daily data? Defaults to 'true.'
#' @param pixel_aggregation Statistical spatial summary: 'mean', 'median', 'min', 'max', 'sum', 'count'. Defaults to 'mean'.
#' @param api_key Your personal OpenET API key as a string. OCan be obtained from https://auth.etdata.org
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
#' <units>   'inches' or 'mm'
#'
#' @examples getOpenET_polygon(geometry = c(-114.2, 33.5, -114.8, 33.7, -114.0, 33.0), start_date = '2020-01-01', end_date = '2021-12-31', model = 'ensemble_mean', units = 'metric', interval = 'daily', api_key = 'mykey')
#'
#' @export


getOpenET_polygon <- function (geometry, start_date = '2021-01-01', end_date = as.character(Sys.Date()),
                               model = 'ensemble', variable = 'et', units = 'english', ref_et_source = 'cimis',
                               provisional = 'false', interval = 'daily', moving_average = '0', best_effort = 'true',
                               pixel_aggregation = 'mean', api_key = '')

{
  httr::set_config(httr::config(ssl_verifypeer=0L))         # turn off ssl_verify (for use behind firewall)

  url <- 'https://openet.dri.edu/raster/timeseries/polygon' # URL for the API's timeseries/features/monthly endpoint

  response <- httr::POST(url,
                         httr::add_headers(accept = 'application/json',         # type of response to accept
                                           Authorization = api_key,             # API key
                                           content_type = 'application/json'),  # tells server how the body data is formatted
                         encode = 'json',                                 # tells POST how to encode the body list
                         body = list(geometry      = geometry,
                                     model         = model,
                                     variable      = variable,
                                     start_date    = start_date,
                                     end_date      = end_date,
                                     units         = units,
                                     output_file_format = 'json',
                                     ref_et_source = ref_et_source,
                                     provisional   = provisional,
                                     interval      = interval,
                                     moving_average = moving_average,
                                     best_effort = best_effort,
                                     pixel_aggregation = pixel_aggregation))

  if (httr::http_error(response)) {                 # If the server returned an error...
    cat('The API server returned an error:\n')
    cat(httr::http_status(response)$message, '\n')    # print the server's error message
    helpful_error <- dplyr::case_when(
      response$status_code == 401 ~ 'API key may be invalid',
      response$status_code == 403 ~ 'API key may be invalid',
      response$status_code == 422 ~ 'Malformed parameter data - check your parameter types and formatting',
    )
    cat(helpful_error, '\n')                        # print a more helpful error message
    return(NULL)
    }                                                # return a null data frame
  else {                                          # Else if successful...
    cat('Server reports', httr::http_status(response)$message, '\n')  # print a success message
    response_data <- httr::content(response)        # extract the returned data as a list
        etdata <- tryCatch ({                       # unpack the list into a data frame
      data.frame(date  = as.Date(sapply(response_data,    function(x) x$time)),
                 et    = as.numeric(sapply(response_data, function(x) x$et)),
                 units = ifelse(units == 'english', 'inches', 'mm'))
    }, error = function(e) {                         # if unpacking returns an error
      cat('Malformed parameter data - check that your parameters are specified correctly\n')
      return(NULL)
    })
  }

  etdata <- dplyr::mutate(etdata, year   = lubridate::year(date),   # extract year from date and add year column
                                  month  = lubridate::month(date),  # extract month from date and add month column
                                  julian = lubridate::yday(date))   # add julian day of year column

  return(etdata)
}
