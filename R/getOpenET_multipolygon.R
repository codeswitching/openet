#' Timeseries of monthly/daily ET for multiple custom polygons
#'
#' Makes calls to the OpenET Raster/Timeseries/Multipolygon API endpoint. Polygons must be uploaded to Google Earth Engine and shared with `openet@googlegroups.com`
#'
#' Note that all parameters are strings. Most parameters have default values and can therefore be omitted,
#' if the defaults are acceptable.
#'
#' OpenET API Documentation for this endpoint:
#'
#' https://openet.gitbook.io/docs/reference/api-reference/raster#timeseries-multipolygon
#'
#' OpenET API Testbed:
#'
#' https://openet-api.org/#/Retrieve%20Raster%20Data/_raster_timeseries_multipolygon_post
#'
#' @param start_date The start date as a string in 'yyyy-mm-dd' format.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to today's date.
#' @param model The ET model: 'ensemble', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble'.
#' @param variable Variable to fetch: 'et', 'et_mad_min', 'et_mad_max', 'eto', 'etr', 'etof', 'ndvi'. Defaults to 'et'.
#' @param units Units for ET; 'mm' or 'in' (inches). Defaults to 'in'.
#' @param asset_id Asset ID for the Google Earth Engine shapefile asset. e.g. 'projects/user/assets/my_shapefile'
#' @param attributes Names of shapefile attributes to include in the response csv file, as an R vector of strings.
#' @param reference_et Reference ET source, either 'cimis' (CA only) or 'gridmet' (all states). Defaults to 'cimis'.
#' @param interval Time interval: 'daily' or 'monthly'. Defaults to 'daily'.
#' @param reducer Pixel aggregation method for the polygon: 'mean', 'median', 'min', 'max', or 'sum'. Defaults to 'mean'.
#' @param overpass Return only data from the satellite overpass days?: 'true' or 'false' as a string. Defaults to 'false'.
#' @param api_key Your personal OpenET API token as a string.
#'
#' @returns Returns a url where the datafile can be downloaded. It may take a few minutes before the file is ready for download. If the url is stored as an R variable, data can then be read in using `read_csv(file = url)`
#'
#' @examples getOpenET_multipolygon(start_date = '2020-01-01', end_date = '2021-12-31', model = 'ensemble', units = 'mm', interval = 'monthly', variable = 'eto', reference_et = 'gridmet', asset_id = 'projects/myname/assets/my_shapefile', api_key = mykey)
#'
#' @export


getOpenET_multipolygon <- function (start_date = '2021-01-01', end_date = as.character(Sys.Date()),
                                    model = 'ensemble', variable = 'et', units = 'in', reference_et = 'cimis',
                                    interval = 'monthly', overpass = 'false', reducer = 'mean', asset_id,
                                    attributes = '', api_key)

{
  httr::set_config(httr::config(ssl_verifypeer=0L))         # turn off ssl_verify (for use behind firewall)

  url <- 'https://openet-api.org/raster/timeseries/multipolygon' # URL for the API's timeseries/features/monthly endpoint

  date_range <- c(start_date, end_date)

  response <- httr::POST(url,
                         httr::add_headers(accept = 'application/json',         # type of response to accept
                                           Authorization = api_key,             # API key
                                           content_type = 'application/json'),  # tells server how the body data is formatted
                         encode = 'json',                                       # tells POST how to encode the body list
                         body = list(model         = model,
                                     variable      = as.list(variable),
                                     date_range    = as.list(date_range),
                                     units         = units,
                                     reducer       = reducer,
                                     reference_et  = reference_et,
                                     asset_id      = asset_id,
                                     attributes    = as.list(attributes),
                                     interval      = interval))

  if (httr::http_error(response)) {                 # If the server returned an error...
    cat('The API server returned the following error:\n')
    cat(httr::http_status(response)$message, '\n')    # print the server's error message
    cat(httr::content(response)$detail, '\n')         # print the server's detailed error message
    helpful_error <- dplyr::case_when(
      response$status_code == 401 ~ 'API key may be invalid, expired, or over quota',
      response$status_code == 403 ~ 'API key may be missing, expired, invalid or over quota',
      response$status_code == 404 ~ 'Data may not be available for this date range [yet]',
      response$status_code == 422 ~ 'Malformed parameter data - check your parameter data types and formatting'
    )
    cat(helpful_error, '\n')                          # print a more helpful error message
    return(NULL)                                      # and return a null data frame
  }
  else {                                            # Else if successful...
    cat('Server reports', httr::http_status(response)$message, '\n')  # print a success message
    response_data <- httr::content(response)           # extract the returned data as a data frame
    etdata <- tryCatch ({                              # test whether the data frame contains meaningful data
       response_url <- response_data$url
    }, error = function(e) {                         # if unpacking returns an error
      cat('Malformed parameter data - check that your parameters are specified correctly\n')
      return(NULL)
    })
  }

 return(response_url)
}
