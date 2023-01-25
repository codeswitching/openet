#' Timeseries of monthly/daily ET for multiple custom polygons
#'
#' `getOpenET_multipolygon` makes calls to the OpenET Raster/Timeseries/Multipolygon API endpoint.
#'
#' OpenET API documentation:
#'
#' https://open-et.github.io/docs/build/html/ras_timeseries.html#raster-timeseries-multipolygon
#'
#' OpenET API Testbed:
#'
#' https://openet.dri.edu/docs#/raster/raster_timeseries_multipolygon_raster_timeseries_multipolygon_get
#'
#' @param start_date The start date in yyyy-mm-dd format.
#' @param end_date            The end date in yyyy-mm-dd format. Defaults to today's date.
#' @param model               'ensemble_mean', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble_mean'.
#' @param variable            'et', 'ndvi', 'et_reference', 'et_fraction', 'count'. Defaults to 'et'.
#' @param units               'metric', 'english'. Defaults to 'english'.
#' @param ref_et_source       Reference ET source, either 'cimis' (CA only) or 'gridmet' (all states). Defaults to 'cimis'.
#' @param interval            'daily', 'monthly'. Defaults to 'monthly'.
#' @param shapefile_asset_id  Path to an Earth Engine shapefile asset containing the polygons.
#' @param include_columns     Additional (non-OpenET) columns from the shapefile to include in the resulting data frame.
#' @param output_file_format  'geojson', 'csv'. Defaults to 'csv'.
#' @param filename_suffix     String to append to filename, for identification.
#' @param api_key             OpenET API key (long string), obtain from https://auth.etdata.org
#'
#' @returns A string containing the url from which the data can be downloaded as a .csv file.
#'
#' @examples getOpenET_multipolygon(start_date = '2021-01-01', end_date = '2021-12-31', model = 'ensemble_mean', units = 'mm', interval = 'daily', shapefile_asset_id = 'projects/penman/my_shapefile', output_file_format = 'csv', api_key = 'mykey')
#'
#' @export


getOpenET_multipolygon <- function (start_date = '2020-01-01', end_date = as.character(Sys.Date()), model = 'ensemble_mean',
                       variable = 'et', ref_et_source = 'cimis', units = 'english', interval = 'monthly',
                       shapefile_asset_id = 'projects/lsteely/assets/MWD_Parcels', include_columns = 'PVID_PARNU,WT_ACRES',
                       output_file_format = 'csv', filename_suffix = 'ls', api_key = '')

{

  library(httr)      # API tools for R

  httr::set_config(httr::config(ssl_verifypeer=0L))

  url <- 'https://openet.dri.edu/raster/timeseries/multipolygon'  # URL for the API raster multipolygon endpoint

  response <- GET(url, add_headers(accept = "application/json", Authorization = api_key),
                  query = list(start_date         = start_date,
                               end_date           = end_date,
                               model              = model,
                               variable           = variable,
                               ref_et_source      = ref_et_source,
                               units              = units,
                               shapefile_asset_id = shapefile_asset_id,
                               interval           = interval,
                               include_columns    = include_columns,
                               filename_suffix    = filename_suffix))

  if (http_error(response)) {                     # if the server returned an error
    cat('The API server returned an error:\n')
    cat(http_status(response)$message) }
  else {                                          # if successful
    cat(content(response)$status)
    response_url <- content(response)$bucket_url  # read the url for the requested data
    cat('When ready, requested data can be accessed at this url:\n', response_url, '\n')
    }

  return(response_url) # return the url for the requested data (may take minutes or hours)
}
