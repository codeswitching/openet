#' Timeseries of monthly ET for one or more OpenET fields
#'
#' `getOpenET_fields` makes calls to the OpenET /timeseries/features/monthly API endpoint. Use this function when you
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
#' @param state Two-letter state abbreviation, e.g. 'CA', 'AZ'. Defaults to 'CA'.
#' @param field_ids One or more OpenET field ids, as a string or vector of strings. To determine the field id, click on a field within the OpenET web explorer. Id is shown in the popup title.
#' @param start_date The start date as a string in 'yyyy-mm-dd' format.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to two weeks ago.
#' @param model ET model to use: 'ensemble_mean', 'eemetric', 'ssebop', 'geesebal', 'sims', 'disalexi', 'ptjpl'. Defaults to 'ensemble_mean'.
#' @param variable Variable to fetch: 'et', 'ndvi', 'et_reference', 'et_fraction', 'count'. Defaults to 'et'.
#' @param units Units for ET; 'metric' will give mm and 'english' will give inches. Defaults to 'english'.
#' @param api_key Your personal OpenET API key as a string. Can be obtained from https://auth.etdata.org
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
#' <units>       'inches' or 'mm'
#'
#' @examples getOpenET_fields('AZ', c('06323746', '06435895'), '2020-01-01', '2021-12-31', 'ensemble_mean', 'et', 'metric', 'mykey')
#' @examples getOpenET_fields(field_ids = '065746833', start_date = '2021-01-01', end_date = '2022-12-31', api_key = 'mykey')
#'
#' @export

getOpenET_fields <- function (state = 'CA', field_ids = '06323746', start_date = '2021-01-01', end_date = as.character(Sys.Date()-14),
                              model = 'ensemble_mean', variable = 'et', units = 'english', api_key = '')
{
  httr::set_config(httr::config(ssl_verifypeer=0L))  # turn off ssl_verify (for use behind firewall)

  url <- 'https://openet.dri.edu/timeseries/features/monthly' # URL for the API's timeseries/features/monthly endpoint

  # Format the vector of field ids into a single comma-separated string
  field_ids <- paste0('\"', field_ids, '\",', collapse='')  # flatten into single string and add commas between ids
  field_ids <- substr(field_ids, 1, nchar(field_ids)-1)     # remove comma after last id
  field_ids <- paste0('[', field_ids, ']')                  # add brackets around the ids

  response <- httr::POST(url,
                         add_headers(accept = 'application/json',         # type of response to accept
                                     Authorization = api_key,             # API key
                                     content_type = 'application/json'),  # tells server how the body data is formatted
                         encode = 'json',                                 # tells POST how to encode the body list
                         body = list(feature_collection_name = state,
                                     field_ids     = field_ids,
                                     model         = model,
                                     variable      = variable,
                                     start_date    = start_date,
                                     end_date      = end_date,
                                     units         = units,
                                     output_format = 'json'))

  if (http_error(response)) {                     # If the server returned an error...
    cat('The API server returned an error:\n')
    cat(http_status(response)$message, '\n')        # print the server's error message
    helpful_error <- case_when(
      response$status_code == 401 ~ 'API key may be invalid',
      response$status_code == 403 ~ 'API key may be invalid',
      response$status_code == 422 ~ 'Malformed parameter data - check your parameter types and formatting',
    )
    cat(helpful_error, '\n')                        # print a more helpful error message
    etdata <- NULL                                  # return a null data frame
    }
  else {                                          # Else if successful...
    cat('Server reports', http_status(response)$message, '\n')        # print a success message
    response_data <- content(response)              # extract the response data as a list
    etdata <- tryCatch({                            # Unpack the list into a data frame
      data.frame(start_date = as.Date(sapply(response_data,      function(x) x$start_date)),
                 end_date   = as.Date(sapply(response_data,      function(x) x$end_date)),
                 field      = as.character(sapply(response_data, function(x) x$feature_unique_id)),
                 et         = as.numeric(sapply(response_data,   function(x) x$data_value)),
                 units      = ifelse(units == 'english', 'inches', 'mm'))
    }, error = function(e) {                        # If unpacking returns as error
      cat('Malformed parameter data - check that your parameters are specified correctly\n')
      return(NULL)
    })
    }

  # Extract date variables for month and year
  etdata <- mutate(etdata, month = month(start_date),
                   year       = year(start_date))

  return(etdata)  # return the data frame
}
