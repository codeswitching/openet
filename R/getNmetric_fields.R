#' Timeseries of monthly ET for one or more PVID parcels IDs
#'
#' Makes calls to the DCSE N-METRIC GetByFieldID API endpoint. Use this function when you want to use PVID's parcel boundaries,
#' and you know the parcel id's of the specific field(s). Data is returned as an R data frame.
#'
#' Note that all parameters are strings, esp. field id's which may have leading zeroes. Most parameters have default values
#' and can therefore be omitted if the defaults are acceptable.
#'
#' DCSE N-METRIC API Documentation:
#'
#' https://web.postman.co/workspace/f32f3b42-8f14-46c4-8844-ee106a414cab/documentation/30264904-71fadd49-897d-4f50-8478-5bcbda6e761a
#'
#' @param field_ids One or more PVID parcelids, as a string or vector of strings, with leading zeroes removed.
#' @param start_date The start date as a string in 'yyyy-mm-dd' format. Defaults to start of current year.
#' @param end_date The end date as a string in 'yyyy-mm-dd' format. Defaults to 60 days ago.
#' @param units Units for ET; 'mm' or 'in' (inches). Defaults to 'in'.
#' @param interval Time interval: 'daily' or 'monthly'. Defaults to 'daily'.
#'
#' @returns Returns a data frame with (months) x (fields) rows and 5 columns:
#'
#' <date>        Date, in 'yyyy-mm-dd' format
#'
#' <month>       Numeric month, extracted from <date>
#'
#' <year>        Numeric year, extracted from <date>
#'
#' <parcel_id>   PVID parcel id
#'
#' <et>          Total daily or monthly ET (sum of all daily values) in either inches or mm, depending on chosen units
#'
#' <units>       'in' or 'mm'
#'
#' @export

getNmetric_fields <- function (field_ids = '1728', start_date = paste0(lubridate::year(Sys.Date()), '-01-01'), end_date = as.character(Sys.Date()-60),
                               interval = 'daily', units = 'in')
{
  httr::set_config(httr::config(ssl_verifypeer=0L))  # turn off ssl_verify (for use behind firewall)

  url <- 'https://mwdmetric2.dcse.com/METRICPortal_Test/api/QueryEt/GetByFieldId'  # URL for the endpoint

  response <- httr::GET(url, query = list(field_ids  = field_ids,
                                          start_date = start_date,
                                          end_date   = end_date,
                                          interval   = interval,
                                          units      = units,
                                          user       = 'admin',
                                          password   = 'Metric!4715',
                                          output_file_format = 'csv'))

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
      # Extract date variables for month and year
      etdata <- dplyr::mutate(response_data,
                              month = lubridate::month(date),
                              year  = lubridate::year(date),
                              units = units)   # Add a units column
      }, error = function(e) {                        # If unpacking returns as error
        cat('Malformed parameter data - check that your parameters are specified correctly\n')
        return(NULL)
      })
  }

  return(etdata)  # return the data frame
}
