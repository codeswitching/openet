#' Timeseries of monthly ET for one or more OpenET fields
#'
#' Makes calls to the OpenET `/geodatabase/timeseries` API endpoint (v3). Use this function when you
#' want to use OpenET's built-in field boundaries and you know the field IDs of the specific field(s).
#' Field IDs can be found by clicking on fields in the OpenET web app (popup title) or by querying
#' the `/geodatabase/metadata/ids` endpoint. Data is returned as an R data frame.
#'
#' Only monthly data is available from this endpoint.
#'
#' As of December 2025, field IDs are 11-digit numerical codes. Old FIPS-prefix field IDs are
#' obsolete.
#'
#' Extractions will only include data within one US State at a time; however, multi-model and
#' multi-variable queries are supported.
#'
#' @param field_ids One or more 11-digit OpenET field IDs, as a character vector. Example: `c('21106155983', '21106158103')`.
#' @param start_date Start date as a string in `'yyyy-mm-dd'` format. Defaults to start of current year.
#' @param end_date End date as a string in `'yyyy-mm-dd'` format. Defaults to two weeks ago.
#' @param models Character vector of one or more ET models: `'ensemble'`, `'eemetric'`, `'ssebop'`,
#'   `'geesebal'`, `'sims'`, `'disalexi'`, `'ptjpl'`. Defaults to `'ensemble'`.
#' @param variables Character vector of one or more variables: `'et'`, `'eto'`, `'etof'`,
#'   `'et_mad_max'`, `'et_mad_min'`, `'ndvi'`, `'pr'`. Defaults to `'et'`.
#' @param units Units for ET output: `'mm'` or `'in'` (inches). Geodatabase data is natively in mm;
#'   if `'in'` is requested, ET variables are converted client-side. Defaults to `'in'`.
#' @param api_key Your OpenET API key as a string.
#'
#' @returns A data frame with columns depending on the requested variables and models. Always includes:
#' \describe{
#'   \item{date}{Date, in `yyyy-mm-dd` format}
#'   \item{month}{Numeric month, extracted from date}
#'   \item{year}{Numeric year, extracted from date}
#'   \item{field_id}{OpenET field ID}
#'   \item{collection}{Model and variable combination (e.g. `'ensemble_et'`)}
#'   \item{value}{Numeric value in the requested units}
#'   \item{units}{`'mm'` or `'in'`}
#' }
#' Returns an empty data frame with a warning if no data is returned or an error occurs.
#'
#' @examples
#' \dontrun{
#' # Single field, default parameters
#' getOpenET_fields(
#'   field_ids = "21106155983",
#'   start_date = "2025-01-01",
#'   end_date = "2025-12-31",
#'   api_key = my_key
#' )
#'
#' # Multiple fields, multiple models
#' getOpenET_fields(
#'   field_ids = c("21106155983", "21106158103"),
#'   start_date = "2024-01-01",
#'   end_date = "2025-12-31",
#'   models = c("ensemble", "ssebop"),
#'   variables = "et",
#'   units = "in",
#'   api_key = my_key
#' )
#' }
#'
#' @export
#' 
getOpenET_fields <- function(field_ids,
                             start_date  = format(Sys.Date(), "%Y-01-01"),
                             end_date    = as.character(Sys.Date() - 14),
                             models      = "ensemble",
                             variables   = "et",
                             units       = "in",
                             api_key     = "") {

  # -- Input validation --------------------------------------------------------

  if (missing(field_ids) || length(field_ids) == 0) {
    warning("field_ids must be provided.")
    return(data.frame())
  }

  if (nchar(api_key) == 0) {
    warning("api_key is required. Register at https://account.etdata.org")
    return(data.frame())
  }

  valid_models    <- c("disalexi", "eemetric", "ensemble", "geesebal", "ptjpl", "sims", "ssebop")
  valid_variables <- c("et", "eto", "etof", "et_mad_max", "et_mad_min", "ndvi", "pr")
  valid_units     <- c("mm", "in")

  models    <- tolower(models)
  variables <- tolower(variables)
  units     <- tolower(units)

  bad_models <- setdiff(models, valid_models)
  if (length(bad_models) > 0) {
    warning("Invalid model(s): ", paste(bad_models, collapse = ", "),
            ". Valid options: ", paste(valid_models, collapse = ", "))
    return(data.frame())
  }

  bad_vars <- setdiff(variables, valid_variables)
  if (length(bad_vars) > 0) {
    warning("Invalid variable(s): ", paste(bad_vars, collapse = ", "),
            ". Valid options: ", paste(valid_variables, collapse = ", "))
    return(data.frame())
  }

  if (!units %in% valid_units) {
    warning("Invalid units: '", units, "'. Must be 'mm' or 'in'.")
    return(data.frame())
  }

  # -- Build and send request --------------------------------------------------

  url <- "https://openet-api.org/geodatabase/timeseries"

  response <- httr::POST(
    url,
    httr::add_headers(
      Authorization  = api_key,
      `Content-Type` = "application/json"
    ),
    encode = "json",
    body = list(
      date_range  = as.list(c(start_date, end_date)),
      interval    = "monthly",
      field_ids   = as.list(as.character(field_ids)),
      models      = as.list(models),
      variables   = as.list(variables),
      file_format = "json"
    )
  )

  # -- Handle HTTP errors ------------------------------------------------------

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    msg <- httr::http_status(response)$message

    # Try to extract detail from the response body
    detail <- tryCatch({
      body <- httr::content(response, as = "parsed", simplifyVector = TRUE)
      if (is.list(body) && !is.null(body$detail)) {
        if (is.data.frame(body$detail)) {
          paste(body$detail$msg, collapse = "; ")
        } else if (is.character(body$detail)) {
          body$detail
        } else {
          as.character(body$detail)
        }
      } else {
        NULL
      }
    }, error = function(e) NULL)

    hint <- switch(as.character(status),
      "401" = "API key may be invalid, expired, or over quota.",
      "403" = "API key may be missing, invalid, or over quota.",
      "404" = "Data may not be available for this date range yet.",
      "422" = "Malformed request. Check parameter types and formatting.",
      "429" = "Rate limit exceeded. Wait before retrying.",
      paste0("HTTP ", status, " error.")
    )

    full_msg <- paste0("OpenET API error (", status, "): ", msg)
    if (!is.null(detail)) full_msg <- paste0(full_msg, "\n  Detail: ", detail)
    full_msg <- paste0(full_msg, "\n  Hint: ", hint)

    warning(full_msg)
    return(data.frame())
  }

  # -- Decompress and parse response -------------------------------------------

  raw_bytes <- httr::content(response, as = "raw")

  if (length(raw_bytes) == 0) {
    warning("API returned an empty response. No data available for the requested parameters.")
    return(data.frame())
  }

  # Detect compression format by magic bytes
  is_gzip <- length(raw_bytes) >= 2 &&
    raw_bytes[1] == as.raw(0x1f) && raw_bytes[2] == as.raw(0x8b)
  is_zip <- length(raw_bytes) >= 2 &&
    raw_bytes[1] == as.raw(0x50) && raw_bytes[2] == as.raw(0x4b)

  etdata <- tryCatch({
    if (is_gzip) {
      # API typically returns gzip-compressed JSON
      raw_text <- rawToChar(memDecompress(raw_bytes, type = "gzip"))
      jsonlite::fromJSON(raw_text, flatten = TRUE)

    } else if (is_zip) {
      # Actual zip archive — extract and read
      tmp_zip <- tempfile(fileext = ".zip")
      tmp_dir <- tempfile()
      on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE), add = TRUE)

      writeBin(raw_bytes, tmp_zip)
      utils::unzip(tmp_zip, exdir = tmp_dir)
      extracted <- list.files(tmp_dir, full.names = TRUE, recursive = TRUE)

      if (length(extracted) == 0) {
        warning("API returned an empty zip archive. No data available for the requested parameters.")
        return(data.frame())
      }

      jsonlite::fromJSON(extracted[1], flatten = TRUE)

    } else {
      # Uncompressed — try parsing raw text directly as JSON
      raw_text <- rawToChar(raw_bytes)
      jsonlite::fromJSON(raw_text, flatten = TRUE)
    }
  }, error = function(e) {
    warning("Failed to parse API response: ", conditionMessage(e))
    return(data.frame())
  })

  if (!is.data.frame(etdata) || nrow(etdata) == 0) {
    warning("No data returned for the requested parameters.")
    return(data.frame())
  }

  # -- Post-process ------------------------------------------------------------

  # Standardise column names to lowercase
  names(etdata) <- tolower(names(etdata))

  # Rename 'time' -> 'date' if present
  if ("time" %in% names(etdata)) {
    etdata <- dplyr::rename(etdata, date = time)
  }

  # Ensure date column is Date type
  if ("date" %in% names(etdata)) {
    etdata <- dplyr::mutate(etdata, date = as.Date(date))
  }

  # Convert mm to inches for ET-related values if requested
  # The API returns a 'value_mm' column for the numeric data
  et_collections <- c("et", "eto", "etof", "et_mad_max", "et_mad_min")
  if (units == "in" && "value_mm" %in% names(etdata)) {
    # Only convert rows whose collection contains an ET-related variable
    et_pattern <- paste0("(", paste(et_collections, collapse = "|"), ")")
    is_et_row <- grepl(et_pattern, etdata$collection, ignore.case = TRUE)

    etdata <- dplyr::mutate(
      etdata,
      value = dplyr::if_else(is_et_row, value_mm / 25.4, value_mm)
    )
    etdata <- dplyr::select(etdata, -value_mm)
  } else if ("value_mm" %in% names(etdata)) {
    etdata <- dplyr::rename(etdata, value = value_mm)
  }

  # Add metadata columns
  etdata$units <- units

  # Extract month and year from date
  if ("date" %in% names(etdata)) {
    etdata <- dplyr::mutate(
      etdata,
      month = lubridate::month(date),
      year  = lubridate::year(date)
    )
  }

  etdata
}
