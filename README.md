# **openet**: an R package for accessing the OpenET API
by **Lauren Steely**

![OpenET screenshot](OpenET_screenshot.PNG?raw=true "OpenET screenshot")

[OpenET](http://openetdata.org) is a web platform that uses the best available science to offer satellite-based estimates of evapotranspiration (ET) for the entire western U.S. In addition to a convenient web map interface, OpenET also offers an API for customized queries.

The `openet` package for R makes your life easier in several ways:

- allows you to automate routine calls to the OpenET API and use the data in markdown reports, dashboards, and reproducible analyses
- returns ET data as an analysis-ready, tidy-formatted R data frame
- provides cleaned dates and extracted month and year variables
- lets you easily tweak query parameters like units, time intervals, and reference ET source
- provides meaningful server error messages to troubleshoot API issues

## How to use it

The package provides three functions for accessing the API. Which one you use depends on what type of area you wish to pull ET data from:

1. One or more of OpenET's built-in fields (visible in the web app) -> `getOpenET_fields()`
2. One user-defined polygon -> `getOpenET_polygon()`
3. Many user-defined polygons -> `getOpenET_multipolygon()`

OpenET's built-in fields are the most convenient to use because you only need to know the field id's, which can be determined from the web app (click on a field and look in the title bar of the pop up). However, you might not want to use this method if: the boundaries don't match up exactly with the area you want; you are interested in a natural area such as a forest or floodplain; or you want daily data rather than monthly. In these cases, you must use one of the polygon functions.

If your area can be contained within a single polygon, the `getOpenET_polygon()` function is the next best option. You can even draw your polygon in the OpenET web app using the Draw Custom Area feature, and then copy-paste the coordinates into an R vector.

The most powerful but least convenient option is `getOpenET_multipolygon()`, in which you can upload your own custom shapefiles containing multiple polygons. However, you must first have a Google Earth Engine account that is linked to OpenET, upload a shapefile to GEE, share the shapefile with OpenET's API, and then pass the shapefile's asset ID to the function. Still, with a bit of effort this is not too difficult by following the [instructions](https://openet.gitbook.io/docs/reference/api-reference/raster#timeseries-multipolygon) on the API documentation for the raster/multipolygon endpoint. Because this data can be quite large, the function does not return the data directly as a data frame like the other two. It instead returns a url that can be used to download a .csv file, either through your browser or within R.

Finally, there is a `getOpenET_quota()` function which will provide the expiration date and quota limits for your API key. This is also useful for testing that your key is valid and working.

## How to install

Install the `devtools` package from CRAN, if not already installed, then run:

`devtools::install_github('codeswitching/openet')`

`library(openet)`

## Documentation

All functions are fully documented with examples. To read the documentation, use e.g. `?getOpenET_fields`

It is recommended to also read the [OpenET API documentation](https://openet.gitbook.io/docs/) for the endpoints of interest. There is also a [testbed](https://openet-api.org/) for the API, which can be useful for confirming whether your parameters are specified correctly.

See the vignette `openet-package-demo.Rmd` in this repository for examples of how to fetch and visualize ET data.

## Parameters

Parameter names and values are *usually* identical to those given in the API documentation.

Note that nearly all parameter values, including 'true', 'false', dates, and field ids, must be passed as strings. It is particularly important to treat OpenET field id's as strings since they may contain leading zeros. One exception is the `geometry` parameter in the `getOpenET_polygon` function, which must be passed as a numeric vector of paired long/lat coordinates. See the documentation for this function as there is a clever way to extract the coordinates for a custom polygon using the OpenET web app.

## API Keys

An OpenET API key is required to use this package and must be passed to all functions. Obtain one at https://account.etdata.org/

API keys are renewed from time to time and it can be inconvenient to have the keys baked into your R scripts. An alternative is to store the key in a .txt file somewhere on the system and then read it in at the start of your script:

`my_key <- readLines('my_API_key.txt', warn=F)`

## API Errors

All functions return friendly error messages for most (though not all) common issues.

## Dependencies

httr, dplyr, tidyr, lubridate

---

Authored and maintained by **Lauren Steely**. Please send bug reports and suggestions to *lsteely at mwdh2o.com*.

*I am not affiliated with the OpenET technical team, please refer to the API documentation for feedback or questions about the API or OpenET itself.*

June 2025

## Example calls

```r
library(openet)

### Check quota usage

getOpenET_quota(my_api_key)

### Get monthly ET data for 3 built-in fields for 2024

et <- getOpenET_fields(
  field_ids  = c('01234567', '01234568', '01234569'),
  start_date = '2024-01-01',
  end_date   = '2024-12-31',
  model      = 'ensemble',
  api_key    = my_api_key
)

### Get daily ET data for a user-defined polygon defined by lat-long coordinates

mygeom <- c(-114.73948359489442,33.481782352519936,-114.73576068878174,33.4817734040128,-114.73561048507692,33.47834605781004,-114.73939776420595,33.47833710894796)

et <- getOpenET_polygon(
  geometry     = mygeom,
  start_date   = '2023-01-01',
  end_date     = '2023-12-31',
  interval     = 'daily',
  model        = 'ssebop',
  reference_et = 'cimis',
  api_key      = my_api_key
)

### Get daily ET data for multiple polygons from a shapefile uploaded to Google Earth Engine

my_shapefile  <- 'projects/assets/farm_fields' # asset id of shapefile in GEE
my_attributes <- c('acres', 'owner')           # shapefile attributes to include in returned data

url <- getOpenET_multipolygon(
  start_date = '2023-01-01',
  end_date   = '2023-12-31',
  model      = 'eemetric',
  interval   = 'daily',
  variable   = 'et',
  asset_id   = my_shapefile,
  attributes = my_attributes,
  api_key    = my_api_key,
  reference_et = 'cimis'
)

url  # download in R with curl or paste into browser (may take a few minutes before data is ready)
```

## ET dataviz examples

![Dataviz example 2](Dataviz_example2.PNG?raw=true "ET dataviz example 2")

![Dataviz example 3](Dataviz_example3.PNG?raw=true "ET dataviz example 3")

![Dataviz example 1](Dataviz_example.PNG?raw=true "ET dataviz example 1")
