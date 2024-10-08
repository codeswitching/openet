# openet
## An R package for accessing the OpenET API

![OpenET screenshot](OpenET_screenshot.PNG?raw=true "OpenET screenshot")

[OpenET](http://openetdata.org) is a web platform that uses the best available science to offer satellite-based estimates of evapotranspiration (ET) for the entire western U.S. In addition to a convenient web map interface, OpenET also offers an API for customized queries.

The `openet` package for R makes your life easier in several ways:

- allows you to automate routine calls to the OpenET API and use the data in markdown reports, dashboards, and reproducible analyses
- returns ET data as an analysis-ready, tidy-formatted R data frame
- lets you easily tweak query parameters like units, time intervals, reference ET source, and spatial statistics
- provides meaningful server error messages to troubleshoot API issues

## Compatibility with new API

A [new version of the OpenET API](https://openetdata.org/api-info/) with expanded features was launched in October 2023. All functions in this library are now compatible with the new API. Some API parameter names and values were changed, so check the help pages for the functions for the current list of acceptable values.

## How to use it

The package provides three functions for accessing the API. Which one you use depends on what type of area you wish to pull ET data from:

1. One or more of OpenET's built-in fields -> `getOpenET_fields()`
2. One user-defined polygon -> `getOpenET_polygon()`
3. Many user-defined polygons -> `getOpenET_multipolygon()`

OpenET's built-in fields are the most convenient to use because you only need to know the field id's, which can be determined from the web app. However, the boundaries might not match up with exactly the area you want. Or you may be interested in a natural area such as a forest or floodplain where there are no defined fields. Moreover, only monthly data is available from this endpoint, not daily. In all these cases, you must use one of the polygon functions.

If your area can be contained within a single polygon, the `getOpenET_polygon()` function is the next best option. It is easy to copy-paste the coordinates of a user-defined polygon into R using the Draw Custom Area feature of the OpenET web app.

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

Note that nearly all parameter values, including 'true', 'false', and dates, must be passed as strings. It is particularly important to treat OpenET field id's as strings since they may contain leading zeros. One exception is the `geometry` parameter in the `getOpenET_polygon` function, which must be passed as a numeric vector of paired long/lat coordinates. See the documentation for this function as there is a clever way to extract the coordinates for a custom polygon using the OpenET web app.

## API Keys

An OpenET API key is required to use this package and must be passed to all functions. Obtain one at https://account.etdata.org/

API keys are renewed from time to time and it can be inconvenient to have the keys baked into your R scripts. An alternative is to store the key in a .txt file somewhere on the system and then read it in at the start of your script:

`my_key <- readLines('my_API_key.txt', warn=F)`

## API Errors

All functions return friendly error messages for most (though not all) common issues.

## Why are functions not provided for the other timeseries endpoints?

The output from all of the other timeseries endpoints can be obtained with the data from /timeseries/features/monthly (`getOpenET_fields`) and a small amount of data wrangling. For example, one can easily derive annual ET totals or mean/median spatial statistics from the monthly ET output. Since the purpose of this package is to make it easy to bring data into R, it is assumed that you will want to fetch the most granular data and do any aggregation or summary stats on your own in R.

## Dependencies

httr, dplyr, tidyr, lubridate

---

Authored and maintained by Lauren Steely *(lsteely at mwdh2o.com)*

*I am not affiliated with the OpenET technical team, please refer to the API documentation for feedback or questions about the API or OpenET itself.*

Oct 2024

## ET dataviz examples

![Dataviz example 2](Dataviz_example2.PNG?raw=true "ET dataviz example 2")

![Dataviz example 3](Dataviz_example3.PNG?raw=true "ET dataviz example 3")

![Dataviz example 1](Dataviz_example.PNG?raw=true "ET dataviz example 1")
