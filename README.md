# openet
## An R package for accessing the OpenET API

![OpenET screenshot](OpenET_screenshot.PNG?raw=true "Open ET screenshot")

The `openet` package makes your life easier in several ways:

- allows you to automate routine calls to OpenET and use the data in automated reports
- returns ET data as an analysis-ready, tidy-formatted R data frame in most cases
- lets you tweak parameters like units, time intervals, reference ET source, and pixel aggregation

## How to use it

The package provides three functions for accessing the API. Which one you use depends on what type of area you wish to pull ET data from:

1. One or more of OpenET's built-in fields -> `getOpenET_fields`
2. One user-defined polygon -> `getOpenET_polygon`
3. Many user-defined polygons -> `getOpenET_multipolygon`

OpenET's built-in fields are the most convenient to use because you only need to know the field id's. However, the boundaries might not match up with exactly the area you want, and only monthly data is available from this endpoint. Or you may be interested in a natural area such as a forest where there are no defined fields. In these cases, you must use one of the polygon functions.

If your area can be contained within a single polygon, the `getOpenET_polygon` function is the next best option. It is easy to copy-paste the coordinates of a user-defined polygon into R using the Draw Custom Area feature of the OpenET web app.

The most cumbersome option is `getOpenET_multipolygon` because you must create a Google Earth Engine account, upload a shapefile, provide read access to OpenET's API, and then pass the shapefile's asset ID to the function. Still, with a bit of effort this is not too difficult by following the instructions on the API documentation. Because this data can be quite large, the function does not return the data directly as a data frame like the other two. It instead returns a url that can be used within R to download a .csv file.

## How to install

Install the `devtools` package, if not already installed, then run:

`devtools::install_github('codeswitching/openet')`

`library(openet)`

## Documentation

All functions are fully documented with examples. To read the documentation, use `?getOpenET_fields`

It is recommended to also read the [OpenET API documentation](https://open-et.github.io/docs/build/html/index.html) for the endpoints of interest. There is also a [Swagger testbed](https://openet.dri.edu/docs) for the API, which can be useful for confirming whether parameters are specified correctly.

## Parameters

Parameter names are identical to those given in the API documentation. Note that nearly all parameters must be passed as strings. It is particularly important to treat OpenET field id's as strings since they may contain leading zeros. The one exception is the `geometry` parameter in the `getOpenET_polygon` function, which must be passed as a numeric vector of lat/long coordinates. See the documentation for this function as there is a clever way to extract the coordinates using the OpenET web app.

## API Keys

An OpenET API key is required to use this package; obtain one at https://auth.etdata.org

API keys are renewed from time to time and it can be annoying if the keys are baked into your R scripts. An alternative is to store the key in a .txt file somewhere on your computer and then read it in at the start of your script:

`my_key <- readLines('my_API_key.txt', warn=F)`

Then you only need to keep the key current in one text file.

## Why are functions not provided for the other timeseries endpoints?

The output from all of the other timeseries endpoints can be obtained with the data from /timeseries/features/monthly (`getOpenET_fields`) and a small amount of data wrangling. For example, one can easily derive annual ET totals or mean/median spatial statistics from the monthly ET output. Since the purpose of this package is to make it easy to bring data into R, it is assumed that you will want to fetch the most granular data and do any aggregation or summary stats on your own in R.

## Why are functions not provided for the other raster endpoints?

These endpoints are more complex and have rarer use cases, or they return raster data that is more difficult to work with in R. They may be implemented in the future.

## Dependencies

httr, dplyr

---

Authored and maintained by Lauren Steely *(lsteely at mwdh2o.com)*

*I am not affiliated with the OpenET technical team, please refer to the API documentation for feedback or questions about the API.*

Jan 2023
