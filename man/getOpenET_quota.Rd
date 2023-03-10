% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOpenET_quota.R
\name{getOpenET_quota}
\alias{getOpenET_quota}
\title{Quota and expiration date of an OpenET API key}
\usage{
getOpenET_quota(api_key = "")
}
\arguments{
\item{api_key}{OpenET API key (long string), obtain from https://auth.etdata.org}
}
\value{
A data frame containing the various quota limits and data use to date, and a console message showing the expiration date. The OpenET quotas reset every 30 days.

<acre_years>, <acre_months>, <acre_days> Total (acreage x timestep) quotas for the 30-day period

Quotas for raster endpoints (polygon and multipolygon) only:

<total_data_month>, <total_data_day> Total acreage that can be retrieved at daily or monthly timestep during the 30-day quota period

<year_access_start>, <year_access_end> The earliest and latest years of data available

<max_acres> The maximum number of acres that can be requested in a single call to the polygon or multipolygon endpoitns

<max_polygons> The maximum number of polygons that can be requested in a single call to the multipolygon endpoint
}
\description{
`getOpenET_quota` fetches the quota and expiration date of the user's API key
}
\details{
OpenET API documentation with description of quotas:

https://open-et.github.io/docs/build/html/faq.html

OpenET API Testbed:

https://openet.dri.edu/docs#/home/user_quotas_home_user_quotas_get
}
\examples{
getOpenET_quota(api_key = 'mykey')
}
