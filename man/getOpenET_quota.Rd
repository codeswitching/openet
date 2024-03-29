% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOpenET_quota.R
\name{getOpenET_quota}
\alias{getOpenET_quota}
\title{Quota and usage of an OpenET API account}
\usage{
getOpenET_quota(api_key = "")
}
\arguments{
\item{api_key}{OpenET API key (string). Obtain from https://wave.etdata.org/settings/api}
}
\value{
prints quota limits to the console and returns nothing. OpenET quotas reset on the 1st of each month.

<Tier> Account subscription tier

<Monthly Requests> Number of calls to the API

<Max Field IDS> Maximum number of OpenET field ids that can be requested at once through the geodatabase/timeseries endpoint

Quotas for raster endpoints (polygon and multipolygon) only:

<Monthly Export EECU Seconds> Earth Engine computing units (amount of compute) used by raster calls

<Max Area Acres> The maximum number of acres that can be requested in a single call to the polygon or multipolygon endpoitns

<Max Polygons> The maximum number of polygons that can be requested in a single call to the multipolygon endpoint

<Earth Engine Linking> Whether your OpenET account is linked to your Earth Engine account
}
\description{
`getOpenET_quota` fetches the quotas and current usage of the user's API account
}
\details{
OpenET API documentation for this endpoint:

https://openet.gitbook.io/docs/reference/api-reference/account

OpenET API testbed for this endpoint:

https://openet-api.org/#/Manage%20Account%20Information/_account_status_get

Description of subscription plan quotas:

https://openet.gitbook.io/docs/additional-resources/quota
}
\examples{
getOpenET_quota(api_key = 'mykey')
}
