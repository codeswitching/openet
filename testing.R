#devtools::install_github('codeswitching/openet')
library(openet)
library(curl)
library(tidyverse)

# Parameters --------------------------------------------------------------

api_key      <- Sys.getenv('OPENET_API_KEY')
start_date   <- '2025-01-01'
end_date     <- '2025-12-31'
model        <- 'eemetric'
reference_et <- 'cimis'
units        <- 'in'
interval     <- 'monthly'
reducer      <- 'mean'
variables    <- 'et'

# Quota --------------

getOpenET_quota(api_key)

# Fields ------------------------------------------------------------------

my_fields <- '21104016818'

et <- getOpenET_fields(
  field_ids  = my_fields,
  start_date = '2025-01-01',
  end_date   = '2025-12-31',
  models      = c('eemetric', 'ensemble'),
  variables  = c('et', 'etof'),
  units      = 'in',
  api_key    = api_key
)


# Polygon -----------------------------------------------------------------

mygeom <- c(-114.73948359489442,33.481782352519936,-114.73576068878174,33.4817734040128,-114.73561048507692,33.47834605781004,-114.73939776420595,33.47833710894796)
mygeom <- c(-114.637116, 33.522224, -114.631024, 33.529552, -114.620975, 33.516723, -114.637116, 33.522224) # DCSE comparison polygon

et <- getOpenET_polygon(geometry = mygeom, start_date = start_date, end_date = end_date, interval = interval,
                        model = model, reference_et = reference_et, api_key = api_key)

# Multipolygon ------------------------------------------------------------

id <- 'projects/lsteely/assets/MWD_Parcels'
attributes <- c('PVID_PARNU', 'WT_ACRES')

id <- 'projects/lsteely/assets/LIQcrops_pvid'
attributes <- c('Acres', 'Crop2020Q1', 'Crop2020Q2', 'Crop2020Q3', 'Crop2020Q4', 'Crop2021Q1', 'Crop2021Q2', 'Crop2021Q3',
                'Crop2021Q4', 'Crop2022Q1', 'Crop2022Q2', 'Crop2022Q3', 'Crop2022Q4', 'Crop2023Q1', 'Crop2023Q2', 'Crop2023Q3', 'Crop2023Q4')

id <- 'projects/lsteely/assets/Alfalfa_Ages'

url <- getOpenET_multipolygon(start_date='2023-01-01', end_date='2023-12-31', model = 'eemetric', interval = 'monthly',
                              variable = 'et', reference_et = 'cimis', asset_id = id, attributes = attributes,
                              api_key = api_key)
url

# Test auto download with curl --------------------------------------------

h <- new_handle()
handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)
curl_download(url, 'et 2023 eemetric download.csv', handle = h)
et <- read_csv('et 2023 eemetric download.csv', col_types = cols(time = col_date(format = "%Y-%m-%d")))

