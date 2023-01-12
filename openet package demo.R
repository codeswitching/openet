# <openet> PACKAGE DEMO
#
# This script shows some examples of how to fetch and visualize ET data


library(openet)
library(tidyverse)

# Read API key from a local text file

mykey <- readLines('OpenET API key.txt', warn=F)


# EXAMPLE 1: Fetch monthly ET for 3 fields using getOpenET_fields() ----------------------------------------------

myfields <- c('06324308', '06324309', '06324310')  # specify OpenET field ids (look up on the web app)

et <- getOpenET_fields(start_date='2019-01-01', end_date='2021-12-31', state = 'CA', field_ids = myfields, model = 'ensemble_mean',
                       api_key = mykey)   # call the API

# Make a column chart of monthly ET, facetted by field
et |>
ggplot(aes(x = start_date, y = et, fill = field)) +
  geom_col() +   # make a column chart
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +  # remove padding around x axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # remove padding around y axis
  labs(x = '',              # x axis label
       y = 'Monthly ET (inches)',   # y axis label
       title = paste0('ET on field ', myfields)) +    # chart title
  facet_grid(field ~ .) +   # facet by field
  theme_light()


# EXAMPLE 2: Fetch daily ET for a custom polygon using getOpenET_polygon() ----------------------------------------

# Specify custom polygon using lat/long pairs
# this is a location along the Rio Grande floodplain
mygeom <- c(-106.71466827392578,34.80414863571865,-106.71325206756592,34.807302387788766,-106.70790910720827,34.81036793055643,-106.70687913894655,34.81142498782864,-106.70668601989748,34.81253488337081,-106.7070508003235,34.81378570011156,-106.70784473419191,34.81556499825054,-106.70900344848634,34.81767896589377,-106.70958280563356,34.81796082414787,-106.7100977897644,34.81933486932708,-106.71151399612427,34.81873592937432,-106.71089172363283,34.81651629040249,-106.71093463897706,34.814014720838465,-106.71237230300905,34.81109025449285,-106.71406745910646,34.808747083070244,-106.71524763107301,34.8074609531423,-106.71576261520387,34.807496189846084,-106.71664237976076,34.80658003065107,-106.71700716018678,34.80592814194564,-106.71743631362915,34.80522339159909)

et <- getOpenET_polygon(geometry = mygeom, start_date = '2020-01-01', end_date = '2021-12-31', interval = 'daily',
                        model = 'ensemble', ref_et_source = 'gridmet', provisional = 'true', api_key = mykey)   # call the API

# Make a line chart of daily ET for each year
et |>
  ggplot(aes(x = julian, y = et, color = as.factor(year))) +
  geom_line(size = 1) +   # make a column chart
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +    # remove padding around x axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # remove padding around y axis
  labs(x = 'Day of year',              # x axis label
       y = 'Daily ET (inches)',        # y axis label
       title = 'ET on custom polygon') +    # chart title
  theme_light()


# EXAMPLE 3: Fetch user quota and expiration date using getOpenET_quota() ------------------------------------------

quota <- getOpenET_quota(api_key = mykey)   # call the API
quota  # print the quota to the console
