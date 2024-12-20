# snzapi

## Description
Helper functions extending the rsdmx package to help interact with the Stats NZ Aotearoa Data Explorer API

## Getting started
Install using remotes::install_git("https://github.com/eigenvictor/snzapi")

You will need an API key to access the ADE API, you can get one here: https://portal.apis.stats.govt.nz/

Set your API key using set_api_key("abcdefg")

Get the full list of data series available in the ADE with the get_all_data_series() function, or search for key words using search_data_series()

E.g. search_data_series("population estimates")

Once you've found the resourceId for the series you're interested in, establish an API connection with connect_to_ade()

Add filters to the query with add_filters() (This is important, some series are quite large!)

To find what variables you can apply filters to, use get_variables(). To find the possible values the variable takes, use get_variable_labels()

When you're ready to download the data, use collect() on your connection.

## Example
library(snzapi)

set_api_key(config::get("stats_nz_key"))

snz_population_table <- search_data_series("population estimates")
snz_population_table

conn <- connect_to_ade("POPES_SUB_004")

get_variables(conn)
get_variable_labels(conn, "sex")
get_variable_labels(conn, "age")

conn <- add_filters(conn, sex = c("SEX1", "SEX2"), age = c("AGE1539", "AGE4064"))

data <- collect(conn, add_labels = T)

data\$area_type = get_area_type(area_name = data\$area_name, area_code = data$area)

head(data)

## Authors and acknowledgment
Created by Jayden Mudge
Heavily based on the rsdmx package created by Emmanuel Blondel, Matthieu Stigler, and Eric Persson
