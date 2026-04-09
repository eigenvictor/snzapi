# snzapi

## Description
snzapi is an R package that provides a streamlined interface to the Stats NZ Aotearoa Data Explorer (ADE) API. Built on top of the rsdmx package, it offers higher‑level helper functions for discovering datasets, inspecting metadata, applying filters, and collecting data efficiently. The package is designed to support reproducible analytical workflows and to reduce the risk of downloading unnecessarily large or poorly scoped datasets when working with Stats NZ data.

## Disclaimer
This is a personal project and the package comes with zero guarantee of functionality. Organisations/individuals utilising this package for production purposes must ensure they have appropriate controls. 

This package is in no way officially related to or endorsed by Statistics NZ.

## Contribution
This repo is not actively managed. Contributions are welcome but there is no process for acceptance of these. Acceptance will be adhoc as free time and interest allow.

## Getting started
Install using remotes::install_github("https://github.com/eigenvictor/snzapi")

### API Authentication
You will need an API key to access the ADE API, you can get one here: https://portal.apis.stats.govt.nz/

### Typical Workflow
1. Authenticate with your Stats NZ API key
2. Search for a dataset of interest
3. Connect to a specific ADE data series
4. Inspect available variables and labels
5. Apply filters to narrow the query
6. Collect the data for analysis

## Example
Authenticate with your Stats NZ API key. You can get one here: https://portal.apis.stats.govt.nz/.
```R
library(snzapi)

#' Keep your API key secret. In general, it is safest to not save API keys
#' directly in your code, rather store them in an external yaml file which is
#' listed in your .gitignore file. 
set_api_key("abcdefg")
```

Search for a dataset of interest. There are several approaches to searching for a dataset.

1. Utilise the ADE browser, https://explore.data.stats.govt.nz/. Once you have found an appropriate table, note the table identifier. You can find this by clicking 'Labels' -> 'Both'. The table identifier is in brackets at the beginning of the table title.

2. Extract the full list of tables available through the API.
```R
series = get_all_data_series()
```

3. Extract a subset of the tables available through the API using a key word.
```R
series = search_data_series("population estimates")
```

Connect to a specific ADE data series. This connects to the API and retrieves key metadata about the table. This step does not download any data.
```R
conn = connect_to_ade("POPES_SUB_004")
```

Inspect available variables and labels. You may be able to find more information about the variables in the ADE browser. Navigate to the table by pasting the table identifier in the search bar. Click the 'i' button beside the title to open the information side panel. The information available here varies between tables. 
```R
#' Retrieve the list of variables in a table
get_variables(conn)

#' Retrieve the list of codes and labels for a given variable
get_variable_labels(conn, "age")
```

It is important to consider the data that you need. You can download entire data tables by applying no filters, for larger tables this can take some time. It is generally recommended to filter the API call to the data that is needed, that may be specific variable codes or the latest time period, etc. 
```R
conn = add_filters(conn, sex = c("1", "2"), age = c("01", "02"))
```

Once the appropriate filters are applied the data can be retrieved. You can choose to download the data with only codes, only labels, or both. The example below retrieves both labels and codes. This returns a dataframe with the selected data.
```R
data = collect(conn, add_labels = T, drop_codes = F)
```

Some published series include multiple area types in the same "area" column. This helper function aims to assign, as best as it can, the relevant area type. If you are querying a table with only one area type, you are best to set the area type manually. Check function documentation for known issues with this function.
```R
data$area_type = get_area_type(area_name = data$area_name, area_code = data$area)
```
Note this function is experimental and prone to errors.

## Authors and acknowledgment
Created by Jayden Mudge.

Heavily based on the rsdmx package created by Emmanuel Blondel, Matthieu Stigler, and Eric Persson.
