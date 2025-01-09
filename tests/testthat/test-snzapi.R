
conn <- connect_to_ade("POPES_SUB_004")

test_that(
  "get variable values", {
    years <- get_variable_labels(conn, "year")
    expect_true(is.data.frame(years))
    expect_true(nrow(years) > 0)
  }
)

years_select <- 2006:2010

age_groups_select <- c(
  "AGE0004", "AGE0509", "AGE1014", "AGE1519"
)

sex_select = c("SEX1", "SEX2")

conn <- conn %>%
  add_filters(
    year = years_select,
    age = age_groups_select,
    sex = sex_select
  )


pop_est <- collect(conn, add_labels = T)

test_that(
  "check query results", {
    expect_true(is.data.frame(pop_est))
    expect_true(nrow(pop_est) > 1)
    expect_contains(pop_est$sex, "SEX1")
    expect_contains(pop_est$sex_name, "Male")
    expect_true(!("SEX3" %in% pop_est$sex))
    expect_true(!("Total people, sex" %in% pop_est$sex))
    expect_contains(pop_est$year, "2006")
    expect_true(!("2011" %in% pop_est$year_name))
  }
)

pop_est$area_type = get_area_type(pop_est$area_name, pop_est$area, options = c("SA2", "Region"))

region_rows <- which(pop_est$area_type == "Region")
areas_assigned_region <- pop_est$area_name[region_rows]
regions <- c(
  "Northland region", "Auckland region", "Waikato region", "Bay of Plenty region",
  "Gisborne region", "Hawke's Bay region", "Taranaki region", "Manawatu-Whanganui region",
  "Wellington region", "West Coast region", "Canterbury region", "Otago region",
  "Southland region", "Tasman region", "Nelson region", "Marlborough region",
  "Area outside region", "Total, North Island regions", "Total, New Zealand by region/SA2",
  "Total, South Island regions"
)

test_that(
  "assigned region", {
    expect_setequal(regions, areas_assigned_region)
  }
)
