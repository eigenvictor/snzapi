
conn <- connect_to_ade("POPES_SUB_004")

test_that(
  "get variable values", {
    years <- get_variable_labels(conn, "year")
    expect_true(is.data.frame(years))
    expect_true(nrow(years) > 0)
  }
)

years_select <- 2020:2024

age_groups_select <- c(
  "01", "02", "03", "04"
)

sex_select = c("1", "2")

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
    expect_contains(pop_est$sex, "1")
    expect_contains(pop_est$sex_name, "Male")
    expect_true(!("3" %in% pop_est$sex))
    expect_true(!("Total people, sex" %in% pop_est$sex))
    expect_contains(pop_est$year, "2024")
    expect_true(!("2019" %in% pop_est$year_name))
  }
)

pop_est$area_type = get_area_type(
  area_name = pop_est$area_name,
  area_code = pop_est$area,
  options = c("SA2", "Region")
)

region_rows <- which(pop_est$area_type == "Region")
areas_assigned_region <- pop_est$area_name[region_rows]
regions <- c(
  "Northland region", "Auckland", "Waikato region", "Bay of Plenty region",
  "Gisborne region", "Hawke's Bay region", "Taranaki region", "Manawatū-Whanganui region",
  "Wellington region", "West Coast region", "Canterbury region", "Otago region",
  "Southland region", "Tasman region", "Nelson region", "Marlborough region",
  "Area outside region", "Total, North Island regions", "Total New Zealand by regional councils",
  "Total, South Island regions"
)

test_that(
  "assigned region", {
    expect_setequal(regions, areas_assigned_region)
  }
)
