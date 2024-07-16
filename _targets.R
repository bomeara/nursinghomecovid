library(targets)
library(tarchetypes)
library(crew)

source("functions.R")

list(
	tar_target(data_2023, get_data('2023')),
	tar_target(data_2024, get_data('2024')),
	tar_target(covid_data, dplyr::bind_rows(data_2023, data_2024)),	
	tar_target(covid_by_time, aggregate_by_state_and_week(covid_data)),
	tar_target(covid_by_time_US_total, aggregate_by_week(covid_data)),
	tar_target(covid_cleaned, clean_data(covid_data)),
	tar_target(covid_geo, geocode_data(covid_cleaned)),
	tar_target(covid_presentation_data, make_presentation_data(covid_geo)),
	tar_target(covid_succinct_data, make_succinct_data(covid_presentation_data))
)
