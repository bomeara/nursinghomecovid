library(readr)
library(tidyverse)
library(tidygeocoder)
library(reactable)
library(stringr)
library(crosstalk)
library(leaflet)
library(plotly)
library(lubridate)

get_data <- function(year=2023) {
	tmp <- tempfile(fileext = ".zip")
	url <- paste0('https://download.cms.gov/covid_nhsn/faclevel_', year, '.zip')
	download.file(url, tmp, mode = "wb")
	file_name <- paste0('faclevel_', year, '.csv')
	covid_data <- readr::read_csv(unz(tmp, file_name))
	file.remove(tmp)
	covid_data$`Week Ending` <- lubridate::mdy(covid_data$`Week Ending`)
	print(max(covid_data$`Week Ending`))
	
	return(covid_data)
}

aggregate_by_state_and_week <- function(covid_data) {
	covid_data <- subset(covid_data, `Submitted Data` == "Y" & `Passed Quality Assurance Check` == "Y")
	result <- covid_data |> group_by(`Week Ending`, `Provider State`) |> summarize(
		weekly_resident_covid_deaths = sum(`Residents Weekly COVID-19 Deaths`, na.rm = TRUE),
		weekly_resident_all_deaths = sum(`Residents Weekly All Deaths`, na.rm=TRUE), 
		weekly_all_beds=sum(`Number of All Beds`, na.rm=TRUE), weekly_occupied_beds = sum(`Total Number of Occupied Beds`, na.rm=TRUE), 
		weekly_resident_confirmed_covid_cases = sum(`Residents Weekly Confirmed COVID-19`, na.rm=TRUE), 
		weekly_staff_confirmed_covid_cases = sum(`Staff Weekly Confirmed COVID-19`, na.rm=TRUE),
		weekly_staff_person_count = sum(`Number of All Healthcare Personnel Eligible to Work in this Facility for At Least 1 Day This Week`, na.rm=TRUE),
		weekly_resident_person_count = sum(`Number of Residents Staying in this Facility for At Least 1 Day This Week`, na.rm=TRUE)
		) |> rename(State = `Provider State`)
		result$weekly_resident_covid_percentage <- round(100*result$weekly_resident_confirmed_covid_cases/result$weekly_resident_person_count,2)
		result$weekly_staff_covid_percentage <- round(100*result$weekly_staff_confirmed_covid_cases/result$weekly_staff_person_count,2)
	return(result)
}

aggregate_by_week <- function(covid_data) {
	covid_data <- subset(covid_data, `Submitted Data` == "Y" & `Passed Quality Assurance Check` == "Y" & !is.na(covid_data$`Number of Residents Staying in this Facility for At Least 1 Day This Week`) & !is.na(covid_data$`Number of All Healthcare Personnel Eligible to Work in this Facility for At Least 1 Day This Week`) & !is.na(covid_data$`Residents Weekly Confirmed COVID-19`) & !is.na(covid_data$`Staff Weekly Confirmed COVID-19`))
	result <- covid_data |> group_by(`Week Ending`) |> summarize(
		weekly_resident_covid_deaths = sum(`Residents Weekly COVID-19 Deaths`, na.rm = TRUE),
		weekly_resident_all_deaths = sum(`Residents Weekly All Deaths`, na.rm=TRUE), 
		weekly_all_beds=sum(`Number of All Beds`, na.rm=TRUE), weekly_occupied_beds = sum(`Total Number of Occupied Beds`, na.rm=TRUE), 
		weekly_resident_confirmed_covid_cases = sum(`Residents Weekly Confirmed COVID-19`, na.rm=TRUE), 
		weekly_staff_confirmed_covid_cases = sum(`Staff Weekly Confirmed COVID-19`, na.rm=TRUE),
		weekly_staff_person_count = sum(`Number of All Healthcare Personnel Eligible to Work in this Facility for At Least 1 Day This Week`, na.rm=TRUE),
		weekly_resident_person_count = sum(`Number of Residents Staying in this Facility for At Least 1 Day This Week`, na.rm=TRUE)
		) 
		result$weekly_resident_covid_percentage <- round(100*result$weekly_resident_confirmed_covid_cases/result$weekly_resident_person_count,2)
		result$weekly_staff_covid_percentage <- round(100*result$weekly_staff_confirmed_covid_cases/result$weekly_staff_person_count,2)
	return(result)
}

clean_data <- function(covid_data) {
	covid_data <- subset(covid_data, `Submitted Data` == "Y" & `Passed Quality Assurance Check` == "Y")
	
	missing_percentage_staff_vax <- is.na(covid_data$`Percentage of Current Healthcare Personnel who Received a Completed COVID-19 Vaccination at Any Time`)
	covid_data$`Percentage of Current Healthcare Personnel who Received a Completed COVID-19 Vaccination at Any Time`[missing_percentage_staff_vax] <- covid_data$`Recent Percentage of Current Healthcare Personnel who Received a Completed COVID-19 Vaccination at Any Time`[missing_percentage_staff_vax] 
	
	missing_up_to_date_resident <- is.na(covid_data$`Percentage of Current Residents Up to Date with COVID-19 Vaccines`)
	covid_data$`Percentage of Current Residents Up to Date with COVID-19 Vaccines`[missing_up_to_date_resident] <- covid_data$`Recent Percentage of Current Residents Up to Date with COVID-19 Vaccines`[missing_up_to_date_resident]
	
	missing_up_to_date_staff <- is.na(covid_data$`Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines`)
	covid_data$`Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines`[missing_up_to_date_staff] <- covid_data$`Recent Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines`[missing_up_to_date_staff]
	
	covid_data <- subset(covid_data, covid_data$`Week Ending`==max(covid_data$`Week Ending`))
	
	covid_data <- covid_data[order(covid_data$`Percentage of Current Residents Up to Date with COVID-19 Vaccines`, covid_data$`Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines`, covid_data$`Total Number of Occupied Beds` , decreasing=TRUE),]
	
	return(covid_data)
	
}

make_presentation_data <- function(clean_data) {
	presentation_data <- clean_data[,c(
		"Provider Name",
		"Provider Address",
		"Provider City",
		"County",
		"Provider State",
		"Provider Zip Code",
		"Percentage of Current Residents Up to Date with COVID-19 Vaccines",
		"Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines",
		"Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
		"Weekly Resident COVID-19 Deaths Per 1,000 Residents",
		"Total Resident COVID-19 Deaths Per 1,000 Residents",
		"Residents Weekly Confirmed COVID-19",
		"Residents Weekly COVID-19 Deaths",
		"Residents Weekly All Deaths",
		"Staff Weekly Confirmed COVID-19",
		"Residents Total COVID-19 Deaths",
		"Residents Total All Deaths",
		"Number of All Beds",
		"Total Number of Occupied Beds",
		"Latitude",
		"Longitude",
		"Week Ending",
		"Number of All Healthcare Personnel Eligible to Work in this Facility for At Least 1 Day This Week",
		"Number of Residents Staying in this Facility for At Least 1 Day This Week"
	)]	
	
	presentation_data <- presentation_data |> rename(
		"Facility" = "Provider Name",
		"Address" = "Provider Address",
		"City" = "Provider City",
		"County" = "County",
		"State" = "Provider State",
		"Zip" = "Provider Zip Code",
		"Residents Weekly Confirmed Covid" = "Residents Weekly Confirmed COVID-19",
		"Residents Weekly All Deaths" = "Residents Weekly All Deaths",
		"Residents Total All Deaths" = "Residents Total All Deaths",
		"Residents Weekly Covid Deaths" = "Residents Weekly COVID-19 Deaths",
		"Residents Total Covid Deaths" = "Residents Total COVID-19 Deaths",
		"Beds All" = "Number of All Beds",
		"Beds Occupied" = "Total Number of Occupied Beds",
		"Staff Weekly Confirmed Covid" = "Staff Weekly Confirmed COVID-19",
		"Weekly Resident Covid Cases Per 1,000 Residents" = "Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
		"Weekly Resident Covid Deaths Per 1,000 Residents" = "Weekly Resident COVID-19 Deaths Per 1,000 Residents",
		"Total Resident Covid Deaths Per 1,000 Residents" = "Total Resident COVID-19 Deaths Per 1,000 Residents",
		"Current Residents Up to Date with Covid Vaccines" = "Percentage of Current Residents Up to Date with COVID-19 Vaccines",
		"Current Healthcare Personnel Up to Date with Covid Vaccines" = "Percentage of Current Healthcare Personnel Up to Date with COVID-19 Vaccines"
	)
	
	presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid` <- round(100*presentation_data$`Staff Weekly Confirmed Covid`/presentation_data$`Number of All Healthcare Personnel Eligible to Work in this Facility for At Least 1 Day This Week`,2)
	
	presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`[!is.finite(presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`)] <- NA
	
	presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid` <- round(100*presentation_data$`Residents Weekly Confirmed Covid`/presentation_data$`Number of Residents Staying in this Facility for At Least 1 Day This Week`,2)
	
	presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`[!is.finite(presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`)] <- NA
	
	presentation_data$Facility <- stringr::str_to_title(presentation_data$Facility)
	presentation_data$Address <- stringr::str_to_title(presentation_data$Address)
	presentation_data$City <- stringr::str_to_title(presentation_data$City)
	presentation_data$County <- stringr::str_to_title(presentation_data$County)
	
	presentation_data$`Current Residents Up to Date with Covid Vaccines` <- presentation_data$`Current Residents Up to Date with Covid Vaccines`/100
	presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines` <- presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`/100
	
	presentation_data <- presentation_data[order(presentation_data$`Current Residents Up to Date with Covid Vaccines`, presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`, presentation_data$`Beds Occupied` , decreasing=TRUE),]
	
	presentation_data$PopupLabel <- paste0(presentation_data$Facility, '<br />', presentation_data$Address, ", ", presentation_data$City, ", ", presentation_data$State, "<br />", "<br />", "Staff Up to Date with Covid Vaccines: ", 100*presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`, "%", "<br />", "Residents Up to Date with Covid Vaccines: ", 100*presentation_data$`Current Residents Up to Date with Covid Vaccines`, "%", "<br /><br />",  "Residents Weekly Confirmed Covid: ", presentation_data$`Residents Weekly Confirmed Covid`, "<br />", "<br />", "Residents Weekly Covid Deaths: ", presentation_data$`Residents Weekly Covid Deaths`, "<br />", "Residents Weekly All Deaths: ", presentation_data$`Residents Weekly All Deaths`, "<br />", "Residents Total Covid Deaths: ", presentation_data$`Residents Total Covid Deaths`,  "<br />", "Residents Total All Deaths: ", presentation_data$`Residents Total All Deaths`, "<br /><br />", "Beds All: ", presentation_data$`Beds All`, "<br />", "Beds Occupied: ", presentation_data$`Beds Occupied`, "<br />", "Weekly Resident Covid Cases Per 1,000 Residents: ", presentation_data$`Weekly Resident Covid Cases Per 1,000 Residents`, "<br />", "Weekly Resident Covid Deaths Per 1,000 Residents: ", presentation_data$`Weekly Resident Covid Deaths Per 1,000 Residents`, "<br />", "Total Resident Covid Deaths Per 1,000 Residents: ", presentation_data$`Total Resident Covid Deaths Per 1,000 Residents`, "<br />", "Staff % with covid this week: ", presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`, "%", "<br />",  "Resident % with covid this week: ", presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`, "%", "<br /><br />", "Week Ending: ", presentation_data$`Week Ending`)
	
	presentation_data$ResidentFullPercent <- presentation_data$`Current Residents Up to Date with Covid Vaccines`*100
	presentation_data$StaffFullPercent <- presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`*100
	
	color_domain <- ifelse(is.na(presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`), 0, presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`)
	
	my_palette_fn <- colorNumeric( "viridis", domain=color_domain, reverse=TRUE)
	
	presentation_data$MarkerColor <- my_palette_fn(presentation_data$`Current Healthcare Personnel Up to Date with Covid Vaccines`)
	
	color_domain <- ifelse(is.na(presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`), 0, presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`)
	
	my_palette_fn <- colorNumeric( "plasma", domain=color_domain, reverse=TRUE)
	
	
	presentation_data$MarkerColorStaffCurrentCovid <- my_palette_fn(presentation_data$`Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid`)
	
	color_domain <- ifelse(is.na(presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`), 0, presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`)
	
	my_palette_fn <- colorNumeric( "plasma", domain=color_domain, reverse=TRUE)
	
	
	presentation_data$MarkerColorResidentCurrentCovid <- my_palette_fn(presentation_data$`Inferred Percentage of Current Residents Who Currently Have Covid`)

	
	presentation_data <- presentation_data[order(presentation_data$`Beds Occupied` , decreasing=FALSE),]
	
	return(presentation_data)
}


make_succinct_data <- function(presentation_data) {
	succinct_data <- presentation_data[,c(
		"Facility",
		"State",
		"Current Residents Up to Date with Covid Vaccines",
		"Current Healthcare Personnel Up to Date with Covid Vaccines",
		"Residents Total All Deaths",
		"Residents Total Covid Deaths",
		"Beds All",
		"Beds Occupied",
		"Total Resident Covid Deaths Per 1,000 Residents",
		"PopupLabel",
		"ResidentFullPercent",
		"StaffFullPercent",
		"MarkerColor",
		"MarkerColorStaffCurrentCovid",
		"MarkerColorResidentCurrentCovid",
		"Longitude",
		"Latitude",
		"Inferred Percentage of Current Healthcare Personnel Who Currently Have Covid",
		"Inferred Percentage of Current Residents Who Currently Have Covid"
	)]
	return(succinct_data)
}

geocode_data <- function(covid_data) {
	results <- covid_data[1:9999,] %>% tidygeocoder::geocode(street="Provider Address", city="Provider City", state="Provider State", postalcode="Provider Zip Code", method="census", lat = Latitude, long = Longitude)
	Sys.sleep(10)
	if(nrow(covid_data) >= 10000) {
		second_set <- covid_data[10000:nrow(covid_data),] %>% tidygeocoder::geocode(street="Provider Address", city="Provider City", state="Provider State", postalcode="Provider Zip Code", method="census", lat = Latitude, long = Longitude)
		results <- rbind(results, second_set)
	}
	return(results)
}