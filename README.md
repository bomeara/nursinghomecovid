# nursinghomecovid

The goal of this is to present https://data.cms.gov/covid-19/covid-19-nursing-home-data in a more accessible way at http://nursinghomecovid.info.

The site is made with quarto and R: the source code is at <https://github.com/bomeara/nursinghomecovid> . It uses the targets, readr, tidyverse, tidygeocoder, reactable, stringr, crosstalk, plotly, and leaflet packages.

It was made by Brian O'Meara. I'm a biologist, but this is outside my area of expertise: I do not know what vaccination rate is ideal in this setting, what covid rates are considered an acceptable tradeoff, biases in the reporting, and more. I'm just trying to make the data more accessible so that families can make informed decisions and journalists can dig into the data to see if there is anything of interest (facilities doing an exceptionally good or bad job, for example). I came across this dataset online and was surprised at how many facilities have no residents nor health care workers fully vaccinated against covid-19, given over 170,000 deaths in nursing homes in the US (as of Dec 22, 2023).

To get in touch with an issue, go to <https://github.com/bomeara/nursinghomecovid/issues>. For issues with the original data, go to <https://data.cms.gov/covid-19/covid-19-nursing-home-data>.

To run this yourself, go to <https://github.com/bomeara/nursinghomecovid>, clone the repository, and then source `run.R` from within R to run quarto to make the site. Make sure you have the packages listed above installed. This will download the data, process it in R, and make the site.
