
# render to html in /docs directory
# rmarkdown::render(input = "../R_analysis/asis90_report_output.Rmd", output_file = "index.html", output_dir = "../docs/")

# build scripts
source("./R_analysis/asis01_setup.R")

source("../R_wrangling/dat01_scrape_weekly_death_registrations.R")
source("../R_wrangling/dat02_scrape_covid_deaths.R")
source("../R_wrangling/dat03_read_european_standard_population.R")
source("../R_wrangling/dat04_read_population_estimates.R")
source("../R_wrangling/dat05_clean_weekly_death_registrations.R")

source("asis02_calculate_exposure_2020.R")
source("asis03_interpolate_exposure.R")
source("asis04_covid_deaths.R")
source("asis05_spread_covid_age_grp_deaths_to_syoa.R")
source("asis06_excess_deaths.R")
source("asis07_smr.R")
source("asis08_covid_years_of_life_lost.R")
source("asis09_covid_impact_on_life_expectancy.R")
source("asis10_heatmap.R")
source("asis11_camoes_plot_excess_deaths.R")

