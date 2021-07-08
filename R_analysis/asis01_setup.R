# Setup project environment
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# create an environment to store things that are created only for the setup of
# the report, e.g. not data


# parameters ------------------------------------------------------------------
setup_env <- new.env()

setup_env$recentWk <- 17
setup_env$rleSmpSize <- 1000L  # for proof of approach demo

library("tidyverse")
# define Covid waves
setup_env$waves <- expand_grid(
  # 2020 has a week 53
  isoYr = c(2020:2021), isoWk = c(1:53)
) %>%
  # 2021 has NO week 53
  filter(!(isoYr == 2021 & isoWk == 53)) %>% 
  mutate(wave = case_when(
    isoYr == 2020 & isoWk < 25 ~ "First wave"
    , isoYr == 2020 & isoWk < 38 ~ "Summer lull"
    , isoYr == 2020 & isoWk >= 38 ~ "Second wave"
    , isoYr == 2021 ~ "Second wave"
    , TRUE ~ "pre-Covid")) 




# setup -----------------------------------------------------------------------
# packages
library("tidyverse")
library("lubridate")
library("kableExtra")
library("janitor")
library("rvest")
library("readxl")
library("openxlsx")
library("testthat")
library("patchwork")
library("directlabels")
library("RcppRoll")
library("PHEindicatormethods")
library("demography")
library("mgcv")
library("devtools")
library("bookdown")
library("knitr")
library("treemapify")
library("heatwaveR")
library("ggtext")
library("rmarkdown")
library("magick")
# library("ISOweek")  # not loaded
# library("zoo")  # not loaded
# library("scales")  # not loaded

# for use with kableExtra::save_kable()
# webshot::install_phantomjs()

# directories
inOffice      <- FALSE
.baseDir      <- ifelse(inOffice, "Z:/Strategic Analytics/Projects 2020/", "C:/Projects/")
.projDir      <- paste0(.baseDir, "708_effect_of_covid_on_pop_health/")
.rawDir       <- paste0(.projDir, "_rawData/")
.datDir       <- paste0(.projDir, "R_data/")
.asisDir      <- paste0(.projDir, "R_analysis/")
.testDir      <- paste0(.projDir, "R_tests/")
.figDir       <- paste0(.projDir, "figures/")

setwd(.asisDir)

# fonts
library("extrafont")
loadfonts(device = "win", quiet = TRUE)
loadfonts(device = "pdf", quiet = TRUE)
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

# functions
source("../R_functions/helper_funs.R")
source("../R_functions/fx_clean_weekly_deaths_dat.R")
source("../R_functions/lookup_age_group.R")
source("../R_functions/fx_interpolate_exposure.R")
source("../R_functions/fx_date_helper_funs.R")
source("../R_functions/fx_covid_yll_helper_funs.R")
source("../R_functions/fx_quantile_funs.R")
source("../R_functions/fx_model_lm_qx.R")

# set default theme
source("../R_functions/theme_708.R")
setup_env$theme_report_output <- theme_708
theme_set(setup_env$theme_report_output())

# scales
#

# load additional files
#

attach(setup_env)

