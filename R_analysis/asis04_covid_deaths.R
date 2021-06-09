# Compare sources for Covid deaths
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
#


# in this script --------------------------------------------------------------
# 1 read
# 2 wrangle
# 3 test weekly Covid deaths
# 4 save




# 1 read ----------------------------------------------------------------------
govDths <- readRDS(str_c(.datDir, "dat02_gov_daily_covidDths.RDS"))
onsDths <- readRDS(str_c(.datDir, "dat05_total_weekly_covidDths_2020_to_2021.RDS"))




# 2 wrangle -------------------------------------------------------------------
wkEndingDates <- ISOweek::ISOweek2date(str_c(onsDths$isoYrWk, 5, sep = "-"))

govWkDths <- govDths %>%
  mutate(wkEnding = case_when(date %in% wkEndingDates ~ "wEnding", TRUE ~ "inWk")) %>% 
  mutate(wkDths = reduce(map(0:6, ~ lag(dths, ., 0)), `+`)) %>%
  filter(wkEnding == "wEnding") %>% 
  select(-dths, -wkEnding) %>% 
  rename(wkEnding = date, dths = wkDths)




# 3 test weekly Covid deaths --------------------------------------------------
source(str_c(.testDir, "test_asis04_gov_weekly_covidDths.R"))




# 4 save ----------------------------------------------------------------------
saveRDS(govWkDths, str_c(.datDir, "asis04_gov_weekly_covidDths.RDS"))

