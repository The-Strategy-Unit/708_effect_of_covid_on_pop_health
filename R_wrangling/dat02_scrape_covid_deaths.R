# Scrape Covid deaths data
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# daily deaths from government dashboard here
# https://coronavirus.data.gov.uk/
# daily hospital deaths from NHSE here,
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
# this is useful on different sources of Covid deaths data 
# https://www.ons.gov.uk/news/statementsandletters/thedifferentusesoffiguresondeathsfromcovid19publishedbydhscandtheons


# in this script --------------------------------------------------------------
# 1 setup
# 2 scrape daily deaths
# 3 scrape hospital deaths
# 4 save




# 1 scrape daily deaths -------------------------------------------------------
# deaths within 28 days of positive test by date of death
govDths <- read_csv(str_c(.rawDir, "data_2021-May-24.csv"))

govDths <- govDths %>%
  filter(areaName %in% c("England", "Wales")) %>% 
  select(-(areaType:areaCode) , -6) %>%
  rename(dths = 2) %>% 
  group_by(date) %>% 
  summarise(dths = sum(dths)) %>% 
  mutate(isoYr = isoyear(date), isoWk = isoweek(date), isoYrWk = iso_year_week(date))

# ggplot(govDths) +
#   geom_line(aes(x = date, y = dths, group = 1))




# 2 scrape hospital deaths ----------------------------------------------------
# Covid-19 total announced deaths
path     <- str_c(.rawDir, "COVID-19-total-announced-deaths-24-May-2021.xlsx")
sheets   <- excel_sheets(path)
sheets   <- sheets[str_detect(sheets, "^Tab1 Deaths by region")]
nhseDths <- read_then_csv(sheets, skip_n = 14, n_max = 2, path = path)

nhseDths <- tibble(
  date = as.Date(unlist(nhseDths[1, 4:(length(nhseDths) -4)]), origin = "1899-12-30")
  , dths = unlist(nhseDths[2, 4:(length(nhseDths) -4)]))
 
padNhseDths <- tibble(
  date = seq.Date(from = as.Date("2020-01-01"), to = nhseDths$date[1]- days(1), by = "days")
  , dths = NA_real_)

nhseDths <- nhseDths %>% 
  bind_rows(padNhseDths) %>% 
  mutate(isoYr = isoyear(date), isoWk = isoweek(date), isoYrWk = iso_year_week(date)) %>% 
  arrange(date)

# ggplot(nhseDths) +
#   geom_line(aes(x = date, y = dths, group = 1))




# 3 save ----------------------------------------------------------------------
saveRDS(govDths, str_c(.datDir, "dat02_gov_daily_covidDths.RDS"))

