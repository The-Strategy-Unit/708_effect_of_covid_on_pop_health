# Clean ONS weekly death registrations
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
#


# in this script --------------------------------------------------------------
# 1 read
# 2 clean total deaths
# 3 clean Covid deaths
# 4 save




# 1 read ----------------------------------------------------------------------
# registrations 2010 to 2019 
wkDthsDat <- readRDS(str_c(.datDir, "dat01_weekly_deaths_2010_to_2019.RDS"))
# registrations 2020+
wkDthsDat_2020plus <- readRDS(str_c(.datDir, "dat01_weekly_deaths_2020_to_2021.RDS"))
# Covid registrations 2020+
wkDthsDat_covid <- readRDS(str_c(.datDir, "dat01_weekly_covidDths_2020_to_2021.RDS"))




# 2 clean total deaths --------------------------------------------------------
# total deaths
# registrations 2010 to 2019
wkDthsTot <- wkDthsDat %>%
  filter(measureorGrp == "totalDeaths") %>%
  group_by(year, wk, wkEnding) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup()

# registrations 2020+ 
wkDthsTot_2020plus <- wkDthsDat_2020plus %>%
  filter(measureorGrp == "totalDeaths") %>%
  group_by(year, wk, wkEnding) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup()

# append historic to 2020+
wkDthsTot <- wkDthsTot %>% 
  bind_rows(wkDthsTot_2020plus) %>%
  mutate(
    isoWk = isoweek(wkEnding)
    , isoYr = isoyear(wkEnding)
    , isoYrWk = iso_year_week(wkEnding)) %>% 
  select(isoYr, isoWk, isoYrWk, dths) %>% 
  mutate(dths = as.double(dths))

# age group deaths
wkDthsAgeGrp <- wkDthsDat %>%
  filter(str_detect(measureorGrp, "^f_|m_")) %>%
  mutate(gender = str_sub(measureorGrp, 1, 1), ageGrp = str_sub(measureorGrp, 3, -1)) %>% 
  group_by(year, wk, wkEnding, gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup()

# re-factor 2020+ age groups 
wkDthsAgeGrp_2020plus <- wkDthsDat_2020plus %>%
  filter(str_detect(measureorGrp, "^f_|m_")) %>%
  mutate(gender = str_sub(measureorGrp, 1, 1), ageGrp = str_sub(measureorGrp, 3, -1)) %>% 
  group_by(year, wk, wkEnding, gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup() %>% 
  left_join(
      age_group_lookup, by = c("ageGrp")
  ) %>%
  group_by(year, wk, wkEnding, gender, onsAgeGrp) %>% 
  summarise(dths = sum(dths)) %>% 
  ungroup() %>% 
  rename(ageGrp = onsAgeGrp)

# 2020+ improved age groups
wkDthsAgeGrp_2020plus_out <- wkDthsDat_2020plus %>%
  filter(str_detect(measureorGrp, "^f_|m_")) %>%
  mutate(gender = str_sub(measureorGrp, 1, 1), ageGrp = str_sub(measureorGrp, 3, -1)) %>% 
  group_by(year, wk, wkEnding, gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup() %>% 
  mutate(
    isoWk = isoweek(wkEnding)
    , isoYr = isoyear(wkEnding)
    , isoYrWk = iso_year_week(wkEnding)) %>% 
  select(isoYr, isoWk, isoYrWk, gender, ageGrp, dths) %>% 
  mutate(dths = as.double(dths))

# append historic to 2020+
wkDthsAgeGrp <- wkDthsAgeGrp %>%
  bind_rows(wkDthsAgeGrp_2020plus) %>%
  mutate(
    isoWk = isoweek(wkEnding)
    , isoYr = isoyear(wkEnding)
    , isoYrWk = iso_year_week(wkEnding)) %>% 
  select(isoYr, isoWk, isoYrWk, gender, ageGrp, dths) %>% 
  mutate(dths = as.double(dths))




# 3 clean Covid deaths --------------------------------------------------------
# total Covid deaths
wkDthsTot_covid <- wkDthsDat_covid %>% 
  filter(str_detect(measureorGrp, "^[m_|f_]")) %>%
  select(-measureorGrp) %>% 
  group_by(year, wk, wkEnding) %>% 
  summarise(dths = sum(dths)) %>% 
  ungroup() %>% 
  mutate(
    isoWk = isoweek(wkEnding)
    , isoYr = isoyear(wkEnding)
    , isoYrWk = iso_year_week(wkEnding)) %>% 
  select(isoYr, isoWk, isoYrWk, dths) %>% 
  mutate(dths = as.double(dths)) %>% 
  arrange(isoYrWk)

# age group Covid deaths
wkDthsAgeGrp_covid <- wkDthsDat_covid %>% 
  filter(str_detect(measureorGrp, "^[m_|f_]")) %>%
  mutate(
    gender = str_sub(measureorGrp, 1, 1)
    , ageGrp = str_sub(measureorGrp, 3)) %>% 
  select(-measureorGrp) %>%
  mutate(
    isoWk = isoweek(wkEnding)
    , isoYr = isoyear(wkEnding)
    , isoYrWk = iso_year_week(wkEnding)) %>% 
  select(isoYr, isoWk, isoYrWk, gender, ageGrp, dths) %>% 
  mutate(dths = as.double(dths))




# 4 save ----------------------------------------------------------------------
# total deaths
saveRDS(wkDthsTot, str_c(.datDir, "dat05_total_weekly_deaths_2010_to_2021.RDS"))
saveRDS(wkDthsAgeGrp, str_c(.datDir, "dat05_age_grp_weekly_deaths_2010_to_2021.RDS"))
# Covid deaths
saveRDS(wkDthsTot_covid, str_c(.datDir, "dat05_total_weekly_covidDths_2020_to_2021.RDS"))
saveRDS(wkDthsAgeGrp_covid, str_c(.datDir, "dat05_age_grp_weekly_covidDths_2020_to_2021.RDS"))
# 2020-plus deaths with improved age breakdown (for life expectancy analysis)
saveRDS(wkDthsAgeGrp_2020plus_out, str_c(.datDir, "dat05_age_grp_weekly_deaths_2020_to_2021.RDS"))

