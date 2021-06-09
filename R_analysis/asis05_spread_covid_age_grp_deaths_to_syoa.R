# Spread Covid age group deaths to syoa 
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# spread Covid age group deaths to single year of age using distribution of all
# deaths from most recent national life tables


# in this script --------------------------------------------------------------
# 1 read
# 2 spread to syoa
# 3 save




# 1 read ----------------------------------------------------------------------
# life table
lifeTbls <- read_rds(str_c(.datDir, "dat04_life_tables_england_and_wales_1980_to_2019.RDS"))
# ONS Covid deaths
wkDthsAgeGrpCovid <- readRDS(str_c(.datDir, "dat05_age_grp_weekly_covidDths_2020_to_2021.RDS"))




# 2 spread to syoa ------------------------------------------------------------
# Covid deaths ytd
ytdDthsCovid <- wkDthsAgeGrpCovid %>%
  left_join(
    waves, by = c("isoYr", "isoWk")
    ) %>% 
  group_by(wave, gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>% 
  ungroup()

ageGrpBrks <- c(0, 1, seq.int(5, 90, by = 5), 101)

ageGrpLbls <- c(str_c(
  sprintf("%02d", c(0, 1, seq(5, 85, by = 5)))
  , "-"
  , sprintf("%02d",c(0, seq(4, 90, by = 5)))), "90+")

# reference is distribution of deaths from most recent NLT
refDist <- lifeTbls %>% 
  filter(year == 2018) %>% 
  select(age, gender, dx) %>% 
  mutate(ageGrp = as.character(
    cut(age, breaks = ageGrpBrks, labels = ageGrpLbls, right = FALSE))) %>% 
  group_by(gender, ageGrp) %>% 
  mutate(ageGrpDx = sum(dx)) %>% 
  mutate(pcnt = dx / ageGrpDx) %>%
  ungroup()

ytdDthsCovidSyoa <- ytdDthsCovid %>%
  left_join(refDist, by = c("gender", "ageGrp")) %>% 
  group_by(wave, gender, ageGrp) %>% 
  mutate(ageGrpDths = max(dths)) %>% 
  mutate(syoaDx = ageGrpDths * pcnt) %>%
  ungroup() %>% 
  select(wave, gender, age, syoaDx) %>% 
  rename(dths = syoaDx)

source(str_c(.testDir, "test_asis05_spread_age_grp_covidDths.R"))




# 3 save ----------------------------------------------------------------------
saveRDS(ytdDthsCovidSyoa, str_c(.datDir, "asis05_cumulative_syoa_covidDths.RDS"))

