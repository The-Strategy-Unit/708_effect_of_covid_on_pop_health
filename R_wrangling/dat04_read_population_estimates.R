# Read national population estimates
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# ONS public domain population estimates are syoa 0-90+ (90-105+ available separately from 'very old' outputs)
# ONS public domain deaths are syoa 0-105+
# ONS national life tables are 0-100+
# CMI user request population estimates & deaths are 0-105+

# read mid-year population estimates series 2001 to 2019 (from web)
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
# read deaths series 1963 to 2019 (from web)
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables
# read national life tables 1980-82 to 2017-19 (from web)
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables
# read mid-year very old population estimates series 2002 to 2019 (from web)
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/ageing/bulletins/estimatesoftheveryoldincludingcentenarians/2002to2019/relateddata
# read mid-year population estimates 1961 to 2018 (from user request) 
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10727populationestimatesanddeathsbysingleyearofageforenglandandwalesandtheuk1961to2018


# in this script --------------------------------------------------------------
# 1 read
# 2 clean
# 3 test very old matches 90+
# 4 save




# 1 read ----------------------------------------------------------------------
# read population estimates
myeSeries <- read_csv(
  str_c(.rawDir, "MYEB1_detailed_population_estimates_series_UK_(2019_geog20).csv"))

# read very old population estimates
path          <- str_c(.rawDir, "engevo2019.xls")
sheets        <- excel_sheets(path)
sheet         <- sheets[str_detect(sheets, "^Table 1")]
myeVeryOldEng <- read_then_csv(sheet, skip_n = 4, path = path)

path            <- str_c(.rawDir, "walesevo2019.xls")
sheets          <- excel_sheets(path)
sheet           <- sheets[str_detect(sheets, "^Table 1")]
myeVeryOldWales <- read_then_csv(sheet, skip_n = 4, path = path)

# # read population estimates (user request)
# path           <- str_c(.rawDir, "ewuksyoadeathspopdata19612018.xlsx")
# sheets         <- excel_sheets(path)
# sheets         <- sheets[str_detect(sheets, "^EW.*pops$")]
# xl_list        <- lapply(sheets, function(x) read_then_csv(x, skip_n = 1, path = path))
# names(xl_list) <- c("male", "female")

# # compile sheets to single df
# xl_list$male   <- xl_list$male %>% mutate(gender = "m") %>% head(-2)
# xl_list$female <- xl_list$female %>% mutate(gender = "f") %>% head(-2)
# myeSeriesUr    <- bind_rows(xl_list)

# read deaths
path           <- str_c(.rawDir, "finalreftables2019.xlsx")
sheets         <- excel_sheets(path)
sheets         <- sheets[str_detect(sheets, "^Table.*[45]")]
xl_list        <- lapply(sheets, function(x) read_then_csv(x, skip_n = 3, path = path))
names(xl_list) <- c("male", "female")

# compile sheets to single df
xl_list$male   <- xl_list$male %>% mutate(gender = "m") %>% tail(-5) %>% head(-2)
xl_list$female <- xl_list$female %>% mutate(gender = "f") %>% tail(-5) %>% head(-2)
deaths         <- bind_rows(xl_list)

# # read life tables - England
# path          <- str_c(.rawDir, "nationallifetables3yearengland.xls")
# sheets        <- excel_sheets(path)
# sheets        <- sheets[str_detect(sheets, "^[[:digit:]]{4}")]
# names(sheets) <- sheets

# lifeTblsE <- map_df(
#   sheets, ~ read_excel(path, sheet = .x, skip = 6, .name_repair = lifeTables_name_repair), .id = "period") %>%
#   # csv caching from ...
#   # https://readxl.tidyverse.org/articles/articles/readxl-workflows.html
#   write_csv(str_c(.datDir, "nationallifetables3yearengland", ".csv"))

# read life tables - England & Wales
path          <- str_c(.rawDir, "nationallifetables3yearew.xlsx")
sheets        <- excel_sheets(path)
sheets        <- sheets[str_detect(sheets, "^[[:digit:]]{4}")]
names(sheets) <- sheets

lifeTblsEW <- map_df(
  sheets, ~ read_excel(path, sheet = .x, skip = 6, .name_repair = lifeTables_name_repair), .id = "period") %>% 
  # csv caching from ...
  # https://readxl.tidyverse.org/articles/articles/readxl-workflows.html
  write_csv(str_c(.datDir, "nationallifetables3yearew", ".csv"))




# 2 clean ---------------------------------------------------------------------
# clean population estimates
mye <- myeSeries %>% 
  pivot_longer(population_2001:population_2019, names_to = "year", values_to = "pop") %>% 
  mutate(year = str_extract(year, "[[:digit:]]{4}")) %>% 
  mutate(across(c(sex, age, year, pop), as.integer)) %>%
  rename(gender = sex) %>%
  mutate(gender = case_when(gender == 1 ~ "m", TRUE ~ "f")) %>% 
  filter(country %in% c("E", "W")) %>% 
  select(-country)

myeSyoa <- mye %>%
  group_by(year, gender, age) %>% 
  summarise(pop = sum(pop)) %>%
  ungroup() %>% 
  arrange(year, gender, age)

agebreaks <- c(0, 1, seq.int(5, 99, by = 5))

agelabels <- c(str_c(
  sprintf("%02d", c(0, 1, seq(5, 85, by = 5)))
  , "-"
  , sprintf("%02d", c(0, seq(4, 90, by = 5)))), "90+")

myeAgeGrp <- myeSyoa %>% 
  mutate(ageGrp = as.character(
    cut(age, breaks = agebreaks, labels = agelabels, right = FALSE))) %>%
  group_by(year, gender, ageGrp) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>% 
  arrange(year, gender, ageGrp)

# clean very old population estimates
myeVeryOldEng <- myeVeryOldEng %>%
  select(-1, -(3:5)) %>% 
  drop_na(., everything()) %>% 
  `colnames<-`(c("year", str_c("age", 90:105))) %>% 
  pivot_longer(age90:age105, names_to = "age", values_to = "pop", names_transform = list(age = function(x) as.integer(str_sub(x, 4, -1)))) %>% 
  mutate(area = "England") %>% 
  mutate(gender = rep(c("p", "m", "f"), each = nrow(.) / 3)) %>% 
  filter(gender != "p") 

myeVeryOldWales <- myeVeryOldWales %>%
  select(-1, -(3:5)) %>% 
  drop_na(., everything()) %>% 
  `colnames<-`(c("year", str_c("age", 90:105))) %>% 
  pivot_longer(age90:age105, names_to = "age", values_to = "pop", names_transform = list(age = function(x) as.integer(str_sub(x, 4, -1)))) %>% 
  mutate(area = "Wales") %>% 
  mutate(gender = rep(c("p", "m", "f"), each = nrow(.) / 3)) %>% 
  filter(gender != "p") 

myeVeryOld <- myeVeryOldEng %>% 
  bind_rows(myeVeryOldWales) %>% 
  select(-area) %>% 
  group_by(year, gender, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(year = as.integer(year))

# # clean population (user request)
# myeSyoaUr <- myeSeriesUr %>%
#   pivot_longer(`1961`:`2018`, names_to = "year", values_to = "pop") %>%
#   mutate(age = case_when(age == "105+" ~ "105", TRUE ~ age)) %>%
#   mutate(across(c(age, year, pop), as.integer))
# 
# agebreaks <- c(0, 1, seq.int(5, 114, by = 5))
# 
# agelabels <- c(str_c(
#   sprintf("%02d", c(0, 1, seq(5, 100, by = 5)))
#   , "-"
#   , sprintf("%02d", c(0, seq(4, 105, by = 5)))), "105+")
# 
# myeAgeGrpUr <- myeSyoaUr %>%
#   mutate(ageGrp = as.character(
#     cut(age, breaks = agebreaks, labels = agelabels, right = FALSE))) %>%
#   group_by(year, gender, ageGrp) %>%
#   summarise(pop = sum(pop)) %>%
#   ungroup() %>%
#   arrange(year, gender, ageGrp)

# clean deaths
deathsSyoa <- deaths %>% 
  pivot_longer(starts_with("..."), names_to = "year", values_to = "dths") %>% 
  rename(age = 1) %>% 
  group_by(gender, age) %>% 
  mutate(year = rep(2019:1963)) %>% 
  ungroup() %>%
  mutate(age = case_when(age == "105+" ~ "105", TRUE ~ age)) %>% 
  mutate(across(c(age, dths), as.integer))

agebreaks <- c(0, 1, seq.int(5, 114, by = 5))

agelabels <- c(str_c(
  sprintf("%02d", c(0, 1, seq(5, 100, by = 5)))
  , "-"
  , sprintf("%02d", c(0, seq(4, 105, by = 5)))), "105+")

deathsAgeGrp <- deathsSyoa %>% 
  mutate(ageGrp = as.character(
    cut(age, breaks = agebreaks, labels = agelabels, right = FALSE))) %>%
  group_by(year, gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>%
  ungroup() %>% 
  arrange(year, gender, ageGrp)

# clean life tables
lifeTblsEW <- lifeTblsEW %>%
  select(-blank) %>% 
  rename(age = x) %>%
  select(period:m_ex) %>% 
  mutate(gender = "m") %>%
  setNames(str_remove(names(.), "m_")) %>% 
  bind_rows(
    lifeTblsEW %>%
      select(-blank) %>% 
      rename(age = x) %>%
      select(starts_with("f"), period, age) %>%
      mutate(gender = "f") %>%
      setNames(str_remove(names(.), "f_"))
    ) %>% 
  mutate(year = as.integer(str_extract(period, ".{4}$")) - 1) %>% 
  mutate(across(c(year, age), as.integer))




# test very old matches 90+ ---------------------------------------------------
source(str_c(.testDir, "test_dat04_very_old_population_matches_90plus.R"))

# append very old
myeSyoa <- myeSyoa %>% 
  filter(year != 2001, age != 90) %>%
  bind_rows(myeVeryOld) %>% 
  group_by(year, gender, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()




# 4 save ----------------------------------------------------------------------
# population series
saveRDS(myeSyoa, str_c(.datDir, "dat04_mye_syoa_england_and_wales_2002_to_2019.RDS"))
saveRDS(myeAgeGrp, str_c(.datDir, "dat04_mye_age_grp_england_and_wales_2001_to_2019.RDS"))
# deaths
saveRDS(deathsSyoa, str_c(.datDir, "dat04_deaths_syoa_england_and_wales_1963_to_2019.RDS"))
saveRDS(deathsAgeGrp, str_c(.datDir, "dat04_deaths_age_grp_england_and_wales_1963_to_2019.RDS"))
# life tables
saveRDS(lifeTblsEW, str_c(.datDir, "dat04_life_tables_england_and_wales_1980_to_2019.RDS"))

