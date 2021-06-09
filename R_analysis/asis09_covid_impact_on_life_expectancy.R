# Estimate effect of Covid on life expectancy
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# 2 packages for calculating LE (A) PHEindicatormethods (B) demography
# A. PHEindicatormethods package is for an abridged life table using 5 year age intervals; 
# B. demography is made for single year age groups (although in theory works with age groups)


# in this script --------------------------------------------------------------
# 1 read
# 2 expectancy 2020
# 3 expectancy 2019
# 4 LE series
# 5 save




# 1 read ______________________________________________________________________
dthsSyoa       <- read_rds(str_c(.datDir, "dat04_deaths_syoa_england_and_wales_1963_to_2019.RDS"))
myeSyoa        <- read_rds(str_c(.datDir, "dat04_mye_syoa_england_and_wales_2002_to_2019.RDS"))
dthsAgeGrp     <- read_rds(str_c(.datDir, "dat04_deaths_age_grp_england_and_wales_1963_to_2019.RDS"))
myeAgeGrp      <- read_rds(str_c(.datDir, "dat04_mye_age_grp_england_and_wales_2001_to_2019.RDS"))
wkDthsAgeGrp20 <- read_rds(str_c(.datDir, "dat05_age_grp_weekly_deaths_2020_to_2021.RDS"))




# 2 expectancy 2020 -----------------------------------------------------------
# number of weeks in 2020
wks2020 <- wkDthsAgeGrp20 %>%
  filter(isoYr == 2020) %>%
  filter(isoWk == max(isoWk)) %>%
  slice(1) %>%
  pull(isoWk)

# estimate exposure for year-to-date
wksinYr <- 365.25/7

wkDthsAgeGrp20 <- wkDthsAgeGrp20 %>%
  filter(isoYr == 2020) %>% 
  group_by(gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>%
  ungroup()

myeAgeGrp20 <- myeAgeGrp %>% 
  filter(year == 2019) %>%
  group_by(gender, ageGrp) %>%
  summarise(pop = sum(pop) * (wks2020/wksinYr)) %>%
  ungroup()

inputDat20 <- wkDthsAgeGrp20 %>%
  left_join(myeAgeGrp20, by = c("gender", "ageGrp")) %>%
  mutate(startAge = as.integer(str_sub(ageGrp, 1, 2)))

le20phe_f <- phe_life_expectancy(inputDat20 %>% filter(gender == "f"), dths, pop, startAge)
le20phe_m <- phe_life_expectancy(inputDat20 %>% filter(gender == "m"), dths, pop, startAge)

# at birth
le20phe_f$value[1]
le20phe_m$value[1]




# 3 expectancy 2019 -----------------------------------------------------------
dthsSyoa19 <- dthsSyoa %>% 
  filter(year == 2019) %>% 
  select(-year)

popSyoa19 <- myeSyoa %>% 
  filter(year == 2019)

# using A. demography package
# demography::lifetable requires input to be object of class "demogdata" obtained from read.demogdata
dthsSyoa19 %>%
  left_join(popSyoa19, by = c("gender", "age")) %>% 
  mutate(dthRt = dths / pop) %>%
  select(-pop, -dths) %>%
  pivot_wider(names_from = "gender", values_from = "dthRt") %>%
  select(year, age, f, m) %>%
  write.table(str_c(.datDir, "asis09_demogdata_mx_2019.csv"))

popSyoa19 %>%
  pivot_wider(names_from = "gender", values_from = "pop") %>%
  select(year, age, f, m) %>%
  write.table(str_c(.datDir, "asis09_demogdata_pop_exp_2019.csv"))

inputDat19 <- read.demogdata(
  file = str_c(.datDir, "asis09_demogdata_mx_2019.csv")
  , popfile = str_c(.datDir, "asis09_demogdata_pop_exp_2019.csv")
  , type = "mortality"
  , label = "EW"
  , skip = 0, popskip = 0, scale = 1)

le19demog_f <- lifetable(inputDat19, series = "f", type = "period")
le19demog_m <- lifetable(inputDat19, series = "m", type = "period")

# at birth
le19demog_f$ex[[1]]
le19demog_m$ex[[1]]

# using B. PHE PHEindicatormethods
popAgeGrp19 <- myeAgeGrp %>% 
  filter(year == 2019) %>%
  group_by(gender, ageGrp) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

dthsAgeGrp19 <- dthsAgeGrp %>% 
  filter(year == 2019) %>%
  mutate(ageGrp = case_when(ageGrp %in% c("90-94", "95-99", "100-104", "105+") ~ "90+", TRUE ~ ageGrp)) %>% 
  group_by(gender, ageGrp) %>%
  summarise(dths = sum(dths)) %>%
  ungroup()

inputDat19 <- dthsAgeGrp19 %>%
  left_join(popAgeGrp19, by = c("gender", "ageGrp")) %>%
  mutate(startAge = as.integer(str_sub(ageGrp, 1, 2)))

le19phe_f <- phe_life_expectancy(inputDat19 %>% filter(gender == "f"), dths, pop, startAge)
le19phe_m <- phe_life_expectancy(inputDat19 %>% filter(gender == "m"), dths, pop, startAge)
# at birth
le19phe_f$value[1]
le19phe_m$value[1]




# 4 LE series -----------------------------------------------------------------
# demography::lifetable requires input to be demogdata object
dthsSyoa %>%
  filter(year %in% c(2002:2019)) %>% 
  left_join(myeSyoa, by = c("year", "gender", "age")) %>% 
  mutate(dthRt = dths / pop) %>%
  select(-pop, -dths) %>%
  pivot_wider(names_from = "gender", values_from = "dthRt") %>%
  select(year, age, f, m) %>%
  arrange(year, age, f, m) %>% 
  write.table(str_c(.datDir, "asis09_demogdata_mx_2002_to_2019.csv"))

myeSyoa %>%
  pivot_wider(names_from = "gender", values_from = "pop") %>%
  select(year, age, f, m) %>%
  arrange(year, age, f, m) %>% 
  write.table(str_c(.datDir, "asis09_demogdata_pop_exp_2002_to_2019.csv"))

inputDatSeries <- read.demogdata(
  file = str_c(.datDir, "asis09_demogdata_mx_2002_to_2019.csv")
  , popfile = str_c(.datDir, "asis09_demogdata_pop_exp_2002_to_2019.csv")
  , type = "mortality"
  , label = "EW"
  , skip = 0, popskip = 0, scale = 1)

leSeriesdemog_f <- lifetable(inputDatSeries, series = "f", max.age = 105, type = "period")
leSeriesdemog_m <- lifetable(inputDatSeries, series = "m", max.age = 105, type = "period")

# at birth
leSeries <- tibble(
  expBirth_f = leSeriesdemog_f$ex[1,]
  , expBirth_m = leSeriesdemog_m$ex[1,]
  , year = names(leSeriesdemog_f$ex[1,])
  ) %>% 
  pivot_longer(expBirth_f:expBirth_m, values_to = "expBirth", names_to = "gender") %>% 
  mutate(gender = str_sub(gender, start = -1L), year = as.double(year))

# LE 2020
le2020 <- tibble(
    year = 2020, gender = c("f", "m"), expBirth = c(le20phe_f$value[1], le20phe_m$value[1]))

# plot LE series
ggplot() +
  geom_line(aes(x = year, y = expBirth, group = gender, color = gender)
            , show.legend = FALSE, data = leSeries) +
  geom_point(aes(x = year, y = expBirth, group = gender, color = gender)
             , show.legend = FALSE, data = le2020)




# 5 save ----------------------------------------------------------------------
saveRDS(leSeries, str_c(.datDir, "asis09_life_expectancy_2002_to_2019.RDS")) 
saveRDS(le2020, str_c(.datDir, "asis09_life_expectancy_2020.RDS"))

