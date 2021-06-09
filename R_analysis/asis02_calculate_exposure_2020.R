# Calculate population exposure for 2020
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# calculate population exposure for 2020 onwards
# we require an estimate of the population exposure in 2020 to calculate mortality rates in 2020
# CMI reporting describes a method:
# initial estimate of exposure for age x in year y is pop(x-1, y-1) - dths(x-1, y-1)
# initial estimates for past 6 years are then compared to true values and the errors are
# used to estimate an adjustment factor for each ONS age group, which is applied to the
# initial estimate for 2020. See p.6 CMI_2019 v01 methods: Supplement to Working Paper 129




# in this script --------------------------------------------------------------
# 1 read
# 2 initial estimate 2020
# 3 adjustment factor
# 4 final estimate 2020
# 5 estimates for future years
# 6 save




# 1 read ----------------------------------------------------------------------
myeSyoa    <- readRDS(str_c(.datDir, "dat04_mye_syoa_england_and_wales_2002_to_2019.RDS"))
deathsSyoa <- readRDS(str_c(.datDir, "dat04_deaths_syoa_england_and_wales_1963_to_2019.RDS"))
life       <- readRDS(str_c(.datDir, "dat04_life_tables_england_and_wales_1980_to_2019.RDS"))




# 2 initial estimate 2020 -----------------------------------------------------
# true exposure 2014-2019 (last 6 years)
trueExp <- myeSyoa %>% 
  filter(year %in% seq(2014, 2019, 1))

# observed deaths 2014-2019
obsDths <- deathsSyoa %>% 
  filter(year %in% as.character(seq(2014, 2019, 1)))

# these are the age groups used in ONS weekly death registrations pre-2020
syoa_age_group_lookup <- tibble(
  age = unique(obsDths$age[obsDths$age >=1])) %>% 
  mutate(onsAgeGrp = case_when(
    age == 0 ~ "under 1"
    , age <= 14 ~ "01-14"
    , age <= 44 ~ "15-44"
    , age <= 64 ~ "45-64"
    , age <= 74 ~ "65-74"
    , age <= 84 ~ "75-84"
    , age <= 105 ~ "85+"
    , TRUE ~ as.character(age)))

estExp2020 <- trueExp %>% 
  left_join(
    obsDths, by = c("year", "gender", "age")
  ) %>% 
  group_by(gender) %>% 
  filter(year == 2019) %>%
  mutate(estExp = lag(pop, 1, order_by = age) - lag(dths, 1, order_by = age)) %>%
  left_join(
    syoa_age_group_lookup, by = "age"
  ) %>% 
  ungroup()




# 3 adjustment factor ---------------------------------------------------------
# calculate adjustment factor
ageGrpAdj <- trueExp %>% 
  left_join(
      obsDths, by = c("year", "gender", "age")
  ) %>% 
  group_by(gender) %>%
  mutate(estExp = lag(pop, 107L) - lag(dths, 107L)) %>%
  filter(year != 2014, age != 0) %>% 
  left_join(
      syoa_age_group_lookup, by = "age"
  ) %>%
  group_by(year, gender, onsAgeGrp) %>% 
  summarise(trueExp = sum(pop), estExp = sum(estExp)) %>% 
  mutate(adj = trueExp/estExp) %>% 
  group_by(gender, onsAgeGrp) %>%
  summarise(mnAdj = mean(adj)) %>% 
  ungroup()

# plot adjustment
ggplot(ageGrpAdj) +
    geom_bar(aes(x = onsAgeGrp, y = mnAdj - 1), stat = "identity") +
    facet_wrap(vars(gender))




# 4 final estimate 2020 -------------------------------------------------------
# apply adjustment to initial estimate for 2020
expFit20 <- estExp2020 %>% 
  left_join(
    ageGrpAdj, by = c("gender", "onsAgeGrp")
    ) %>% 
  mutate(expFit = estExp * mnAdj, year = 2020) %>% 
  select(year, gender, age, expFit)

# plot population exposure full series
myeSyoa %>% 
  bind_rows(
    expFit20 %>%
      rename(pop = expFit)
    ) %>% 
  mutate(age = as.character(age)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pop, group = age, color = age), show.legend = FALSE) +
  facet_wrap(vars(gender))




# 5 estimates for future years ------------------------------------------------
# require estimates for 2021 and 2022
# to use same method as for 2020 requires deaths in 2020 - not due until June 2021
# ONS use population projections - not sure about this
# interesting question: scale of deaths from Covid will have caused population estimates
# (esp. for some age groups) to have deviated from historical trend. How to calculate
# revised population estimates for current and future years that account for impact of Covid.

# for now: set 2021 and 2022 equal to 2020
expFit21 <- expFit20 %>% mutate(year = 2021)
expFit22 <- expFit21 %>% mutate(year = 2022)

# plot population change
expFit22 %>% 
  bind_rows(expFit20) %>% 
  bind_rows(expFit21) %>%
  rename(pop = expFit) %>% 
  bind_rows(myeSyoa) %>% 
  filter(age != 0) %>%
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>%
  mutate(id = case_when(year < 2020 ~ "obs", TRUE ~ "est")) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = pop, fill = id), stat = "identity")

expFitSeries <- expFit22 %>%
  bind_rows(expFit21) %>%
  bind_rows(expFit20) %>%
  rename(pop = expFit) %>% 
  bind_rows(myeSyoa)




# 4 save ----------------------------------------------------------------------
saveRDS(expFitSeries, str_c(.datDir, "asis02_fitted_exposure_2002_to_2022.RDS"))

