# Interpolate a weekly population exposure
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# there exist different approaches ...
# this script follows approach used by ONS (as oppose to CMI) although practical
# difference is likely to be small


# in this script --------------------------------------------------------------
# 1 read
# 2 interpolate weekly exposure
# 3 save




# 1 read ----------------------------------------------------------------------
fitExp <- read_rds(str_c(.datDir, "asis02_fitted_exposure_2002_to_2022.RDS"))




# 2 interpolate weekly exposure -----------------------------------------------
agebreaks <- c(0, 1, seq.int(5, 90, by = 5), 109)

agelabels <- c(str_c(
  sprintf("%02d", c(0, 1, seq(5, 85, by = 5)))
  , "-"
  , sprintf("%02d", c(0, seq(4, 90, by = 5)))), "90+")

fitExp <- fitExp %>% 
  mutate(ageGrp = as.character(
    cut(age, breaks = agebreaks, labels = agelabels, right = FALSE))) %>%
  group_by(year, gender, ageGrp) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>% 
  arrange(year, gender, ageGrp)

# number of iso weeks in year
isoWeeksinYear <- tibble(
  yearEndDt = seq(ymd('2002-12-31'), ymd('2022-12-31'), by = "year")
  , maxIsoWk = as.integer(isoweek(yearEndDt))) %>% 
  mutate(
      isoWks_n = case_when(maxIsoWk != 53 ~ 52L, TRUE ~ maxIsoWk)
      , isoYr = as.integer(year(yearEndDt))) %>% 
  select(isoYr, isoWks_n)

# calculate weekly exposure
wkFitExp <- fitExp %>%
  mutate(
    myeDt = as_date(str_c(year, "07", "01", sep = "-"))
    , myeEndDt = as_date(str_c(year + 1, "01", "01", sep = "-"))) %>% 
  # calculate 1 Jan population using formula in ONS technical report
  group_by(gender, ageGrp) %>% 
  mutate(
    m = as.numeric(myeEndDt - myeDt)
    , M = as.numeric(myeDt - lag(myeDt, 1L, order_by = year))) %>%
  mutate(exp = pop + ( lead(pop, 1L, order_by = year) - pop ) * (m / M)) %>%
  mutate(expPlus1 = lead(pop, 1L, order_by = year)) %>% 
  ungroup() %>%
  select(year, gender, ageGrp, myeEndDt, exp, expPlus1) %>% 
  mutate(isoWk = 1L) %>% 
  left_join(
    isoWeeksinYear, by = c("year" = "isoYr")
    ) %>%
  group_by(gender, ageGrp) %>%
  nest(dat = c(exp:isoWks_n)) %>%
  # interpolate weekly exposure using formula in ONS technical report
  mutate(
    dat = map(
      dat, ~ .x %>% expand(exp, expPlus1, isoWk = isoWk:isoWks_n) %>% 
        mutate(expWk = exp + ((isoWk - 1) / max(isoWk)) * (expPlus1 - exp)))
    ) %>% 
  unnest(cols = c(dat)) %>% 
  mutate(isoYrWk = str_c(year, "-W", formatC(isoWk, width = 2, format = "d", flag = "0"))) %>% 
  ungroup()

wkFitExp <- wkFitExp %>%
  filter(!year %in%  c(2002, 2022)) %>% 
  rename(isoYr = year) %>% 
  select(isoYr, isoYrWk, isoWk, gender, ageGrp, expWk) %>% 
  rename(exp = expWk)

# plot weekly exposure
ggplot(wkFitExp) +
  geom_line(aes(x = isoYr, y = exp, group = ageGrp, color = ageGrp)
            , show.legend = FALSE) +
  facet_wrap(vars(gender))




# 3 save ------------------------------------------------------------------
saveRDS(wkFitExp, str_c(.datDir, "asis03_weekly_exposure_2003_to_2021.RDS"))

