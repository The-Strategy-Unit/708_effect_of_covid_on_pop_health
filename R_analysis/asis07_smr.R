# Calculate weekly SMRs
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# requires 4 inputs: deaths, exposure, mx, standard population


# in this script --------------------------------------------------------------
# 1 read
# 2 SMR
# 3 rcSMR
# 4 cSMRI
# 5 save




# 1 read  ---------------------------------------------------------------------
# (1a) death counts
# ONS weekly death registrations
wkDthsAgeGrp <- read_rds(str_c(.datDir, "dat05_age_grp_weekly_deaths_2010_to_2021.RDS"))

# (1b) exposure
# interpolated weekly population exposure
wkExpAgeGrp <- read_rds(str_c(.datDir, "asis03_weekly_exposure_2003_to_2021.RDS"))

# (1c) mortality rate (mx)
# national life tables
lifeTbls <- read_rds(str_c(.datDir, "dat04_life_tables_england_and_wales_1980_to_2019.RDS"))

# (1d) standard population
# European standard population
espAgeGrp <- read_rds(str_c(.datDir, "dat03_european_standard_population_age_grp.RDS"))
espSyoa <- read_rds(str_c(.datDir, "dat03_european_standard_population_syoa.RDS"))




# 2 SMR -----------------------------------------------------------------------
# CMI report SMR for ages 20 to 100. However, I'm working with ONS big age groups
# (not syoa) therefore closest match is from 15-44 upward
# extend life tables
lifeTbls <- lifeTbls %>%
  bind_rows(
    lifeTbls %>%
      filter(year == 2018) %>% 
      mutate(period = NA_character_) %>%
      select(-year) %>% 
      nest(data = everything()) %>% 
      bind_cols(
        year = c(2019:2020)
        ) %>% 
      unnest(cols = c(data))
    )

# deaths only available pre-2020 in big age groups
# weekly SMR
smr15to85 <- wkDthsAgeGrp %>%
  filter(!ageGrp %in% c("under1", "01-14")) %>% 
  left_join(
    espAgeGrp %>%
      left_join(
        age_group_lookup, by = "ageGrp"
        ) %>% 
      group_by(onsAgeGrp) %>% 
      summarise(stdPop = sum(stdPop))
    , by = c("ageGrp" = "onsAgeGrp")) %>% 
  left_join(
    wkExpAgeGrp %>%
      left_join(age_group_lookup, by = "ageGrp") %>%
      group_by(isoYr, gender, onsAgeGrp, isoWk, isoYrWk) %>% 
      summarise(exp = sum(exp))
    , by = c("isoYrWk", "isoYr", "isoWk", "gender", "ageGrp" = "onsAgeGrp")
    ) %>%
  group_by(isoYr, isoWk, isoYrWk) %>%
  # ONS & CMI alternative formulae for SMR
  # summarise(smr = sum(dths / expWk * stdPop) * (365/7)) %>%
  summarise(smr = sum(stdPop * dths / (exp/(365/7))) / sum(stdPop)) %>% 
  ungroup()

# plot weekly SMR
ggplot(smr15to85) +
  geom_line(aes(x = isoYrWk, y = smr, group = 1L))

# calculate quarterly and annual centred average SMRs
smr15to85 <- smr15to85 %>%
  mutate(
    qxSmr = roll_mean(smr, 13, align = "center", fill = NA_real_)
    , yxSmr = roll_mean(smr, 53, align = "center", fill = NA_real_))

ggplot(smr15to85) +
  geom_line(aes(x = isoYrWk, y = smr * 100, group = 1L), color = "red", alpha = .1, size = 1) +
  geom_line(aes(x = isoYrWk, y = qxSmr * 100, group = 1L), color = "gold", size = 1) +
  geom_line(aes(x = isoYrWk, y = yxSmr * 100, group = 1L), color = "blue", size = 1) +
  scale_x_discrete(name = NULL, breaks = smr15to85$isoYrWk[seq(1, length(smr15to85$isoYrWk), by = 26)])




# 3 rcSMR ---------------------------------------------------------------------
# relative cumulative mortality over first n weeks of year compared to historical average
# what period to use for the historical average?

# cSMR
cSmr15to85 <- smr15to85 %>% 
  group_by(isoYr) %>% 
  mutate(cSmr = cumsum(smr * 100)) %>% 
  ungroup()

# rcSMR 
rcSmr15to85 <- cSmr15to85 %>%
  # first calculate historical average
  filter(isoYr %in% c(2011:2020)) %>% 
  group_by(isoWk) %>%
  # average for last 10 years by week
  mutate(mncSmr = mean(cSmr, na.rm = TRUE)) %>%
  # average in final week for last 10 years
  group_by(isoYr) %>% 
  mutate(cSmr365 = last(mncSmr)) %>%
  ungroup() %>% 
  mutate(mncSmr365 = mean(cSmr365)) %>%
  # add 2021
  bind_rows(
    cSmr15to85 %>% 
      filter(isoYr >= 2021) 
    ) %>%
  # fill 2021 with previous years values
  group_by(isoWk) %>% 
  fill(mncSmr, mncSmr365) %>%
  mutate(rcSmr = (cSmr - mncSmr) / mncSmr365)

# plot rcSMR
ggplot(rcSmr15to85 %>% mutate(isoYr = as.character(isoYr))) +
  geom_line(aes(x = isoWk, y = rcSmr * 100, group = isoYr, color = isoYr)
            , show.legend = FALSE) +
  geom_dl(aes(x = isoWk, y = rcSmr * 100, color = isoYr, label = isoYr)
          , method = list(dl.trans(x = x, y = y), "last.bumpup"))




# 4 cSMRI ---------------------------------------------------------------------
# cumulative annual standardised mortality improvement (v. previous year)
cSmri15to85 <- cSmr15to85 %>% 
  filter(isoYr != 2021) %>% 
  group_by(isoYr) %>%
  mutate(cSmri365 = last(cSmr)) %>%
  # add 2021
  bind_rows(
    cSmr15to85 %>% 
      filter(isoYr >= 2021) 
    ) %>%
  # fill 2021 with previous years values
  group_by(isoWk) %>% 
  fill(cSmri365) %>%
  group_by(isoWk) %>%
  mutate(cSmri = (lag(cSmr, 1L, order_by = isoYr) - cSmr) / cSmri365) %>%
  ungroup()

# plot cSMRI
ggplot(cSmri15to85 %>% mutate(isoYr = as.character(isoYr))) +
  geom_line(aes(x = isoWk, y = cSmri * 100, group = isoYr, color = isoYr)
            , show.legend = FALSE) +
  geom_dl(aes(x = isoWk, y = cSmri * 100, color = isoYr, label = isoYr)
          , method = list(dl.trans(x = x, y = y), "last.bumpup"))




# 5 save ----------------------------------------------------------------------
saveRDS(smr15to85, str_c(.datDir, "asis07_smr15to85.RDS"))
saveRDS(rcSmr15to85, str_c(.datDir, "asis07_rcSmr15to85.RDS"))

