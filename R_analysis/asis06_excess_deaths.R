# Estimate excess deaths from Covid
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# under-1s omitted from all models because of missing weekly exposure for years after most recent mye
# include weeks with public holidays in models


# in this script --------------------------------------------------------------
# 1 read
# 2 simple counterfactuals
# 3 build models
# 4 model predictions
# 5 summarise
# 6 save




# 1 read ----------------------------------------------------------------------
wkDthsAgeGrp <- readRDS(str_c(.datDir, "dat05_age_grp_weekly_deaths_2010_to_2021.RDS"))
popExpWk     <- readRDS(str_c(.datDir, "asis03_weekly_exposure_2003_to_2021.RDS"))




# 2 simple counterfactuals ----------------------------------------------------
# (1) deaths from 2019 - last normal year before pandemic
expDths2019 <- expand_grid(
  # there is a week 53 in 2020
  isoYr = c(2020:2021), isoWk = c(1:53)
  ) %>%
  # but no week 53 in 2021
  filter(!(isoYr == 2021 & isoWk == 53)) %>% 
  left_join(
    wkDthsAgeGrp %>%
      filter(isoYr == 2019, ageGrp != "under1") %>% 
      select(isoWk, gender:dths)
    , by = "isoWk"
  ) %>%
  filter(!(isoYr == 2020 & isoWk == 53)) %>% 
  # use week 1 2020 as comparator for week 53 2020 (as per CMI)
  bind_rows(
    wkDthsAgeGrp %>%
      filter(isoYr == 2020, isoWk == 1, ageGrp != "under1") %>% 
      select(isoYr:isoWk, gender:dths) %>% 
      mutate(isoWk = 53)
  ) %>% 
  mutate(isoYrWk = str_c(isoYr, "-W", formatC(isoWk, width = 2, format = "d", flag = "0"))) %>% 
  # rm future weeks
  filter(!(isoYr == 2021 & isoWk > recentWk))

# (2) average of deaths over previous 5 years 
expDthsMn5 <- wkDthsAgeGrp %>%
  # there is a week 53 in 2015
  filter(isoYr %in% c(2015:2019), ageGrp != "under1") %>%
  group_by(isoWk, isoYr, isoYrWk) %>%
  summarise(dths = sum(dths)) %>%
  ungroup() %>% 
  select(isoWk, dths) %>% 
  group_by(isoWk) %>% 
  summarise(dths = mean(dths)) %>% 
  ungroup() %>%
  left_join(
    expand_grid(
    # there is a week 53 in 2020
    isoYr = c(2020:2021), isoWk = c(1:53)
    ) %>% 
      # but no week 53 in 2021
      filter(!(isoYr == 2021 & isoWk == 53))
    , by = "isoWk"
  ) %>% 
  mutate(isoYrWk = str_c(isoYr, "-W", formatC(isoWk, width = 2, format = "d", flag = "0"))) %>% 
  # rm future weeks
  filter(!(isoYr == 2021 & isoWk > recentWk))

# (3) average mortality by week, gender, and age group multiplied by exposure (like standardisation)
# see Arburo paper
avgMx <- wkDthsAgeGrp %>%
  filter(isoYr %in% c(2015:2019), ageGrp != "under1") %>%
  left_join(
    popExpWk %>% 
      left_join(age_group_lookup, by = "ageGrp") %>%
      group_by(isoYr, isoYrWk, isoWk, gender, onsAgeGrp) %>% 
      summarise(exp = sum(exp)) %>% 
      ungroup()
    , by = c("isoYr", "gender", "ageGrp" = "onsAgeGrp", "isoWk", "isoYrWk")
    ) %>%
  mutate(exp = exp/(365/7)) %>% 
  group_by(isoWk, gender, ageGrp) %>% 
  mutate(a = dths / exp) %>%
  summarise(avgMx = mean(dths / exp)) %>% 
  ungroup()
    
expDthsAvgMx <- wkDthsAgeGrp %>%
  # retain 2020+ weeks
  filter(isoYr >= 2020, ageGrp != "under1") %>% 
  left_join(
    popExpWk %>%  
      left_join(age_group_lookup, by = "ageGrp") %>%  
      group_by(isoYr, isoYrWk, isoWk, gender, onsAgeGrp) %>% 
      summarise(exp = sum(exp)) %>% 
      ungroup()
    , by = c("isoYr", "gender", "ageGrp" = "onsAgeGrp", "isoWk", "isoYrWk")
    ) %>%
  mutate(exp = exp/(365/7)) %>% 
  left_join(
    avgMx, by = c("isoWk", "gender", "ageGrp")
    ) %>%
  mutate(expDths = avgMx * exp) %>%
  group_by(isoYr, isoWk, isoYrWk) %>%
  summarise(expDths = sum(expDths))




# 3 build models  -------------------------------------------------------------
# setup model df
expDthsModelDf <- wkDthsAgeGrp %>%
  # no weekly exposure for under-1s from 2019 onward
  filter(isoYr %in% c(2015:2021), ageGrp != "under1") %>%
  rename(onsAgeGrp = ageGrp) %>% 
  left_join(
    popExpWk %>% 
      left_join(age_group_lookup, by = "ageGrp") %>%
      group_by(isoYr, isoYrWk, isoWk, gender, onsAgeGrp) %>% 
      summarise(exp = sum(exp)) %>% 
      ungroup()
    , by = c("isoYr", "gender", "onsAgeGrp", "isoWk", "isoYrWk")
    ) %>% 
  mutate(time = group_indices(., isoYrWk)) %>%
  mutate(phWk = case_when(isoWk %in% c(1, 52, 53) ~ 1, TRUE ~ 0)) %>% 
  # would require updating if under-1s not filtered out at top of chain
  mutate(onsAgeGrpN = as.integer(substr(onsAgeGrp, 1, 2))) %>%
  mutate(across(c(gender, onsAgeGrp), as.factor)) %>% 
  mutate(train = case_when(isoYr>= 2020 ~ 0L, TRUE ~ 1L))

# (3a) GAM (negBin)
# GAM model with smooth effects for long term trend, age, & seasonality,
# & interaction between age and seasonality (smooth effects stratified by gender)
gamNb <- gam(
  dths ~
    # public holidays
    phWk
    # log-linear long-term trend
    + time * gender * onsAgeGrp
    # penalized spline for age effect
    + s(onsAgeGrpN, bs = "ps", k = 6, by = gender)
    # penalized cyclic spline for seasonality
    + s(isoWk, bs = "cp", k = 8, by = gender)
    # smooth interaction between age and seasonality
    + ti(isoWk, onsAgeGrpN, bs = c("cp", "ps"), k = c(8, 6), by = gender)
    # population exposure
    + offset(log(exp))
    , family = nb(link = "log"), weights = exp, method = "REML"
    , data = expDthsModelDf %>% filter(train == 1))

# (3b) GAM (Poisson)
# same as (3a) but Poisson not negative binomial
gamPoi <- gam(
  dths ~
    # public holidays
    phWk
    # log-linear long-term trend
    + time * gender * onsAgeGrp
    # penalized spline for age effect
    + s(onsAgeGrpN, bs = "ps", k = 6, by = gender)
    # penalized cyclic spline for seasonality
    + s(isoWk, bs = "cp", k = 8, by = gender)
    # smooth interaction between age and seasonality
    + ti(isoWk, onsAgeGrpN, bs = c("cp", "ps"), k = c(8, 6), by = gender)
    # population exposure
    + offset(log(exp))
    , family = poisson(link = "log"), weights = exp, method = "REML"
    , data = expDthsModelDf %>% filter(train == 1))

# (3c) FluMOMO model
# GLM with ...
glmFm <- glm(
  dths ~
    # public holidays
    phWk
    # log linear long-term trend
    + time * gender * onsAgeGrp
    # seasonality by age and sex
    # full year period
    + sin(2 * pi * isoWk / (365.25/7)) * gender * onsAgeGrp
    + cos(2 * pi * isoWk / (365.25/7)) * gender * onsAgeGrp
    # half year period
    + sin(2 * pi * isoWk / (365.25/2/7)) * gender * onsAgeGrp
    + cos(2 * pi * isoWk / (365.25/2/7))  * gender * onsAgeGrp
    # population exposure
    + offset(log(exp))
    , family = poisson(link = "log"), weights = NULL, method = "glm.fit"
    , data = expDthsModelDf %>% filter(train == 1))




# 4 model predictions ---------------------------------------------------------
# compare mean absolute error (MAE)
mean(abs(residuals(gamNb, type = "response")))
mean(abs(residuals(gamPoi, type = "response")))
mean(abs(residuals(glmFm, type = "response")))

# compare root mean squared error (RMSE)
sqrt(mean(residuals(gamNb, type = "response")^2))
sqrt(mean(residuals(gamPoi, type = "response")^2))
sqrt(mean(residuals(glmFm, type = "response")^2))
# gam Poisson wins!

# gam negBin model
gamNbFit <- predict.gam(gamNb, type = "response", newdata = expDthsModelDf)

# gam Poisson model
gamPoiFit <- predict.gam(gamPoi, type = "response", newdata = expDthsModelDf)

# FluMOMO model
glmFmFit <- predict.glm(glmFm, type = "response", newdata = expDthsModelDf)

# model results
expDthsModelRes <- expDthsModelDf %>% 
  select(-onsAgeGrpN) %>% 
  mutate(gamNbFit = gamNbFit, gamPoiFit = gamPoiFit, glmFmFit = glmFmFit)

# plot model predictions v. actual
ggplot(expDthsModelRes) +
  geom_line(aes(x = time, y = dths, group = onsAgeGrp), color = "grey") +
  geom_line(aes(x = time, y = gamNbFit, group = onsAgeGrp), color = "orange") +
  geom_line(aes(x = time, y = gamPoiFit, group = onsAgeGrp), color = "red") +
  geom_line(aes(x = time, y = glmFmFit, group = onsAgeGrp), color = "blue") +
  facet_wrap(vars(gender)) +
  scale_x_continuous(breaks = c(2010:2021))

# models weekly predictions
expDthsModelResTot <- expDthsModelRes %>% 
  group_by(isoYr, isoWk, isoYrWk) %>% 
  summarise(across(c(dths, gamNbFit, gamPoiFit, glmFmFit), sum)) %>% 
  ungroup()

# simple counterfactuals weekly predictions
simpleCfResTot <- expDthsMn5 %>%
  rename(mn5 = dths) %>% 
  left_join(
    expDths2019 %>%
      group_by(isoWk, isoYr, isoYrWk) %>% 
      summarise(dths = sum(dths)) %>% 
      ungroup() %>% 
      rename(dths19 = dths)
    , by = c("isoWk", "isoYr", "isoYrWk")) %>% 
  left_join(
    expDthsAvgMx %>% 
      rename(avgMx = expDths)
    , by = c("isoWk", "isoYr", "isoYrWk")) %>% 
  left_join(
    wkDthsAgeGrp %>%
      group_by(isoYr, isoYrWk, isoWk) %>% 
      summarise(dths = sum(dths))
    , by = c("isoWk", "isoYr", "isoYrWk"))

# xs deaths v 2019 by age group
xsDthsv2019AgeGrp <- expDths2019 %>% 
  rename(dths19 = dths) %>% 
  left_join(
    wkDthsAgeGrp, by = c("isoWk", "isoYr", "isoYrWk", "gender", "ageGrp")) %>% 
  mutate(xsDths = dths - dths19)

ggplot(expDthsModelResTot) +
  geom_line(aes(x = isoYrWk, y = dths, group = 1), color = "grey") +
  geom_line(aes(x = isoYrWk, y = gamNbFit, group = 1), color = "orange") +
  geom_line(aes(x = isoYrWk, y = gamPoiFit, group = 1), color = "red") +
  geom_line(aes(x = isoYrWk, y = glmFmFit, group = 1), color = "blue") +
  scale_x_discrete(breaks = c(
    "2010-W01", "2011-W01", "2012-W01", "2013-W01", "2014-W01", "2015-W01"
    , "2016-W01", "2017-W01", "2018-W01", "2019-W01", "2020-W01", "2021-W01"))

ggplot(simpleCfResTot) +
  geom_line(aes(x = isoYrWk, y = dths, group = 1), color = "grey") +
  geom_line(aes(x = isoYrWk, y = mn5, group = 1), color = "green") +
  geom_line(aes(x = isoYrWk, y = dths19, group = 1), color = "black") +
  geom_line(aes(x = isoYrWk, y = avgMx, group = 1), color = "gold") + 
  scale_x_discrete(breaks = c(
    "2010-W01", "2011-W01", "2012-W01", "2013-W01", "2014-W01", "2015-W01"
    , "2016-W01", "2017-W01", "2018-W01", "2019-W01", "2020-W01", "2021-W01"))




# 5 summarise -----------------------------------------------------------------
a <- 
expDthsModelResTot %>% 
  left_join(
    waves, by = c("isoYr", "isoWk")
    ) %>% 
  filter(wave != "preCovid") %>% 
  group_by(wave) %>% 
  summarise(across(c(dths, gamNbFit, gamPoiFit, glmFmFit), sum)) %>% 
  mutate(across(c(gamNbFit , gamPoiFit, glmFmFit), ~ dths - .x)) %>% 
  pivot_longer(dths:glmFmFit, names_to = "model", values_to = "dths")

b <- 
simpleCfResTot %>%
  select(-avgMx) %>% 
  left_join(
    waves, by = c("isoYr", "isoWk")
  ) %>% 
  filter(wave != "pre-Covid") %>% 
  group_by(wave) %>% 
  summarise(across(c(dths, mn5, dths19), sum)) %>% 
  mutate(across(c(mn5, dths19), ~ dths - .x)) %>%
  pivot_longer(dths:dths19, names_to = "model", values_to = "dths") %>% 
  filter(model != "dths")

xsDthsSumTb <- a %>% bind_rows(b) %>% 
  pivot_wider(wave, names_from = model, values_from = dths) %>%
  bind_rows(
    summarise(.
              , across(where(is.numeric), sum)
              , across(where(is.character), ~"Total"))) %>%
  arrange(factor(wave, levels = c("First wave", "Summer lull", "Second wave", "pre-Covid")))




# 6 save ----------------------------------------------------------------------
saveRDS(xsDthsSumTb, str_c(.datDir, "asis06_xs_deaths_summary_table.RDS"))
saveRDS(expDthsModelResTot, str_c(.datDir, "asis06_xs_deaths_model_results.RDS"))
saveRDS(expDthsModelRes, str_c(.datDir, "asis06_xs_deaths_model_age_grp_results.RDS"))
saveRDS(simpleCfResTot, str_c(.datDir, "asis06_xs_deaths_simple_counterfactuals.RDS"))
saveRDS(xsDthsv2019AgeGrp, str_c(.datDir, "asis06_xs_deaths_v2019_age_grp.RDS")) 

