# Estimate total years of life lost to Covid
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# 2 approaches:
# A. crude - combine Covid deaths by syoa with life table to estimate
# total/mean YLL;
# B. sampling - for each syoa take weighted sample of all possible
# years of life remaining (weights = probability of death at each age, size of
# sample = number of Covid deaths)
# Main advantage of sampling approach is a richer set of outputs incl. full
# distribution of yll for all Covid deaths rather than just summary measures.
# If sample is large summary measures should agree closely. Sampling approach
# developed to help Anastasiia with work on eol project.

# For Anastasiia:
# inputs - (a) Covid deaths by syoa and gender; (b) national life tables
# output - estimate of remaining life expectancy for each Covid fatality (or all excess deaths)
# i.e. in the absence of the Covid pandemic how many years would this person have lived for?
 

# in this script --------------------------------------------------------------
# 1 read
# 2 estimate qx 90+
# 3 sampling - proof of concept
# 4 for Anastasiia
# 5 sampling - Covid deaths
# 6 crude approach
# 7 save




# 1 read ----------------------------------------------------------------------
# national life tables
life <- readRDS(str_c(.datDir, "dat04_life_tables_england_and_wales_1980_to_2019.RDS"))
# ONS Covid deaths
ytdDthsCovidSyoa <- readRDS(str_c(.datDir, "asis05_cumulative_syoa_covidDths.RDS"))




# 2 estimate qx 90+ -----------------------------------------------------------
# in order to estimate remaining life expectancy beyond age 100 an estimate of qx
# (the probability a person aged x will die before age x+1) is required.  
# qx from age 90 is linear so I fit a linear model and use the model to predict
# qx for ages 100 to 110 (separately for men & women).
lifeDat <- life %>% filter(year == 2018)

qxAge90plus <- lifeDat %>%
  filter(age >=90L) %>% 
  select(age, gender, qx) %>%
  group_by(gender) %>% 
  nest(dat = c(age, qx)) %>% 
  ungroup()

qxPred <- qxAge90plus %>% 
  mutate(mod = map(dat, model_lm_qx)) %>%
  mutate(newDat = list(tibble(age = 101:110))) %>% 
  mutate(qxPred = map2(mod, newDat, predict)) %>% 
  mutate(newDat = map2(newDat, qxPred, ~ .x %>% mutate(qx = .y))) %>% 
  select(gender, newDat) %>% 
  unnest(cols = c(newDat))

lifeDat <- lifeDat %>%
  select(gender, age, qx) %>% 
  bind_rows(qxPred) %>% 
  arrange(gender, age)

# plot qx
ggplot(lifeDat %>% filter(age >= 65)) +
  geom_point(aes(x = age, y = qx, group = gender, color = gender)) +
  geom_vline(xintercept = 100, color = "orange", linetype = "dashed") +
  scale_color_viridis_d(option = "inferno")




# 3 sampling - proof of concept -----------------------------------------------
# test approach with constant sample size across all age/gender combinations
# calculate probabilities of death
pDthDat <- lifeDat %>%
  select(gender, age, qx) %>%
  mutate(start = row_number()) %>% 
  group_by(gender) %>% 
  mutate(end = max(start)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(dat = list(.[start:end, ])) %>% 
  group_by(gender) %>% 
  mutate(dat = map(dat, ~ .x %>% make_pDth())) %>% 
  ungroup()

source(str_c(.testDir, "test_asis08_probabilities_of_death_sum_to_one.R"))

# use probabilities to sample remaining life expectancy at each age 
set.seed(54321)  # for reproducibility
rleSmpDat <- pDthDat %>% 
  mutate(
    rleSmp = map(
      dat, ~ sample(x = .x$rle, size = rleSmpSize, replace = TRUE, prob = .x$pDth))
    , rleSmpMn = map_dbl(
      rleSmp, ~ mean(.x)))

# plot mean RLE by age/gender
ggplot(rleSmpDat) +
  geom_line(aes(x = age, y = rleSmpMn, group = gender, color = gender)
            , show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 110, 10))




# 4 for Anastasiia ------------------------------------------------------------
# extra outputs to help Anastasiia with her eol project
# group rle
rleGrpDat_Anas <- rleSmpDat %>%
  select(gender, age, rleSmp) %>%
  mutate(rleGrp = map(rleSmp, ~ make_rle_grp(.))) %>%
  select(-rleSmp)

# reformat
rleGrpDat_Anas <- rleGrpDat_Anas %>%
  unnest(cols = c(rleGrp)) %>%
  pivot_wider(
    names_from = rleGrp, names_glue = "rle_{rleGrp}"
    , values_from = n, values_fill = 0L) %>%
  relocate(rle_3plus, .after = last_col())




# 5 sampling - Covid deaths ---------------------------------------------------
# calculate probabilities of death
pDthDat <- lifeDat %>%
  select(gender, age, qx) %>% 
  mutate(start = row_number()) %>% 
  group_by(gender) %>% 
  mutate(end = max(start)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(dat = list(.[start:end, ])) %>%
  ungroup() %>% 
  mutate(dat = map(dat, ~ .x %>% make_pDth()))

# join Covid deaths to pDthDat
pDthDat <- pDthDat %>% 
  left_join(
    ytdDthsCovidSyoa, by = c("gender", "age")
    ) %>% 
  mutate(dths = case_when(is.na(dths) ~ 0, TRUE ~ dths)) %>% 
  mutate(dat = map2(dat, dths, ~ .x %>% mutate(dths = round(.y))))

set.seed(54321)  # for reproducibility
rleSmpDat <- pDthDat %>% 
  mutate(
    rleSmp = map(
      dat, ~ sample(x = .x$rle, size = .x$dths, replace = TRUE, prob = .x$pDth))
    , rleSmpMn = map_dbl(
      rleSmp, ~ mean(.x)))

# plot mean RLE by age/gender
ggplot(rleSmpDat) +
  geom_line(aes(x = age, y = rleSmpMn, group = gender, color = gender)
            , show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 110, 10))

# outputs
# YLL each Covid fatality
yllDat <- rleSmpDat %>% 
  select(gender, age, rleSmp) %>% 
  unnest(rleSmp) %>% 
  rename(yll = rleSmp)

ggplot(yllDat) +
  geom_histogram(aes(x = yll), breaks = c(0:100))

# total
yllDat %>% group_by(gender) %>% summarise(yll = sum(yll))
# mean
yllDat %>% group_by(gender) %>% summarise(mnYll = mean(yll))
# sd/quantiles
yllDat %>% group_by(gender) %>% summarise(sdYll = sd(yll))
yllDat %>% group_by(gender) %>% summarise(across(yll, quantile_funs))




# 6 crude approach --------------------------------------------------------
lifeDat <- life %>%
  filter(year == 2018) %>%
  select(age, gender, ex)

# total YLL
yllTotal <- ytdDthsCovidSyoa %>% 
  left_join(lifeDat, by = c("gender", "age")) %>% 
  mutate(yll = dths * ex) %>% 
  group_by(wave, gender) %>% 
  summarise(yll = sum(yll)) %>% 
  ungroup()

# mean YLL
yllWeightMeanWave <- ytdDthsCovidSyoa %>% 
  left_join(lifeDat, by = c("gender", "age")) %>% 
  mutate(yll = dths * ex) %>% 
  group_by(wave) %>% 
  summarise(mnYll = sum(yll) / sum(dths))

yllWeightMean <- ytdDthsCovidSyoa %>% 
  left_join(lifeDat, by = c("gender", "age")) %>% 
  mutate(yll = dths * ex) %>% 
  summarise(mnYll = sum(yll) / sum(dths))

# sd/quantiles
ytdDthsCovidSyoa %>% 
  left_join(lifeDat, by = c("gender", "age")) %>% 
  mutate(yll = map2(.x = ex, .y = dths, ~ rep(.x, round(.y)))) %>% 
  select(wave, gender, age, yll) %>%
  unnest(yll) %>% 
  group_by(wave, gender) %>%
  # summarise(sdYll = sd(yll))
  summarise(across(yll, quantile_funs))




# 7 save ----------------------------------------------------------------------
saveRDS(list(yllTotal, yllWeightMeanWave, yllWeightMean), str_c(.datDir, "asis08_years_of_life_lost.RDS"))

