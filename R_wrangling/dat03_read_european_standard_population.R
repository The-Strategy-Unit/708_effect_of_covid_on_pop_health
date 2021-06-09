# Read European standard population
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# source: https://ec.europa.eu/eurostat/cache/metadata/Annexes/hlth_cdeath_esms_an1.pdf


# in this script --------------------------------------------------------------
# 1 read ESP
# 2 split ESP to syoa
# 3 save




# 1 read ESP ------------------------------------------------------------------
agelabels <- c(paste0(
  sprintf("%02d", seq(0, 90, by = 5))
  , "-"
  , sprintf("%02d",seq(4, 95, by = 5))), "95+")

esp <- tibble(
  ageGrp = c("00-00", "01-04", agelabels[-1])
  , stdPop = c(
    1000, 4000, 5500, 5500
    , 5500, 6000, 6000, 6500
    , 7000, 7000, 7000, 7000
    , 6500, 6000, 5500, 5000
    , 4000, 2500, 1500, 800
    , 200))




# 2 split ESP to syoa ---------------------------------------------------------
# follow p.7 CMI working paper 111
# we assume that the population within each five-year age band is split equally between its five ages;
# and that the open age band 95+ is split equally between the six ages from 95 to 100 inclusive.
espSyoa <- esp %>%
  mutate(
    startYr = as.integer(str_sub(ageGrp, 1, 2))
    , endYr = as.integer(str_sub(ageGrp, 4, 5))) %>% 
  mutate(endYr = case_when(
    ageGrp == "00" ~ 0L
    , ageGrp == "95+" ~ 100L
    , TRUE ~ endYr)) %>% 
  mutate(dat = pmap(
    list(x = startYr, y = endYr, z = stdPop),
    ~ tibble(age = seq.int(..1, ..2, by = 1), stdPop = ..3))) %>%
  mutate(dat = map(dat, ~ .x %>% mutate(syoaStdPop = stdPop / nrow(.x)))) %>% 
  select(dat) %>% 
  unnest(c(dat)) %>%
  select(-stdPop) %>% 
  rename(stdPop = syoaStdPop)




# 3 save ----------------------------------------------------------------------
saveRDS(esp, str_c(.datDir, "dat03_european_standard_population_age_grp.RDS"))
saveRDS(espSyoa, str_c(.datDir, "dat03_european_standard_population_syoa.RDS"))

