# Scrape ONS weekly death registrations
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# provisional weekly death registrations England & Wales from here
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
# note: definition for respiratory deaths changed in 2014
# note: from 2020 5-year age bands used (previously 15-44 etc)


# in this script --------------------------------------------------------------
# 1 scrape pre-2020 datasets
# 2 clean pre-2020 datasets
# 3 scrape 2020+ datasets
# 4 clean 2020+ datasets
# 5 clean Covid deaths
# 6 test weekly deaths
# 7 save




# 1 scrape pre-2020 datasets --------------------------------------------------
# weekly death registrations 
wkDthReg_html <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales")
linkId <- "weeklyprovisionalfiguresondeathsregisteredinenglandandwales/([0-9]{4})/publishedweek([0-9]{4}|[0-9]{6}).*xls"

# discover link destinations for annual datasets
wkDthReg_links <- html_nodes(wkDthReg_html, css = "a") %>% html_attr("href")
wkDthReg_links <- wkDthReg_links[str_detect(wkDthReg_links, linkId)]
wkDthReg_links <- str_c("https://www.ons.gov.uk", wkDthReg_links)

# 2020+ include Covid deaths
# rm 2020-plus links
wkDthReg_2020plus <- str_subset(wkDthReg_links, "/2020/|/2021/") 
wkDthReg_links    <- str_subset(wkDthReg_links, "/2020/|/2021/", negate = TRUE)

# tidy file names
wkDthReg_fileNms <- str_extract(wkDthReg_links, "([^/]+$)")

# download files
for (i in seq_along(wkDthReg_links)) {
  
  download.file(
    wkDthReg_links[i], destfile = paste0(.rawDir, wkDthReg_fileNms[i]), method = "libcurl", mode = "wb")
}




# 2 clean pre-2020 datasets ---------------------------------------------------
wkDthReg_xls <- list.files(.rawDir, "publishedweek")

# arrange by year
ext          <- str_extract(wkDthReg_xls, "20[0-9]{2}")
wkDthReg_xls <- wkDthReg_xls[order(rank(as.double(ext)))]

# rm 2020+
wkDthReg_xls <- str_subset(wkDthReg_xls, "week[0-9]{2}2020|week[0-9]{2}2021", negate = TRUE)

# read files
N          <- length(wkDthReg_xls)
wkDths_dat <- vector("list", N)

for (i in seq_along(wkDthReg_xls)) {
  
  sheets <- paste0(.rawDir, wkDthReg_xls[i]) %>%
    excel_sheets() %>% 
    set_names()
    
  sheets <- sheets[grepl("Weekly Figures", sheets, ignore.case = TRUE)] 
    
  wkDths_dat[[i]] <- map_df(
    sheets
    , ~ read_excel(paste0(.rawDir, wkDthReg_xls[i]), sheet = .x, na = ":", skip = ifelse(i == 5, 2, 3)))
}

names(wkDths_dat) <- str_extract(wkDthReg_xls, "20[0-9]{2}")

# clean
newColNames <- c(
  "wkEnding"
  , "totalDeaths"
  , "totalDeathsAvgPrev5"
  , "respDeaths"
  , paste0("p_", c("under1", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
  , paste0("m_", c("under1", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
  , paste0("f_", c("under1", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
  , "NE", "NW", "Y&H", "EM", "WM", "E", "L", "SE", "SW", "W")

# the usual work-around is to pass it the names or indices of the vector instead of the vector itself
wkDths_dat <- lapply(names(wkDths_dat), function(x) wkDths_dat[[x]] %>% mutate(year = x))
wkDths_dat <- lapply(wkDths_dat, function(x) clean_weekly_deaths_dat(x))

# collapse to single df
wkDthsDat <- bind_rows(wkDths_dat)
wkDthsDat <- wkDthsDat %>% rename(measureorGrp = newColNames)

# manual fix - wkEnding format is different in .xls file for weeks 1-16 in 2011
wkDthsDat <- wkDthsDat %>%
  filter(year != "2011") %>% 
  bind_rows(
    wkDthsDat %>%
    filter(year == "2011") %>% 
    arrange(measureorGrp, as.integer(wk)) %>%
    group_by(measureorGrp) %>% 
    mutate(wkEnding = seq(ymd("2011-01-07"), ymd('2011-12-30'), by = "weeks"))
  ) %>%
  mutate(across(c(year, wk), as.integer)) %>% 
  arrange(year, measureorGrp, wk)




# 3 scrape 2020+ datasets -----------------------------------------------------
# discover link destinations for annual datasets
wkDthReg_links <- html_nodes(wkDthReg_html, css = "a") %>% html_attr("href")
wkDthReg_links <- wkDthReg_links[str_detect(wkDthReg_links, linkId)]
wkDthReg_links <- str_c("https://www.ons.gov.uk", wkDthReg_links)

# tidy file names
wkDthReg_2020plus_fileNms <- str_extract(wkDthReg_2020plus, "([^/]+$)")

# 2020-plus include Covid deaths
wkDthReg_links <- str_subset(wkDthReg_links, wkDthReg_2020plus_fileNms)

# download files
for (i in seq_along(wkDthReg_links)) {
  
  download.file(
    wkDthReg_links[i], destfile = paste0(.rawDir, wkDthReg_2020plus_fileNms[i]), method = "libcurl", mode = "wb")
}




# 4 clean 2020+ datasets ------------------------------------------------------
wkDthReg_xls <- list.files(.rawDir, "publishedweek")

# arrange by year
ext          <- str_extract(wkDthReg_xls, "20[0-9]{2}")
wkDthReg_xls <- wkDthReg_xls[order(rank(as.double(ext)))]

# 2020+ include Covid deaths
wkDthReg_xls <- str_subset(wkDthReg_xls, "week[0-9]{2}2020|week[0-9]{2}2021")

# read files
N          <- length(wkDthReg_xls)
wkDths_dat <- vector("list", N)

for (i in seq_along(wkDthReg_xls)) {

  sheets <- paste0(.rawDir, wkDthReg_xls[i]) %>%
    excel_sheets() %>% 
    set_names()
  
  sheets <- sheets[grepl("Weekly Figures 20[0-9]{2}", sheets, ignore.case = TRUE)] 
  
  wkDths_dat[[i]] <- map_df(
    sheets
    , ~ read_xlsx(paste0(.rawDir, wkDthReg_xls[i]), sheet = .x, col_types = c("text", "text", rep("numeric", 53)), skip = 4))
}

names(wkDths_dat) <- str_extract(wkDthReg_xls, "20[0-9]{2}")
  
# 2020+ include Covid deaths
newColNames_2020 <- c(
    "wkEnding"
    , "totalDeaths"
    , "totalDeathsAvgPrev5EW"
    , "totalDeathsAvgPrev5E"
    , "totalDeathsAvgPrev5W"
    , "respDeaths"
    , "covid-19"    
    , paste0("p_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
    , paste0("m_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
    , paste0("f_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
    , "NE", "NW", "Y&H", "EM", "WM", "E", "L", "SE", "SW", "W")    

# 2021 - respiratory deaths & Covid deaths in separate sheet; and new line for 2020 deaths
newColNames_2021 <- str_subset(newColNames_2020, "respDeaths|covid-19", negate = TRUE)
newColNames_2021 <- append(newColNames_2021, "totalDeaths2020", after = 2)

# imap an indexed map, is short hand for map2(x, names(x), ...) if x has names
wkDths_dat <- imap(wkDths_dat, ~ .x %>% mutate(year = .y))
wkDths_dat_2020 <- clean_weekly_deaths_dat_post_covid(wkDths_dat[["2020"]], recentWk = NULL, repColNames = newColNames_2020)
wkDths_dat_2021 <- clean_weekly_deaths_dat_post_covid(wkDths_dat[["2021"]], recentWk = recentWk, repColNames = newColNames_2021)

# collapse to single df
wkDthsDat_post_2020 <- bind_rows(wkDths_dat_2020, wkDths_dat_2021)




# 5 clean Covid deaths --------------------------------------------------------
wkDthReg_xls <- list.files(.rawDir, "publishedweek")

# arrange by year
ext          <- str_extract(wkDthReg_xls, "20[0-9]{2}")
wkDthReg_xls <- wkDthReg_xls[order(rank(as.double(ext)))]

# 2020+ include Covid deaths
wkDthReg_xls <- str_subset(wkDthReg_xls, "week[0-9]{2}2020|week[0-9]{2}2021")

# read files
N          <- length(wkDthReg_xls)
wkDths_dat <- vector("list", N)

for (i in seq_along(wkDthReg_xls)) {
  
  sheets <- paste0(.rawDir, wkDthReg_xls[i]) %>%
    excel_sheets() %>% 
    set_names()
  
  sheets <- sheets[grepl("Covid-19 - Weekly registrations", sheets, ignore.case = TRUE)] 
  
  wkDths_dat[[i]] <- map_df(
    sheets
    , ~ read_xlsx(paste0(.rawDir, wkDthReg_xls[i])
                  , sheet = .x
                  , col_types = ifelse(i == 1, c("text", "text", rep("numeric", 53)), c("text", "text", rep("numeric", 52)))
                  , skip = 4))
}

names(wkDths_dat) <- str_extract(wkDthReg_xls, "20[0-9]{2}")

# 2020+ include Covid deaths
newColNames <- c(
  "wkEnding"
  , "totalCovidDths"    
  , paste0("p_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
  , paste0("m_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
  , paste0("f_", c("00-00", "01-04", "05-09", paste0(seq(10, 85, 5), "-", seq(14, 90, 5)), "90+"))
  , "NE", "NW", "Y&H", "EM", "WM", "E", "L", "SE", "SW", "W")

# imap an indexed map, is short hand for map2(x, names(x), ...) if x has names
wkDths_dat <- imap(wkDths_dat, ~ .x %>% mutate(year = .y))
wkDths_dat_2020 <- clean_weekly_covidDths_dat(wkDths_dat[["2020"]], recentWk = NULL, repColNames = newColNames)
wkDths_dat_2021 <- clean_weekly_covidDths_dat(wkDths_dat[["2021"]], recentWk = recentWk, repColNames = newColNames)

# collapse to single df
wkDthsDat_covid <- bind_rows(wkDths_dat_2020, wkDths_dat_2021)




# 6 test weekly deaths --------------------------------------------------------
source(str_c(.testDir, "test_dat01_scrape_and_compile_weekly_death_registrations.R"))




# 7 save ----------------------------------------------------------------------
# registrations 2010 to 2019
saveRDS(wkDthsDat, str_c(.datDir, "dat01_weekly_deaths_2010_to_2019.RDS"))
# registrations 2020+
saveRDS(wkDthsDat_post_2020, str_c(.datDir, "dat01_weekly_deaths_2020_to_2021.RDS"))
# Covid registrations 2020+
saveRDS(wkDthsDat_covid, str_c(.datDir, "dat01_weekly_covidDths_2020_to_2021.RDS"))

