
# README
# functions to clean ONS weekly death registrations data


# clean_weekly_deaths_dat (pre Covid) -----------------------------------------
clean_weekly_deaths_dat <- function(dat) {
  
  if (dat$year[1] == "2014") {
    
    dat <- dat %>%
    # because of 2 respiratory deaths rows
    drop_na(`2`) %>% 
    filter(str_detect(`Week number`, "^Source", negate = TRUE))
    
    wkEndingDat <- dat %>% 
      slice(1) %>%
      select_at(vars(starts_with(as.character(1:9))), as.double)
    
    wkEndingDat <- unlist(unname(wkEndingDat))
    
    respDths <- dat %>% 
      filter(str_detect(`Week number`, "ICD-10")) %>%
      mutate_all(list(~ replace(., . == ":", NA)))
      
    respDths <- coalesce(unlist(respDths[1,]), unlist(respDths[2,], use.names = FALSE))
      
    respDths <- as_tibble_row(respDths)
      
    dat <- dat %>%
      mutate_at(vars(starts_with(as.character(1:9))), as.double) %>%
      filter(str_detect(`Week number`, "ICD-10", negate = TRUE)) %>% 
      bind_rows(respDths %>% mutate_at(vars(starts_with(as.character(1:9))), as.double))
      
    newColNames <- c(newColNames[str_detect(newColNames, "respDeaths", negate = TRUE)], "respDeaths")
      
    dat <- dat %>%
      mutate(newColNames = newColNames) %>%
      select_at(vars(!starts_with(c("Week", ".")))) %>%
      slice(-1) %>%
      pivot_longer(starts_with(as.character(1:9)), names_to = "wk", values_to = "dths") %>% 
      mutate(dths = as.integer(dths)) %>% 
      mutate(wkEnding = rep(wkEndingDat, nrow(.) / length(wkEndingDat))) %>% 
      mutate(wkEnding = excel_numeric_to_date(as.numeric(wkEnding), date_system = "modern"))
      
  } else {
    
    wkEndingDat <- dat %>% 
      slice(1) %>%
      select_at(vars(starts_with(as.character(1:9))), as.double)
      
    wkEndingDat <- unlist(unname(wkEndingDat))
      
    dat <- dat %>%
      drop_na(`1`:`52`) %>% 
      filter(case_when(
        year %>% head(1) == "2010" ~ str_detect(`Week number`, "notExists", negate = TRUE) %>% replace_na(TRUE)
        , TRUE ~ str_detect(`Week number`, "ICD-10 v 2001", negate = TRUE) %>% replace_na(TRUE))) %>%
      mutate_at(vars(starts_with(as.character(1:9))), as.double) %>% 
      mutate(newColNames = newColNames) %>%
      slice(-1) %>% 
      select_at(vars(!starts_with(c("Week", ".")))) %>%
      pivot_longer(starts_with(as.character(1:9)), names_to = "wk", values_to = "dths") %>% 
      mutate(dths = as.integer(dths)) %>% 
      mutate(wkEnding = rep(wkEndingDat, nrow(.) / length(wkEndingDat))) %>% 
      mutate(wkEnding = excel_numeric_to_date(as.numeric(wkEnding), date_system = "modern"))
    
    }
  
  return(dat)
  
}




# clean_weekly_deaths_dat_post_Covid ------------------------------------------
clean_weekly_deaths_dat_post_covid <- function(dat, recentWk = NULL, repColNames = newColNames_2020) {
  
  recentWk <- min(recentWk, 53)
  maxCol <- 53+1

  wkEndingDat <- dat %>% 
    slice(1) %>%
    # superscript footnote in week 53 2021
    rename_at(vars(matches("^53 7$")), function(x) "53") %>% 
    select_at(vars(!any_of(as.character((recentWk +1):maxCol)))) %>% 
    select_at(vars(!starts_with(c("Week", ".", "year"))))
    
  wkEndingDat <- unlist(unname(wkEndingDat))
  wkEndingDat <- sort(excel_numeric_to_date(as.numeric(wkEndingDat), date_system = "modern"))
    
  dat <- dat %>%
    # superscript footnote in week 53 2021
    rename_at(vars(matches("^53 7$")), function(x) "53") %>% 
    select_at(vars(!any_of(as.character((recentWk +1):maxCol)))) %>%
    select_at(vars(!starts_with(c("Week", ".", "year")))) %>% 
    drop_na(any_of(as.character(1:recentWk))) %>% 
    mutate_at(vars(starts_with(as.character(1:9))), as.double) %>% 
    mutate(repColNames = repColNames) %>%
    slice(-1) %>% 
    select_at(vars(!starts_with(c("Week", ".")))) %>%
    pivot_longer(starts_with(as.character(1:9)), names_to = "wk", values_to = "dths") %>% 
    mutate(across(c(wk, dths), as.integer)) %>%
    arrange(repColNames, wk) %>% 
    mutate(wkEnding = rep(wkEndingDat, nrow(.) / length(wkEndingDat))) %>% 
    mutate(year = as.integer(year(wkEnding))) %>% 
    rename(measureorGrp = repColNames)
  
  return(dat)
  
}




# clean_weekly_covidDths_dat -----------------------------------------------
clean_weekly_covidDths_dat <- function(dat, recentWk = NULL, repColNames = newColNames) {
  
  recentWk <- min(recentWk, 53)
  maxCol <- 53+1
    
  wkEndingDat <- dat %>% 
    slice(1) %>%
    select_at(vars(!any_of(as.character((recentWk +1):maxCol)))) %>% 
    select_at(vars(!starts_with(c("Week", ".", "year"))))
    
  wkEndingDat <- unlist(unname(wkEndingDat))
  wkEndingDat <- sort(excel_numeric_to_date(as.numeric(wkEndingDat), date_system = "modern"))
    
  dat <- dat %>%
    select_at(vars(!any_of(as.character((recentWk +1):maxCol)))) %>%
    drop_na(any_of(as.character(1:recentWk))) %>% 
    mutate_at(vars(starts_with(as.character(1:9))), as.double) %>% 
    mutate(repColNames = repColNames) %>%
    slice(-1) %>% 
    select_at(vars(!starts_with(c("Week", ".", "year")))) %>% 
    pivot_longer(starts_with(as.character(1:9)), names_to = "wk", values_to = "dths") %>% 
    mutate(across(c(wk, dths), as.integer)) %>%
    arrange(repColNames, wk) %>% 
    mutate(wkEnding = rep(wkEndingDat, nrow(.) / length(wkEndingDat))) %>% 
    mutate(year = as.integer(year(wkEnding))) %>% 
    rename(measureorGrp = repColNames)
  
  return(dat)
  
}


