
# README
# lookup from big age groups (ONS weekly deaths pre-2020) to 5-year age groups (2020+)


# age_group_lookup ------------------------------------------------------------
age_group_lookup <- tibble(
  ageGrp = c(
    "00-00", "01-04", 
    str_c(
      sprintf("%02d", seq(5, 100, by = 5))
      , "-"
      , sprintf("%02d",seq(9, 105, by = 5)))
    , "90+", "95+", "105+")
  ) %>%
  mutate(onsAgeGrp = case_when(
    ageGrp == "00-00" ~ "under1"
    , ageGrp %in% c("01-04", "05-09", "10-14") ~ "01-14"
    , ageGrp %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44"
    , ageGrp %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64"
    , ageGrp %in% c("65-69", "70-74") ~ "65-74"
    , ageGrp %in% c("75-79", "80-84") ~ "75-84"
    , ageGrp %in% c("85-89", "90-94", "95-99", "100-104", "90+", "95+", "105+") ~ "85+"
    , TRUE ~ ageGrp))

