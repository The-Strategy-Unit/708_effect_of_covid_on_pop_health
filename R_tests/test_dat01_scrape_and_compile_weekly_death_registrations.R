
# README
# test the consistency of ONS weekly death registrations data after scraping and compiling


# 1 exploratory plots ---------------------------------------------------------
# wkDthsDat %>%
#   filter(measureorGrp == "totalDeaths") %>%
#   group_by(year) %>%
#   summarise(dths = sum(dths)) %>%
#   ggplot() +
#   geom_line(aes(x = year, y = dths, group = 1))

# wkDthsDat %>%
#   filter(measureorGrp == "respDeaths") %>%
#   group_by(year) %>%
#   summarise(dths = sum(dths)) %>%
#   ggplot() +
#   geom_line(aes(x = year, y = dths, group = 1))
# 
# wkDthsDat %>%
#   filter(measureorGrp == "totalDeathsAvgPrev5") %>%
#   group_by(year) %>%
#   summarise(dths = sum(dths)) %>%
#   ggplot() +
#   geom_line(aes(x = year, y = dths, group = 1))
# 
# regions <- c("NE", "NW", "Y&H", "EM", "WM", "E", "L", "SE", "SW", "W")
# 
# wkDthsDat %>%
#   filter(measureorGrp %in% regions) %>%
#   group_by(year) %>%
#   summarise(dths = sum(dths)) %>%
#   ggplot() +
#   geom_line(aes(x = year, y = dths, group = 1))




# tests -----------------------------------------------------------------------
test_that("male plus female equals total", {
  expect_equal(
    wkDthsDat %>%
      filter(measureorGrp == "totalDeaths") %>% 
      pull(dths)
    , wkDthsDat %>%
      filter(str_detect(measureorGrp, "^[m_|f_]")) %>% 
      group_by(year, wk) %>% 
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , tolerance = 0.001)
})


test_that("male plus female equals persons", {
  expect_equal(
    wkDthsDat %>%
      filter(str_detect(measureorGrp, "^p_")) %>% 
      group_by(year, wk) %>% 
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , wkDthsDat %>%
      filter(str_detect(measureorGrp, "^[m_|f_]")) %>% 
      group_by(year, wk) %>% 
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , tolerance = 0.001)
})


test_that("average deaths previous 5 years equals manual calculation", {
  expect_equal(
    wkDthsDat_post_2020 %>%
      filter(year == 2021, wk != 53, measureorGrp == "totalDeathsAvgPrev5EW") %>% 
      pull(dths)
    , wkDthsDat %>%
      filter(measureorGrp == "totalDeaths", year %in% c(2015:2019)) %>%
      filter(wk != 53, wk <= recentWk) %>%
      select(year, wk, dths) %>% 
      group_by(wk) %>% 
      summarise(dths = mean(dths)) %>% 
      pull(dths)
    , tolerance = 0.001)
})

