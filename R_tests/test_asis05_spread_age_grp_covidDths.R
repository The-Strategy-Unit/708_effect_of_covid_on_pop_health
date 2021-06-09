
# README
# test for errors in spreading age group Covid deaths to syoa


# 1 test ----------------------------------------------------------------------
test_that("syoa Covid deaths equals age group Covid deaths", {
  expect_equal(
    ytdDthsCovidSyoa %>%
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , ytdDthsCovid %>%
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , tolerance = 0)
})

