
# README
# test weekly aggregation of government daily deaths is correct


# test ------------------------------------------------------------------------
test_that("total from aggregating government daily deaths to weekly matches daily total", {
  expect_equal(
    govWkDths %>%
      summarise(dths = sum(dths)) %>% 
      pull(dths)
    , govDths %>% 
      filter(date <= max(govWkDths$wkEnding)) %>% 
      summarise(dths = sum(dths)) %>% 
      pull(dths))
})

