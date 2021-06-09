
# README
# test very old population matches 90+ population


# test ------------------------------------------------------------------------
test_that("very old population matches 90+ population", {
  expect_equal(
    myeSyoa %>%
      filter(year != 2001, age == 90) %>%
      group_by(year, gender) %>%
      summarise(pop = sum(pop)) %>% 
      pull(pop)
    , myeVeryOld %>%
      group_by(year, gender) %>%
      summarise(pop = sum(pop)) %>% 
      pull(pop)
    , tolerance = 0.001)
})

