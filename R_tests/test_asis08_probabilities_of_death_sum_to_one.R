

# test pDth sums to 1
test_that("probabilities of death sum to 1 for each starting age (excl. age 100+)", {
    expect_true(
        isTRUE(
            all.equal(
                pDthDat %>%
                    filter(age < 101) %>% 
                    mutate(testpDth = map_dbl(
                        dat, . %>% summarise(pDth = sum(pDth)) %>% pull)) %>% 
                    pull(testpDth)
                , rep(1L, nrow(pDthDat %>% filter(age < 101))), tolerance = .001)))
})

