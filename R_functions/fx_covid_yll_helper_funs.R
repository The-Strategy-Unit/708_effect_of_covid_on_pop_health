
# README
# functions to help estimate YLL for Covid fatalities


# make_pDth -------------------------------------------------------------------
# fx to calculate p(dth) at current age and all possible future ages for each syoa
# e.g. for someone age 65: p(dth)_age65, p(dth)_age66 etc. 
make_pDth <- function(df) {
  
  df <- df %>%
    ungroup() %>%  # rm rowwise()
    mutate(
      pSv = cumprod(1-qx)
      , pDth = qx * lag(pSv, n = 1L, default = 1L, order_by = age)
      , rleF = str_c(0:(nrow(.)-1), "-", row_number())
      , rleF = factor(rleF)
      , rle = seq(from = .5, by = 1, length.out = nrow(.))
      )
  
  return(df)
  
}




# make_rle_grp ----------------------------------------------------------------
# fx to group rle into 4 groups (0-1, 1-2, 2-3, 3+ years)
make_rle_grp <- function(x) {
  
  df <- tibble(rleGrp =as.character(
    cut(x
        , breaks = c(0, 1, 2, 3, 111)
        , labels = c("0to1", "1to2", "2to3", "3plus")))
    ) %>%
    group_by(rleGrp) %>%
    tally()
  
  return(df)
  
}

