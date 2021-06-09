
# README
# calculate quantiles for groups, amended from here,
# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/


# quantiles -------------------------------------------------------------------
q <- c(.2, .5, .8)

q_names <- map_chr(q, ~ str_c(.x * 100, "%"))

quantile_funs <- map(q, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = q_names)

