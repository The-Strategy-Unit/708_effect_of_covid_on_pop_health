
# README
# date helper functions


# count_weekdays --------------------------------------------------------------
count_weekdays <- Vectorize(function(from, to) sum(!wday(seq(from, to, "days")) %in% c(1, 7)))


# iso_year_week --------------------------------------------------------------
iso_year_week <- function(x) {
  
  str_c(
    isoyear(x)
    , "-W"
    , formatC(
      isoweek(x)
      , width = 2, format = "d", flag = "0"))
}

