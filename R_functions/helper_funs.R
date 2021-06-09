
# README
# helper functions


# helper functions ------------------------------------------------------------

# capitalize first letter
first_up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
  }

# lowercase first letter
first_down <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  return(x)
}

# camel case
camel_case <- function(x) {
  y <- str_replace_all(x, "_|&", " ")
  y <- str_to_title(y)
  y <- str_remove_all(y, "\\s|/|-")
  return(y)
}

# repair names for ONS life tables
lifeTables_name_repair <- function(nms) paste0(c("", rep("m_", 5), "blank", rep("f_", 5)), nms)

# calculate a cagr
cagr_growth <- function(PV, FV, fractional_years, type = "geometric") {
  
  if (type == "geometric")
    return (((FV / PV) ** (1 / fractional_years)) - 1)
  
  if (type == "continuous")
    return (log(FV / PV) / fractional_years)
  
  stop("cagr takes type = ['geometric' | 'continuous']")
}

# read excel sheet and save to csv
read_then_csv <- function(sheet, path, skip_n = 1, n_max = Inf) {
  
  pathbase <- tools::file_path_sans_ext(basename(path))
  
  df <- read_excel(path = path, sheet = sheet, skip = skip_n, n_max = n_max)
  
  write.csv(df, paste0(.datDir, pathbase, "_", sheet, ".csv"), quote = FALSE, row.names = FALSE)
  
  return(df)
}

