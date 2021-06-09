
# README
# fx to estimate linear relationship between qx and age


# model_lm_qx -----------------------------------------------------------------
model_lm_qx <- function(df) {
  
  lm(qx ~ age, data = df)
  
}

