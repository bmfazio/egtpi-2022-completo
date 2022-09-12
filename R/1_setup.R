suppressPackageStartupMessages({
  library(renv)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(writexl)
  suppressWarnings(library(fuzzyjoin))
  library(lubridate)
  library(googlesheets4)
})

source("R/subscripts/fun_fuzzy-joins.R", encoding = "utf-8")

empty_tibble <- function(dates) { # uso en 4_ready
  values <- c(NA, NA, replicate(length(dates), NA_real_, simplify = FALSE))
  names(values) <- c("UBIGEO", "REGISTRO_IAL_2022", paste("TOTAL", dates, sep = "_"))
  as_tibble(values)
}

bdcols_relocate <- function(x, dates){ # uso en 5_bd
  x %>%
    relocate(
      any_of(paste0(c(paste0("H", 1:5, "_"), "TOTAL_"), rep(dates[-1], each = 6))),
      .after = paste0("TOTAL_", dates[1])
    )
}