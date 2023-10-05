

vitamind <- read.csv("data-raw/vitamind.csv") |>
  dplyr::rename(vit_d = Y) |>
  dplyr::select(vit_d, time, X)

usethis::use_data(vitamind, overwrite = TRUE)
