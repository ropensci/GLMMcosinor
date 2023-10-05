

vitamind <- read.csv("data-raw/vitamind.csv")

# vitamind <- dplyr::rename([newname] = Y) # rename to something better than Y?

usethis::use_data(vitamind, overwrite = TRUE)
