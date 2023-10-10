

vitamind <- read.csv("data-raw/vitamind.csv") |>
  dplyr::rename(vit_d = Y) |>
  dplyr::select(vit_d, time, X)

usethis::use_data(vitamind, overwrite = TRUE)

# generate a dataset for a mixed model with 'subject' as a random effect
withr::with_seed(
  50,
  {

    library(dplyr)

    # generate a dataset with a random effect variable comprising 30 subjects
    f_sample_id <- function(id_num,
                            n = 30,
                            mesor,
                            amp,
                            acro,
                            family = "gamma",
                            sd = 0.2,
                            period,
                            n_components,
                            beta.group = TRUE
    ) {
      data <- simulate_cosinor(
        n = n,
        mesor = mesor,
        amp = amp,
        acro = acro,
        family = family,
        sd = sd,
        period = period,
        n_components = n_components
      )
      data$subject <- id_num
      data
    }

    dat_mixed <- do.call(
      "rbind",
      lapply(1:5, function(x) {
        f_sample_id(
          id_num = x,
          mesor = rnorm(1, mean = 2, sd = 1),
          amp = rnorm(1, mean = 5, sd = 0.1),
          acro = rnorm(1, mean = 1.5, sd = 0.2),
          period = 24,
          n_components = 1
        )
      })
    ) |>
      mutate(subject = as.factor(subject))


    #An example of a model for this data
    # mixed_mod <- cglmm(
    #   Y ~ amp_acro(times,
    #                n_components = 1,
    #                period = 24
    #   ) + (1 + amp_acro1 | subject),
    #   data = dat_mixed
    # )

})


cosinor_mixed <- select(dat_mixed, -group)
usethis::use_data(cosinor_mixed, overwrite = TRUE)
