#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}


test_that("autoplot input checks work", {
  data(vitamind)
  object <- cglmm(
    vit_d ~ amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )

  # testing various inputs
  expect_no_error(autoplot(object, superimpose.data = TRUE))
  expect_no_error(autoplot(object, predict.ribbon = TRUE))
})


test_that("autoplot produces error messages", {
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )

  # Test 1
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, x_str = 10)
  }

  expect_error(
    f(),
    regex = paste(
      "'x_str' must be string corresponding to a group",
      "name in cglmm object"
    ), fixed = TRUE
  )

  # Test 2
  data(vitamind)
  vitamind$Z <- rbinom(length(vitamind$X), 3, prob = 0.5)
  object <- cglmm(
    vit_d ~ X + amp_acro(time,
      n_components = 3,
      group = c("Z", NA, "X"),
      period = c(12, 10, 8)
    ),
    data = vitamind
  )

  f <- function() {
    autoplot(object, x_str = c("X", "Z"), pred.length.out = 200)
  }

  expect_no_error(f())

  f <- function() {
    autoplot(object)
  }

  # Test 3
  expect_no_error(f())

  # Test 4
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, type = 20)
  }

  expect_error(
    f(),
    regex = paste(
      "'type' must be a string. See type in ?predict",
      "for more information about valid inputs"
    ), fixed = TRUE
  )

  # Test 5
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, xlims = c(2, 1))
  }

  expect_error(
    f(),
    regex = paste(
      "'xlims' must be a vector with the first element",
      "being the lower x coordinate, and the second being",
      "the upper x coordinate"
    ),
    fixed = TRUE
  )

  # Test 6
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, pred.length.out = 100.5)
  }

  expect_error(
    f(),
    regex = "'pred.length.out' must be an integer greater than 0", fixed = TRUE
  )

  # Test 7
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, superimpose.data = 10)
  }

  expect_error(
    f(),
    regex = paste(
      "'superimpose.data' must be a logical argument,",
      "either TRUE or FALSE"
    ),
    fixed = TRUE
  )

  # Test 8
  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, data_opacity = 1.5)
  }

  expect_error(
    f(),
    regex = "data_opacity' must be a number between 0 and 1 inclusive",
    fixed = TRUE
  )


  data(vitamind)
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  f <- function() {
    autoplot(object, predict.ribbon = 10)
  }

  expect_error(
    f(),
    regex = "'predict.ribbon' must be a logical argument, either TRUE or FALSE",
    fixed = TRUE
  )

  library(dplyr)
  f_sample_id <- function(id_num,
                          n = 30,
                          mesor,
                          amp,
                          acro,
                          family = "gaussian",
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
        mesor = rnorm(1, mean = 0, sd = 1),
        amp = rnorm(1, mean = 3, sd = 0.5),
        acro = rnorm(1, mean = 1.5, sd = 0.2),
        period = 24,
        n_components = 1
      )
    })
  ) |>
    mutate(subject = as.factor(subject))
  mixed_mod <- cglmm(
    Y ~ amp_acro(times,
      n_components = 1,
      period = 24
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed
  )
  expect_no_error(autoplot(mixed_mod))
  expect_error(autoplot(mixed_mod, ranef_plot = "group"))
  expect_no_error(autoplot(mixed_mod, ranef_plot = 'subject', superimpose.data = TRUE))

  #
  f_sample_id_2 <- function(id_num,
                          n = 30,
                          mesor,
                          amp,
                          acro,
                          family = "gaussian",
                          sd,
                          beta.sd,
                          period,
                          n_components,
                          beta.mesor,
                          beta.amp,
                          beta.acro,
                          beta.group = TRUE
  ) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sd = sd,
      beta.sd = beta.sd,
      period = period,
      n_components = n_components,
      beta.mesor = beta.mesor,
      beta.amp = beta.amp,
      beta.acro = beta.acro,
      beta.group = beta.group

    )
    if(id_num %% 2 == 0) {
    # data$group = 1
    rows_to_keep <- seq_len(nrow(data) %/% 2)
    data <- data[-rows_to_keep, ]
    } else {
    # data$group =0
    rows_to_delete <- seq_len(nrow(data) %/% 2)
    data <- data[rows_to_delete, ]
    }
    data$subject <- id_num
    data
  }

  dat_mixed_2 <- do.call(
    "rbind",
    lapply(1:5, function(x) {
      f_sample_id_2(
        id_num = x,
        mesor = rnorm(1, mean = 0, sd = 1),
        amp = rnorm(1, mean = 3, sd = 0.5),
        acro = rnorm(1, mean = 1.5, sd = 0.2),
        beta.mesor = rnorm(1, mean = 10, sd = 1),
        beta.amp = rnorm(1, mean = 5, sd = 0.5),
        beta.acro = rnorm(1, mean = 1.5, sd = 0.2),
        period = 24,
        n_components = 1,
        sd = 0.2,
        beta.sd = 0.2
      )
    })
  ) |>
    mutate(subject = as.factor(subject))

  mixed_mod_2 <- cglmm(
    Y ~ group + amp_acro(times,
                 n_components = 1,
                 period = 24,
                 group = "group"
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed_2
  )
  expect_no_error(autoplot(mixed_mod_2, superimpose.data = TRUE, ranef_plot = 'subject'))
})
