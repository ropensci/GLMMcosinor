#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}


test_that("autoplot input checks work", {
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ amp_acro(time,
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
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )

  # Test 1
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
    regex = paste("'x_str' must be string corresponding to a group",
                  "name in cosinor.glmm object"), fixed = TRUE
  )

  # Test 2
  data(vitamind)
  vitamind$Z <- rbinom(length(vitamind$X), 3, prob = 0.5)
  object <- cosinor.glmm(
    Y ~ X + amp_acro(time,
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
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
    regex = paste("'type' must be a string. See type in ?predict",
                  "for more information about valid inputs"), fixed = TRUE
  )

  # Test 5
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
    regex = paste("'xlims' must be a vector with the first element",
                  "being the lower x coordinate, and the second being",
                  "the upper x coordinate"),
    fixed = TRUE
  )

  # Test 6
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
    regex = paste("'superimpose.data' must be a logical argument,",
                  "either TRUE or FALSE"),
    fixed = TRUE
  )

  # Test 8
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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

  # Test 9
  data(vitamind)
  object <- cosinor.glmm(
    Y ~ 1 + amp_acro(time,
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
                          beta.group = TRUE) {
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
    lapply(1:30, function(x) {
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
  mixed_mod <- cosinor.glmm(
    Y ~ amp_acro(times,
                 n_components = 1,
                 period = 24
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  expect_no_error(autoplot(mixed_mod))
})
