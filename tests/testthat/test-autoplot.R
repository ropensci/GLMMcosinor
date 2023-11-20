#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("autoplot works with non-grouped model", {
  object <- cglmm(
    vit_d ~ amp_acro(time, period = 12),
    data = vitamind[vitamind$X == 1,]
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger("non-grouped plot with data",
                              autoplot(object, superimpose.data = TRUE))
  vdiffr::expect_doppelganger("non-grouped plot with predict ribbon",
                              autoplot(object, predict.ribbon = TRUE))
})

###
test_that("autoplot works model that has other covariates", {
  withr::with_seed(
    50,{

  data(vitamind)
  test_data <- vitamind[vitamind$X == 1,]

  test_data$new_cat_var <- sample(c("a", "b", "c"), size = nrow(test_data), replace = TRUE)
  test_data$new_num_var <- rnorm(n = nrow(test_data))

  object <- cglmm(
    vit_d ~ amp_acro(time, period = 12) + new_cat_var + new_num_var,
    data = test_data
  )


  vdiffr::expect_doppelganger("model-with-covariates",
                              autoplot(object,
                                       cov_list = list(new_cat_var = 'a',
                                                       new_num_var = 1),
                                       superimpose.data = TRUE))

  vdiffr::expect_doppelganger("model-with-covariates_not_specified",
                              autoplot(object))
  expect_message(autoplot(object, quietly = FALSE),
                 regex = paste0("'cov_list' was not specified, but there are",
                 " covariates in the original model; the first element of each",
                 " covariate column from the original dataframe will be used",
                 " as reference levels:
cov_list = list(new_cat_var = 'c', new_num_var = -0.540933398827827)"),
                 fixed = TRUE)

  vdiffr::expect_doppelganger("model-with-covariates_some_specified",
                              autoplot(object,
                                       cov_list = list(new_cat_var = 'c')))

  expect_message(autoplot(object, cov_list = list(new_cat_var = 'c'), quietly = FALSE))


})
})
###
test_that("autoplot works with simple inputs", {
  data(vitamind)
  object <- cglmm(
    vit_d ~ amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger("plot with superimposed data",
                              autoplot(object, superimpose.data = TRUE))
  vdiffr::expect_doppelganger("plot with predict ribbon enabled",
                              autoplot(object, predict.ribbon = TRUE))
})


test_that("autoplot produces error messages", {
  data(vitamind)

  #Testing error messages

  # test 'x_str' argument
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

  # test plots with different groups on different components
  withr::with_seed(
    50,{

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
      vdiffr::expect_doppelganger("check plots with multiple groups",f())

      f <- function() {
        autoplot(object)
      }

      vdiffr::expect_doppelganger("check a simple plot command", f())

    }
  )


  # test 'type' argument
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

  # test 'xlims' argument
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

  # test 'pred.length.out' argument
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

  # test 'superimpose.data' argument
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

  # test 'data_opacity' argument
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

  #test 'predict.ribbon' argument
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

  #check that plots are generated correctly
  withr::with_seed(
    50, {
      vdiffr::expect_doppelganger("check predict.ribbon arg",autoplot(object,
                                           predict.ribbon = TRUE))
      vdiffr::expect_doppelganger("check x_str arg",autoplot(object,
                                           x_str = NULL))
      vdiffr::expect_doppelganger("check superimpose.data arg",autoplot(object,
                                           x_str = "X",
                                           superimpose.data = TRUE))
      vdiffr::expect_doppelganger("chceck that multiple args work",
                                  autoplot(object,
                                           x_str = NULL,
                                           predict.ribbon = TRUE,
                                           superimpose.data = TRUE))

    }
  )


  #test mixed model plots
  withr::with_seed(
    50,
    {

  # generate a dataset with a random effect variable comprising 30 subjects
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
    dplyr::mutate(subject = as.factor(subject))
  mixed_mod <- cglmm(
    Y ~ amp_acro(times,
      n_components = 1,
      period = 24
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  #test that the plots return expected figures for different configurations
  vdiffr::expect_doppelganger("check simple plot",autoplot(mixed_mod))
  vdiffr::expect_doppelganger("test superimpose",
                              autoplot(mixed_mod,
                                       x_str = NULL,
                                       superimpose.data = TRUE))
  expect_error(autoplot(mixed_mod, ranef_plot = "group"))
  vdiffr::expect_doppelganger("test ranef_plot arg",
                              autoplot(mixed_mod, ranef_plot = 'subject',
                           superimpose.data = TRUE))

  #testing a more complicated mixed model with multiple groups
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
    lapply(1:15, function(x) {
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
    dplyr::mutate(subject = as.factor(subject))

  mixed_mod_2 <- cglmm(
    Y ~ group + amp_acro(times,
                 n_components = 1,
                 period = 24,
                 group = "group"
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed_2
  )

  #test that the plots return expected figures for different configurations
  vdiffr::expect_doppelganger("test_complex_mixed_mod",(autoplot(mixed_mod_2,
                           superimpose.data = TRUE, ranef_plot = 'subject')))
  vdiffr::expect_doppelganger("test group arg",(autoplot(mixed_mod_2,
                                                         x_str = 'group',
                           superimpose.data = TRUE, ranef_plot = 'subject')))
  vdiffr::expect_doppelganger("test superimpose arg",(autoplot(mixed_mod_2,
                                                               x_str = 'group',
                           superimpose.data = TRUE)))

    })
})
