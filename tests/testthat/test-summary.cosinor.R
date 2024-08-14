test_that("summary print works", {
  withr::local_seed(50)

  object <- cglmm(
    vit_d ~ amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  print_obj <- summary(object)
  testthat::expect_no_error(print_obj)
  testthat::expect_snapshot_output(print(print_obj, digits = 2))

  testthat::expect_true(inherits(print_obj, "cglmmSummary"))

  # test the dispersion and zeroinflation summaries
  object_2 <- cglmm(
    vit_d ~ amp_acro(time,
      group = "X",
      period = 12
    ),
    dispformula = ~ amp_acro(time,
      group = "X",
      period = 12
    ), ziformula = ~ amp_acro(time,
      group = "X",
      period = 12
    ),
    data = vitamind
  )
  print_obj <- summary(object_2)
  testthat::expect_no_error(print_obj)
  testthat::expect_snapshot_output(print(print_obj, digits = 2))

})
