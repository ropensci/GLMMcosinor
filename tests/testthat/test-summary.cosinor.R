test_that("summary print works", {
  withr::with_seed(
    50,
    {
      data(vitamind)
      object <- cosinor.glmm(
        Y ~ amp_acro(time,
          group = "X",
          period = 12
        ),
        data = vitamind
      )
      print_obj <- summary(object)
      testthat::expect_no_error(print_obj)
      testthat::expect_snapshot_output(print(print_obj, digits = 2))

      testthat::expect_true(inherits(print_obj, "summary.cosinor.glmm"))
    }
  )
})
