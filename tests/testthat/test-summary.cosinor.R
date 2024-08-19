test_that("summary print works", {
  withr::local_seed(50)
  object <- cglmm(
    vit_d ~ amp_acro(time,group = "X",period = 12),
    data = vitamind
  )
  print_obj <- summary(object)
  expect_no_error(print_obj)
  expect_snapshot(print(print_obj, digits = 2))

  expect_true(inherits(print_obj, "cglmmSummary"))

  # test the dispersion and zeroinflation summaries
  object_2 <- cglmm(
    vit_d ~ amp_acro(time,group = "X",period = 12),
    dispformula = ~ amp_acro(time,group = "X",period = 12),
    ziformula = ~ amp_acro(time,group = "X",period = 12),
    data = vitamind
  )
  print_obj <- summary(object_2)
  expect_no_error(print_obj)
  expect_snapshot(print(print_obj, digits = 2))
})
