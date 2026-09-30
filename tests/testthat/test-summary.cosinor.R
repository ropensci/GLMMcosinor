test_that("summary print works", {
  simple_model <- readRDS(test_path("fixtures", "simple_model.rds"))
  multi_model <- readRDS(test_path("fixtures", "multi_model.rds"))

  print_obj <- summary(simple_model)
  expect_snapshot(print(print_obj, digits = 2))
  expect_s3_class(print_obj, "cglmmSummary")

  print_obj <- summary(multi_model)
  expect_snapshot(print(print_obj, digits = 1))
})

test_that("summary works for components sharing a period but different groups (#29)", {
  withr::local_seed(42)
  vitamind_multi <- vitamind
  vitamind_multi$Z <- sample(0:1, size = nrow(vitamind_multi), replace = TRUE)

  # two components share the same period (12), so they share the same
  # underlying main_rrr1/main_sss1 columns, but are tied to different
  # grouping variables (X and Z)
  object <- cglmm(
    vit_d ~ X + Z + amp_acro(time, n_components = 2, group = c("X", "Z"), period = c(12, 12)),
    data = vitamind_multi
  )

  print_obj <- summary(object)
  expect_s3_class(print_obj, "cglmmSummary")

  smat <- print_obj$main_output$transformed.table
  expect_false(anyNA(rownames(smat)))
  expect_false(anyNA(smat$estimate))

  # summary()'s transformed coefficient estimates should match those used by
  # print() (get_new_coefs()) - both are derived from the same raw
  # coefficients, just labelled differently ("[X=0]:amp1" vs "X0:amp1")
  expect_equal(unname(smat$estimate), unname(object$coefficients))

  # each component keeps its own group's coefficients distinct, rather than
  # being NA or accidentally shared with the other component
  expect_true(any(grepl("^\\[X=0\\]:amp1$", rownames(smat))))
  expect_true(any(grepl("^\\[X=1\\]:amp1$", rownames(smat))))
  expect_true(any(grepl("^\\[Z=1\\]:amp2$", rownames(smat))))
})
