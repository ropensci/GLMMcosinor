test_that("summary print works", {
  simple_model <- readRDS(test_path("fixtures", "simple_model.rds"))
  multi_model <- readRDS(test_path("fixtures", "multi_model.rds"))

  print_obj <- summary(simple_model)
  expect_snapshot(print(print_obj, digits = 2))
  expect_s3_class(print_obj, "cglmmSummary")

  print_obj <- summary(multi_model)
  expect_snapshot(print(print_obj, digits = 1))
})
