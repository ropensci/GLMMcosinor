test_that("multiplication works", {
#TEST outputs from summary script, compare with simulation-study results,
  #seed should give the same output - use this as the expect_equal(...)
  withr::with_seed(
    1,
    out1 <- do_nmb_sim(
      sample_size = 500, n_sims = 500, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.3, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
    )
  )


})
