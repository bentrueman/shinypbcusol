test_that("kinetic_release() returns 1 for 5 mm radius pipe at 24 hrs", {
  expect_equal(kinetic_release(5e-3, 24 * 3600)$value, 1)
})

test_that("kinetic_release() returns 0 for all radii at 0 hrs", {
  out <- unique(kinetic_release(seq(1e-3,25e-3,1e-3), 0)$value)
  expect_equal(out, 0)
})

test_that("plot_kinetics() returns a ggplot", {
  out <- class(plot_kinetics(25))
  expect_equal(out, c("patchwork", "gg", "ggplot"))
})
