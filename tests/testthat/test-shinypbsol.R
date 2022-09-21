test_that("shinypbsol() returns an object of the expected class", {
  expect_equal(class(shinypbsol()), "shiny.appobj")
})
