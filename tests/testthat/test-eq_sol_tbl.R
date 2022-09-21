
input <- list(
  pH = 7,
  alkalinity = 20,
  humic = 0,
  phosphate = 1,
  aluminum = 0,
  chloride = 10,
  sulfate = 10,
  calcium = 10,
  magnesium = 10
)

test_that("eq_sol_tbl() returns expected result", {
  out <- eq_sol_tbl(input)
  ref <- pbcusol::pb_sol_fixed(
    element = "Pb",
    ph = input$pH,
    dic = pbcusol::calculate_dic(input$pH, input$alkalinity),
    phosphate = input$phosphate,
    Cl = input$chloride / chemr::mass("Cl"),
    `S(6)` = input$sulfate / chemr::mass("SO4"),
    Ca = input$calcium / chemr::mass("Ca"),
    Mg = input$magnesium / chemr::mass("Mg"),
    phase = "Hxypyromorphite"
  )$pb_ppb
  expect_equal(out, ref)
})


