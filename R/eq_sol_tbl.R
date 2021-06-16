
select_pb <- function(x) {
  x %>%
    dplyr::slice_min(.data$pb_ppb) %>%
    dplyr::pull(.data$pb_ppb)
}

#' Generate a lead solubility table using inputs from `shiny`.
#'
#' @param input Input from `shiny` UI.
#' @param model Currently either "leadsol" or "minteq.v4".
#' @param dbase A `phreeqc` or `pbcusol` database.
#'
#' @return A numeric vector of lead concentrations.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
eq_sol_tbl <- function(input, model = "leadsol", dbase = pbcusol:::pbcu2sol) {

  conversion <- tibble::tibble(
    phase = c("Cerussite", "Hydcerussite", "Hydrocerussite", "Hxypyromorphite", "Hydroxylpyromorphite"),
    no_Pb = c(1, 3, 3, 5, 5)
  )

  phases <- list(
    leadsol = c("Cerussite", "Hydcerussite", "Hxypyromorphite"),
    minteq.v4 = c("Cerussite", "Hydrocerussite", "Hydroxylpyromorphite")
  )

  phases <- if (input$phosphate == 0) {
    phases %>% purrr::map(~ .x[!stringr::str_detect(.x, "pyromorphite")])
  } else {
    phases
  }

  sol_table <- purrr::map_dfr(
    phases[[model]],
    ~ pbcusol::eq_sol_wham(
      # basic inputs:
      element = "Pb",
      ph = input$pH,
      dic = pbcusol::calculate_dic(input$pH, input$alkalinity),
      mass_ha = 1e-3 * input$humic,
      phosphate = input$phosphate,
      # supplementary inputs:
      Cl = input$chloride / chemr::mass("Cl"),
      `S(6)` = input$sulfate / chemr::mass("SO4"),
      Ca = input$calcium / chemr::mass("Ca"),
      Mg = input$magnesium / chemr::mass("Mg"),
      phase = .x,
      db = dbase
    )
  )

  if (input$humic == 0) {
    sol_table %>%
      select_pb()
  } else {
    sol_table %>%
      dplyr::left_join(conversion, by = "phase") %>%
      dplyr::mutate(
        mol_diss = stats::na.omit(dplyr::c_across(tidyselect::matches("^mol_[CH]"))),
        pb_ppb = 1e6 * .data$no_Pb * .data$mol_diss * chemr::mass("Pb")
      ) %>%
      select_pb()
  }
}


