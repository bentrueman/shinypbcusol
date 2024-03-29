
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

  variscite <- list(
    "Variscite",
    "AlPO4:2H2O = Al+3 + PO4-3 + 2H2O",
    "log_k" = -22.36 # https://doi.org/10.1016/j.gca.2010.10.012
  )

  aqueous_species <- list(
    # from https://doi.org/10.1016/j.gca.2010.10.012 and
    # https://doi.org/10.1080/09593332708618735
    "HPO4-2 + Al+3 = AlHPO4+",
    "log_k" = 7.4,
    "HPO4-2 + H+ + Al+3 = AlH2PO4+2",
    "log_k" = 3.1
  )

  conversion <- tibble::tibble(
    phase = c("Cerussite", "Hydcerussite", "Hydrocerussite", "Hxypyromorphite", "Hydroxylpyromorphite"),
    no_Pb = c(1, 3, 3, 5, 5)
  )

  phases <- list(
    leadsol = c("Cerussite", "Hydcerussite", "Hxypyromorphite"),
    minteq.v4 = c("Cerussite", "Hydrocerussite", "Hydroxylpyromorphite")
  )

  # remove hydroxylpyropmorhite if there is no phosphate in solution:
  if (input$phosphate == 0) {
    phases <- purrr::map(phases, ~ .x[!stringr::str_detect(.x, "pyromorphite")])
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
      # effect of Al:
      Al = input$aluminum / chemr::mass("Al"),
      eq_phase_components = list("Variscite" = c(0, 0)),
      new_species = aqueous_species,
      new_phase = variscite,
      phase_out = "Variscite",
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

select_pb <- function(x) {
  x %>%
    dplyr::slice_min(.data$pb_ppb) %>%
    dplyr::pull(.data$pb_ppb)
}
