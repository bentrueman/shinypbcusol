
#' Calculate equilibrium lead solubility using `pbcusol` and `shiny`
#'
#' @param ... Argument passed to function. (Currently ignored.)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
shinypbsol <- function(...) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Calculate equilibrium lead solubility"),
    shiny::sliderInput("pH", label = "pH", min = 4, max = 11, value = 7.3, step = .1),
    shiny::sliderInput("alkalinity", label = shiny::HTML("Alkalinity (mg CaCO<sub>3</sub> L<sup>-1</sup>)"), min = 0, max = 150, value = 20, step = .1),
    shiny::sliderInput("phosphate", label = shiny::HTML("Orthophosphate (mg P L<sup>-1</sup>)"), min = 0, max = 3, value = .16, step = .01),
    shiny::sliderInput("chloride", label = shiny::HTML("Chloride (mg L<sup>-1</sup>)"), min = 0, max = 500, value = 10, step = 1),
    shiny::sliderInput("sulfate", label = shiny::HTML("Sulfate (mg L<sup>-1</sup>)"), min = 0, max = 500, value = 10, step = 1),
    shiny::sliderInput("calcium", label = shiny::HTML("Calcium (mg L<sup>-1</sup>)"), min = 0, max = 200, value = 10, step = 1),
    shiny::sliderInput("magnesium", label = shiny::HTML("Magnesium (mg L<sup>-1</sup>)"), min = 0, max = 200, value = 10, step = 1),
    shiny::sliderInput("humic", label = shiny::HTML("Humic/fulvic substances (mg DOC L<sup>-1</sup>) (experimental)"), min = 0, max = 5, value = 0, step = .01),
    shiny::HTML("Equilibrium lead solubility at 25&deg;C:"),
    shiny::tableOutput("pbsol")
  )

  server <- function(input, output, session) {
    output$pbsol <- shiny::renderTable({
      conversion <- tibble::tibble(
        phase = c("Cerussite", "Hydcerussite", "Hydrocerussite", "Hxypyromorphite", "Hydroxylpyromorphite"),
        no_Pb = c(1, 3, 3, 5, 5)
      )

      select_pb <- function(x) {
        x %>%
          dplyr::slice_min(.data$pb_ppb) %>%
          dplyr::pull(.data$pb_ppb) %>%
          round() %>%
          paste0(" ppb")
      }

      pbsol_out <- function(phases, dbase = pbcusol:::pbcu2sol) {
        sol_table <- purrr::map_dfr(
          phases,
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

      phases <- list(
        leadsol = c("Cerussite", "Hydcerussite", "Hxypyromorphite"),
        minteq.v4 = c("Cerussite", "Hydrocerussite", "Hydroxylpyromorphite")
      )

      phases <- if (input$phosphate == 0) {
        phases %>% purrr::map(~ .x[!str_detect(.x, "pyromorphite")])
      } else {
        phases
      }

      tibble::tibble(
        LEADSOL = pbsol_out(phases$leadsol),
        minteq.v4 = pbsol_out(phases$minteq.v4, db = phreeqc::minteq.v4.dat),
      )
    })
  }

  shiny::shinyApp(ui, server)
}
