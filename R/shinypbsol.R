
#' Calculate equilibrium lead solubility using `pbcusol` and `shiny`
#'
#' @description This function is used in deploying the `shinypbcusol` `shiny` app.
#'
#' @param ... Argument passed to function. (Currently ignored.)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
shinypbsol <- function(...) {
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
    shiny::titlePanel("Calculate equilibrium lead solubility"),
    shiny::fluidRow(
      shiny::column(2,
        shiny::sliderInput("pH", label = "pH", min = 4, max = 11, value = 7.3, step = .1),
        shiny::sliderInput(
          "alkalinity", label = shiny::HTML("Alkalinity (mg CaCO<sub>3</sub> L<sup>-1</sup>)"),
          min = 0, max = 150, value = 20, step = .1
        ),
        shiny::sliderInput(
          "phosphate", label = shiny::HTML("Orthophosphate (mg P L<sup>-1</sup>)"),
          min = 0, max = 3, value = .16, step = .01
        ),
        shiny::sliderInput(
          "chloride", label = shiny::HTML("Chloride (mg L<sup>-1</sup>)"),
          min = 0, max = 500, value = 10, step = 1
        ),
        shiny::sliderInput(
          "sulfate", label = shiny::HTML("Sulfate (mg L<sup>-1</sup>)"),
          min = 0, max = 500, value = 10, step = 1
        ),
        shiny::sliderInput(
          "calcium", label = shiny::HTML("Calcium (mg L<sup>-1</sup>)"),
          min = 0, max = 200, value = 10, step = 1
        ),
        shiny::sliderInput(
          "magnesium", label = shiny::HTML("Magnesium (mg L<sup>-1</sup>)"),
          min = 0, max = 200, value = 10, step = 1
        ),
        shiny::sliderInput(
          "humic", label = shiny::HTML("Humic/fulvic substances (mg DOC L<sup>-1</sup>) (experimental)"),
          min = 0, max = 5, value = 0, step = .01
        )
      ),
      shiny::column(3, align = "left",
        shiny::HTML("&ensp;&nbsp;&nbsp;Equilibrium lead solubility at 25&deg;C:"),
        shiny::tableOutput("pbsol"),
        shiny::plotOutput("plot", width = 450, height = 765)
      )

    )
  )

  server <- function(input, output, session) {

    out_leadsol <- shiny::reactive(eq_sol_tbl(input))
    out_minteq.v4 <- shiny::reactive(eq_sol_tbl(input, "minteq.v4", dbase = phreeqc::minteq.v4.dat))

    table1 <- shiny::reactive(
      tibble::tibble(
        LEADSOL = glue::glue("{round(out_leadsol())} ppb"),
        minteq.v4 = glue::glue("{round(out_minteq.v4())} ppb")
      )
    )

    output$plot <- shiny::renderPlot({
      list(
        "LEADSOL" = out_leadsol(),
        "minteq.v4" = out_minteq.v4()
      ) %>%
        plot_kinetics()
    })

    output$pbsol <- shiny::renderTable({table1()})

  }

  shiny::shinyApp(ui, server)
}
