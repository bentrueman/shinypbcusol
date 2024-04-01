
#' Calculate normalized lead release from a lead pipe
#'
#' @description `kinetic_release` computes the normalized Pb concentration (a and t can either be vectors or scalars), as described by
#' Kuch & Wagner (\url{http://doi.org/10.1016/0043-1354(83)90256-7}).
#'
#' @param a A vector of pipe radii (m).
#' @param t A vector of stagnation times (s).
#' @param D Diffusion coefficient (m^2/s).
#' @param beta Mass-transfer coefficient (1 to negate its effects, i.e., no diffusion barrier on pipe surface).
#'
#' @return A tibble with three columns: time in seconds, a variable name, and a normalized concentration.
#' @export
#'
#' @examples
#' kinetic_release(25e-3, 3600)
kinetic_release <- function(
  a,
  t,
  D = 1e-9,
  beta = 1
) {
  results <- matrix(NA, ncol = length(t), nrow = length(a)) #collector matrix for concentrations
  rownames(results) <- paste0("rad_", a)
  for(i in 1:length(t)) {
    Fo <- D * t[i] / (4 * a ^ 2) # Fourier number
    Bi <- beta * 2 * a / D # Biot number
    numerator <- -4 * Fo
    denominator <- 1 / Bi + (5.78 ^ 2 + 4 / (pi * Fo)) ^ (-0.5)
    results[, i] <-  1 - exp( numerator / denominator ) # Ct is the normalized Pb conc. post-stag.
  }
  t(results) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(time_s = t) %>%
    reshape2::melt(id.vars = "time_s") %>%
    tibble::as_tibble()
}

#' Plot a kinetic metal release curve due to stagnation in a pipe.
#'
#' @param eq_sol The equilibrium solubility value.
#' @param return Options are "plot" for a ggplot object, or "table" for the table of data that the plot represent.
#'
#' @return A ggplot2 object or a data frame.
#' @export
#'
#' @examples
#' plot_kinetics(100)
plot_kinetics <- function(eq_sol, return = "plot") {
  plot_in <- kinetic_release(
    a = seq(5, 25, by = 1) / 1000, # sequence of pipe radii
    t = 3600 * seq(0, 24, by = 0.1), # sequence of time t for plot
  ) %>%
    dplyr::mutate(
      radius = stringr::str_extract(.data$variable, "0.[0-9]+") %>%
        as.numeric(),
      diameter = 1e3 * 2 * .data$radius
    )

  table_out <- purrr::map_dfr(eq_sol, ~ dplyr::mutate(plot_in, pb_ppb = .x * value), .id = "model") |>
    dplyr::transmute(time_hours = .data$time_s / 3600, pb_ppb = .data$pb_ppb, pipe_diameter_mm = .data$diameter, model = .data$model)

  kplot <- table_out %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$time_hours,
        y = .data$pb_ppb,
        col = .data$pipe_diameter_mm,
        group = .data$pipe_diameter_mm
      )
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$model), ncol = 1) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
      x = "Stagnation time (h)",
      y = "[Pb] after stagnation (&mu;g L <sup>-1<sup>)",
      col = "Pipe diameter (mm)"
    ) +
    ggplot2::guides(col = ggplot2::guide_colorbar()) +
    ggplot2::theme_minimal(18) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#606060"),
      legend.position = "bottom",
      axis.title.y = ggtext::element_markdown()
    )

    final_plot <- patchwork::wrap_plots(kplot) +
    patchwork::plot_annotation(
      title = "Lead release vs. stagnation time",
      subtitle = "<span style = 'color:#132B43;'>Small diameter pipes</span><span style = 'color:#808080;'> reach equilibrium faster than </span><br>
      <span style = 'color:#56B1F7;'>large diameter pipes.</span>"
    ) &
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, color = "#606060"),
      plot.subtitle = ggtext::element_markdown(size = 16)
    )
    stopifnot("argument 'return' must be either 'plot' or 'table'" = return %in% c("plot", "table"))
    if (return == "plot") final_plot else table_out
}
