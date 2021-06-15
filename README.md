
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinypbcusol

<!-- badges: start -->

<!-- badges: end -->

`shinypbcusol` provides a convenient interface for `pbcusol` (Trueman,
2021; Dunnington, 2019) using `shiny` (Chang et al., 2021).

## Getting started

The `shinypbcusol` app is available
[here](https://bentrueman.shinyapps.io/shinypbcusol/).

Currently, it generates lead solubility predictions using the LEADSOL
(Schock et al. 1996) and *minteq.v4* (Charlton and Parkhurst, 2011;
Parkhurst and Appelo, 2013) databases. Lead binding to humic substances
is modeled using an approximation to the Windermere Humic Aqueous Model
(Tipping and Hurley, 1992). See `pbcusol::eq_sol_wham()`.

# References

Chang, W., J. Cheng, J.J. Allaire, C. Sievert, B. Schloerke, Y. Xie, J.
Allen, J. McPherson, A. Dipert and B. Borges. 2021. shiny: Web
Application Framework for R. R package version 1.6.0.
<https://CRAN.R-project.org/package=shiny>

Charlton, S.R., and D. L. Parkhurst. 2011. Modules based on the
geochemical model PHREEQC for use in scripting and programming
languages. Computers & Geosciences, v. 37, p. 1653-1663.

Dunnington, D. 2019. tidyphreeqc: Tidy Geochemical Modeling Using
PHREEQC. <https://github.com/paleolimbot/tidyphreeqc>.

Parkhurst, D. L., and C. A. J. Appelo. 2013. Description of input and
examples for PHREEQC version 3–A computer program for speciation, batch-
reaction, one-dimensional transport, and inverse geochemical
calculations: U.S. Geological Survey Techniques and Methods, book 6,
chap. A43, 497 p. <http://pubs.usgs.gov/tm/06/a43>.

Schock, M. R., I. Wagner, and R. J. Oliphant. 1996. “Corrosion and
solubility of lead in drinking water.” In Internal corrosion of water
distribution systems, 2nd ed., p. 131–230. Denver, CO: American Water
Works Association Research Foundation.

Tipping, E., and M. A. Hurley. 1992. A unifying model of cation binding
by humic substances. Geochimica et Cosmochimica Acta, v. 56, no. 10,
p. 3627-3641.

Trueman, Benjamin. 2021. pbcusol: Predict Lead and Copper Solubility. R
package version 0.0.0.9000. <https://github.com/bentrueman/pbcusol>
