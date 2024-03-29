% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_kinetics.R
\name{kinetic_release}
\alias{kinetic_release}
\title{Calculate normalized lead release from a lead pipe}
\usage{
kinetic_release(a, t, D = 1e-09, beta = 1)
}
\arguments{
\item{a}{A vector of pipe radii (m).}

\item{t}{A vector of stagnation times (s).}

\item{D}{Diffusion coefficient (m^2/s).}

\item{beta}{Mass-transfer coefficient (1 to negate its effects, i.e., no diffusion barrier on pipe surface).}
}
\value{
A tibble with three columns: time in seconds, a variable name, and a normalized concentration.
}
\description{
\code{kinetic_release} computes the normalized Pb concentration (a and t can either be vectors or scalars), as described by
Kuch & Wagner (\url{http://doi.org/10.1016/0043-1354(83)90256-7}).
}
\examples{
kinetic_release(25e-3, 3600)
}
