% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lbdp.R
\name{lbdp}
\alias{lbdp}
\alias{LBDP}
\alias{runLBDP}
\alias{continueLBDP}
\alias{lbdp_exact}
\alias{lbdp_pomp}
\title{Linear birth-death-sampling model}
\usage{
runLBDP(time, t0 = 0, lambda = 2, mu = 1, psi = 1, n0 = 5)

continueLBDP(object, time, lambda = NA, mu = NA, psi = NA)

lbdp_exact(x, lambda, mu, psi, n0 = 1)

lbdp_pomp(x, lambda, mu, psi, n0 = 1, t0 = 0)
}
\arguments{
\item{time}{final time}

\item{t0}{initial time}

\item{lambda}{per capita birth rate}

\item{mu}{per capita recovery rate.}

\item{psi}{per capita sampling rate.}

\item{n0}{initial population size}

\item{object}{either the name of the model to simulate
\emph{or} a previously computed \sQuote{gpsim} object}

\item{x}{genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).}
}
\value{
\code{runLBDP} and \code{continueLBDP} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.

\code{lbdp_exact} returns the log likelihood of the genealogy.
Note that the time since the most recent sample is informative.
}
\description{
The genealogy process induced by a simple linear birth-death process
with constant-rate sampling.
}
\details{
\code{lbdp_exact} gives the exact log likelihood of a linear birth-death process, conditioned on \eqn{n_0 = 0}{n0=0} (Stadler, 2010, Thm 3.5).
The derivation is also given in comments in the code.

\code{lbdp_pomp} constructs a \pkg{pomp} object containing a given set of data and a linear birth-death-sampling process.
}
\examples{
simulate("LBDP",time=4) |> plot(points=TRUE)

simulate("LBDP",lambda=2,mu=1,psi=3,n0=1,time=1) |>
  simulate(time=10,lambda=1) |>
  plot()

simulate("LBDP",time=4) |>
  lineages() |>
  plot()

}
\references{
\King2024

\King2022

\Stadler2010

\Stadler2010
}
\seealso{
More example genealogy processes: 
\code{\link{moran}},
\code{\link{s2i2r2}},
\code{\link{seir}},
\code{\link{si2r}},
\code{\link{siir}},
\code{\link{simulate}()},
\code{\link{sir}}
}
\concept{Genealogy processes}
