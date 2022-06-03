% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seir.R
\name{seir}
\alias{seir}
\alias{SEIR}
\alias{runSEIR}
\alias{continueSEIR}
\title{SEIR model.}
\usage{
runSEIR(
  time,
  t0 = 0,
  Beta = 5,
  sigma = 3,
  gamma = 1,
  psi = 1,
  delta = 0,
  S0 = 500,
  E0 = 10,
  I0 = 10,
  R0 = 0
)

continueSEIR(
  object,
  time,
  Beta = NA,
  sigma = NA,
  gamma = NA,
  psi = NA,
  delta = NA
)
}
\arguments{
\item{time}{final time}

\item{t0}{initial time}

\item{Beta}{transmission rate for the infectious classes.}

\item{sigma}{transition rate from exposed class to infectious class.}

\item{gamma}{recovery rate.}

\item{psi}{sampling rate.}

\item{delta}{rate of loss of immunity}

\item{S0}{initial size of susceptible population.}

\item{E0}{initial size of exposed population.}

\item{I0}{initial size of infectious population.}

\item{R0}{initial size of recovered population.}

\item{object}{either the name of the model to simulate
\emph{or} a previously computed \sQuote{gpsim} object}
}
\value{
An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SEIR}.
}
\description{
SEIR model.
}
\examples{
simulate("SEIR",Beta=2,sigma=3,gamma=1,psi=2,S0=1000,E0=10,I0=5,time=5) |>
  simulate(Beta=5,sigma=2,gamma=2,time=10,psi=3) |>
  plot()

runSEIR(Beta=3,sigma=2,gamma=1,psi=2,S0=20,I0=5,R0=0,time=5,t0=-1) |>
  plot(points=TRUE, obscure=FALSE)

runSEIR(Beta=3,sigma=2,gamma=0.1,psi=0.2,S0=100,I0=5,R0=0,time=2,t0=0) -> x
plot_grid(plotlist=list(plot(x,points=TRUE),diagram(x)),
  ncol=1,rel_heights=c(4,1))

simulate("SEIR",delta=1,time=20,I0=4) |> plot()
simulate("SEIR",delta=1,time=20,I0=4) |> lineages() |> plot.gplin()
}
\seealso{
More example genealogy processes: 
\code{\link{lbdp}},
\code{\link{moran}},
\code{\link{si2r}},
\code{\link{siir}},
\code{\link{simulate}()},
\code{\link{sirwr}},
\code{\link{sir}}
}
\concept{Genealogy processes}