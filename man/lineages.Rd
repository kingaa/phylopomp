% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineages.R
\name{lineages}
\alias{lineages}
\alias{plot.gplin}
\title{Lineage-count function}
\usage{
lineages(object, prune = TRUE, obscure = TRUE)

\method{plot}{gplin}(x, ..., palette = scales::hue_pal(l = 30, h = c(220, 580)))
}
\arguments{
\item{object}{\code{gpsim} object.}

\item{prune}{logical; prune the genealogy?}

\item{obscure}{logical; obscure the demes?}

\item{x}{object of class \sQuote{gpgen}}

\item{...}{passed to \code{\link[ggplot2]{theme}}.}

\item{palette}{color palette for branches.
This can be furnished either as a function or a vector of colors.
If this is a function, it should take a single integer argument, the number of colors required.
If it is a vector, it should have at least as many elements as there are demes in the genealogy.}
}
\value{
A \code{\link[tibble]{tibble}} containing information about the genealogy.
See Details for specifics.
The \code{\link[tibble]{tibble}} returned by \code{lineages} has a \code{\link[=plot.gplin]{plot}} method.
}
\description{
Lineage-counts, saturations, and event-codes.
}
\details{
This function extracts from the specified genealogy several important time-varying quantities.
These include: \describe{
 \item{lineages}{number of lineages through time}
 \item{saturation}{the number of lineages emerging from the event}
 \item{event_type}{an integer coding the type of event}
}

If the genealogy has been obscured (the default), the number in the \code{lineages} returned is the total number of lineages present at the specified time and the saturation is the total saturation.
If the genealogy has not been obscured (\code{obscure = FALSE}), the deme-specific data are returned.
In this case, the \code{deme} column specifies the pertinent deme.

The event types are: \describe{
  \item{0}{no event,}
  \item{-1}{a root,}
  \item{1}{a sample event,}
  \item{2}{a non-sample event,}
  \item{3}{the end of the time interval, which may or may not coincide with the latest tip of the genealogy.}
}
}
\examples{
library(tidyverse)

pal <- c("#00274c","#ffcb05")

simulate("SIIR",time=3) -> x
plot_grid(
  x |> plot(),
  x |> lineages() |> plot(),
  x |> plot(obscure=FALSE,palette=pal),
  x |> lineages(obscure=FALSE) |>
    plot(palette=pal,legend.position=c(0.8,0.9)),
  align="v",axis="b",
  ncol=2,byrow=FALSE
)
}
