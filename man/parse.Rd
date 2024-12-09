% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_newick}
\alias{parse_newick}
\title{parse a Newick string}
\usage{
parse_newick(x, t0 = 0, tf = NA)
}
\arguments{
\item{x}{character; the Newick description.
See Details for specifics.}

\item{t0}{numeric; the root time.}

\item{tf}{numeric; the current or final time.}
}
\value{
An object of class \dQuote{gpgen}.
}
\description{
Parses a Newick description and returns a binary version of the genealogy.
}
\details{
\code{parse_newick} can only handle a subset of the full Newick specification.
In particular, labels are assumed to be of the form <TYPE>_<DEME>_<LABEL>,
i.e., each label has three parts, separated by underscores (\sQuote{_}).
The parts are as follows.
\itemize{
\item TYPE must be a single character from among the following: \sQuote{b}, \sQuote{g}, \sQuote{m}, \sQuote{o}.
\itemize{
\item \sQuote{b} signifies a sample.
\item \sQuote{g} signifies an internal node.
\item \sQuote{m} signifies a root.
\item \sQuote{o} indicates an extant lineage.
}
\item DEME must be a non-negative integer, specifying the deme in which the branch resides.
If deme information is not present, use 0.
\item LABEL is ignored and may be left out.
}
}
