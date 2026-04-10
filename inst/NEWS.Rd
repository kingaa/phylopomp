\name{NEWS}
\title{News for package `phylopomp'}
\section{Changes in \pkg{phylopomp} version 0.17}{
  This release contains a relatively large refactorization of the C++ library.
  \itemize{
    \item There is a new facility for summing genealogies together.
    The sum of two genealogies is defined as the union of all the lineages, curtailed to the intersections of their time-intervals.
    \item The default behavior of the \code{newick()} command is to generate code in an extended Newick format.
    Specifically, information on the type and deme of nodes is returned using a \pkg{phylopomp}-specific metadata tag embedded in each label.
    Optionally, one can generate ordinary Newick-format, in which case all sample demes are represented as tips, the terminal twigs of inline tips having zero length.
    See the manual pages for details.
    \item The Newick parser, \code{parse_newick()} can now read either of these formats.
    It interprets all encountered tips to be samples, unless otherwise indicated using the metadata tag mentioned above.
    See the manual pages for details.
    \item \code{lbdp_exact()} now gives correct results even for empty genealogies.
    \item Internally, \code{phylopomp} now represents the demes more carefully.
    The deme-number 0 is reserved for the \sQuote{undeme}, i.e., nodes or branches with unspecified deme.
    \item The use of palettes in the plotting routines is now more consistent.
    \item The \code{curtail} now takes arguments \code{t0} and \code{time}, consistent with the usage in the rest of the package.
    \item \code{sir_pomp}, \code{seirs_pomp}, and \code{strains_pomp} now expect initial conditions expressed as fractions of a specified host population size (\code{pop}).
    \item When times or branch-lengths are converted to decimal representation (e.g., by \code{newick()} or \code{yaml()}), more care is taken to preserve precision.
  }
}
\section{Changes in \pkg{phylopomp} version 0.16}{
  \itemize{
    \item \code{gendat()} now returns the deme for samples.
    \item The \code{Strains} model (see \code{?Strains}) now has a filtering code: \code{strains_pomp()}.
    \item The supported linear birth-death-sampling processes now allow for both destructive and non-destructive sampling (see \code{?LBDP}).
  }
}
\section{Changes in \pkg{phylopomp} version 0.15}{
  \itemize{
    \item New model in which three distinct strains compete for susceptibles from a single pool.
    See \code{Strains}.
    \item New model which is identical to \code{TwoSpecies} except in that lineages that terminate are replaced by \emph{ghosts}.
    \item Bug fix in \code{curtail()}.
    \item Refactoring of \code{lbdp_exact()}.
  }
}
\section{Changes in \pkg{phylopomp} version 0.14}{
  \itemize{
    \item The \code{curtail()} function now allows one to curtail a genealogy from the left as well as the right.
    \item The filtering codes for the two-species model are now working.
    \item A new sample/death event-type has been introduced into the \code{genealogy_t} type.
    \item Corrections of filtering equations for the SEIR and Two-species models.
    \item Yet another refactoring.
    The SEIRS, LBDP, and SIRS models have been refactored to streamline the filtering calculations in a generalizable fashion.
  }
}
\section{Changes in \pkg{phylopomp} version 0.13}{
  \itemize{
    \item Much faster computations with complex genealogies.
    \item New S2I2R2 model of two-species system.
  }
}
\section{Changes in \pkg{phylopomp} version 0.11}{
  \itemize{
    \item The SEIRS model pomp object has been refactored.
  }
}
\section{Changes in \pkg{phylopomp} version 0.10}{
  \itemize{
    \item \code{getInfo()}, \code{parse_newick()} now always trace lineages.
    \item Lineage and deme information has been refactored: nodes no longer carry deme information, balls do.
    \item The genealogy description (\code{getInfo(description=TRUE)}) now reports both deme and lineage.
    \item \code{curtail()} more carefully rewinds the history.
    \item Parameter name changes in some of the examples.
    \item In plotting, trees are shifted down on the y axis.
  }
}
\section{Changes in \pkg{phylopomp} version 0.9}{
  \itemize{
    \item Inclusion of \pkg{pomp} methods for SEIRS model.
    \item Refactored \code{sirs_pomp} and \code{lbdp_pomp}.
    \item The new function \code{geneal} extracts the bare genealogy from a simulated master process object.
    \item Changes in the interfaces to \code{lbdp_exact}, \code{lbdp_pomp}, \code{moran_exact}, and \code{sir_pomp}.
    These functions now take as data objects of class \sQuote{gpgen}.
    \item The old function \code{newick2df} is no more.
    \item Considerable reworking of the internals.
    \item Definition of new class, \sQuote{gpsim}, for bare genealogies.
    \item Definition of new class, \sQuote{gpyaml}, for YAML descriptions.
  }
}
\section{Changes in \pkg{phylopomp} version 0.8}{
  \itemize{
    \item The interface to \code{getInfo} has changed.
  }
}
\section{Changes in \pkg{phylopomp} version 0.7}{
  \itemize{
    \item In genealogy diagrams, the nodes are now colored according to deme.
    This can be controlled through the use of the \code{diagram} function's \code{palette} argument (cf. \code{plot}).
    \item The new function \code{curtail} curtails a genealogy (i.e., casts away nodes beyond a given time).
    \item The new function \code{newick} extracts a Newick-format description of a genealogy.
    \item The SIRS model is included as a synonym for SIR.
    Both allow waning of immunity.
    \item The \code{lineages} function now returns the saturation and a code for the event-type as well as the lineage-count.
    The returned data frame is now in a long format.
    \item The \code{newick2df} function has been rewritten.
    \item A new function \code{parse_newick} allows one to parse a Newick string into the formats used in \pkg{phylopomp}.
  }
}
\section{Changes in \pkg{phylopomp} version 0.6}{
  \itemize{
    \item The option to generate \dQuote{non-compact} Newick-format tree representations has been removed:
    all Newick representations are now in the \dQuote{compact} format.
    In this format, each node has a color indicated by a letter immediately following its closing parenthesis:
    this is \sQuote{b} if the node holds a blue ball (i.e., it represents a sample) and \sQuote{g} otherwise.
    Information about the node's deme and its name are provided as well.
    \item Previously, the Newick formatter employed \dQuote{invisible} nodes to assist in the formatting of plots.
    As of this release, they no longer contain such nodes:
    the generated Newick strings now accurately represent the forest of trees corresponding to the genealogy process.
    The proper formatting for plots is accomplished within the plotting routines themselves.
  }
}
\section{Changes in \pkg{phylopomp} version 0.5}{
  \itemize{
    \item Function \code{moran_exact} computes the exact likelihood under the Moran model.
  }
}
\section{Changes in \pkg{phylopomp} version 0.4}{
  \itemize{
    \item Refactored the SIRS model \sQuote{pomp} object.
    \item The \code{obscure} operation now drops single-child internal nodes in addition to erasing all deme information.
    \item Corrected certain inconsistencies in the way the lineage-count table was reported and used in pomp objects.
    The \code{lbdp_pomp} and \code{sir_pomp} functions assume a particular form of the lineage count table, such that every entry corresponds to either a root, a branch point, or a sample (but NB, the last entry is treated differently).
    This assumption was violated beginning with version 0.2.0.0 in that the lineage count table returned by \code{lineages()} contained an entry for internal nodes.
    Now, when \code{obscure=TRUE} (the default), the behavior is as it was before, consistent with the assumptions of \code{lbdp_pomp} and \code{sir_pomp}
  }
}
\section{Changes in \pkg{phylopomp} version 0.2}{
  \itemize{
    \item Streamlined of the GP definition.
  }
}
\section{Changes in \pkg{phylopomp} version 0.1}{
  \itemize{
    \item Updated for use with \pkg{pomp} version 4.6.
  }
}
