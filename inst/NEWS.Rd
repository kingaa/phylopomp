\name{NEWS}
\title{News for package `phylopomp'}
\section{Changes in \pkg{phylopomp} version 0.14}{
  \itemize{
    \item Yet another refactoring of the SEIRS model.
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
