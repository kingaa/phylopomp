\name{NEWS}
\title{News for package `phylopomp'}
\section{Changes in \pkg{phylopomp} version 0.5.0}{
  \itemize{
    \item Function \code{moran_exact} computes the exact likelihood under the Moran model.
  }
}
\section{Changes in \pkg{phylopomp} version 0.4.0}{
  \itemize{
    \item Refactored the SIRS model \sQuote{pomp} object.
    \item The \code{obscure} operation now drops single-child internal nodes in addition to erasing all deme information.
    \item Corrected certain inconsistencies in the way the lineage-count table was reported and used in pomp objects.
    The \code{lbdp_pomp} and \code{sir_pomp} functions assume a particular form of the lineage count table, such that every entry corresponds to either a root, a branch point, or a sample (but NB, the last entry is treated differently).
    This assumption was violated beginning with version 0.2.0.0 in that the lineage count table returned by \code{lineages()} contained an entry for internal nodes.
    Now, when \code{obscure=TRUE} (the default), the behavior is as it was before, consistent with the assumptions of \code{lbdp_pomp} and \code{sir_pomp} 
  }
}
\section{Changes in \pkg{phylopomp} version 0.2.0}{
  \itemize{
    \item Streamlined of the GP definition.
  }
}
\section{Changes in \pkg{phylopomp} version 0.1.0}{
  \itemize{
    \item Updated for use with \pkg{pomp} version 4.6.
  }
}
