##' @rdname moran
##' @include moran.R
##' @details
##' \code{moran_exact} gives the exact log likelihood of a genealogy under the uniformly-sampled Moran process.
##' @return \code{moran_exact} returns the log likelihood of the genealogy.
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @export
moran_exact <- function (x, n = 100, mu = 1, psi = 1) {
  x |> lineages() -> data
  ndat <- nrow(data)
  code <- as.integer(c(2L,diff(data$lineages)))
  code[ndat] <- -2L
  intervals <- diff(data$time)
  ell <- data$lineages[-ndat]
  ncoal <- sum(code==1L)   # number of coalescence points
  nanc <- sum(code==0L)    # number of ancestral samples
  tips <- which(code==-1L) # positions of tip samples
  mfact <- 2*mu/(n-1)      # = mu*n/choose(n,2)
  if (any(ell>n) || any(ell[tips]>=n)) {
    -Inf
  } else {
    -mfact*sum(choose(ell,2)*intervals)-
      psi*n*diff(range(data$time))+
        ncoal*log(mfact)+
        (nanc+length(tips))*log(psi)+
        sum(log(n-ell[tips]))
  }
}
