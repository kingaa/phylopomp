##' The classical Moran model
##'
##' The Markov genealogy process induced by the classical Moran process,
##' in which birth/death events occur at a constant rate and the
##' population size remains constant.
##'
##' @name moran
##' @aliases Moran
##' @include getinfo.R
##' @family Genealogy processes
##' @param mu event rate
##' @param psi sampling rate.
##' @param n population size
##' @param time final time
##' @param t0 initial time
##' @return \code{runMoran} and \code{continueMoran} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Moran}.
##' 
NULL

##' @rdname moran
##' @export
runMoran <- function (
  time,  t0 = 0, n = 100,
  mu = 1, psi = 1
) {
  params <- c(mu=mu,psi=psi)
  ivps <- c(n=n)
  x <- .Call(P_makeMoran,params,ivps,t0)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class=c("gpsim","gpgen"))
}

##' @rdname moran
##' @inheritParams simulate
##' @export
continueMoran <- function (
  object, time, mu = NA, psi = NA
) {
  params <- as.numeric(c(mu=mu,psi=psi))
  x <- .Call(P_reviveMoran,object,params)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class=c("gpsim","gpgen"))
}

##' @rdname moran
##' @details
##' \code{moran_exact} gives the exact log likelihood of a genealogy under the uniformly-sampled Moran process.
##' @return \code{moran_exact} returns the log likelihood of the genealogy.
##' @param data data frame containing the genealogy event times. 
##' @export
moran_exact <- function (data, n = 100, mu = 1, psi = 1) {
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
