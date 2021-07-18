##' Leventhal (2014) SI model
##'
##' Simulation and inference for Leventhal's model
##'
##' @name leventhal
##' @rdname leventhal
##' @include getinfo.R sirws.R
##'
##' @family Genealogy processes
##'
##' @inheritParams lbdp
##' @param data optional data frame; output from \code{playLeventhal}.
##' @param beta contact rate
##' @param gamma recovery rate
##' @param psi sampling rate
##' @param N population size
##' @param I0 initial number of infections
##' 
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
NULL

##' @name leventhal_pomp
##' @rdname leventhal
##'
##' @details
##' \code{leventhal_pomp} constructs a \pkg{pomp} object containing a given set of data and a Leventhal-type SIS infection.
##'
##' It is assumed that \code{data} is in the format returned by \code{\link{nwk2df}}.
##'
##' @importFrom pomp pomp onestep covariate_table
##' @inheritParams lbdp_exact
##'
##' @export
leventhal_pomp <- function (data, beta, gamma, psi, N, I0 = 1, t0 = 0) {
  data[,"time"] %>%
    pomp(
      times="time",t0=t0,
      params=c(beta=beta,gamma=gamma,psi=psi,N=N,I0=I0),
      rprocess=onestep("leventhal_stepfn"),
      rinit="leventhal_rinit",
      dmeasure="leventhal_dmeas",
      accumvars=c("ll"),
      statenames=c("I","ll"),
      paramnames=c("beta","gamma","psi","N","I0"),
      PACKAGE="phylopomp",
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      )
    )
}
