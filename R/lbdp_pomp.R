##' @rdname lbdp
##' @include lbdp.R
##' @details
##' \code{lbdp_pomp} constructs a \pkg{pomp} object containing a given set of data and a linear birth-death-sampling process.
##' @importFrom pomp pomp onestep covariate_table
##' @inheritParams lbdp_exact
##' @export
lbdp_pomp <- function (x, lambda, mu, psi, n0 = 1, t0 = 0)
{
  x |> gendat() -> gi
  n0 <- round(n0)
  if (n0 < 0)
    pStop(sQuote("n0")," must be a nonnegative integer.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(lambda=lambda,mu=mu,psi=psi,n0=n0),
    userdata=gi,
    rinit="lbdp_rinit",
    dmeasure="lbdp_dmeas",
    rprocess=onestep("lbdp_gill"),
    statenames=c("n","ll","ell","node"),
    paramnames=c("lambda","mu","psi","n0"),
    PACKAGE="phylopomp"
  )
}

encode_data <- function (data) {
  data <- as.data.frame(data)
  data$code <- as.integer(c(2,diff(data$lineages)))
  data$code[data$event_type==0] <- 2L
  data$code[data$event_type==3] <- -2L
  stopifnot(
    `misformatted data (1)`=all(data$event_type!=2 | data$code==1),
    `misformatted data (2)`=all(data$event_type!=1 | data$code==0 | data$code==-1)
  )
  data
}
