##' Two-host infection model with spillover and demography.
##' Hosts are culled upon sampling.
##'
##' The population is structured by infection progression
##' and host species.
##'
##' @name mers
##' @family Genealogy processes
##' @aliases MERS
##' @include getinfo.R
##' @param Beta_cc transmission rate within camels
##' @param Beta_ch transmission from human to camel
##' @param Beta_hc transmission from camel to human
##' @param Beta_hh transmission rate within humans
##' @param gamma_c removal rate in camels
##' @param gamma_h removal rate in humans
##' @param chi_c per capita destructive sampling rate for camels
##' @param chi_h per capita destructive sampling rate for humans
##' @param Bc gross birth rate for camels
##' @param Bh gross birth rate for humans
##' @param Sc0 initial susceptible fraction of camel population
##' @param Sh0 initial susceptible fraction of human population
##' @param Ic0 initial infected fraction of camel population
##' @param Ih0 initial infected fraction of human population
##' @param Nc camel population size
##' @param Nh human population size
##' @inheritParams sir
##' @return \code{runMERS} and \code{continueMERS} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{MERS}.
##'
NULL

##' @rdname mers
##' @export
runMERS <- function (
  time, t0 = 0,
  Beta_cc = 4, Beta_ch = 0, Beta_hc = 0, Beta_hh = 4, gamma_c = 1, gamma_h = 1, chi_c = 1, chi_h = 0, Bc = 0, Bh = 0, Sc0 = 1, Sh0 = 1, Ic0 = 0.01, Ih0 = 0, Nc = 10000, Nh = 10000
) {
  params <- c(Beta_cc=Beta_cc,Beta_ch=Beta_ch,Beta_hc=Beta_hc,Beta_hh=Beta_hh,gamma_c=gamma_c,gamma_h=gamma_h,chi_c=chi_c,chi_h=chi_h,Bc=Bc,Bh=Bh)
  ivps <- c(Sc0=Sc0,Sh0=Sh0,Ic0=Ic0,Ih0=Ih0,Nc=Nc,Nh=Nh)
  x <- .Call(P_makeMERS,params,ivps,t0)
  .Call(P_runMERS,x,time) |>
    structure(model="MERS",class=c("gpsim","gpgen"))
}

##' @rdname mers
##' @export
continueMERS <- function (
  object, time,
  Beta_cc = NA, Beta_ch = NA, Beta_hc = NA, Beta_hh = NA, gamma_c = NA, gamma_h = NA, chi_c = NA, chi_h = NA, Bc = NA, Bh = NA
) {
  params <- c(
    Beta_cc=Beta_cc,Beta_ch=Beta_ch,Beta_hc=Beta_hc,Beta_hh=Beta_hh,gamma_c=gamma_c,gamma_h=gamma_h,chi_c=chi_c,chi_h=chi_h,Bc=Bc,Bh=Bh
  )
  x <- .Call(P_reviveMERS,object,params)
  .Call(P_runMERS,x,time) |>
    structure(model="MERS",class=c("gpsim","gpgen"))
}