##' Within-host viral dynamic model with ALT
##'
##' Within-host viral dynamics of four types of cells and the released ALT.
##'
##' @name timva
##' @family Genealogy processes
##' @aliases TIMVA
##' @param lambda target cell replenishment rate
##' @param d target cell death rate
##' @param Beta transmission rate
##' @param delta infected cell death rate
##' @param p free virus particle production rate per infected cell
##' @param a immune cell activation rate per infected cell
##' @param kappa infected cell killing rate
##' @param c free virus particle clearance rate
##' @param mu immune cell death rate
##' @param omega proportion of ALT release per dead infected cell
##' @param s ALT replenishment rate
##' @param sigma ALT decay rate
##' @param psi free virus particle sampling rate
##' @param f probability of getting sampled
##' @param dt time step for state recording
##' @param T0 initial size of target cells
##' @param I0 initial size of infected cells
##' @param M0 initial size of immune cells
##' @param V0 initial size of free virus particles
##' @param A0 initial value of ALT
##' @inheritParams sir
##' @return \code{runTIMVA} and \code{continueTIMVA} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{TIMVA}.
##'
NULL

##' @rdname timva
##' @export
runTIMVA <- function (
  time, t0 = 0,
  lambda = 10, d = 0, Beta = 1e-4, delta = 0.01, p = 10, a = 0.1, kappa = 0.005, c = 3, mu = 0.05, omega = 0.1, s = 3, sigma = 0.3, psi = 0.1, f = 0.1, dt = 0.1, T0 = 1000, I0 = 0, M0 = 0, V0 = 1, A0 = 0
) {
  params <- c(lambda=lambda,d=d,Beta=Beta,delta=delta,p=p,a=a,kappa=kappa,c=c,mu=mu,omega=omega,s=s,sigma=sigma,psi=psi,f=f,dt=dt)
  ivps <- c(T0=T0,I0=I0,M0=M0,V0=V0,A0=A0)
  x <- .Call(P_makeTIMVA,params,ivps,t0)
  .Call(P_runTIMVA,x,time) |>
    structure(model="TIMVA",class=c("gpsim","gpgen"))
}

##' @rdname timva
##' @export
continueTIMVA <- function (
  object, time,
  lambda = NA, d = NA, Beta = NA, delta = NA, p = NA, a = NA, kappa = NA, c = NA, mu = NA, omega = NA, s = NA, sigma = NA, psi = NA, f = NA, dt = NA
) {
  params <- c(
    lambda=lambda,d=d,Beta=Beta,delta=delta,p=p,a=a,kappa=kappa,c=c,mu=mu,omega=omega,s=s,sigma=sigma,psi=psi,f=f,dt=dt
  )
  x <- .Call(P_reviveTIMVA,object,params)
  .Call(P_runTIMVA,x,time) |>
    structure(model="TIMVA",class=c("gpsim","gpgen"))
}