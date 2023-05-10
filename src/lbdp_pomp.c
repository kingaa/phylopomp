#include <pomp.h>
#include <R_ext/Rdynload.h>
#include "internal.h"

#define lambda          (__p[__parindex[0]])
#define mu              (__p[__parindex[1]])
#define psi             (__p[__parindex[2]])
#define n0              (__p[__parindex[3]])
#define lineages        (__covars[__covindex[0]])
#define code            (__covars[__covindex[1]])
#define n               (__x[__stateindex[0]])
#define ll              (__x[__stateindex[1]])

void lbdp_rinit
(
 double *__x,
 const double *__p,
 double t,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  n = nearbyint(n0);
  ll = 0;
}

void lbdp_gill
(
 double *__x,
 const double *__p,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars,
 double t,
 double dt
 ){
  double tstep = 0, tmax = t + dt;
  int ind = nearbyint(code);
  if (ind == 1) {                // coalescent
    ll += (n > 0) ? log(lambda*n) : R_NegInf;
    n += 1;
    ll += (n >= lineages && lineages > 1) ? -log(n*(n-1)/2) : R_NegInf;
  } else if (ind == 0) {         // dead sample
    ll += (n >= lineages) ? log(psi) : R_NegInf;
  } else if (ind == -1) {        // live sample
    ll += (n > lineages) ? log(psi*(n-lineages)) : R_NegInf;
  }
  // Gillespie steps:
  double event_rate = 0;
  double cfact, u;
  double cutoff[2];
  int event;
  cfact = (lineages > 1) ? lineages*(lineages-1)/n/(n+1) : 0;
  cfact = (cfact < 1) ? cfact : 1;
  cutoff[0] = (1-cfact)*lambda*n;
  cutoff[1] = mu*n;
  event_rate = cutoff[0] + cutoff[1];
  tstep = exp_rand()/event_rate;
  while (t + tstep < tmax) {
    ll -= psi*n*tstep;
    ll -= cfact*lambda*n*tstep;
    u = event_rate*unif_rand();
    event = -1;
    while (u > 0) {
      event++;
      u -= cutoff[event];
    }
    switch (event) {
    case 0:			// birth
      n += 1;
      break;
    case 1:			// death
      n -= 1;
      break;
    default:
      error("impossible error in 'lbdp_pomp'!");
      break;
    }
    cfact = (lineages > 1) ? lineages*(lineages-1)/n/(n+1) : 0;
    cfact = (cfact < 1) ? cfact : 1;
    cutoff[0] = (1-cfact)*lambda*n;
    cutoff[1] = mu*n;
    event_rate = cutoff[0] + cutoff[1];
    t += tstep;
    tstep = exp_rand()/event_rate;
  }
  tstep = tmax - t;
  ll -= psi*n*tstep;
  ll -= cfact*lambda*n*tstep;
}

# define lik  (__lik[0])

void lbdp_dmeas
(
 double *__lik,
 const double *__y,
 double *__x,
 const double *__p,
 int give_log,
 const int *__obsindex,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars,
 double t
 ){
  lik = (give_log) ? ll : exp(ll);
}

#undef lik
#undef lambda
#undef mu
#undef n0
#undef psi
#undef lineages
#undef code
#undef n
#undef ll
