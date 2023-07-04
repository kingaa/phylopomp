#include <pomp.h>
#include <R_ext/Rdynload.h>
#include "internal.h"

#define lambda  (__p[__parindex[0]])
#define mu      (__p[__parindex[1]])
#define psi     (__p[__parindex[2]])
#define n0      (__p[__parindex[3]])
#define lin     (__covars[__covindex[0]])
#define code    (__covars[__covindex[1]])
#define n       (__x[__stateindex[0]])
#define ll      (__x[__stateindex[1]])

static double event_rates
(
 double *__x,
 const double *__p,
 double t,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars,
 double *rate,
 double *penalty
 ) {
  double event_rate = 0;
  *penalty = 0;
  // birth with saturation 0 or 1
  *rate = lambda*(n-lin*(lin-1)/(n+1));
  *rate = (*rate > 0) ? *rate : 0;
  event_rate += *rate;
  *penalty += lambda*n-(*rate);
  rate++;
  // death
  event_rate += (*rate = mu*n);
  rate++;
  // sampling
  *penalty += psi*n;
  return event_rate;
}

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
  ll = 0;
  if (ind == 1) {               // branch point with s = 2
    n += 1;
    ll += (n >= lin) ? log(2*lambda/n) : R_NegInf;
  } else if (ind == 0) {        // sample with s = 1
    ll += (n >= lin) ? log(psi) : R_NegInf;
  } else if (ind == -1) {       // sample with s = 0
    ll += (n >= lin) ? log(psi*(n-lin)) : R_NegInf;
  }

  // Gillespie steps:
  int event;
  double penalty = 0;
  double rate[2];

  double event_rate = event_rates(__x,__p,t,
                                  __stateindex,__parindex,__covindex,
                                  __covars,rate,&penalty);
  tstep = exp_rand()/event_rate;

  while (t + tstep < tmax) {
    ll -= penalty*tstep;
    event = rcateg(event_rate,rate,2);
    switch (event) {
    case 0:                     // birth
      n += 1;
      break;
    case 1:                     // death
      n -= 1;
      break;
    default:                                     // #nocov
      err("impossible error in '%s'!",__func__); // #nocov
      break;                                     // #nocov
    }
    t += tstep;
    event_rate = event_rates(__x,__p,t,
                             __stateindex,__parindex,__covindex,
                             __covars,rate,&penalty);
    tstep = exp_rand()/event_rate;
  }
  tstep = tmax - t;
  ll -= penalty*tstep;
}

# define lik  (__lik[0])

void lbdp_dmeas
(
 double *__lik,
 const double *__y,
 const double *__x,
 const double *__p,
 int give_log,
 const int *__obsindex,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars,
 double t
 ) {
  lik = (give_log) ? ll : exp(ll);
}

#undef lik
#undef lambda
#undef mu
#undef n0
#undef psi
#undef lin
#undef code
#undef n
#undef ll
