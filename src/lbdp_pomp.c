#include "pomplink.h"
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
  double alpha, disc;
  *penalty = 0;
  assert(n >= lin);
  // birth with saturation 0 or 1
  alpha = lambda*n;
  disc = lin*(lin-1)/n/(n+1);
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // death
  alpha = mu*n;
  if (n > lin) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // sampling
  *penalty += psi*n;
  return event_rate;
}

//! Latent-state initializer (rinit).
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

//! Latent-state process simulator (rprocess).
//!
//! This integrates the filter equation.
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
    ll += log(2*lambda/n);
  } else if (ind == 0) {        // sample with s = 1
    ll += log(psi);
  } else if (ind == -1) {       // sample with s = 0
    ll += log(psi*(n-lin));
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
    event = rcateg(event_rate,rate,2);
    ll -= penalty*tstep;
    switch (event) {
    case 0:                     // birth
      n += 1;
      break;
    case 1:                     // death
      n -= 1;
      break;
    default:                    // #nocov
      assert(0);                // #nocov
      break;                    // #nocov
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

//! Measurement model likelihood (dmeasure).
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
  assert(!ISNAN(ll));
  lik = (give_log) ? ll : exp(ll);
}
