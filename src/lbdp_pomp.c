#include "pomplink.h"
#include "internal.h"

#define lambda  (__p[__parindex[0]])
#define mu      (__p[__parindex[1]])
#define psi     (__p[__parindex[2]])
#define n0      (__p[__parindex[3]])
#define n       (__x[__stateindex[0]])
#define ll      (__x[__stateindex[1]])
#define ell     (__x[__stateindex[2]])
#define node    (__x[__stateindex[3]])

#define EVENT_RATES                                     \
  event_rates(__x,__p,t,                                \
              __stateindex,__parindex,rate,&penalty)    \

static double event_rates
(
 double *__x,
 const double *__p,
 double t,
 const int *__stateindex,
 const int *__parindex,
 double *rate,
 double *penalty
 ) {
  double event_rate = 0;
  double alpha, disc;
  *penalty = 0;
  assert(n >= ell);
  assert(ell >= 0);
  // birth with saturation 0 or 1
  alpha = lambda*n;
  disc = (n > 0) ? ell*(ell-1)/n/(n+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // death
  alpha = mu*n;
  if (n > ell) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // sampling
  *penalty += psi*n;
  assert(!ISNAN(event_rate));
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
  ell = 0;
  node = 0;
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
  double tstep = 0.0, tmax = t + dt;
  const int *nodetype = get_userdata_int("nodetype");
  const int *saturation = get_userdata_int("saturation");
  int parent = (int) nearbyint(node);

#ifndef NDEBUG
  int nnode = *get_userdata_int("nnode");
  assert(parent>=0);
  assert(parent<=nnode);
#endif

  // singular portion of filter equation
  switch (nodetype[parent]) {
  default:                      // non-genealogical event
    break;
  case 0:                       // root
    ll = 0;
    ell += 1;
    break;
  case 1:                       // sample
    ll = 0;
    assert(n >= ell);
    assert(ell >= 0);
    if (saturation[parent] == 1) { // s=1
      ll += log(psi);
    } else {                    // s=0
      ell -= 1;
      ll += log(psi*(n-ell));
    }
    break;
  case 2:                       // branch point s=2
    ll = 0;
    assert(n >= 0);
    assert(ell > 0);
    assert(saturation[parent]==2);
    n += 1;
    ll += log(2*lambda/n);
    ell += 1;
    break;
  }

  // Gillespie steps:
  int event;
  double penalty = 0;
  double rate[2];

  double event_rate = EVENT_RATES;
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
    event_rate = EVENT_RATES;
    tstep = exp_rand()/event_rate;
  }
  tstep = tmax - t;
  ll -= penalty*tstep;
  node += 1;
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
