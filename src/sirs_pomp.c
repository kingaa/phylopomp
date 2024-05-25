#include "pomplink.h"
#include "internal.h"

#define Beta      (__p[__parindex[0]])
#define gamma     (__p[__parindex[1]])
#define psi       (__p[__parindex[2]])
#define omega     (__p[__parindex[3]])
#define S0        (__p[__parindex[4]])
#define I0        (__p[__parindex[5]])
#define R0        (__p[__parindex[6]])
#define N         (__p[__parindex[7]])
#define ell       (__covars[__covindex[0]])
#define code      (__covars[__covindex[1]])
#define S         (__x[__stateindex[0]])
#define I         (__x[__stateindex[1]])
#define R         (__x[__stateindex[2]])
#define ll        (__x[__stateindex[3]])

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
  double alpha, disc;

  assert(I >= ell);

  // transmission with saturation 0 or 1
  alpha = Beta*S*I/N;
  disc = (I > 0) ? ell*(ell-1)/I/(I+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // recovery
  alpha = gamma*I;
  if (I > ell) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // loss of immunity
  alpha = omega*R;
  event_rate += (*rate = alpha); rate++;
  // sampling
  alpha = psi*I;
  *penalty += alpha;

  return event_rate;
}

//! Latent-state initializer (rinit).
void sirs_rinit
(
 double *__x,
 const double *__p,
 double t,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  double m = N/(S0+I0+R0);
  S = nearbyint(S0*m);
  I = nearbyint(I0*m);
  R = nearbyint(R0*m);
  ll = 0;
}

//! Latent-state process simulator (rprocess).
//!
//! This integrates the filter equation.
void sirs_gill
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
  // deal with event at start of interval:
  ll = 0;
  int ind = nearbyint(code);
  if (ind == 1) {                // birth, s = 2
    ll += (I > 0) ? log(Beta*S*I/N) : R_NegInf;
    S -= 1; I += 1;
    ll += (I >= ell && ell > 1) ? -log(I*(I-1)/2) : R_NegInf;
  } else if (ind == 0) {         // sample, s = 1
    ll += (I >= ell) ? log(psi) : R_NegInf;
  } else if (ind == -1) {        // sample, s = 0
    ll += (I > 0) ? log(psi*I) : R_NegInf;
    ll += (I > ell) ? log(1-ell/I) : R_NegInf;
  }
  assert(I>=ell);

  // take Gillespie steps to the end of the interval:
  int event;
  double penalty = 0;
  double rate[3];

  double event_rate = event_rates(__x,__p,t,
                                  __stateindex,__parindex,__covindex,
                                  __covars,rate,&penalty);
  tstep = exp_rand()/event_rate;

  while (t + tstep < tmax) {
    event = rcateg(event_rate,rate,3);
    ll -= penalty*tstep;
    switch (event) {
    case 0:                     // transmission
      S -= 1; I += 1;
      break;
    case 1:                     // recovery
      I -= 1; R += 1;
      break;
    case 2:                     // loss of immunity
      R -= 1; S += 1;
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
void sirs_dmeas
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
 ){
  assert(!ISNAN(ll));
  lik = (give_log) ? ll : exp(ll);
}
