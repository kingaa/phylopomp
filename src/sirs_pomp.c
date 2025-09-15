#include "pomplink.h"
#include "internal.h"

static const int nrate = 3;

#define Beta      (__p[__parindex[0]])
#define gamma     (__p[__parindex[1]])
#define psi       (__p[__parindex[2]])
#define omega     (__p[__parindex[3]])
#define S0        (__p[__parindex[4]])
#define I0        (__p[__parindex[5]])
#define R0        (__p[__parindex[6]])
#define N         (__p[__parindex[7]])
#define S         (__x[__stateindex[0]])
#define I         (__x[__stateindex[1]])
#define R         (__x[__stateindex[2]])
#define ll        (__x[__stateindex[3]])
#define ellI      (__x[__stateindex[4]])
#define node      (__x[__stateindex[5]])

#define EVENT_RATES                                     \
  event_rates(__x,__p,t,                                \
              __stateindex,__parindex,__covindex,       \
              __covars,rate,&penalty)                   \

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
  assert(I >= ellI);
  assert(ellI >= 0);
  assert(S >= 0);
  // 0: transmission with saturation 0 or 1
  alpha = Beta*S*I/N;
  disc = (I > 0) ? ellI*(ellI-1)/I/(I+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // 1: recovery
  alpha = gamma*I;
  if (I > ellI) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // 2: loss of immunity
  alpha = omega*R;
  event_rate += (*rate = alpha); rate++;
  // sampling
  alpha = psi*I;
  *penalty += alpha;
  assert(R_FINITE(event_rate));
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
  ellI = 0;
  node = 0;
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
  const int *nodetype = get_userdata_int("nodetype");
  const int *sat = get_userdata_int("saturation");

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
    ellI += 1;
    break;
  case 1:                       // sample
    ll = 0;
    assert(I >= ellI);
    assert(ellI >= 0);
    if (sat[parent] == 1) {
      ll += log(psi);
    } else if (sat[parent] == 0) {
      ellI -= 1;
      ll += log(psi*(I-ellI));
    } else {
      assert(0);                // #nocov
      ll += R_NegInf;           // #nocov
    }
    break;
  case 2:                       // branch point s=(1,1)
    ll = 0;
    assert(S >= 0);
    assert(I >= 0);
    assert(ellI > 0);
    assert(sat[parent]==2);
    ll += (I > 0 && I >= ellI) ? log(Beta*S*I/N) : R_NegInf;
    S -= 1; I += 1;
    ellI += 1;
    ll -= log(I*(I-1)/2);
    S = (S > 0) ? S : 0;
    break;
  }

  if (tmax > t) {

    // take Gillespie steps to the end of the interval:
    int event;
    double penalty = 0;
    double rate[nrate];

    double event_rate = EVENT_RATES;
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      event = rcateg(event_rate,rate,nrate);
      assert(event>=0 && event<nrate);
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
      event_rate = EVENT_RATES;
      tstep = exp_rand()/event_rate;
    }
    tstep = tmax - t;
    ll -= penalty*tstep;
  }
  node += 1;
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
