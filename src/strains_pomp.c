#include "pomplink.h"
#include "internal.h"

static const int nrate = 6;

#define STRAIN1 0
#define STRAIN2 1
#define STRAIN3 2

#define Beta1     (__p[__parindex[0]])
#define Beta2     (__p[__parindex[1]])
#define Beta3     (__p[__parindex[2]])
#define gamma     (__p[__parindex[3]])
#define psi1      (__p[__parindex[4]])
#define psi2      (__p[__parindex[5]])
#define psi3      (__p[__parindex[6]])
#define S0        (__p[__parindex[7]])
#define I1_0      (__p[__parindex[8]])
#define I2_0      (__p[__parindex[9]])
#define I3_0      (__p[__parindex[10]])
#define R0        (__p[__parindex[11]])
#define N         (__x[__parindex[12]])
#define S         (__x[__stateindex[0]])
#define I1        (__x[__stateindex[1]])
#define I2        (__x[__stateindex[2]])
#define I3        (__x[__stateindex[3]])
#define R         (__x[__stateindex[4]])
#define ll        (__x[__stateindex[5]])
#define ellI1     (__x[__stateindex[6]])
#define ellI2     (__x[__stateindex[7]])
#define ellI3     (__x[__stateindex[8]])
#define node      (__x[__stateindex[9]])

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
  assert(I1 >= ellI1);
  assert(ellI1 >= 0);
  assert(S >= 0);
  // 0: strain-1 transmission with saturation 0 or 1
  alpha = Beta1*S*I1/N;
  disc = (I1 > 0) ? ellI1*(ellI1-1)/I1/(I1+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // 1: strain 1 recovery
  alpha = gamma*I1;
  if (I1 > ellI1) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // strain 1 sampling
  alpha = psi1*I1;
  *penalty += alpha;
  // 2: strain-2 transmission with saturation 0 or 1
  alpha = Beta2*S*I2/N;
  disc = (I2 > 0) ? ellI2*(ellI2-1)/I2/(I2+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // 3: strain 2 recovery
  alpha = gamma*I2;
  if (I2 > ellI2) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // strain 2 sampling
  alpha = psi2*I2;
  *penalty += alpha;
  // 4: strain-3 transmission with saturation 0 or 1
  alpha = Beta3*S*I3/N;
  disc = (I3 > 0) ? ellI3*(ellI3-1)/I3/(I3+1) : 1;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *penalty += alpha*disc;
  // 5: strain 3 recovery
  alpha = gamma*I3;
  if (I3 > ellI3) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  // strain 3 sampling
  alpha = psi3*I3;
  *penalty += alpha;
  assert(R_FINITE(event_rate));
  return event_rate;
}

//! Latent-state initializer (rinit).
void strains_rinit
(
 double *__x,
 const double *__p,
 double t,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  double m = N/(S0+I1_0+I2_0+I3_0+R0);
  S = nearbyint(S0*m);
  I1 = nearbyint(I1_0*m);
  I2 = nearbyint(I2_0*m);
  I3 = nearbyint(I3_0*m);
  R = nearbyint(R0*m);
  ll = 0;
  ellI1 = 0;
  ellI2 = 0;
  ellI3 = 0;
  node = 0;
}

//! Latent-state process simulator (rprocess).
//!
//! This integrates the filter equation.
void strains_gill
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
  const int *deme = get_userdata_int("deme");

  int parent = (int) nearbyint(node);

#ifndef NDEBUG
  int nnode = *get_userdata_int("nnode");
  assert(parent>=0);
  assert(parent<=nnode);
#endif

  ll = 0;

  // singular portion of filter equation
  switch (nodetype[parent]) {
  default:                      // non-genealogical event
    break;
  case 0:                       // root
    switch (deme[parent]) {
    case STRAIN1:
      ellI1 += 1; break;
    case STRAIN2:
      ellI2 += 1; break;
    case STRAIN3:
      ellI3 += 1; break;
    default:
      assert(0); break;
    }
  case 1:                       // sample
    switch (deme[parent]) {
    case STRAIN1:
      assert(I1 >= ellI1);
      assert(ellI1 >= 0);
      if (sat[parent] == 1) {
        ll += log(psi1);
      } else if (sat[parent] == 0) {
        ellI1 -= 1;
        ll += log(psi1*(I1-ellI1));
      } else {
        assert(0);                // #nocov
        ll += R_NegInf;           // #nocov
      }
      break;
    case STRAIN2:
      assert(I2 >= ellI2);
      assert(ellI2 >= 0);
      if (sat[parent] == 1) {
        ll += log(psi2);
      } else if (sat[parent] == 0) {
        ellI2 -= 1;
        ll += log(psi2*(I2-ellI2));
      } else {
        assert(0);                // #nocov
        ll += R_NegInf;           // #nocov
      }
      break;
    case STRAIN3:
      assert(I3 >= ellI3);
      assert(ellI3 >= 0);
      if (sat[parent] == 1) {
        ll += log(psi3);
      } else if (sat[parent] == 0) {
        ellI3 -= 1;
        ll += log(psi3*(I3-ellI3));
      } else {
        assert(0);                // #nocov
        ll += R_NegInf;           // #nocov
      }
      break;
    default:
      assert(0);
      break;
    }
    break;
  case 2:                       // branch point s=(1,1)
    assert(S >= 0);
    switch (deme[parent]) {
    case STRAIN1:
      assert(I1 >= 0);
      assert(ellI1 > 0);
      assert(sat[parent]==2);
      ll += (I1 > 0 && I1 >= ellI1) ? log(Beta1*S*I1/N) : R_NegInf;
      S -= 1; I1 += 1;
      ellI1 += 1;
      ll -= log(I1*(I1-1)/2);
      break;
    case STRAIN2:
      assert(I2 >= 0);
      assert(ellI2 > 0);
      assert(sat[parent]==2);
      ll += (I2 > 0 && I2 >= ellI2) ? log(Beta2*S*I2/N) : R_NegInf;
      S -= 1; I2 += 1;
      ellI2 += 1;
      ll -= log(I2*(I2-1)/2);
      break;
    case STRAIN3:
      assert(I3 >= 0);
      assert(ellI3 > 0);
      assert(sat[parent]==2);
      ll += (I3 > 0 && I3 >= ellI3) ? log(Beta3*S*I3/N) : R_NegInf;
      S -= 1; I3 += 1;
      ellI3 += 1;
      ll -= log(I3*(I3-1)/2);
      break;
    default:
      assert(0); break;
    }
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
      case 0:                     // strain-1 transmission
        S -= 1; I1 += 1;
        break;
      case 1:                     // strain-1 recovery
        I1 -= 1; R += 1;
        break;
      case 2:                     // strain-2 transmission
        S -= 1; I2 += 1;
        break;
      case 3:                     // strain-2 recovery
        I2 -= 1; R += 1;
        break;
      case 4:                     // strain-3 transmission
        S -= 1; I3 += 1;
        break;
      case 5:                     // strain-3 recovery
        I3 -= 1; R += 1;
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
void strains_dmeas
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
