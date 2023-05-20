#include <pomp.h>
#include <R_ext/Rdynload.h>
#include "internal.h"

#define Beta      (__p[__parindex[0]])
#define gamma     (__p[__parindex[1]])
#define psi       (__p[__parindex[2]])
#define Delta     (__p[__parindex[3]])
#define S0        (__p[__parindex[4]])
#define I0        (__p[__parindex[5]])
#define R0        (__p[__parindex[6]])
#define N         (__p[__parindex[7]])
#define lineages  (__covars[__covindex[0]])
#define code      (__covars[__covindex[1]])
#define S         (__x[__stateindex[0]])
#define I         (__x[__stateindex[1]])
#define R         (__x[__stateindex[2]])
#define ll        (__x[__stateindex[3]])

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
  ll = 0.0;
}

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
  int ind = nearbyint(code);
  if (ind == 1) {                // coalescent
    ll += (I > 0) ? log(Beta*S*I/N) : R_NegInf;
    S -= 1; I += 1;
    ll += (I >= lineages && lineages > 1) ? -log(I*(I-1)/2) : R_NegInf;
  } else if (ind == 0) {         // dead sample
    ll += (I >= lineages) ? log(psi) : R_NegInf;
  } else if (ind == -1) {        // live sample
    ll += (I > 0) ? log(psi*I) : R_NegInf;
    ll += (I > lineages) ? log(1-lineages/I) : R_NegInf;
  }
  // take Gillespie steps to the end of the interval:
  double event_rate = 0;
  double cfact, u;
  double cutoff[3];
  int event;
  cfact = (lineages > 1) ? lineages*(lineages-1)/I/(I+1) : 0;
  cfact = (cfact < 1) ? cfact : 1;
  cutoff[0] = (1-cfact)*Beta*S*I/N; // transmission
  cutoff[1] = gamma*I;		    // recovery
  cutoff[2] = Delta*R;		    // loss of immunity
  event_rate = cutoff[0] + cutoff[1] + cutoff[2];
  tstep = exp_rand()/event_rate;
  while (t + tstep < tmax) {
    ll -= psi*I*tstep;
    ll -= cfact*Beta*S*I/N*tstep;
    u = event_rate*unif_rand();
    event = -1;
    while (u > 0) {
      event++;
      u -= cutoff[event];
    }
    switch (event) {
    case 0:                     // transmission
      S -= 1; I += 1;
      cfact = (lineages > 1) ? lineages*(lineages-1)/I/(I+1) : 0;
      cfact = (cfact < 1) ? cfact : 1;
      cutoff[0] = (1-cfact)*Beta*S*I/N;
      cutoff[1] = gamma*I;
      break;
    case 1:                     // recovery
      I -= 1; R += 1;
      cfact = (lineages > 1) ? lineages*(lineages-1)/I/(I+1) : 0;
      cfact = (cfact < 1) ? cfact : 1;
      cutoff[0] = (1-cfact)*Beta*S*I/N;
      cutoff[1] = gamma*I;
      cutoff[2] = Delta*R;
      break;
    case 2:                     // loss of immunity
      R -= 1; S += 1;
      cutoff[0] = (1-cfact)*Beta*S*I/N;
      cutoff[2] = Delta*R;
      break;
    default:
      error("impossible error in 'sirs_pomp'!");
      break;
    }
    event_rate = cutoff[0] + cutoff[1] + cutoff[2];
    t += tstep;
    tstep = exp_rand()/event_rate;
  }
  tstep = tmax - t;
  ll -= psi*I*tstep;
  ll -= cfact*Beta*S*I/N*tstep;
}

# define lik  (__lik[0])

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
  if (S >= 0 && I >= 0 && R >= 0 && I >= lineages) {
    lik = (give_log) ? ll : exp(ll);
  } else {
    lik = (give_log) ? R_NegInf : 0;
  }
}

#undef lik
#undef Delta
#undef Beta
#undef gamma
#undef psi
#undef S0
#undef I0
#undef R0
#undef N
#undef lineages
#undef code
#undef S
#undef I
#undef R
#undef ll
