#include <pomp.h>
#include <R_ext/Rdynload.h>

#define Beta		(__p[__parindex[0]])
#define gamma		(__p[__parindex[1]])
#define psi		(__p[__parindex[2]])
#define N		(__p[__parindex[3]])
#define I0		(__p[__parindex[4]])
#define lineages	(__covars[__covindex[0]])
#define code		(__covars[__covindex[1]])
#define I		(__x[__stateindex[0]])
#define ll		(__x[__stateindex[1]])

void leventhal_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)
{
  I = nearbyint(I0);
  ll = 0;
}

void leventhal_stepfn (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)
{

  int ind;
  double tstep = 0, tmax = t + dt;
  ind = nearbyint(code);
  if (ind == 1) {                // coalescent
    ll += (I > 0 && I < N) ? log(Beta*I*(N-I)) : R_NegInf;
    I += 1;
    ll += (I >= lineages && lineages > 1) ? -log(I*(I-1)) : R_NegInf;
  } else if (ind == -1) {        // live sample
    ll += (I > 0) ? log(psi*I) : R_NegInf;
  }

  // Gillespie steps
  tstep = exp_rand()/(Beta*I*(N-I)+gamma*I);
  while (t + tstep < tmax) {
    ll -= psi*I*tstep;
    if (unif_rand() < Beta*(N-I)/(Beta*(N-I)+gamma)) {   // infection
      I += 1;
      ll += (I > lineages) ? log(1-lineages*(lineages-1)/I/(I-1)) : R_NegInf;
    } else {            // recovery
      I -= 1;
    }
    t += tstep;
    tstep = exp_rand()/(Beta*I*(N-I)+gamma*I);
  }

  tstep = tmax - t;
  ll -= psi*I*tstep;
   
}

# define lik  (__lik[0])

void leventhal_dmeas (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)
{
  lik = (give_log) ? ll : exp(ll);
}
