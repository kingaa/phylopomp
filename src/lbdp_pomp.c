#include <pomp.h>
#include <R_ext/Rdynload.h>

#define lambda          (__p[__parindex[0]])
#define mu              (__p[__parindex[1]])
#define n0              (__p[__parindex[2]])
#define psi             (__p[__parindex[3]])
#define lineages        (__covars[__covindex[0]])
#define code            (__covars[__covindex[1]])
#define n               (__x[__stateindex[0]])
#define ll              (__x[__stateindex[1]])

void lbdp_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)
{
  n = nearbyint(n0);
  ll = 0;
}

void lbdp_gill (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)
{

  int ind;
  double tstep = 0, tmax = t + dt;
  ind = nearbyint(code);
  if (ind == 1) {                // coalescent
    ll += (n > 0) ? log(lambda*n) : R_NegInf;
    n += 1;
    ll += (n >= lineages && lineages > 1) ? -log(n*(n-1)/2) : R_NegInf;
  } else if (ind == 0) {         // dead sample
    ll += (n >= lineages) ? log(psi) : R_NegInf;
  } else if (ind == -1) {        // live sample
    ll += (n > lineages) ? log(psi*(n-lineages)) : R_NegInf;
  }

  // Gillespie steps
  tstep = (n > 0) ? exp_rand()/(lambda+mu)/n : R_PosInf;
  while (R_FINITE(tstep) && t + tstep < tmax) {
    ll -= psi*n*tstep;
    if (unif_rand() < lambda/(lambda+mu)) {
      n += 1;			// birth
      ll += (n > lineages) ? log(1-lineages*(lineages-1)/n/(n-1)) : R_NegInf;
    } else {
      n -= 1;			// death
    }
    t += tstep;
    tstep = (n > 0) ? exp_rand()/(lambda+mu)/n : R_PosInf;
  }
  tstep = tmax - t;
  ll -= psi*n*tstep;
}

# define lik  (__lik[0])

void lbdp_dmeas (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)
{
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
