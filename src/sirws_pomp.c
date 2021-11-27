#include <pomp.h>
#include <R_ext/Rdynload.h>

#define Beta	  (__p[__parindex[0]])
#define gamma	  (__p[__parindex[1]])
#define psi       (__p[__parindex[2]])
#define S0        (__p[__parindex[3]])
#define I0        (__p[__parindex[4]])
#define R0        (__p[__parindex[5]])
#define N         (__p[__parindex[6]])
#define lineages  (__covars[__covindex[0]])
#define code	  (__covars[__covindex[1]])
#define S         (__x[__stateindex[0]])
#define I         (__x[__stateindex[1]])
#define R         (__x[__stateindex[2]])
#define ll        (__x[__stateindex[3]])

void sir_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)
{ 
    double m = N/(S0+I0+R0);
    S = nearbyint(S0*m);
    I = nearbyint(I0*m);
    R = nearbyint(R0*m);
    ll = 0.0;
}

void sir_gill (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)
{

  int ind = nearbyint(code);
  double tstep = 0.0, tmax = t + dt;

  // params
  double lambda = Beta*S/N;
  double mu = gamma;
  if (ind == 1) {                // coalescent
    ll += (I > 0) ? log(lambda*I) : R_NegInf;
    I += 1;
    S -= 1;
    ll += (I >= lineages && lineages > 1) ? -log(I*(I-1)/2) : R_NegInf;
  } else if (ind == 0) {         // dead sample
    ll += (I >= lineages) ? log(psi) : R_NegInf;
  } else if (ind == -1) {        // live sample
    ll += (I > 0) ? log(psi*I) : R_NegInf;
    ll += (I > lineages) ? log(1-lineages/I) : R_NegInf;
  }
  
  // Gillespie steps
  tstep = exp_rand()/(lambda+mu)/I;
  while (t + tstep < tmax) {
    ll -= psi*I*tstep;
    if (unif_rand() < lambda/(lambda+mu)) {   // birth
      I += 1;
      S -= 1;
      ll += (I > lineages) ? log(1-lineages*(lineages-1)/I/(I-1)) : R_NegInf;
    } else {            // death
      I -= 1;
      R += 1;
    }
    t += tstep;
    lambda = Beta*S/N;
    tstep = exp_rand()/(lambda+mu)/I;
  }

  tstep = tmax - t;
  ll -= psi*I*tstep;
   
}


void sir_euler (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)
{
    int ind = nearbyint(code);
    // params
    double lambda = Beta*S/N;
    if (ll < 1e-300 && ll > -1e-300) {
      if (ind == 1) {                // coalescent
        if (S > 0) {
          ll += (I > 0) ? log(lambda*I) : R_NegInf;
          I += 1;
          S -= 1;
          ll += (I >= lineages && lineages > 1) ? -log(I*(I-1)/2) : R_NegInf;
        } else {
          ll += R_NegInf;
        }
      } else if (ind == 0) {         // dead sample
        ll += (I >= lineages) ? log(psi) : R_NegInf;
      } else if (ind == -1) {        // live sample
        ll += (I > 0) ? log(psi*I) : R_NegInf;
        ll += (I > lineages) ? log(1-lineages/I) : R_NegInf;
      }
    }

    // euler steps
    int nrate = 2;
    double rate[nrate], trans[nrate];

    rate[0] = Beta*I/N;
    rate[1] = gamma;

    // method 1:
    reulermultinom(1, S, &rate[0], dt, &trans[0]);
    reulermultinom(1, I, &rate[1], dt, &trans[1]);
    
    S -= trans[0];
    I += trans[0] - trans[1];
    R += trans[1];
    
    // method 2:
    // trans[0] = rnbinom_mu(n, exp(rate[0]*dt));
    // reulermultinom(1, trans[0], &rate[1], dt, &n);

    // method 3:
    // reulermulitnom(1, n, &rate[1], dt, &trans[1]);
    // n = rbinom_mu(trans[1], exp(rate[0]*dt));
    
    // assume all births and deaths occur at the end of the interval
    if (trans[0] > 0)
      ll += (I > lineages) ? nearbyint(trans[0])*log(1-(lineages-1)*lineages/(I-1)/(I)) : R_NegInf;
    
    ll -= psi*I*dt;
   
}

# define lik  (__lik[0])

void sir_dmeas (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)
{
  lik = (give_log) ? ll : exp(ll);
}

#undef lik
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
