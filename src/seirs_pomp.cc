#include <pomp.h>
#include <R_ext/Rdynload.h>
#include "internal.h"
#include "genealogy.h"

static int nSAMPLE = 0;

static inline int random_choice (double n) {
  return 1+int(floor(R_unif_index(n)));
}

static void check_lineages
(
 const double *lineage, double linE, double linI,
 double t, const char *func
 ) {
  int nE = 0;
  int nI = 0;
  for (int i = 0; i < nSAMPLE; i++) {
    if (lineage[i]==0) nE++;
    else if (lineage[i]==1) nI++;
  }
  if ((linE != nE) || (linI != nI)) {
    Rprintf("%d %d (%g %g): ",nE,nI,linE,linI);		   // #nocov
    for (int i = 0; i < nSAMPLE; i++) {                    // #nocov
      Rprintf("%lg ",lineage[i]);                          // #nocov
    }                                                      // #nocov
    Rprintf("\n");                                         // #nocov
    err("in '%s' at time %lg: bad lineage count!",func,t); // #nocov
  }
}

#define Beta      (__p[__parindex[0]])
#define sigma     (__p[__parindex[1]])
#define gamma     (__p[__parindex[2]])
#define psi       (__p[__parindex[3]])
#define omega     (__p[__parindex[4]])
#define S0        (__p[__parindex[5]])
#define E0        (__p[__parindex[6]])
#define I0        (__p[__parindex[7]])
#define R0        (__p[__parindex[8]])
#define N         (__p[__parindex[9]])
#define S         (__x[__stateindex[0]])
#define E         (__x[__stateindex[1]])
#define I         (__x[__stateindex[2]])
#define R         (__x[__stateindex[3]])
#define ll        (__x[__stateindex[4]])
#define node      (__x[__stateindex[5]])
#define linE      (__x[__stateindex[6]])
#define linI      (__x[__stateindex[7]])
#define LINEAGE   (__x[__stateindex[8]])

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
  // transmission
  event_rate += (*rate = Beta*S*I/N);
  rate++;
  // progression
  event_rate += (*rate = sigma*E);
  rate++;
  // recovery
  if (I > linI) {
    event_rate += (*rate = gamma*I);
  } else {
    *rate = 0;
    *penalty += gamma*I;
  }
  rate++;
  // waning
  event_rate += (*rate = omega*R);
  rate++;
  // sampling
  *penalty += psi*I;
  return event_rate;
}

extern "C" {

  void seirs_rinit
  (
   double *__x,
   const double *__p,
   double t0,
   const int *__stateindex,
   const int *__parindex,
   const int *__covindex,
   const double *__covars
   ){
    double *linvec = &LINEAGE;
    get_userdata_t *gud = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
    genealogy_t G = gud("genealogy");

    nSAMPLE = *INTEGER(gud("nsample"));

    double m = N/(S0+E0+I0+R0);
    S = nearbyint(S0*m);
    E = nearbyint(E0*m);
    I = nearbyint(I0*m);
    R = nearbyint(R0*m);

    linE = 0;
    linI = 0;
    ll = 0;

    double prop_prob = E0/(E0+I0);
    int node_count = -1;
    node_it i = G.cbegin();
    while ((*i)->is_root() && i != G.cend()) {
      node_count++;
      node_t *p = *(i++);
      for (ball_it j = p->cbegin(); j != p->cend(); j++) {
        ball_t *b = *j;
        if (p->green_ball() != b) { // exclude own balls
          if (unif_rand() < prop_prob) {
            linvec[p->lineage(b)] = 0; // lineage is put into E deme
            linE += 1.0;
            ll -= log(prop_prob);
          } else {
            linvec[p->lineage(b)] = 1; // lineage is put into I deme
            linI += 1.0;
            ll -= log(1-prop_prob);
          }
        }
      }
    }
    linE = nearbyint(linE);
    linI = nearbyint(linI);
    check_lineages(linvec,linE,linI,t0,__func__);
    node = node_count;
  }

  void seirs_gill
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
    double rate[4];
    double *linvec = &LINEAGE;

    get_userdata_t *gud = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
    genealogy_t G = gud("genealogy");

    int M = nearbyint(node);
    int node_count = 0;
    node_it k = G.cbegin();
    while (node_count < M && k != G.cend()) {
      node_count++; k++;
    }
    if (k == G.cend())
      err("something is terribly wrong in '%s'!",__func__); // #nocov

    node_t *p = *k;
    node = node+1;

    if (!p->is_root()) {
      ll = 0;
      int parent = p->lineage();
      if (parent < 0 || parent >= nSAMPLE) err("big trouble!"); // #nocov
      if (ISNA(linvec[parent])) {
        err("undefined parent lineage"); // #nocov
      } else if (nearbyint(linvec[parent]) != 1) {
        ll += R_NegInf;
        linvec[parent] = 1;
        linE -= 1; linI += 1;
      }
      if (p->holds(blue)) {     // sample
        ball_t *b = p->ball(blue);
        if (p->holds(green)) {  // saturation = (0,1)
          ball_t *g = p->other(b);
          ll += log(psi);
          linvec[p->lineage(g)] = 1;
          linI += 1;
        } else {                // saturation = (0,0)
          ll += log(psi*(I-linI+1));
        }
        linvec[parent] = R_NaReal;
        linI -= 1;
      } else if (p->holds(green)) { // branch point s = (1,1)
        ll += (S > 0 && I > 0) ? log(Beta*S/N/(E+1)) : R_NegInf;
        S -= 1; E += 1;
        linE += 1;
        ball_t *a = p->last_ball();
        ball_t *b = p->other(a);
        if (a==b || p->lineage(a) == p->lineage(b)) err("oh no.");
        if (p->lineage(a) != p->lineage() &&
            p->lineage(b) != p->lineage()) err("oh dear.");
        if (unif_rand() < 0.5) {
          linvec[p->lineage(a)] = 0;
          linvec[p->lineage(b)] = 1;
        } else {
          linvec[p->lineage(a)] = 1;
          linvec[p->lineage(b)] = 0;
        }
        ll -= log(0.5);
      } else {
        err("inconceivable!! in '%s'!!",__func__);       // #nocov
      }
    }

    // continuous portion:
    // take Gillespie steps to the end of the interval:
    double u;
    int event;
    double event_rate = 0;
    double penalty = 0;

    event_rate = event_rates(__x,__p,t,
                             __stateindex,__parindex,__covindex,
                             __covars,rate,&penalty);
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      ll -= penalty*tstep;
      u = event_rate*unif_rand();
      event = -1;
      while (u > 0) {
        event++;
        u -= rate[event];
      }
      switch (event) {
      case 0:                   // transmission
        if (linI > 0) {
          double p = linI/(I+1)/2;
          u = unif_rand();
          if (u < p) {          // propose an I -> E deme change
            ll += log(2*(I-linI)/I*(I+1)/(E+1));
            int n = random_choice(linI);
            int i = -1;
            while (i < nSAMPLE && n > 0) {
              i++;
              if (!ISNA(linvec[i]) && nearbyint(linvec[i]) == 1) n--;
            }
            if (n != 0 || linvec[i] != 1)
              err("yikes 0! %d %d %d %lg",nSAMPLE,i,n,linvec[i]); // #nocov
            linvec[i] = 0;
            linE += 1; linI -= 1;
          } else if (u < 2*p) { // propose an I -> I deme change
            ll += log(2*(E-linE+1)/(E+1)*(I+1)/I);
          } else {              // propose no deme change
            ll += log((I-linI)/I*(E-linE+1)/(I-linI+1)*(I+1)/(E+1));
          }
        } else {                // no deme change possible
          ll += log((E-linE+1)/(E+1));
        }
        S -= 1; E += 1;
        break;
      case 1:                   // progression
        if (linE > 0) {
          double p = linE/(E+1);
          if (unif_rand() < p) { // propose an E -> I deme change
            ll += log((E+1)/(I+1));
            int n = random_choice(linE);
            int i = -1;
            while (i < nSAMPLE && n > 0) {
              i++;
              if (!ISNA(linvec[i]) && nearbyint(linvec[i]) == 0) n--;
            }
            if (n != 0 || linvec[i] != 0)
              err("yikes 1! %d %d %d %lg",nSAMPLE,i,n,linvec[i]); // #nocov
            linvec[i] = 1;
            linE -= 1; linI += 1;
          } else {              // propose no deme change
            ll += log((I-linI+1)/(E-linE+1)*(E+1)/(I+1));
          }
        } else {                // no deme change possible (i.e. p = 0)
          ll += log((I-linI+1)/(I+1));
        }
        E -= 1; I += 1;
        break;
      case 2:                   // recovery
        I -= 1; R += 1;
        break;
      case 3:                   // waning
        R -= 1; S += 1;
        break;
      default:                                     // #nocov
        err("impossible error in '%s'!",__func__); // #nocov
        break;                                     // #nocov
      }

      linE = nearbyint(linE);
      linI = nearbyint(linI);
      check_lineages(linvec,linE,linI,t,__func__);

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

  void seirs_dmeas
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

    if (R_FINITE(ll) &&
        S >= 0 && R >= 0 &&
        E >= linE && I >= linI &&
        linE >= 0 && linI >= 0) {
      lik = (give_log) ? ll : exp(ll);
    } else {
      lik = (give_log) ? R_NegInf : 0;
    }
  }

}
