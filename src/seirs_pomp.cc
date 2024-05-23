#include "genealogy.h"
#include "internal.h"

#include <pomp.h>
#include <R_ext/Rdynload.h>

static int nSAMPLE = 0;

static inline int random_choice (double n) {
  return 1+int(floor(R_unif_index(n)));
}

static void change_color (double *y, int n, int from, int to) {
  int i = -1;
  while (i < nSAMPLE && n > 0) {
    i++;
    if (!ISNA(y[i]) && nearbyint(y[i]) == from) n--;
  }
  if (i >= nSAMPLE || n != 0 || nearbyint(y[i]) != from)
    err("error in '%s': %d %d %d %lg",__func__,nSAMPLE,i,n,y[i]); // #nocov
  y[i] = to;
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
#define ellE      (__x[__stateindex[6]])
#define ellI      (__x[__stateindex[7]])
#define COLOR     (__x[__stateindex[8]])

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
 double *logpi,
 double *penalty
 ) {
  double event_rate = 0;
  double alpha, pi;
  *penalty = 0;
  // 0: transmission, s=(0,0)
  alpha = Beta*S*I/N;
  pi = 1-ellI/(I+1);
  *rate = alpha*pi;
  *logpi = log(pi);
  event_rate += *rate;
  rate++; logpi++;
  // 1: transmission, s=(0,1)
  pi = (1-pi)/2;
  *rate = alpha*pi;
  *logpi = log(pi)-log(ellI);
  event_rate += *rate;
  rate++; logpi++;
  // 2: transmission, s=(1,0)
  *rate = alpha*pi;
  *logpi = log(pi)-log(ellI);
  event_rate += *rate;
  rate++; logpi++;
  // 3: progression, s=(0,0)
  // 4: progression, s=(0,1)
  alpha = sigma*E;
  pi = ellE/(E+1);
  if (E > ellE) {
    *rate = alpha*(1-pi);
    *logpi = log(1-pi);
    event_rate += *rate;
    rate++; logpi++;
    *rate = alpha*pi;
    *logpi = log(pi)-log(ellE);
    event_rate += *rate;
    rate++; logpi++;
  } else {
    *rate = 0;
    *logpi = 0;
    rate++; logpi++;
    *rate = 0;
    *logpi = 0;
    rate++; logpi++;
    *penalty += alpha;
  }
  // 5: recovery
  alpha = gamma*I;
  if (I > ellI) {
    *rate = alpha;
    *logpi = 0;
    event_rate += *rate;
    rate++; logpi++;
  } else {
    *rate = 0;
    *logpi = 0;
    *penalty += alpha;
    rate++; logpi++;
  }
  // 6: waning
  *rate = omega*R;
  *logpi = 0;
  event_rate += *rate;
  rate++; logpi++;
  // 7: sampling (Q = 0)
  *penalty += psi*I;
  return event_rate;
}

extern "C" {

  //! Latent-state initializer (rinit component).
  //!
  //! The state variables include S, E, I, R
  //! plus 'ellE' and 'ellI' (numbers of E- and I-deme lineages),
  //! the accumulated weight ('ll'),
  //! and the coloring of each lineage ('COLOR').
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
    double *color = &COLOR;
    get_userdata_t *gud = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
    genealogy_t G = gud("genealogy");

    nSAMPLE = *INTEGER(gud("nsample"));

    double m = N/(S0+E0+I0+R0);
    S = nearbyint(S0*m);
    E = nearbyint(E0*m);
    I = nearbyint(I0*m);
    R = nearbyint(R0*m);

    ellE = 0;
    ellI = 0;
    ll = 0;

    // color lineages by sampling without replacement
    double nE = E;
    double nI = I;
    int node_count = -1;
    node_it i = G.cbegin();
    while ((*i)->is_root() && i != G.cend()) {
      node_count++;
      node_t *p = *(i++);
      for (ball_it j = p->cbegin(); j != p->cend(); j++) {
        ball_t *b = *j;
        if (p->green_ball() != b) { // exclude own balls
          int pick = random_choice(nE+nI);
          if (pick <= nearbyint(nE)) {
            color[p->lineage(b)] = 0; // lineage is put into E deme
            ellE += 1; nE -= 1;
          } else {
            color[p->lineage(b)] = 1; // lineage is put into I deme
            ellI += 1; nI -= 1;
          }
        }
      }
    }
    ellE = nearbyint(ellE);
    ellI = nearbyint(ellI);
    node = node_count;
  }

  //! Simulator for the latent-state process (rprocess).
  //!
  //! This is the Gillespie algorithm applied to the solution of the
  //! filter equation for the SEIRS process.
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
    double *color = &COLOR;

    get_userdata_t *gud = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
    genealogy_t G = gud("genealogy");

    // traverse to current genealogical node
    int M = nearbyint(node);
    int node_count = 0;
    node_it k = G.cbegin();
    while (node_count < M && k != G.cend()) {
      node_count++; k++;
    }
    assert(k != G.cend());      // #nocov

    node_t *p = *k;
    node = node+1;

    // singular portion of filter equation
    if (!p->is_root()) {
      ll = 0;
      int parent = p->lineage();

      assert(parent >= 0 && parent < nSAMPLE); // #nocov
      assert(!ISNA(color[parent]));            // #nocov

      // if parent is not in deme I, likelihood = 0
      if (nearbyint(color[parent]) != 1) {
        ll += R_NegInf;
        color[parent] = 1;
        ellE -= 1; ellI += 1;
        E -= 1; I += 1;
      }

      if (p->holds(blue)) {     // sample
        ball_t *b = p->ball(blue);
        if (p->holds(green)) {  // s=(0,1)
          ball_t *g = p->other(b);
          ll += log(psi);       // boost
          color[p->lineage(g)] = 1;
        } else {                // s=(0,0)
          ellI -= 1;
          ll += log(psi*(I-ellI)); // boost
        }
        color[parent] = R_NaReal;
      } else if (p->holds(green)) { // branch point s=(1,1)
        ll += (S > 0 && I > 0) ? log(Beta*S/N/(E+1)) : R_NegInf;
        S -= 1; E += 1;
        ellE += 1;
        ball_t *a = p->last_ball();
        ball_t *b = p->other(a);
        assert(a != b && p->lineage(a) != p->lineage(b)); // #nocov
        assert(p->lineage(a) == p->lineage() || p->lineage(b) == p->lineage()); // #nocov
        if (unif_rand() < 0.5) {
          color[p->lineage(a)] = 0;
          color[p->lineage(b)] = 1;
        } else {
          color[p->lineage(a)] = 1;
          color[p->lineage(b)] = 0;
        }
        ll -= log(0.5);
      } else {
        err("inconceivable!! in '%s'!!",__func__);       // #nocov
      }
    }

    // continuous portion of filter equation:
    // take Gillespie steps to the end of the interval
    double rate[7], logpi[7];
    int event;
    double event_rate = 0;
    double penalty = 0;

    event_rate = event_rates(__x,__p,t,
                             __stateindex,__parindex,__covindex,
                             __covars,rate,logpi,&penalty);
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      event = rcateg(event_rate,rate,7);
      ll -= penalty*tstep + logpi[event];
      switch (event) {
      case 0:                   // transmission, s=(0,0)
        S -= 1; E += 1;
        ll += log(1-ellI/I)+log(1-ellE/E);
        break;
      case 1:                   // transmission, s=(0,1)
        S -= 1; E += 1;
        ll += log(1-ellE/E)-log(I);
        break;
      case 2:                   // transmission, s=(1,0)
        S -= 1; E += 1;
        ll += log(1-ellI/I)-log(E);
        change_color(color,random_choice(ellI),1,0);
        ellE += 1; ellI -= 1;
        break;
      case 3:                   // progression, s=(0,0)
        E -= 1; I += 1;
        ll += log(1-ellI/I);
        break;
      case 4:                   // progression, s=(0,1)
        E -= 1; I += 1;
        ll -= log(I);
        change_color(color,random_choice(ellE),0,1);
        ellE -= 1; ellI += 1;
        break;
      case 5:                   // recovery
        I -= 1; R += 1;
        break;
      case 6:                   // waning
        R -= 1; S += 1;
        break;
      default:                  // #nocov
        assert(0);              // #nocov
        break;                  // #nocov
      }

      ellE = nearbyint(ellE);
      ellI = nearbyint(ellI);

      t += tstep;
      event_rate = event_rates(__x,__p,t,
                               __stateindex,__parindex,__covindex,
                               __covars,rate,logpi,&penalty);
      tstep = exp_rand()/event_rate;

    }
    tstep = tmax - t;
    ll -= penalty*tstep;
  }

# define lik  (__lik[0])

  //! Measurement model likelihood (dmeasure).
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
    assert(!ISNA(ll));          // #nocov
    lik = (give_log) ? ll : exp(ll);
  }

}
