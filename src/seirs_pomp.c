#include "pomplink.h"
#include "internal.h"

static inline int random_choice (double n) {
  return (int) floor(R_unif_index(n));
}

static void change_color (double *color, int nsample,
                          int n, int from, int to) {
  int i = -1;
  while (n >= 0 && i < nsample) {
    i++;
    if (!ISNA(color[i]) && nearbyint(color[i]) == from) n--;
  }
  assert(i < nsample);
  assert(n == -1);
  assert(nearbyint(color[i]) == from);
  color[i] = to;
}

static int find_child (int start, const int parent,
                       const int *ancestor, const int end)
{
  int i = start+1;
  while (ancestor[i] != parent && i < end) i++;
  return i;
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
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  // 1: transmission, s=(0,1)
  pi = (1-pi)/2;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi)-log(ellI); logpi++;
  // 2: transmission, s=(1,0)
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi)-log(ellI); logpi++;
  // 3: progression, s=(0,0)
  // 4: progression, s=(0,1)
  alpha = sigma*E;
  pi = ellE/(E+1);
  if (E > ellE) {
    event_rate += (*rate = alpha*(1-pi)); rate++;
    *logpi = log(1-pi); logpi++;
    event_rate += (*rate = alpha*pi); rate++;
    *logpi = log(pi)-log(ellE); logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *penalty += alpha;
  }
  // 5: recovery
  alpha = gamma*I;
  if (I > ellI) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *penalty += alpha;
  }
  // 6: waning
  event_rate += (*rate = omega*R); rate++;
  *logpi = 0; logpi++;
  // 7: sampling (Q = 0)
  *penalty += psi*I;
  return event_rate;
}

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
  const int nnode = *get_userdata_int("nnode");
  const int *nodetype = get_userdata_int("nodetype");
  const int *ancestor = get_userdata_int("ancestor");
  const int *lineage = get_userdata_int("lineage");

  double adj = N/(S0+E0+I0+R0);
  S = nearbyint(S0*adj);
  E = nearbyint(E0*adj);
  I = nearbyint(I0*adj);
  R = nearbyint(R0*adj);

  ellE = 0;
  ellI = 0;
  ll = 0;

  // color lineages by sampling without replacement
  double nE = E;
  double nI = I;
  int parent = 0;
  while (nodetype[parent] == 0 && parent < nnode) {
    int child = parent;
    while (child < nnode) {
      child = find_child(child,parent,ancestor,nnode);
      if (child < nnode) {
        int pick = random_choice(nE+nI);
        if (pick <= nearbyint(nE)) {
          color[lineage[child]] = 0; // lineage is put into E deme
          ellE += 1; nE -= 1;
        } else {
          color[lineage[child]] = 1; // lineage is put into I deme
          ellI += 1; nI -= 1;
        }
      }
    }
    parent++;
  }
  ellE = nearbyint(ellE);
  ellI = nearbyint(ellI);
  node = parent-1;
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
  const int nsample = *get_userdata_int("nsample");
  const int nnode = *get_userdata_int("nnode");
  const int *nodetype = get_userdata_int("nodetype");
  const int *saturation = get_userdata_int("saturation");
  const int *ancestor = get_userdata_int("ancestor");
  const int *lineage = get_userdata_int("lineage");

  int parent = (int) node;
  node += 1;

  // singular portion of filter equation
  if (nodetype[parent] != 0) {    // not a root
    ll = 0;
    int parlin = lineage[parent];
    assert(parlin >= 0 && parlin < nsample);
    assert(!ISNA(color[parlin]));

    // if parent is not in deme I, likelihood = 0
    if (nearbyint(color[parlin]) != 1) {
      ll += R_NegInf;
      color[parlin] = 1;
      ellE -= 1; ellI += 1;
      E -= 1; I += 1;
    }

    if (nodetype[parent] == 1) {     // sample
      if (saturation[parent] == 1) { // s=(0,1)
        int child = find_child(parent,parent,ancestor,nnode);
        color[lineage[child]] = 1;
        ll += log(psi);       // boost
      } else {                // s=(0,0)
        ellI -= 1;
        ll += log(psi*(I-ellI)); // boost
      }
      color[parlin] = R_NaReal;
    } else if (nodetype[parent] == 2) { // branch point s=(1,1)
      assert(saturation[parent]==2);
      ll += (S > 0 && I > 0) ? log(Beta*S/N/(E+1)) : R_NegInf;
      S -= 1; E += 1;
      ellE += 1;
      int child1 = find_child(parent,parent,ancestor,nnode);
      int child2 = find_child(child1,parent,ancestor,nnode);
      assert(child1 != child2);
      assert(lineage[child1] != lineage[child2]);
      assert(lineage[child1] != parlin || lineage[child2] != parlin);
      assert(lineage[child1] == parlin || lineage[child2] == parlin);
      if (unif_rand() < 0.5) {
        color[lineage[child1]] = 0;
        color[lineage[child2]] = 1;
      } else {
        color[lineage[child1]] = 1;
        color[lineage[child2]] = 0;
      }
      ll -= log(0.5);
    } else {
      assert(0);                                 // #nocov
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
      change_color(color,nsample,random_choice(ellI),1,0);
      ellE += 1; ellI -= 1;
      break;
    case 3:                   // progression, s=(0,0)
      E -= 1; I += 1;
      ll += log(1-ellI/I);
      break;
    case 4:                   // progression, s=(0,1)
      E -= 1; I += 1;
      ll -= log(I);
      change_color(color,nsample,random_choice(ellE),0,1);
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
  assert(!ISNAN(ll));
  lik = (give_log) ? ll : exp(ll);
}
