#include "pomplink.h"
#include "internal.h"

static inline int random_choice (double n) {
  return floor(R_unif_index(n));
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
  assert(S>=0 && I>=0);
  alpha = Beta*S*I/N;
  pi = (I > 0) ? 1-ellI/I : 0;
  assert(I >= ellI);
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
  assert(E>=0);
  alpha = sigma*E;
  pi = (E > 0) ? ellE/E : 1;
  assert(E >= ellE);
  event_rate += (*rate = alpha*(1-pi)); rate++;
  *logpi = log(1-pi); logpi++;
  // 4: progression, s=(0,1)
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi)-log(ellE); logpi++;
  // 5: recovery
  assert(I>=0);
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
  assert(!ISNAN(event_rate));
  return event_rate;
}

//! Latent-state initializer (rinit component).
//!
//! The state variables include S, E, I, R
//! plus 'ellE' and 'ellI' (numbers of E- and I-deme lineages),
//! the accumulated weight ('ll'), the current node number ('node'),
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
  const int *nodetype = get_userdata_int("nodetype");
  const int *lineage = get_userdata_int("lineage");
  const int *index = get_userdata_int("index");
  const int *child = get_userdata_int("child");
  int nnode = *get_userdata_int("nnode");
#ifndef NDEBUG
  const int *saturation = get_userdata_int("saturation");
#endif

  double adj = N/(S0+E0+I0+R0);
  S = nearbyint(S0*adj);
  E = nearbyint(E0*adj);
  I = nearbyint(I0*adj);
  R = nearbyint(R0*adj);
  ellE = 0;
  ellI = 0;
  ll = 0;

  int parent = 0;
  // for all roots:
  while (parent < nnode && nodetype[parent] == 0) {
    // color lineages by sampling without replacement
    assert(saturation[parent]==1);
    int c = child[index[parent]];
    assert(lineage[parent]==lineage[c]);
    double x = (E-ellE)/(E-ellE + I-ellI);
    if (unif_rand() < x) {
      // lineage is put into E deme      
      color[lineage[c]] = 0;
      ellE += 1;
      ll -= log(x);
    } else {
      color[lineage[c]] = 1; // lineage is put into I deme
      ellI += 1;
      ll -= log(1-x);
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
  const int *nodetype = get_userdata_int("nodetype");
  const int *lineage = get_userdata_int("lineage");
  const int *saturation = get_userdata_int("saturation");
  const int *index = get_userdata_int("index");
  const int *child = get_userdata_int("child");

  int parent = (int) nearbyint(node);

#ifndef NDEBUG
  int nnode = *get_userdata_int("nnode");
  assert(parent>=0);
  assert(parent<=nnode);
#endif

  int parlin = lineage[parent];
  assert(parlin >= 0 && parlin < nsample);
  assert(!ISNA(color[parlin]));

  // singular portion of filter equation
  switch (nodetype[parent]) {
  case 0: default:
    break;
  case 1:                       // sample
    ll = 0;
    // If parent is not in deme I, likelihood = 0.
    if (nearbyint(color[parlin]) != 1) {
      ll += R_NegInf;
      color[parlin] = 1;
      ellE -= 1; ellI += 1;
      E -= 1; I += 1;
    }
    if (saturation[parent] == 1) { // s=(0,1)
      int c = child[index[parent]];
      color[lineage[c]] = 1;
      ll += log(psi);
    } else {                    // s=(0,0)
      ellI -= 1;
      ll += log(psi*(I-ellI));
    }
    color[parlin] = R_NaReal;
    break;
  case 2:                       // branch point s=(1,1)
    ll = 0;
    // If parent is not in deme I, likelihood = 0.
    if (nearbyint(color[parlin]) != 1) {
      ll += R_NegInf;
      color[parlin] = 1;
      ellE -= 1; ellI += 1;
      E -= 1; I += 1;
    }
    assert(saturation[parent]==2);
    ll += (S > 0 && I > 0) ? log(Beta*S/N/(E+1)) : R_NegInf;
    S -= 1; E += 1;
    ellE += 1;
    S = (S > 0) ? S : 0;
    int c1 = child[index[parent]];
    int c2 = child[index[parent]+1];
    assert(c1 != c2);
    assert(lineage[c1] != lineage[c2]);
    assert(lineage[c1] != parlin || lineage[c2] != parlin);
    assert(lineage[c1] == parlin || lineage[c2] == parlin);
    if (unif_rand() < 0.5) {
      color[lineage[c1]] = 0;
      color[lineage[c2]] = 1;
    } else {
      color[lineage[c1]] = 1;
      color[lineage[c2]] = 0;
    }
    ll -= log(0.5);
    break;
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
      assert(!ISNAN(ll));
      break;
    case 1:                   // transmission, s=(0,1)
      S -= 1; E += 1;
      ll += log(1-ellE/E)-log(I);
      assert(!ISNAN(ll));
      break;
    case 2:                   // transmission, s=(1,0)
      S -= 1; E += 1;
      ll += log(1-ellI/I)-log(E);
      change_color(color,nsample,random_choice(ellI),1,0);
      ellE += 1; ellI -= 1;
      assert(!ISNAN(ll));
      break;
    case 3:                   // progression, s=(0,0)
      E -= 1; I += 1;
      ll += log(1-ellI/I);
      assert(!ISNAN(ll));
      break;
    case 4:                   // progression, s=(0,1)
      E -= 1; I += 1;
      ll -= log(I);
      change_color(color,nsample,random_choice(ellE),0,1);
      ellE -= 1; ellI += 1;
      assert(!ISNAN(ll));
      break;
    case 5:                   // recovery
      I -= 1; R += 1;
      assert(!ISNAN(ll));
      break;
    case 6:                   // waning
      R -= 1; S += 1;
      assert(!ISNAN(ll));
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
  node += 1;
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
