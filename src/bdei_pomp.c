#include "pomplink.h"
#include "internal.h"

#define Exposed  1
#define Infected 2

static const int nrate = 5;

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

#define sigma     (__p[__parindex[0]])
#define lambda    (__p[__parindex[1]])
#define mu        (__p[__parindex[2]])
#define chi       (__p[__parindex[3]])
#define POP       (__p[__parindex[4]])
#define E0        (__p[__parindex[5]])
#define I0        (__p[__parindex[6]])
#define E         (__x[__stateindex[0]])
#define I         (__x[__stateindex[1]])
#define ll        (__x[__stateindex[2]])
#define node      (__x[__stateindex[3]])
#define ellE      (__x[__stateindex[4]])
#define ellI      (__x[__stateindex[5]])
#define COLOR     (__x[__stateindex[6]])

#define EVENT_RATES                                     \
  event_rates(__x,__p,t,                                \
              __stateindex,__parindex,__covindex,       \
              __covars,rate,logpi,&penalty)             \

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
  // 0: birth, s=(0,0) or s=(0,1)
  assert(I >= ellI);
  assert(ellI >= 0);
  alpha = lambda*I;
  pi = 1/(ellI+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  // 1: birth, s=(1,0)
  pi = ellI/(ellI+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = (ellI > 0) ? -log(ellI+1) : 0; logpi++;
  // 2: progression, s=(0,0)
  assert(E >= ellE);
  assert(ellE >= 0);
  alpha = sigma*E;
  pi = (E > 0) ? 1-ellE/E : 1;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  // 3: progression, s=(0,1)
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = (ellE > 0) ? log(pi)-log(ellE) : 0; logpi++;
  // 4: death
  alpha = mu*I;
  if (I > ellI) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *penalty += alpha;
  }
  // sampling (Q = 0): destructive only
  *penalty += chi*I;
  assert(R_FINITE(event_rate));
  return event_rate;
}

//! Latent-state initializer (rinit component).
//!
//! The state variables include E, I
//! plus 'ellE' and 'ellI' (numbers of E- and I-deme lineages),
//! the accumulated weight ('ll'), the current node number ('node'),
//! and the coloring of each lineage ('COLOR').
void bdei_rinit
(
 double *__x,
 const double *__p,
 double t0,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  if (!R_FINITE(sigma) || sigma < 0 ||
      !R_FINITE(lambda) || lambda < 0 ||
      !R_FINITE(mu) || mu < 0 || !R_FINITE(chi) || chi < 0)
    err("BDEI rates must be finite and nonnegative.");
  if (!R_FINITE(POP) || POP < 0 ||
      !R_FINITE(E0) || E0 < 0 || !R_FINITE(I0) || I0 < 0 ||
      !R_FINITE(E0+I0) || E0+I0 <= 0)
    err("BDEI initial values must be finite and nonnegative, with E0+I0 > 0.");
  double adj = POP/(E0+I0);
  if (!R_FINITE(adj)) err("BDEI: pop/(E0+I0) must be finite.");
  E = nearbyint(E0*adj);
  I = nearbyint(I0*adj);
  ellE = 0;
  ellI = 0;
  ll = 0;
  node = 0;
}

//! Simulator for the latent-state process (rprocess).
//!
//! This is the Gillespie algorithm applied to the solution of the
//! filter equation for the BDEI process.
void bdei_gill
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
  const int *sat = get_userdata_int("saturation");
  const int *index = get_userdata_int("index");
  const int *child = get_userdata_int("child");

  int parent = (int) nearbyint(node);

#ifndef NDEBUG
  int nnode = *get_userdata_int("nnode");
  assert(parent>=0);
  assert(parent<=nnode);
#endif

  int parlin = lineage[parent];
  int parcol = color[parlin];
  assert(parlin >= 0 && parlin < nsample);

  ll = 0;

  // singular portion of filter equation
  switch (nodetype[parent]) {
  default:
    break;
  case 0:                       // root
    assert(sat[parent]==1);
    {
      int c = child[index[parent]];
      assert(lineage[parent]==lineage[c]);
      if (E-ellE + I-ellI > 0) {
        double x = (E-ellE)/(E-ellE + I-ellI);
        if (unif_rand() < x) {
          color[lineage[c]] = Exposed;
          ellE += 1;
          ll -= log(x);
        } else {
          color[lineage[c]] = Infected;
          ellI += 1;
          ll -= log(1-x);
        }
      } else {
        ll += R_NegInf;
        if (unif_rand() < 0.5) {
          color[lineage[c]] = Exposed;
          ellE += 1; E += 1;
        } else {
          color[lineage[c]] = Infected;
          ellI += 1; I += 1;
        }
      }
    }
    break;
  case 1:                       // sample
    if (parcol != Infected) {
      ll += R_NegInf;
      color[parlin] = Infected;
      ellE -= 1; ellI += 1;
      E -= 1; I += 1;
    }
    if (sat[parent] == 0) {     // destructive sample
      ellI -= 1;
      ll += (I > 0) ? log(chi*I) : R_NegInf;
      I -= 1;
    } else {
      assert(0);                // #nocov
      ll += R_NegInf;           // #nocov
    }
    color[parlin] = R_NaReal;
    break;
  case 2:                       // branch point: I births E
    if (parcol != Infected) {
      ll += R_NegInf;
      color[parlin] = Infected;
      ellE -= 1; ellI += 1;
      E -= 1; I += 1;
    }
    assert(sat[parent]==2);
    ll += (I > 0) ? log(lambda/(E+1)) : R_NegInf;
    E += 1;
    ellE += 1;
    {
      int c1 = child[index[parent]];
      int c2 = child[index[parent]+1];
      assert(c1 != c2);
      assert(lineage[c1] != lineage[c2]);
      assert(lineage[c1] != parlin || lineage[c2] != parlin);
      assert(lineage[c1] == parlin || lineage[c2] == parlin);
      if (unif_rand() < 0.5) {
        color[lineage[c1]] = Exposed;
        color[lineage[c2]] = Infected;
      } else {
        color[lineage[c1]] = Infected;
        color[lineage[c2]] = Exposed;
      }
      ll -= log(0.5);
    }
    break;
  }

  // regular portion of filter equation
  if (tmax > t && R_FINITE(ll)) {

    double rate[nrate], logpi[nrate];
    int event;
    double event_rate = 0;
    double penalty = 0;

    event_rate = EVENT_RATES;
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      event = rcateg(event_rate,rate,nrate);
      assert(event>=0 && event<nrate);
      ll -= penalty*tstep + logpi[event];
      switch (event) {
      case 0:                   // birth, s=(0,0) or s=(0,1)
        E += 1;
        ll += log(1-ellE/E);
        assert(!ISNAN(ll));
        break;
      case 1:                   // birth, s=(1,0)
        change_color(color,nsample,random_choice(ellI),Infected,Exposed);
        ellE += 1; ellI -= 1;
        E += 1;
        ll += log(1-ellI/I)-log(E);
        assert(!ISNAN(ll));
        break;
      case 2:                   // progression, s=(0,0)
        assert(E>=1);
        E -= 1; I += 1;
        ll += log(1-ellI/I);
        assert(!ISNAN(ll));
        break;
      case 3:                   // progression, s=(0,1)
        assert(E>=1);
        change_color(color,nsample,random_choice(ellE),Exposed,Infected);
        ellE -= 1; ellI += 1;
        E -= 1; I += 1;
        ll -= log(I);
        assert(!ISNAN(ll));
        break;
      case 4:                   // death
        assert(I>=1);
        I -= 1;
        assert(!ISNAN(ll));
        break;
      default:                  // #nocov
        assert(0);              // #nocov
        break;                  // #nocov
      }

      ellE = nearbyint(ellE);
      ellI = nearbyint(ellI);

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
void bdei_dmeas
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
