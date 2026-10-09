#include "pomplink.h"
#include "internal.h"

#define Normal       1
#define Superspreader 2

static const int nrate = 8;

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

#define lambda_nn (__p[__parindex[0]])
#define lambda_ns (__p[__parindex[1]])
#define lambda_sn (__p[__parindex[2]])
#define lambda_ss (__p[__parindex[3]])
#define mu        (__p[__parindex[4]])
#define chi       (__p[__parindex[5]])
#define POP       (__p[__parindex[6]])
#define N0        (__p[__parindex[7]])
#define S0        (__p[__parindex[8]])
#define N         (__x[__stateindex[0]])
#define S         (__x[__stateindex[1]])
#define ll        (__x[__stateindex[2]])
#define node      (__x[__stateindex[3]])
#define ell1      (__x[__stateindex[4]])
#define ell2      (__x[__stateindex[5]])
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
  double alpha, pi, disc;
  *penalty = 0;
  assert(N >= ell1 && ell1 >= 0);
  assert(S >= ell2 && ell2 >= 0);
  // 0: N->N birth, s=(0,0) or s=(1,0)
  alpha = lambda_nn*N;
  disc = (N > 0) ? ell1*(ell1-1)/N/(N+1) : 1;
  *penalty += alpha*disc;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *logpi = 0; logpi++;
  // 1: N->S birth, s=(0,0) or s=(0,1)
  alpha = lambda_ns*N;
  pi = 1/(ell1+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  // 2: N->S birth, s=(1,0)
  pi = ell1/(ell1+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = (ell1 > 0) ? -log(ell1+1) : 0; logpi++;
  // 3: S->S birth, s=(0,0) or s=(0,1)
  alpha = lambda_ss*S;
  disc = (S > 0) ? ell2*(ell2-1)/S/(S+1) : 1;
  *penalty += alpha*disc;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *logpi = 0; logpi++;
  // 4: S->N birth, s=(0,0) or s=(1,0)
  alpha = lambda_sn*S;
  pi = 1/(ell2+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  // 5: S->N birth, s=(0,1)
  pi = ell2/(ell2+1);
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = (ell2 > 0) ? -log(ell2+1) : 0; logpi++;
  // 6: death N
  alpha = mu*N;
  if (N > ell1) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *penalty += alpha;
  }
  // 7: death S
  alpha = mu*S;
  if (S > ell2) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *penalty += alpha;
  }
  // sampling (Q = 0): destructive only
  *penalty += chi*(N+S);
  assert(R_FINITE(event_rate));
  return event_rate;
}

//! Latent-state initializer (rinit component).
//!
//! The state variables include N, S
//! plus 'ell1' and 'ell2' (numbers of N- and S-deme lineages),
//! the accumulated weight ('ll'), the current node number ('node'),
//! and the coloring of each lineage ('COLOR').
void bdss_rinit
(
 double *__x,
 const double *__p,
 double t0,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  if (!R_FINITE(lambda_nn) || lambda_nn < 0 ||
      !R_FINITE(lambda_ns) || lambda_ns < 0 ||
      !R_FINITE(lambda_sn) || lambda_sn < 0 ||
      !R_FINITE(lambda_ss) || lambda_ss < 0 ||
      !R_FINITE(mu) || mu < 0 || !R_FINITE(chi) || chi < 0)
    err("BDSS rates must be finite and nonnegative.");
  if (!R_FINITE(POP) || POP < 0 ||
      !R_FINITE(N0) || N0 < 0 || !R_FINITE(S0) || S0 < 0 ||
      !R_FINITE(N0+S0) || N0+S0 <= 0)
    err("BDSS initial values must be finite and nonnegative, with N0+S0 > 0.");
  double adj = POP/(N0+S0);
  if (!R_FINITE(adj)) err("BDSS: pop/(N0+S0) must be finite.");
  N = nearbyint(N0*adj);
  S = nearbyint(S0*adj);
  ell1 = 0;
  ell2 = 0;
  ll = 0;
  node = 0;
}

//! Simulator for the latent-state process (rprocess).
//!
//! This is the Gillespie algorithm applied to the solution of the
//! filter equation for the BDSS process.
void bdss_gill
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
      if (N-ell1 + S-ell2 > 0) {
        double x = (N-ell1)/(N-ell1 + S-ell2);
        if (unif_rand() < x) {
          color[lineage[c]] = Normal;
          ell1 += 1;
          ll -= log(x);
        } else {
          color[lineage[c]] = Superspreader;
          ell2 += 1;
          ll -= log(1-x);
        }
      } else {
        ll += R_NegInf;
        if (unif_rand() < 0.5) {
          color[lineage[c]] = Normal;
          ell1 += 1; N += 1;
        } else {
          color[lineage[c]] = Superspreader;
          ell2 += 1; S += 1;
        }
      }
    }
    break;
  case 1:                       // sample (destructive only)
    if (sat[parent] == 0) {
      if (parcol == Normal) {
        ell1 -= 1;
        ll += (N > 0) ? log(chi*N) : R_NegInf;
        N -= 1;
      } else if (parcol == Superspreader) {
        ell2 -= 1;
        ll += (S > 0) ? log(chi*S) : R_NegInf;
        S -= 1;
      } else {
        ll += R_NegInf;
      }
    } else {
      assert(0);                // #nocov
      ll += R_NegInf;           // #nocov
    }
    color[parlin] = R_NaReal;
    break;
  case 2:                       // branch point
    assert(sat[parent]==2);
    {
      int c1 = child[index[parent]];
      int c2 = child[index[parent]+1];
      assert(c1 != c2);
      assert(lineage[c1] != lineage[c2]);
      assert(lineage[c1] != parlin || lineage[c2] != parlin);
      assert(lineage[c1] == parlin || lineage[c2] == parlin);

      if (parcol == Normal) {
        double lam = lambda_nn + lambda_ns;
        if (lam <= 0) { ll += R_NegInf; break; }
        double p_nn = lambda_nn/lam;
        if (unif_rand() < p_nn) {
          // N->N birth: both children Normal
          N += 1; ell1 += 1;
          ll += log(2*lam/N);
          if (N*(N-1) <= 0) ll += R_NegInf;
          color[lineage[c1]] = Normal;
          color[lineage[c2]] = Normal;
        } else {
          // N->S birth: one N, one S
          S += 1; ell2 += 1;
          ll += log(lam/S);
          if (N*S <= 0) ll += R_NegInf;
          if (unif_rand() < 0.5) {
            color[lineage[c1]] = Normal;
            color[lineage[c2]] = Superspreader;
          } else {
            color[lineage[c1]] = Superspreader;
            color[lineage[c2]] = Normal;
          }
          ll -= log(0.5);
        }
      } else if (parcol == Superspreader) {
        double lam = lambda_sn + lambda_ss;
        if (lam <= 0) { ll += R_NegInf; break; }
        double p_ss = lambda_ss/lam;
        if (unif_rand() < p_ss) {
          // S->S birth: both children Superspreader
          S += 1; ell2 += 1;
          ll += log(2*lam/S);
          if (S*(S-1) <= 0) ll += R_NegInf;
          color[lineage[c1]] = Superspreader;
          color[lineage[c2]] = Superspreader;
        } else {
          // S->N birth: one S, one N
          N += 1; ell1 += 1;
          ll += log(lam/N);
          if (N*S <= 0) ll += R_NegInf;
          if (unif_rand() < 0.5) {
            color[lineage[c1]] = Normal;
            color[lineage[c2]] = Superspreader;
          } else {
            color[lineage[c1]] = Superspreader;
            color[lineage[c2]] = Normal;
          }
          ll -= log(0.5);
        }
      } else {
        ll += R_NegInf;
      }
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
      case 0:                   // N->N birth
        N += 1;
        assert(!ISNAN(ll));
        break;
      case 1:                   // N->S birth, s=(0,0) or s=(0,1)
        S += 1;
        ll += log(1-ell2/S);
        assert(!ISNAN(ll));
        break;
      case 2:                   // N->S birth, s=(1,0)
        change_color(color,nsample,random_choice(ell1),Normal,Superspreader);
        ell1 -= 1; ell2 += 1;
        S += 1;
        ll += log(1-ell1/N)-log(S);
        assert(!ISNAN(ll));
        break;
      case 3:                   // S->S birth
        S += 1;
        assert(!ISNAN(ll));
        break;
      case 4:                   // S->N birth, s=(0,0) or s=(1,0)
        N += 1;
        ll += log(1-ell1/N);
        assert(!ISNAN(ll));
        break;
      case 5:                   // S->N birth, s=(0,1)
        change_color(color,nsample,random_choice(ell2),Superspreader,Normal);
        ell2 -= 1; ell1 += 1;
        N += 1;
        ll += log(1-ell2/S)-log(N);
        assert(!ISNAN(ll));
        break;
      case 6:                   // death N
        assert(N>=1);
        N -= 1;
        break;
      case 7:                   // death S
        assert(S>=1);
        S -= 1;
        break;
      default:                  // #nocov
        assert(0);              // #nocov
        break;                  // #nocov
      }

      ell1 = nearbyint(ell1);
      ell2 = nearbyint(ell2);

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
void bdss_dmeas
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
