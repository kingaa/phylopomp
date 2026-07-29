#include "pomplink.h"
#include "internal.h"

#define Low  1
#define High 2

static const int nrate = 10;

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
#define kappa     (__p[__parindex[1]])
#define gamma     (__p[__parindex[2]])
#define omega     (__p[__parindex[3]])
#define chi       (__p[__parindex[4]])
#define etaL      (__p[__parindex[5]])
#define etaH      (__p[__parindex[6]])
#define POP       (__p[__parindex[7]])
#define S0        (__p[__parindex[8]])
#define IL0       (__p[__parindex[9]])
#define IH0       (__p[__parindex[10]])
#define R0        (__p[__parindex[11]])
#define S         (__x[__stateindex[0]])
#define IL        (__x[__stateindex[1]])
#define IH        (__x[__stateindex[2]])
#define R         (__x[__stateindex[3]])
#define ll        (__x[__stateindex[4]])
#define node      (__x[__stateindex[5]])
#define ellL      (__x[__stateindex[6]])
#define ellH      (__x[__stateindex[7]])
#define COLOR     (__x[__stateindex[8]])

#define EVENT_RATES                                     \
  event_rates(__x,__p,t,                                \
              __stateindex,__parindex,__covindex,       \
              __covars,rate,logpi,&decay)               \

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
 double *decay
 ) {
  double event_rate = 0;
  double alpha, pi;
  *decay = 0;
  assert(R_FINITE(event_rate));
  // 0: TL, s=(0,0) or s=(1,0)
  assert(S>=0 && IL>=0);
  alpha = (POP > 0) ? Beta*S*IL/POP : 0;
  assert(IL >= ellL);
  pi = 1;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 1: TH, s=(0,0)
  assert(S>=0 && IH>=0);
  alpha = (POP > 0) ? kappa*Beta*S*IH/POP : 0;
  assert(IH >= ellH);
  pi = (IH > 0) ? 1-0.5*ellH/IH : 1;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 2: TH, s=(1,0)
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi/ellH); logpi++;
  assert(R_FINITE(event_rate));
  // 3: L, s=(0,0)
  assert(IL>=0);
  alpha = etaL*IL;
  assert(IL >= ellL);
  pi = (IL > 0) ? 1-ellL/IL : 1;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 4: L, s=(0,1)
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi/ellL); logpi++;
  assert(R_FINITE(event_rate));
  // 5: H, s=(0,0)
  assert(IH>=0);
  alpha = etaH*IH;
  assert(IH >= ellH);
  pi = (IH > 0) ? 1-ellH/IH : 1;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 6: H, s=(1,0)
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi/ellH); logpi++;
  assert(R_FINITE(event_rate));
  // 7: RL
  assert(IL>=0);
  alpha = gamma*IL;
  if (IL > ellL) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *decay += alpha;
  }
  assert(R_FINITE(event_rate));
  // 8: RH
  assert(IH>=0);
  alpha = gamma*IH;
  if (IH > ellH) {
    event_rate += (*rate = alpha); rate++;
    *logpi = 0; logpi++;
  } else {
    *rate = 0; rate++;
    *logpi = 0; logpi++;
    *decay += alpha;
  }
  assert(R_FINITE(event_rate));
  // 9: W
  event_rate += (*rate = omega*R); rate++;
  *logpi = 0; logpi++;
  // sampling
  *decay += chi*(IL+IH);
  assert(R_FINITE(event_rate));
  return event_rate;
}

//! Latent-state initializer (rinit component).
//!
//! The state variables include S, IL, IH, R
//! plus 'ellL' and 'ellH' (numbers of L- and H-deme lineages),
//! the accumulated weight ('ll'), the current node number ('node'),
//! and the coloring of each lineage ('COLOR').
void si2rs_rinit
(
 double *__x,
 const double *__p,
 double t0,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  double adj = POP/(S0+IL0+IH0+R0);
  S = nearbyint(S0*adj);
  IL = nearbyint(IL0*adj);
  IH = nearbyint(IH0*adj);
  R = nearbyint(R0*adj);
  ellL = 0;
  ellH = 0;
  ll = 0;
  node = 0;
}

//! Simulator for the latent-state process (rprocess).
//!
//! This is the Gillespie algorithm applied to the solution of the
//! filter equation for the SI2RS process.
void si2rs_gill
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
  const int *index = get_userdata_int("index");
  const int *child = get_userdata_int("child");

  int parent = (int) nearbyint(node);

#ifndef NDEBUG
  const int *sat = get_userdata_int("saturation");
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
  default:                      // non-genealogical event #nocov
    break;                      // #nocov
  case 0:                       // root
    // color lineages by sampling without replacement
    assert(sat[parent]==1);
    int c = child[index[parent]];
    assert(lineage[parent]==lineage[c]);
    if (IL-ellL + IH-ellH > 0) {
      double x = (IL-ellL)/(IL-ellL + IH-ellH);
      if (unif_rand() < x) {      // lineage is put into Low deme
        color[lineage[c]] = Low;
        ellL += 1;
        ll -= log(x);
      } else {                    // lineage is put into High deme
        color[lineage[c]] = High;
        ellH += 1;
        ll -= log(1-x);
      }
      assert(!ISNAN(ll));
    } else {                // more roots than infectives
      ll += R_NegInf;       // this is incompatible with the genealogy
      // the following keeps the state valid
      color[lineage[c]] = Low;
      ellL += 1; IL += 1;
    }
    break;
  case 1:                       // sample
    assert(sat[parent]==0);
    if (parcol == Low) {
      assert(ellL>=1 && IL >= ellL);
      ll += log(chi*IL);
      ellL -= 1; IL -= 1;
    } else if (parcol == High) {
      assert(ellH>=1 && IH >= ellH);
      ll += log(chi*IH);
      ellH -= 1; IH -= 1;
    } else {
      assert(0);                // #nocov
    }
    color[parlin] = R_NaReal;
    break;
  case 2:
    assert(sat[parent]==2);
    int c1 = child[index[parent]];
    int c2 = child[index[parent]+1];
    assert(c1 != c2);
    assert(lineage[c1] != lineage[c2]);
    assert(lineage[c1] != parlin || lineage[c2] != parlin);
    assert(lineage[c1] == parlin || lineage[c2] == parlin);
    if (parcol == Low) {
      assert(ellL >= 1 && IL >= ellL);
      if (S >= 1 && POP > 0) {
        ll += log(Beta*S*IL/POP);
        S -= 1; IL += 1;
        ellL += 1;
        color[lineage[c1]] = Low;
        color[lineage[c2]] = Low;
      } else {
        ll += R_NegInf;
        IL += 1; ellL += 1;
        color[lineage[c1]] = Low;
        color[lineage[c2]] = Low;
      }
    } else if (parcol == High) {
      assert(ellH >= 1 && IH >= ellH);
      if (S>=1 && POP > 0) {
        ll += log(kappa*Beta*S*IH/POP);
        S -= 1; IL += 1;
        ellL += 1;
        if (unif_rand() < 0.5) {
          color[lineage[c1]] = Low;
          color[lineage[c2]] = High;
        } else {
          color[lineage[c1]] = High;
          color[lineage[c2]] = Low;
        }
        ll -= log(0.5);
        assert(!ISNAN(ll));
      } else {
        ll += R_NegInf;
        IL += 1; ellL += 1;
        color[lineage[c1]] = Low;
        color[lineage[c2]] = High;
      }
    } else {
      assert(0);                // #nocov
    }
    break;
  }

  // continuous portion of filter equation:
  // take Gillespie steps to the end of the interval
  if (tmax > t && R_FINITE(ll)) {

    double rate[nrate], logpi[nrate];
    int event;
    double event_rate = 0;
    double decay = 0;

    event_rate = EVENT_RATES;
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      event = rcateg(event_rate,rate,nrate);
      assert(event>=0 && event<nrate);
      ll -= decay*tstep + logpi[event];
      switch (event) {
      case 0:                   // TL, s=(0,0) or s=(1,0)
        assert(S>=1 && IL>=1);
        S -= 1; IL += 1;
        ll += log(1-ellL*(ellL-1)/IL/(IL-1));
        break;
      case 1:                   // TH, s = (0,0)
        assert(S>=1 && IH >= 1);
        S -= 1; IL += 1;
        ll += log(1-ellL/IL);
        break;
      case 2:                   // TH, s=(1,0)
        assert(S>=1 && IH >= 1);
        S -= 1; IL += 1;
        change_color(color,nsample,random_choice(ellH),High,Low);
        ellH -= 1; ellL += 1;
        ll += log(1-ellH/IH/IL);
        break;
      case 3:                   // L, s=(0,0)
        assert(IL>=1);
        IL -= 1; IH += 1;
        ll += log(1-ellH/IH);
        break;
      case 4:                   // L, s=(0,1)
        assert(IL>=1);
        change_color(color,nsample,random_choice(ellL),Low,High);
        ellL -= 1; ellH += 1;
        IL -= 1; IH += 1;
        ll -= log(IH);
        break;
      case 5:                   // H, s=(0,0)
        assert(IH>=1);
        IL += 1; IH -= 1;
        ll += log(1-ellL/IL);
        break;
      case 6:                   // H, s=(1,0)
        assert(IH>=1);
        change_color(color,nsample,random_choice(ellH),High,Low);
        ellL += 1; ellH -= 1;
        IL += 1; IH -= 1;
        ll -= log(IL);
        break;
      case 7:                   // RL
        assert(IL>=1);
        IL -= 1; R += 1;
        break;
      case 8:                   // RH
        assert(IH>=1);
        IH -= 1; R += 1;
        break;
      case 9:                   // W
        assert(R>=1);
        R -= 1; S += 1;
        break;
      default:                  // #nocov
        assert(0);              // #nocov
        ll += R_NegInf;         // #nocov
        break;                  // #nocov
      }

      ellL = nearbyint(ellL);
      ellH = nearbyint(ellH);

      t += tstep;
      event_rate = EVENT_RATES;
      tstep = exp_rand()/event_rate;

    }
    tstep = tmax - t;
    ll -= decay*tstep;
  }
  node += 1;
}

# define lik  (__lik[0])

//! Measurement model likelihood (dmeasure).
void si2rs_dmeas
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
