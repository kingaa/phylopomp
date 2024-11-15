#include "pomplink.h"
#include "internal.h"

static const int nrate = 18;
#define host1 0
#define host2 1

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

#ifndef NDEBUG
static int check_color (double *color, int nsample,
                        double size1, double size2) {
  int n1 = 0, n2 = 0;
  int s1 = (int) nearbyint(size1);
  int s2 = (int) nearbyint(size2);
  for (int i = 0; i < nsample; i++) {
    if (!ISNA(color[i])) {
      if (nearbyint(color[i]) == host1) n1++;
      if (nearbyint(color[i]) == host2) n2++;
    }
  }
  return (n1==s1) && (n2==s2);
}
#endif

#define Beta11      (__p[__parindex[0]])
#define Beta12      (__p[__parindex[1]])
#define Beta21      (__p[__parindex[2]])
#define Beta22      (__p[__parindex[3]])
#define gamma1      (__p[__parindex[4]])
#define gamma2      (__p[__parindex[5]])
#define psi1        (__p[__parindex[6]])
#define psi2        (__p[__parindex[7]])
#define omega1      (__p[__parindex[8]])
#define omega2      (__p[__parindex[9]])
#define b1         (__p[__parindex[10]])
#define b2         (__p[__parindex[11]])
#define d1         (__p[__parindex[12]])
#define d2         (__p[__parindex[13]])
#define C1         (__p[__parindex[14]])
#define C2         (__p[__parindex[15]])
#define S1_0       (__p[__parindex[16]])
#define S2_0       (__p[__parindex[17]])
#define I1_0       (__p[__parindex[18]])
#define I2_0       (__p[__parindex[19]])
#define R1_0       (__p[__parindex[20]])
#define R2_0       (__p[__parindex[21]])
#define S1       (__x[__stateindex[0]])
#define S2       (__x[__stateindex[1]])
#define I1       (__x[__stateindex[2]])
#define I2       (__x[__stateindex[3]])
#define R1       (__x[__stateindex[4]])
#define R2       (__x[__stateindex[5]])
#define N1       (__x[__stateindex[6]])
#define N2       (__x[__stateindex[7]])
#define ll       (__x[__stateindex[8]])
#define node     (__x[__stateindex[9]])
#define ell1    (__x[__stateindex[10]])
#define ell2    (__x[__stateindex[11]])
#define COLOR   (__x[__stateindex[12]])

#define EVENT_RATES                                     \
  event_rates(__x,__p,t,                                \
              __stateindex,__parindex,__covindex,       \
              __covars,rate,logpi,&penalty)             \

// FIXME: At the moment, the following codes exclude the possibility of
// importation of infection.
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
  // 0: Trans_11, s = (0,0),(1,0)
  assert(S1>=0 && I1>=ell1 && ell1>=0);
  alpha = (N1 > 0) ? Beta11*S1*I1/N1 : 0;
  disc = (I1 > 0) ? ell1*(ell1-1)/I1/(I1+1) : 1;
  *penalty += alpha*disc;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 1: Trans_22, s = (0,0),(0,1)
  assert(S2>=0 && I2>=ell2 && ell2>=0);
  alpha = (N2 > 0) ? Beta22*S2*I2/N2 : 0;
  disc = (I2 > 0) ? ell2*(ell2-1)/I2/(I2+1) : 1;
  *penalty += alpha*disc;
  event_rate += (*rate = alpha*(1-disc)); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 2: Trans_21, s = (0,0),(1,0)
  // s = (0,0): pi = (I1-ell1)/I1, m = 1,
  // s = (1,0): pi = 1/(2*I1), m = ell1
  // total pi = (I1-0.5*ell1)/I1
  assert(S2>=0 && I1>=ell1 && ell1>=0);
  alpha = (N1 > 0) ? Beta21*S2*I1/N1 : 0;
  pi = (I1 > 0) ? (I1-0.5*ell1)/I1 : 0;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 3: Trans_21, s = (0,1)
  // s = (0,1): pi = 1/(2*I1), m = ell1
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi)-log(ell1); logpi++;
  assert(R_FINITE(event_rate));
  // 4: Trans_12, s = (0,0),(0,1)
  assert(S1>=0 && I2>=ell2 && ell2>=0);
  alpha = (N2 > 0) ? Beta12*S1*I2/N2 : 0;
  pi = (I2 > 0) ? (I2-ell2*0.5)/I2 : 0;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi); logpi++;
  assert(R_FINITE(event_rate));
  // 5: Trans_12, s = (1,0)
  pi = 1-pi;
  event_rate += (*rate = alpha*pi); rate++;
  *logpi = log(pi)-log(ell2); logpi++;
  assert(R_FINITE(event_rate));
  // 6: Recov_1
  assert(I1>=0 && ell1>=0);
  alpha = gamma1*I1;
  if (I1 > ell1) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 7: Recov_2
  assert(I2>=0 && ell2>=0);
  alpha = gamma2*I2;
  if (I2 > ell2) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 8: Wane_1
  assert(R1>=0);
  alpha = omega1*R1;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 9: Wane_2
  assert(R2>=0);
  alpha = omega2*R2;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 10: Birth_1
  assert(N1>=0);
  alpha = b1*N1;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 11: Birth_2
  assert(N2>=0);
  alpha = b2*N2;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 12: Death_S1
  assert(S1>=0);
  alpha = d1*S1;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 13: Death_I1
  assert(I1>=0);
  alpha = d1*I1;
  if (I1 > ell1) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 14: Death_R1
  assert(R1>=0);
  alpha = d1*R1;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 15: Death_S2
  assert(S2>=0);
  alpha = d2*S2;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 16: Death_I2
  assert(I2>=0);
  alpha = d2*I2;
  if (I2 > ell2) {
    event_rate += (*rate = alpha); rate++;
  } else {
    *rate = 0; rate++;
    *penalty += alpha;
  }
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // 17: Death_R2
  assert(R2>=0);
  alpha = d2*R2;
  event_rate += (*rate = alpha); rate++;
  *logpi = 0; logpi++;
  assert(R_FINITE(event_rate));
  // Sample_1 (Q = 0) // NB: (1-C1)*psi1+C1*psi1 = psi1
  *penalty += psi1*I1;
  // Sample_2 (Q = 0) // NB: (1-C2)*psi2+C2*psi2 = psi2
  *penalty += psi2*I2;
  return event_rate;
}

//! Latent-state initializer (rinit component).
//!
//! The state variables include S, E, I, R
//! plus 'ellE' and 'ellI' (numbers of E- and I-deme lineages),
//! the accumulated weight ('ll'), the current node number ('node'),
//! and the coloring of each lineage ('COLOR').
void twospecies_rinit
(
 double *__x,
 const double *__p,
 double t0,
 const int *__stateindex,
 const int *__parindex,
 const int *__covindex,
 const double *__covars
 ){
  double adj;
  N1 = S1_0+I1_0+R1_0;
  N2 = S2_0+I2_0+R2_0;
  adj = N1/(S1_0+I1_0+R1_0);
  S1 = nearbyint(S1_0*adj);
  I1 = nearbyint(I1_0*adj);
  R1 = N1-S1-I1;
  adj = N2/(S2_0+I2_0+R2_0);
  S2 = nearbyint(S2_0*adj);
  I2 = nearbyint(I2_0*adj);
  R2 = N2-S2-I2;
  ell1 = 0;
  ell2 = 0;
  ll = 0;
  node = 0;
}

//! Simulator for the latent-state process (rprocess).
//!
//! This is the Gillespie algorithm applied to the solution of the
//! filter equation for the TwoSpecies model.
//! It advances the state from time `t` to time `t+dt`.
//!
//! A tricky aspect of this function is that it must return a "valid"
//! state even when the state is incompatible with the genealogy.
//! In such a case, we set the log likelihood (`ll`) to `R_NegInf`,
//! but the state must remain valid.  Hence insertion of extra infectives,
//! etc.
//!
//! FIXME: At the moment, the following codes exclude the possibility of
//! importation of infection.
void twospecies_gill
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
  assert(nearbyint(N1)==nearbyint(S1+I1+R1));
  assert(nearbyint(N2)==nearbyint(S2+I2+R2));
  assert(check_color(color,nsample,ell1,ell2));

  // singular portion of filter equation
  switch (nodetype[parent]) {
  default:                      // non-genealogical event
    break;
  case 0:                       // root
    ll = 0;
    // color lineages by sampling without replacement
    assert(sat[parent]==1);
    int c = child[index[parent]];
    assert(parlin==lineage[c]);
    if (I1-ell1+I2-ell2 > 0) {
      double x = (I1-ell1)/(I1-ell1 + I2-ell2);
      if (unif_rand() < x) {    // lineage is put into I1 deme
        color[lineage[c]] = host1;
        ell1 += 1;
        ll -= log(x);
      } else {                  // lineage is put into I2 deme
        color[lineage[c]] = host2;
        ell2 += 1;
        ll -= log(1-x);
      }
    } else {                // more roots than infectives
      ll += R_NegInf;       // this is incompatible with the genealogy
      // the following keeps the state valid
      if (unif_rand() < 0.5) {  // lineage is put into I1 deme
        color[lineage[c]] = host1;
        ell1 += 1; I1 += 1; N1 += 1;
        //        ll -= log(0.5);
      } else {                  // lineage is put into I2 deme
        color[lineage[c]] = host2;
        ell2 += 1; I2 += 1; N2 += 1;
        //        ll -= log(0.5);
      }
    }
    assert(nearbyint(N1)==nearbyint(S1+I1+R1));
    assert(nearbyint(N2)==nearbyint(S2+I2+R2));
    assert(check_color(color,nsample,ell1,ell2));
    break;
  case 1:                       // sample
    ll = 0;
    if (sat[parent] == 0) {     // s=(0,0)
      if (parcol == host1) {
        ell1 -= 1;
        if (C1 < 1 && unif_rand() > C1) {
          ll += log(psi1*(I1-ell1));
        } else {
          ll += log(psi1*I1);
          I1 -= 1; N1 -= 1;
        }
      } else if (parcol == host2) {
        ell2 -= 1;
        if (C2 < 1 && unif_rand() > C2) {
          ll += log(psi2*(I2-ell2));
        } else {
          ll += log(psi2*I2);
          I2 -= 1; N2 -= 1;
        }
      } else {
        assert(0);              // #nocov
        ll += R_NegInf;         // #nocov
      }
    } else if (sat[parent] == 1) {
      int c = child[index[parent]];
      color[lineage[c]] = parcol;
      if (parcol==host1) {
        ll += log(psi1*(1-C1)); // s=(1,0)
      } else if (parcol==host2) {
        ll += log(psi2*(1-C2)); // s=(0,1)
      } else {
        assert(0);              // #nocov
        ll += R_NegInf;         // #nocov
      }
    } else {
      assert(0);                // #nocov
      ll += R_NegInf;           // #nocov
    }
    color[parlin] = R_NaReal;
    assert(nearbyint(N1)==nearbyint(S1+I1+R1));
    assert(nearbyint(N2)==nearbyint(S2+I2+R2));
    assert(check_color(color,nsample,ell1,ell2));
    break;
  case 2:                       // branch point
    ll = 0;
    assert(sat[parent]==2);
    if (parcol == host1) {      // parent is in I1
      assert(S1>=0 && S2 >=0 && I1>=ell1 && ell1>=0);
      double lambda11 = Beta11*S1*I1/N1;
      double lambda21 = Beta21*S2*I1/N1;
      double lambda = lambda11+lambda21;
      double x = (lambda > 0) ? lambda11/lambda : 0;
      int c1 = child[index[parent]];
      int c2 = child[index[parent]+1];
      assert(c1 != c2);
      assert(lineage[c1] != lineage[c2]);
      assert(lineage[c1] != parlin || lineage[c2] != parlin);
      assert(lineage[c1] == parlin || lineage[c2] == parlin);
      if (unif_rand() < x) {    // s = (2,0)
        color[lineage[c1]] = host1;
        color[lineage[c2]] = host1;
        if (S1 > 0) {
          S1 -= 1; I1 += 1; ell1 += 1;
          ll += log(lambda)-log(I1*(I1-1)/2);
        } else {
          // the genealogy is incompatible with the state.
          // nevertheless, the state remains valid.
          I1 += 1; N1 += 1; ell1 += 1;
          ll += R_NegInf;
        }
      } else {                  // s = (1,1)
        if (unif_rand() < 0.5) {
          color[lineage[c1]] = host1;
          color[lineage[c2]] = host2;
        } else {
          color[lineage[c1]] = host2;
          color[lineage[c2]] = host1;
        }
        ll -= log(0.5);
        if (S2 > 0) {
          S2 -= 1; I2 += 1; ell2 += 1;
          ll += log(lambda)-log(I1*I2);
        } else {
          // the genealogy is incompatible with the state.
          // nevertheless, the state remains valid.
          I2 += 1; N2 += 1; ell2 += 1;
          ll += R_NegInf;
        }
      }
    } else if (parcol == host2) { // parent is in I2
      assert(S1>=0 && S2 >=0 && I2>=ell2);
      double lambda12 = Beta12*S1*I2/N2;
      double lambda22 = Beta22*S2*I2/N2;
      double lambda = lambda12+lambda22;
      double x = (lambda > 0) ? lambda22/lambda : 0;
      int c1 = child[index[parent]];
      int c2 = child[index[parent]+1];
      assert(c1 != c2);
      assert(lineage[c1] != lineage[c2]);
      assert(lineage[c1] != parlin || lineage[c2] != parlin);
      assert(lineage[c1] == parlin || lineage[c2] == parlin);
      if (unif_rand() < x) { // s = (0,2)
        color[lineage[c1]] = host2;
        color[lineage[c2]] = host2;
        if (S2 > 0) {
          S2 -= 1; I2 += 1; ell2 += 1;
          ll += log(lambda)-log(I2*(I2-1)/2);
        } else {
          // the genealogy is incompatible with the state.
          // nevertheless, the state remains valid.
          I2 += 1; N2 += 1; ell2 += 1;
          ll += R_NegInf;
        }
      } else {                  // s = (1,1)
        if (unif_rand() < 0.5) {
          color[lineage[c1]] = host1;
          color[lineage[c2]] = host2;
        } else {
          color[lineage[c1]] = host2;
          color[lineage[c2]] = host1;
        }
        ll -= log(0.5);
        if (S1 > 0) {
          S1 -= 1; I1 += 1; ell1 += 1;
          ll += log(lambda)-log(I1*I2);
        } else {
          // the genealogy is incompatible with the state.
          // nevertheless, the state remains valid.
          I1 += 1; N1 += 1; ell1 += 1;
          ll += R_NegInf;
        }
      }
    } else {
      assert(0);                // #nocov
      ll += R_NegInf;           // #nocov
    }
    assert(nearbyint(N1)==nearbyint(S1+I1+R1));
    assert(nearbyint(N2)==nearbyint(S2+I2+R2));
    assert(check_color(color,nsample,ell1,ell2));
    break;
  }

  // continuous portion of filter equation:
  // take Gillespie steps to the end of the interval.
  if (tmax > t) {

    double rate[nrate], logpi[nrate];
    int event;
    double event_rate = 0;
    double penalty = 0;

    event_rate = EVENT_RATES;
    tstep = exp_rand()/event_rate;

    while (t + tstep < tmax) {
      event = rcateg(event_rate,rate,nrate);
      ll -= penalty*tstep + logpi[event];
      switch (event) {
      case 0:                   // 0: Trans_11, s = (0,0),(1,0)
        assert(S1>=1 && I1>=0);
        S1 -= 1; I1 += 1;
        break;
      case 1:                   // 1: Trans_22, s = (0,0),(0,1)
        assert(S2>=1 && I2>=0);
        S2 -= 1; I2 += 1;
        break;
      case 2:                   // 2: Trans_21, s = (0,0),(1,0)
        assert(S2>=1 && I1>=0);
        S2 -= 1; I2 += 1;
        ll += log(1-ell2/I2);
        assert(!ISNAN(ll));
        break;
      case 3:                   // 3: Trans_21, s = (0,1)
        assert(S2>=1 && I1>=0);
        S2 -= 1; I2 += 1;
        change_color(color,nsample,random_choice(ell1),host1,host2);
        ell1 -= 1; ell2 += 1;
        ll += log(1-ell1/I1)-log(I2);
        assert(check_color(color,nsample,ell1,ell2));
        assert(!ISNAN(ll));
        break;
      case 4:                   // 4: Trans_12, s = (0,0),(0,1)
        assert(S1>=1 && I2>=0);
        S1 -= 1; I1 += 1;
        ll += log(1-ell1/I1);
        assert(!ISNAN(ll));
        break;
      case 5:                   // 5: Trans_12, s = (1,0)
        assert(S1>=1 && I2>=0);
        S1 -= 1; I1 += 1;
        change_color(color,nsample,random_choice(ell2),host2,host1);
        ell2 -= 1; ell1 += 1;
        ll += log(1-ell2/I2)-log(I1);
        assert(check_color(color,nsample,ell1,ell2));
        assert(!ISNAN(ll));
        break;
      case 6:                   // 6: Recov_1
        assert(I1>=1);
        I1 -= 1; R1 += 1;
        break;
      case 7:                   // 7: Recov_2
        assert(I2>=1);
        I2 -= 1; R2 += 1;
        break;
      case 8:                   // 8: Wane_1
        assert(R1>=1);
        R1 -= 1; S1 += 1;
        break;
      case 9:                   // 9: Wane_2
        assert(R2>=1);
        R2 -= 1; S2 += 1;
        break;
      case 10:                  // 10: Birth_1
        assert(N1>=1);
        S1 += 1; N1 += 1;
        break;
      case 11:                  // 11: Birth_2
        assert(N2>=1);
        S2 += 1; N2 += 1;
        break;
      case 12:                  // 12: Death_S1
        assert(S1>=1 && N1>=1);
        S1 -= 1; N1 -= 1;
        break;
      case 13:                  // 13: Death_I1
        assert(I1>=1 && N1>=1);
        I1 -= 1; N1 -= 1;
        break;
      case 14:                  // 14: Death_R1
        assert(R1>=1 && N1>=1);
        R1 -= 1; N1 -= 1;
        break;
      case 15:                  // 15: Death_S2
        assert(S2>=1 && N2>=1);
        S2 -= 1; N2 -= 1;
        break;
      case 16:                  // 16: Death_I2
        assert(I2>=1 && N2>=1);
        I2 -= 1; N2 -= 1;
        break;
      case 17:                  // 17: Death_R2
        assert(R2>=1 && N2>=1);
        R2 -= 1; N2 -= 1;
        break;
      default:                  // #nocov
        assert(0);              // #nocov
        ll += R_NegInf;         // #nocov
        break;                  // #nocov
      }

      ell1 = nearbyint(ell1);
      ell2 = nearbyint(ell2);

      assert(nearbyint(N1)==nearbyint(S1+I1+R1));
      assert(nearbyint(N2)==nearbyint(S2+I2+R2));
      assert(check_color(color,nsample,ell1,ell2));

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
void twospecies_dmeas
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
