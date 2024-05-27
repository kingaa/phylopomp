/* src/init.c */
extern void R_init_phylopomp(DllInfo *info);
/* src/lbdp_pomp.c */
extern void lbdp_rinit(double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars);
extern void lbdp_gill(double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double Rf_dt);
extern void lbdp_dmeas(double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t);
/* src/seirs_pomp.c */
extern void seirs_rinit(double *__x, const double *__p, double t0, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars);
extern void seirs_gill(double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double Rf_dt);
extern void seirs_dmeas(double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t);
/* src/sirs_pomp.c */
extern void sirs_rinit(double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars);
extern void sirs_gill(double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double Rf_dt);
extern void sirs_dmeas(double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t);
