/* src/init.c */
extern void R_init_phylopomp(DllInfo *);
/* src/sirs_pomp.c */
extern void sirs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void sirs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void sirs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
