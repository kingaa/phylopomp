/* src/init.c */
extern void R_init_phylopomp(DllInfo *);
/* src/lbdp_pomp.c */
extern void lbdp_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void lbdp_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void lbdp_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/seirs_pomp.c */
extern void seirs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void seirs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void seirs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/sirs_pomp.c */
extern void sirs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void sirs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void sirs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/twospecies_pomp.c */
extern void twospecies_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void twospecies_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void twospecies_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
