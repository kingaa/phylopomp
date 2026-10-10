/* src/init.c */
extern void R_init_phylopomp(DllInfo *);
/* src/bdei_pomp.c */
extern void bdei_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void bdei_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void bdei_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/bdss_pomp.c */
extern void bdss_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void bdss_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void bdss_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/lbdp_pomp.c */
extern void lbdp_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void lbdp_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void lbdp_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/seirs_pomp.c */
extern void seirs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void seirs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void seirs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/si2r_pomp.c */
extern void si2rs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void si2rs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void si2rs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/sirs_pomp.c */
extern void sirs_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void sirs_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void sirs_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/strains_pomp.c */
extern void strains_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void strains_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void strains_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
/* src/twospecies_pomp.c */
extern void twospecies_rinit(double *, const double *, double, const int *, const int *, const int *, const double *);
extern void twospecies_gill(double *, const double *, const int *, const int *, const int *, const double *, double, double);
extern void twospecies_dmeas(double *, const double *, const double *, const double *, int, const int *, const int *, const int *, const int *, const double *, double);
