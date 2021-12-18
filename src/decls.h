#ifndef _PHYLOPOMP_DECLS_H_
#define _PHYLOPOMP_DECLS_H_

#ifdef __cplusplus
extern "C" {
#endif

  SEXP makeSIR (SEXP Params, SEXP ICs, SEXP T0);
  SEXP reviveSIR (SEXP State, SEXP Params);
  SEXP runSIR (SEXP State, SEXP Times);
  SEXP infoSIR (SEXP State, SEXP Prune, SEXP T0, SEXP Time, SEXP Descript,
		SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree, SEXP Compact);

  SEXP makeSIIR (SEXP Params, SEXP ICs, SEXP T0);
  SEXP reviveSIIR (SEXP State, SEXP Params);
  SEXP runSIIR (SEXP State, SEXP Times);
  SEXP infoSIIR (SEXP State, SEXP Prune, SEXP T0, SEXP Time, SEXP Descript,
		 SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree, SEXP Compact);

#ifdef __cplusplus
}
#endif

#endif
