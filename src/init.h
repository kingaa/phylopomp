#ifndef _INIT_H_
#define _INIT_H_

#include "internal.h"
#include <R_ext/Rdynload.h>

#define DECLARATIONS(X)						                                     \
SEXP make ## X (SEXP Params, SEXP IVPs, SEXP T0);		               \
SEXP revive ## X (SEXP State, SEXP Params);			                    \
SEXP run ## X (SEXP State, SEXP Times);			                        \
SEXP batch ## X (SEXP State);                                     \
SEXP info ## X (SEXP State, SEXP Prune, SEXP Obscure, SEXP Hide,		\
                SEXP T0, SEXP Time, SEXP Descript, SEXP Retimes,		\
                SEXP Yaml, SEXP Structure, SEXP Lineages,	        \
                SEXP Tree, SEXP Compact);			                      \

#define METHODS(X)				                    \
{"make" #X, (DL_FUNC) &make ## X, 3},		   \
{"revive" #X, (DL_FUNC) &revive ## X, 2},	\
{"run" #X, (DL_FUNC) &run ## X, 2},		     \
{"batch" #X, (DL_FUNC) &batch ## X, 1},   \
{"info" #X, (DL_FUNC) &info ## X, 13}		   \

#endif
