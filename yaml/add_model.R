library(yaml)

render <- function (template, ...) {
  vars <- list(...)
  if (length(vars)==0) return(template)
  n <- sapply(vars,length)
  if (!all((n==max(n))|(n==1)))
    stop("in render: ","incommensurate lengths of replacements.") # nocov
  short <- which(n==1)
  n <- max(n)
  for (i in short) vars[[i]] <- rep(vars[[i]],n)
  retval <- vector(mode="list",length=n)
  for (i in seq_len(n)) {
    tpl <- template
    for (v in names(vars)) {
      src <- sprintf("\\{%%%s%%\\}",v)
      tgt <- vars[[v]][i]
      tpl <- gsub(src,tgt,tpl)
    }
    retval[[i]] <- tpl
  }
  do.call(paste0,retval)
}

make_model <- function (model) {

  ## Render the model-specific C++ file
  r"{// {%name%}: {%descript%} (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

{%demenames%}

//! {%name%} process state.
typedef struct {
{%state_decls%}
} {%state_type%};

//! {%name%} process parameters.
typedef struct {
{%param_decls%}
} {%param_type%};

using {%proc%} = popul_proc_t<{%state_type%},{%param_type%},{%nevent%}>;
using {%gen%} = master_t<{%proc%},{%ndeme%}>;

template<>
std::string {%proc%}::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    {%yaml_params%};
  std::string s = tab + "state:\n"
    {%yaml_states%};
  return p+s;
}

template<>
void {%proc%}::update_params (double *p, int n) {
  int m = 0;
  {%set_params%}
  if (m != n) err("wrong number of parameters!");
}

template<>
void {%proc%}::update_IVPs (double *p, int n) {
  int m = 0;
  {%set_ivps%}
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double {%proc%}::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  {%rates%}
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void {%gen%}::rinit (void) {
  {%rinit%}
}

template<>
void {%gen%}::jump (int event) {
  switch (event) {
  {%jumps%}
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t {%proc%}::n_integer_elements() const {
  return {%n_int%};  // Number of integer state variables
}

template<>
size_t {%proc%}::n_double_elements() const {
  return {%n_dbl%};  // Number of double state variables
}

static const char* {%name%}_int_names[] = {%int_names%};
static const char* {%name%}_dbl_names[] = {%dbl_names%};

template<>
const char** {%proc%}::integer_names() const {
  return {%name%}_int_names;
}

template<>
const char** {%proc%}::double_names() const {
  return {%name%}_dbl_names;
}

template<>
void {%proc%}::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const {%state_type%}& s = state_history[i];
  {%int_assignments%}
  {%dbl_assignments%}
}

extern "C" {
  SEXP get_states_{%name%} (SEXP State) {
    {%gen%} x(State);
    return x.get_states();
  }
}

GENERICS({%name%},{%gen%})
}" |>
  render(
    name=model$name,
    descript=model$description,
    gen=paste0(tolower(model$name),"_genealogy_t"),
    proc=paste0(tolower(model$name),"_proc_t"),
    param_type=paste0(tolower(model$name),"_parameters_t"),
    state_type=paste0(tolower(model$name),"_state_t"),
    ndeme=length(model$demes),
    n_int=sum(sapply(model$state, function(x) x$type == "int")),  # New: Number of integer states
    n_dbl=sum(sapply(model$state, function(x) x$type == "double")), # New: Number of double states
    int_names=sprintf('{"' |> paste0(
      paste(sapply(model$state[sapply(model$state, function(x) x$type == "int")],
                   function(x) x$name), collapse='", "'),
      '"}')
    ),
    dbl_names=sprintf('{"' |> paste0(
      paste(sapply(model$state[sapply(model$state, function(x) x$type == "double")],
                   function(x) x$name), collapse='", "'),
      '"}')
    ),
    int_assignments=paste(mapply(
      function(idx, name) sprintf("  intg[%d] = s.%s;", idx-1, name),
      seq_along(model$state[sapply(model$state, function(x) x$type == "int")]),
      sapply(model$state[sapply(model$state, function(x) x$type == "int")], function(x) x$name)
    ), collapse="\n"),
    dbl_assignments=paste(mapply(
      function(idx, name) sprintf("  dbl[%d] = s.%s;", idx-1, name),
      seq_along(model$state[sapply(model$state, function(x) x$type == "double")]),
      sapply(model$state[sapply(model$state, function(x) x$type == "double")], function(x) x$name)
    ), collapse="\n"),
    demenames=paste(
      mapply(
        \(d,n) {
          render(
            r"{static int {%name%} = {%number%};}",
            name=d,
            number=n-1L
          )
        },
        d=model$demes,
        n=seq_along(model$demes)
      ),
      collapse="\n"
    ),
    nevent=length(model$events),
    param_decls=paste(
      lapply(
        c(model$parameter,model$ivp),
        \(p) render("  {%type%} {%var%};",type=p$type,var=p$name)
      ),
      collapse="\n"
    ),
    state_decls=paste(
      lapply(
        model$state,
        \(p) render("  {%type%} {%var%};",type=p$type,var=p$name)
      ),
      collapse="\n"
    ),
    yaml_params=paste(
      lapply(
        c(model$parameter,model$ivp),
        \(p) render(r"{+ YAML_PARAM({%var%})}",var=p$name)
      ),
      collapse="\n    "
    ),
    yaml_states=paste(
      lapply(
        model$state,
        \(p) render(r"{+ YAML_STATE({%var%})}",var=p$name)
      ),
      collapse="\n    "
    ),
    set_params=paste(
      lapply(
        model$parameter,
        \(p) render(r"{PARAM_SET({%var%});}",var=p$name)
      ),
      collapse="\n  "
    ),
    set_ivps=paste(
      lapply(
        model$ivp,
        \(p) render(r"{PARAM_SET({%var%});}",var=p$name)
      ),
      collapse="\n  "
    ),
    rates=paste(
      lapply(
        model$events,
        \(p) render(r"{RATE_CALC({%rate%});}",rate=p$rate)
      ),
      collapse="\n  "
    ),
    rinit=model$rinit,
    jumps=paste(
      lapply(
        seq_along(model$events),
        \(n)
        render(
          "case {%n%}:\n      {%jump%}\n      break;",
          n=n-1,
          jump=model$events[[n]]$jump
        )
      ),
      collapse="\n    "
    )
  ) |>
  cat(file=sprintf("src/%s.cc",tolower(model$name)))

  ## Render the model-specific R file
  ## This should be minimally sufficient, but edits can be made to improve it.
  r"[##' {%description%}
##'
##' {%details%}
##'
##' @name {%rdname%}
##' @family Genealogy processes
##' @aliases {%name%}
{%param_descript%}
##' @inheritParams sir
##' @return \code{run{%name%}} and \code{continue{%name%}} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{{%name%}}.
##'
NULL

##' @rdname {%rdname%}
##' @export
run{%name%} <- function (
  time, t0 = 0,
  {%params%}
) {
  params <- c({%paramvec%})
  ivps <- c({%ivpvec%})
  x <- .Call(P_make{%name%},params,ivps,t0)
  .Call(P_run{%name%},x,time) |>
    structure(model="{%name%}",class=c("gpsim","gpgen"))
}

##' @rdname {%rdname%}
##' @export
continue{%name%} <- function (
  object, time,
  {%params_na%}
) {
  params <- c(
    {%paramvec%}
  )
  x <- .Call(P_revive{%name%},object,params)
  .Call(P_run{%name%},x,time) |>
    structure(model="{%name%}",class=c("gpsim","gpgen"))
}]" |>
  render(
    name=model$name,
    rdname=tolower(model$name),
    description=model$description,
    details=model$details,
    param_descript=paste(
      lapply(
        c(model$parameter,model$ivp),
        \(p) render(template="##' @param {%name%} {%description%}",
          name=p$name,description=p$description)
      ),
      collapse="\n"),
    params=paste(
      lapply(
        c(model$parameter,model$ivp),
        \(p) render(r"{{%name%} = {%default%}}",
          name=p$name, default=p$default
        )
      ),
      collapse=", "
    ),
    params_na=paste(
      lapply(
        model$parameter,
        \(p) render(r"{{%name%} = NA}",name=p$name)
      ),
      collapse=", "
    ),
    paramvec=paste(
      lapply(
        model$parameter,
        \(p) render(r"{{%name%}={%name%}}",name=p$name)
      ),
      collapse=","
    ),
    ivpvec=paste(
      lapply(
        model$ivp,
        \(p) render(r"{{%name%}={%name%}}",name=p$name)
      ),
      collapse=","
    )
  ) |>
  cat(file=sprintf("R/%s.R",tolower(model$name)))
  invisible(NULL)
}

## Render the package 'init.c' file.
render_init_c_file <- function (models) {
  models <- sapply(models,getElement,"name")
  r"{#include "init.h"
#include "decls.h"
#include "pomplink.h"

get_userdata_t *get_userdata;
get_userdata_double_t *get_userdata_double;
get_userdata_int_t *get_userdata_int;

SEXP parse_newick (SEXP, SEXP, SEXP);
SEXP getInfo (SEXP);
SEXP curtail (SEXP, SEXP);
SEXP yaml (SEXP);
SEXP gendat (SEXP);

// for each model, there must be
// one DECLARATIONS line and one METHODS line.

{%declarations%}

static const R_CallMethodDef callMethods[] = {
{%methods%}
  {"parse_newick", (DL_FUNC) &parse_newick, 3},
  {"curtail", (DL_FUNC) &curtail, 2},
  {"yaml", (DL_FUNC) &yaml, 1},
  {"gendat", (DL_FUNC) &gendat, 1},
  {NULL, NULL, 0}
};

static const R_CallMethodDef extMethods[] = {
  {"getInfo", (DL_FUNC) &getInfo, -1},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,extMethods);
  R_useDynamicSymbols(info,TRUE);
  //  R_useDynamicSymbols(info,FALSE);
  //  R_forceSymbols(info,TRUE);
  get_userdata = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
  get_userdata_double = (get_userdata_double_t*) R_GetCCallable("pomp","get_userdata_double");
  get_userdata_int = (get_userdata_int_t*) R_GetCCallable("pomp","get_userdata_int");
}
}" |>
  render(
    declarations=paste(sprintf("DECLARATIONS(%s)",models),collapse="\n"),
    methods=paste(sprintf("  METHODS(%s),",models),collapse="\n")
  ) |>
  cat(file="src/init.c")
  invisible(NULL)
}

## Render the 'R/yaml.R' file.
render_yaml_R_file <- function (models) {
  models <- sapply(models,getElement,"name")
  lapply(
    models,
    \(y) render(
           r"[    model{%model%} = .Call(P_yaml{%model%},object),]",
           model=y
         )
  ) |>
    paste(collapse="\n") -> yaml_calls

  r"{##' YAML output
##'
##' Human- and machine-readable description.
##'
##' @name yaml
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A string in YAML format, with class \dQuote{gpyaml}.
##' @examples
##' simulate("SIIR",time=1) |> yaml()
##'
NULL

##' @rdname yaml
##' @export
yaml <- function (object) {
  switch(
    paste0("model",as.character(attr(object,"model"))),
{%calls%}
    model = .Call(P_yaml,object),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  ) |>
    structure(class="gpyaml")
}

##' @importFrom yaml as.yaml
##' @docType import
##' @export
yaml::as.yaml

##' @importFrom yaml read_yaml
##' @docType import
##' @export
yaml::read_yaml

##' @rdname internals
##' @method print gpyaml
##' @export
print.gpyaml <- function (x, ...) {
  cat(x)
}
}" |>
  render(
    calls=yaml_calls
  ) |>
  cat(file="R/yaml.R")
  invisible(NULL)
}

## Render the 'R/geneal.R' file.
render_geneal_R_file <- function (models) {
  models <- sapply(models,getElement,"name")
  lapply(
    models,
    \(y) render(
           r"[    model{%model%} = .Call(P_geneal{%model%},object),]",
           model=y
         )
  ) |>
    paste(collapse="\n") -> calls

  r"{##' Bare genealogy
##'
##' Extracts the bare genealogy from a Markov genealogy process simulation
##'
##' @name geneal
##' @include package.R
##' @param object a \sQuote{gpgen} object.
##' @return A bare genealogy object.
##' @rdname geneal
##' @export
geneal <- function (object) {
  switch(
    paste0("model",as.character(attr(object,"model"))),
{%calls%}
    model = structure(object,class=c("gpgen")),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  )
}
}" |>
  render(
    calls=calls
  ) |>
  cat(file="R/geneal.R")
  invisible(NULL)
}

## Render the 'R/simulate.R' file.
render_simulate_R_file <- function (models) {
  models |>
    lapply(
      \(y) render(
             r"{      "- {%name%}: {%description%}\\n",}",
             name=y$name,
             description=y$description
           )
    ) |>
    paste(collapse="\n") -> descriptions

  models |>
    lapply(
      \(y) render(
             r"[    model{%model%} = run{%model%}(time=time,...),]",
             model=y$name
           )
    ) |>
    paste(collapse="\n") -> sim_calls

  models |>
    lapply(
      \(y) render(
             r"[    model{%model%} = continue{%model%}(object,time=time,...),]",
             model=y$name
           )
    ) |>
    paste(collapse="\n") -> cont_calls

  r"{##' simulate
##'
##' Simulate Markov genealogy processes
##'
##' @name simulate
##' @include getinfo.R seir.R sir.R siir.R si2r.R lbdp.R moran.R
##' @family Genealogy processes
##' @param object either the name of the model to simulate
##' \emph{or} a previously computed \sQuote{gpsim} object
##' @param ... additional arguments to the model-specific simulation functions
##' @return An object of \sQuote{gpsim} class.
##' @references
##' \King2024
##'
##' \King2022
NULL

##' @rdname simulate
##' @export
simulate <- function (object, ...) {
  UseMethod("simulate")
}

##' @rdname simulate
##' @method simulate default
##' @export
simulate.default <- function (object, ...) {
  if (missing(object) || is.null(object))
    message(
      "Available phylopomp models:\n",
{%modellist%}
      "- SIRS: synonymous with SIR\n",
      "- SEIRS: synonymous with SEIR\n",
      "\n"
    )
  else
    pStop(
      sQuote("object")," must be specified as either ",
      "the name of a model or the result of a previous simulation.\n",
      "Do ",sQuote("simulate()")," to view available models."
    )
}

##' @rdname simulate
##' @method simulate character
##' @param time end timepoint of simulation
##' @export
simulate.character <- function (object, time, ...) {
  switch(
    paste0("model",object),
{%simcalls%}
    modelSIRS = runSIR(time=time,...),
    modelSEIRS = runSEIR(time=time,...),
    pStop_("unrecognized model: ",sQuote(object),".\n",
      "Do ",sQuote("simulate()")," to view available models.")
  )
}

##' @rdname simulate
##' @method simulate gpsim
##' @details
##' When \code{object} is of class \sQuote{gpsim}, i.e., the result of a genealogy-process
##' simulation, \code{simulate} acts to continue the simulation to a later timepoint.
##' Note that, one cannot change initial conditions or \code{t0} when continuing a simulation.
##'
##' @export
simulate.gpsim <- function (object, time, ...) {
  model <- as.character(attr(object,"model"))
  switch(
    paste0("model",model),
{%contcalls%}
    model = pStop_("no model attribute detected."),
    pStop_("unrecognized model ",sQuote(model),".")
  )
}
}" |>
  render(
    modellist=descriptions,
    simcalls=sim_calls,
    contcalls=cont_calls
  ) |>
  cat(file="R/simulate.R")
  invisible(NULL)
}

# Add render_get_states_R_file function
render_get_states_R_file <- function (models) {
  models <- sapply(models,getElement,"name")
  lapply(
    models,
    \(y) render(
      r"[    model{%model%} = .Call(P_get_states_{%model%},object),]",
      model=y
    )
  ) |>
    paste(collapse="\n") -> get_states_calls

  r"{##' Get population state history
##'
##' Retrieves the history of population states (compartment sizes over time)
##' from a Markov genealogy process simulation
##'
##' @name get_states
##' @param object A \sQuote{gpsim} object
##' @return A \code{\link[tibble]{tibble}} containing the population state history
##' @export
get_states <- function(object) {
  if (!inherits(object, "gpsim")) {
    stop("object must be a gpsim object")
  }

  switch(
    paste0("model", as.character(attr(object, "model"))),
{%calls%}
    stop("unrecognized model ", sQuote(attr(object, "model")))
  ) |>
    tibble::as_tibble()
}
}" |>
  render(
    calls=get_states_calls
  ) |>
  cat(file="R/get_states.R")
invisible(NULL)
}

files <- list.files(pattern=r"{.*\.yml$}")
models <- list()
for (f in files) {
  cat("processing",f,"\n")
  read_yaml(f) -> model
  make_model(model)
  models[[f]] <- model
}
render_init_c_file(models)
render_yaml_R_file(models)
render_geneal_R_file(models)
render_simulate_R_file(models)
render_get_states_R_file(models)
