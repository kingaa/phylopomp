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

cc_template <-   r"{// {%name%}: {%descript%} (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
{%state_decls%}
} {%state_type%};

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
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
    break;
  }
}

GENERICS({%name%},{%gen%})
}"

r_template <- r"[##' {%description%}
##' 
##' {%details%}
##' 
##' @name {%rdname%}
##' @family Genealogy processes
##' @aliases {%name%}
{%param_descript%}
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{{%name%}}.
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
  x <- .Call(P_run{%name%},x,time)
  structure(x,model="{%name%}",class="gpsim")
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
  .Call(P_run{%name%},x,time)
}]"

make_model <- function (file) {

  read_yaml(file) -> model
  
  render(
    cc_template,
    name=model$name,
    descript=model$description,
    gen=paste0(tolower(model$name),"_genealogy_t"),
    proc=paste0(tolower(model$name),"_proc_t"),
    param_type=paste0(tolower(model$name),"_parameters_t"),
    state_type=paste0(tolower(model$name),"_state_t"),
    ndeme=model$ndeme,
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

  render(
    template=r_template,
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

for (f in list.files(pattern=r"{.*\.yml$}")) {
  cat("processing",f,"\n")
  make_model(f)
}
