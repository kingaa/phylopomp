##' YAML output
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
    modelSIR = .Call(P_yamlSIR,object),
    modelSIRS = .Call(P_yamlSIR,object),
    modelSEIR = .Call(P_yamlSEIR,object),
    modelS2I2R2 = .Call(P_yamlS2I2R2,object),
    modelSIIR = .Call(P_yamlSIIR,object),
    modelLBDP = .Call(P_yamlLBDP,object),
    modelMoran = .Call(P_yamlMoran,object),
    modelSI2R = .Call(P_yamlSI2R,object),
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
