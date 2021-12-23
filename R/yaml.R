##' YAML output
##'
##' Human- and machine-readable description
##'
##' @name yaml
##' @include getinfo.R
##'
##' @inheritParams getInfo
##' @return A string in yaml format.
##' @examples
##' simulate("SIIR",time=1) |> yaml() |> cat()
##' 
##' @rdname yaml
##' @export
yaml <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,yaml=TRUE,prune=prune,obscure=obscure) |>
    getElement("yaml")
}

##' @importFrom yaml as.yaml
##' @docType import
##' @export
yaml::as.yaml

##' @importFrom yaml read_yaml
##' @docType import
##' @export
yaml::read_yaml
