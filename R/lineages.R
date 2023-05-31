##' Lineage-count function
##'
##' Lineage-counts, saturations, and event-codes.
##'
##' This function extracts from the specified genealogy several important time-varying quantities.
##' These include: \describe{
##'  \item{lineages}{number of lineages through time}
##'  \item{saturation}{the number of lineages emerging from the event}
##'  \item{event_type}{an integer coding the type of event}
##' }
##' 
##' If the genealogy has been obscured (the default), the number in the \code{lineages} returned is the total number of lineages present at the specified time and the saturation is the total saturation.
##' If the genealogy has not been obscured (\code{obscure = FALSE}), the deme-specific data are returned.
##' In this case, the \code{deme} column specifies the pertinent deme.
##'
##' The event types are: \describe{
##'   \item{0}{no event,}
##'   \item{-1}{a root,}
##'   \item{1}{a sample event,}
##'   \item{2}{a non-sample event,}
##'   \item{3}{the end of the time interval, which may or may not coincide with the latest tip of the genealogy.}
##' }
##'
##' @name lineages
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A \code{\link[tibble]{tibble}} containing information about the genealogy.
##' See Details for specifics.
##' The \code{\link[tibble]{tibble}} returned by \code{lineages} has a \code{\link[=plot.gplin]{plot}} method.
##' 
##' @example examples/lineages.R
##' 
##' @rdname lineages
##' @export
lineages <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,lineages=TRUE,prune=prune,obscure=obscure) |>
    getElement("lineages")
}

##' @rdname lineages
##' @method plot gplin
##' @inheritParams treeplot
##' @param ... passed to \code{\link[ggplot2]{theme}}.
##' @importFrom ggplot2 ggplot guides geom_step labs guide_legend
##' @importFrom ggplot2 scale_color_manual theme_classic
##' @importFrom scales hue_pal
##' @importFrom tidyr pivot_longer
##' @importFrom dplyr mutate
##' @export
plot.gplin <- function (
  x, ...,
  palette = scales::hue_pal(l=30,h=c(220,580))
) {
  demes <- sort(unique(x$deme))
  ndeme <- length(demes)
  if (is.function(palette)) {
    palette <- structure(palette(ndeme),names=as.character(demes))
  } else {
    if (length(palette) < ndeme)
      pStop(sQuote("palette")," must have length at least ",ndeme,
        " if specified as a vector.")
  }
  x |>
    ggplot(aes(x=time,y=lineages,color=factor(deme),group=factor(deme)))+
    geom_step()+
    scale_color_manual(values=palette)+
    guides(color="none")+
    theme_classic()+
    theme(...) -> pl
  if (ndeme>1L) {
    pl+
      guides(color=guide_legend(title="deme")) -> pl
  }
  pl
}

##' @importFrom utils globalVariables
globalVariables(
  c("deme","time","lineages")
)

##' @title Coerce to a Data Frame
##' @description Functions to coerce an object to a data frame.
##' @name as.data.frame
##' @rdname as_data_frame
##' @method as.data.frame gplin
##' @keywords internal
##' @inheritParams base::as.data.frame
##' @details
##' An object of class \sQuote{gplin} is coerced to a data frame
##' by means of \code{as.data.frame}.
##' @export
as.data.frame.gplin <- function (x, ...) {
  class(x) <- setdiff(class(x),"gplin")
  as.data.frame(x)
}
