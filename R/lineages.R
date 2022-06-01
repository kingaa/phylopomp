##' Lineage-count function
##'
##' The number of lineages through time
##'
##' @name lineages
##' @include getinfo.R
##'
##' @inheritParams getInfo
##' @return A \code{\link[tibble]{tibble}} containing the lineage count function.
##' If the genealogy has been obscured (the default), the number in the \code{lineages}
##' column is the total number of lineages present at the times in the \code{time} column.
##' If the genealogy has not been obscured (\code{obscure = FALSE}), the deme-specific
##' lineage counts are returned.
##'
##' The \code{\link[tibble]{tibble}} returned by \code{lineages} has a \code{\link[=plot.gplin]{plot}} method.
##' 
##' @example examples/lineages.R
##' 
##' @rdname lineages
##' @export
lineages <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,lineages=TRUE,prune=prune,obscure=obscure) |>
    getElement("lineages") -> x
  lapply(x, function(l) {structure(l,obscured=obscure,class=c("gplin",class(l)))})
}

##' @rdname lineages
##' @method plot gplin
##' @inheritParams treeplot
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
  plot_grid(plotlist=lapply(x, function(l) {
    obsc <- attr(l,"obscured")
    
    if (!obsc) {
      l |>
        pivot_longer(
          -time,
          names_to="deme",
          values_to="lineages"
        ) |>
        mutate(
          deme=gsub("deme","",deme)
        ) -> l
    } else {
      l$deme <- "1"
    }
    
    demes <- sort(unique(l$deme))
    ndeme <- max(1L,length(demes))
    if (is.function(palette)) {
      palette <- structure(palette(ndeme),names=demes)
    } else {
      if (length(palette) < ndeme)
        stop("in ",sQuote("plot.gplin"),": ",sQuote("palette"),
             " must have length at least ",ndeme,
             " if specified as a vector.",call.=FALSE)
    }
    
    l |>
      ggplot(aes(x=time,y=lineages,color=deme,group=deme))+
      geom_step()+
      scale_color_manual(values=palette)+
      guides(color="none")+
      theme_classic()+
      theme(...) -> pl
    
    if (!obsc) {
      pl+
        guides(color=guide_legend()) -> pl
    }
    pl
  }))
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
