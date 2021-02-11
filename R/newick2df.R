##' Convert a tree in Newick format to data frame
##'
##' Convert a genealogical tree in Newick format to a data frame suitable for use with \pkg{pomp}.
##'
##' @name newick2df
##' @rdname newick2df
##' 
##' @include package.R
##'
##' @param tree tree data in Newick format.
##' @param time time of the genealogy.
##' @param root_time time of the root.
##'
##' @details
##' If \code{time} is furnished, it is assumed that the absence of samples between the latest leaf and \code{time} is informative.
##'
##' Invisible nodes (labeled 'X_' for any X) are dropped.
##'
##' @return A data frame suitable for use as \code{pomp} input, containing three columns:
##' \describe{
##'   \item{time}{numeric; time of the genealogy event.}
##'   \item{lineages}{integer; the value of the lineage-count function at the specified time.
##'     Note that this function is right-continuous with left limits, and constant on the inter-event intervals.
##'   }
##'   \item{code}{integer; a code describing the nature of the event.
##'     1 indicates a coalescence;
##'     0 indicates a dead sample;
##'    -1 indicates a live sample;
##'     2 indicates the root.
##'   }
##' }
##' 
##' @example examples/newick2df.R
##'
##' @importFrom ape read.tree
##' @importFrom ggplot2 fortify
##' @importFrom dplyr mutate arrange select filter summarize n group_by ungroup
##' @importFrom tidyr separate
##' @importFrom utils globalVariables
##' @importFrom tibble tibble
##'
##' @export
##' 
newick2df <- function (tree, time = NA, root_time = 0) {
  if (missing(tree) || is.null(tree) || !is.character(tree))
    stop(sQuote("tree")," must be furnished as a string in Newick format.",call.=FALSE)
  time <- as.numeric(time)
  root_time <- as.numeric(root_time)
  read.tree(text=tree) %>%
    fortify(ladderize=TRUE) %>%
    separate(label,into=c("type","label")) %>%
    filter(label!="") %>%
    select(label,time=x,isTip) %>%
    arrange(time) %>%
    mutate(
      ##      ng=sum(!isTip)-cumsum(!isTip), # no. of branch-points to right
      ##      nt=sum(isTip)-cumsum(isTip),   # no. of tips to right
      ##      ell=nt-ng                      # no. of lineages
      lineages = sum(isTip) - cumsum(isTip) - sum(!isTip) + cumsum(!isTip)
    ) %>%
    group_by(time) %>%
    summarize(
      lineages=lineages[n()],
      code=1-sum(isTip) # 1 = branch, 0 = dead sample, -1 = live sample
    ) %>%
    ungroup() %>%
    mutate(
      time=time+root_time,
      code=c(2,code[-1]) # root gets code 2
    ) -> dat

  time <- as.numeric(time)
  if (length(time)>0 && !is.na(time)) {
    if (time <= max(dat$time))
      stop(sQuote("time")," should be later than the latest leaf.",call.=FALSE)
    dat %>%
      bind_rows(
        tibble(time=time,lineages=0,code=9) # note that 'code' matches none of the above
      ) -> dat
  }
  
  dat
}

utils::globalVariables(c("lineages","isTip","time","code","type"))
