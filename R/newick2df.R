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
##' @param show_branches whether to show number of new branches at coalesecens.
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
##'    -1 indicates a leaf sample;
##'     2 indicates the root;
##'     9 indicates the end time of sampling.
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
newick2df <- function(tree, time = NA, root_time = 0, show_branches = FALSE) {
  if (missing(tree) || is.null(tree) || !is.character(tree))
    stop(sQuote("tree")," must be furnished as a string in Newick format.",call.=FALSE)
  time <- as.numeric(time)
  root_time <- as.numeric(root_time)
  read.tree(text=tree) |>
    fortify(ladderize=TRUE) |>
    arrange(x) |>
    mutate(time = x,
           newbs=(table(parent)[as.character(node)]-1) |> replace_na(0),  # regarding "node", no, of new branches
           lineages=sum(isTip) - cumsum(isTip) - sum(newbs) + cumsum(newbs),
    ) |>
    group_by(time) |> 
    summarize(lineages=lineages[n()]) |> 
    ungroup() |> 
    mutate(time=time+root_time, code=c(2, sign(diff(lineages)))) -> df
  
  if (length(time)>0 && !is.na(time)) {
    if (time <= max(df$time))
      stop(sQuote("time")," should be later than the latest leaf.",call.=FALSE)
    df |>
      bind_rows(
        tibble(time=time,lineages=0,code=9) # note that 'code' matches none of the above
      ) -> df
  }
  
  if (show_branches) {
    df |>
      mutate(branches=if_else(code==1, diff(c(0,lineages)), 0)) -> df
    df[1,"branches"] <- df[1,"lineages"]
  }
  
  df
}

utils::globalVariables(c("lineages","isTip","time","code","type","parent","node","newbs"))
