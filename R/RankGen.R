#' Ranking genotypes with respect to the ideal genotype
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param axis_expand multiplication factor to expand the axis limits by to
#'   enable fitting of labels. Defaults to 1.4 for genotype ranking plot as the
#'   circles usually extend beyond limits of the other biplot types.
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' RankGen(GGE1)
RankGen<-function(GGEModel,axis_expand=1.4,...){
  GGEPlot(GGEModel,type=10,axis_expand=axis_expand,...)
}
