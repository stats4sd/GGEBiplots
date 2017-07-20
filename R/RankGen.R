#' 
#' 
#' Ranking genotypes with respect to the ideal genotype
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(GGEBiplotGUI)
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' RankGen(GGE1)
RankGen<-function(GGEModel,...){
  GGEPlot(GGEModel,type=10,...)
}