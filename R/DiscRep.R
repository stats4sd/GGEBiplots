#' Discrimination vs. representativeness biplot
#' 
#' Evaluating the environments based on both discriminating ability and representativeness
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(GGEBiplotGUI)
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' DiscRep(GGE1)
DiscRep<-function(GGEModel,...){
  GGEPlot(GGEModel,type=7,...)
}