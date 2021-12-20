#' Mean vs. Stability Biplot
#'
#' Evaluating cultivars based on both average yield and stability
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' MeanStability(GGE1)
MeanStability<-function(GGEModel,...){
  GGEPlot(GGEModel,type=9,...)
}
