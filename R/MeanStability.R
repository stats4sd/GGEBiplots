#' Mean vs Stability
#' 
#' Evaluating cultivars based on both average yield and stability 
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' MeanStability(GGE)
MeanStability<-function(GGEModel,...){
  GGEPlot(GGEModel,type=9,...)
}