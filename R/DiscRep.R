#' Discrimination vs. representativeness
#' 
#' Evaluating the environments based on both discriminating ability and representativeness
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' DiscRep(GGE)
DiscRep<-function(GGEModel,...){
  GGEPlot(GGEModel,type=7,...)
}