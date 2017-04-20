#' Discrimination vs. representativeness
#' @param GGEModel A GGEModel produced from a call to GGEModels()
#' @param ... Other arguments sent to GGEPlot()
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=T)
#' GGE<-GGEModels(GxEMeans)
#' GGEPlot(GGE)
#' DiscRep(GGE)
DiscRep<-function(GGEModel,...){
  GGEPlot(GGEModel,type=7,...)
}