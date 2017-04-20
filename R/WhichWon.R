#' Which won where/what
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
#' WhichWon(GGE)
WhichWon<-function(GGEModel,axis_expand=1.6,...){
  GGEPlot(GGEModel,axis_expand=axis_expand,type=6,...)
}