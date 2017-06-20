#' Which won where/what
#' 
#' Identifying the 'best' cultivar in each environment
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param axis_expand multiplication factor to expand the axis limits by to enable fitting of labels. Defaults to 1.6 for the Which Won Plot
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' WhichWon(GGE)
WhichWon<-function(GGEModel,axis_expand=1.6,...){
  GGEPlot(GGEModel,axis_expand=axis_expand,type=6,...)
}