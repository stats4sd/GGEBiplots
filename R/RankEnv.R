#' Ranking environments with respect to the ideal environment
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' RankEnv(GGE)
RankEnv<-function(GGEModel,...){
  GGEPlot(GGEModel,type=8,...)
}