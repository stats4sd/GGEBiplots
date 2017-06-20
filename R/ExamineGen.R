#' Examine a genotype
#' 
#' Ranking the environments based on the relative performance of any given cultivar
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param Gen genotype to examine. Must be a string which perfectly matches an genotype label 
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' ExamineGen(GGE,"Unica")
ExamineGen<-function(GGEModel,Gen,...){
  GGEPlot(GGEModel,type=3,selectedG=Gen,...)
}