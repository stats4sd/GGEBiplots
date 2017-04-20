#' Examine a genotype
#' @param GGEModel A GGEModel produced from a call to GGEModels()
#' @param selectedG genotype to examine. Must be a string which perfectly matches an genotype label 
#' @param ... Other arguments sent to GGEPlot()
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=T)
#' GGE<-GGEModels(GxEMeans)
#' GGEPlot(GGE)
#' ExamineGen(GGE,selectedG="Unica")
ExamineGen<-function(GGEModel,selectedE,...){
  GGEPlot(GGEModel,type=3,selectedE=selectedE,...)
}