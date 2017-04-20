#' Examine an environment
#' @param GGEModel A GGEModel produced from a call to GGEModels()
#' @param selectedE environment to examine. Must be a string which perfectly matches an environment label 
#' @param ... Other arguments sent to GGEPlot()
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=T)
#' GGE<-GGEModels(GxEMeans)
#' GGEPlot(GGE)
#' ExamineEnv(GGE,selectedE="Ayac")
ExamineEnv<-function(GGEModel,selectedE,...){
  GGEPlot(GGEModel,type=2,selectedE=selectedE,...)
}