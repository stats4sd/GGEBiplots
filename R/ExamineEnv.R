#' Examine an environment 
#' 
#' Ranking the cultivars based on their performance in any given environment
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param Env environment to examine. Must be a string which perfectly matches an environment label 
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' ExamineEnv(GGE,"Ayac")
ExamineEnv<-function(GGEModel,Env,...){
  GGEPlot(GGEModel,type=2,selectedE=Env,...)
}