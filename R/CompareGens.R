#' Compare two genotypes
#' @param GGEModel A GGEModel produced from a call to GGEModels()
#' @param selectedG1 genotype to compare when type=5. Must be a string which perfectly matches a genotype label
#' @param selectedG2 genotype to compare when type=5. Must be a string which perfectly matches a genotype label and not equal to selectedG1
#' @param ... Other arguments sent to GGEPlot()
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=T)
#' GGE<-GGEModels(GxEMeans)
#' GGEPlot(GGE)
#' CompareGens(GGE,selectedG1="Unica",selectedG2="Canchan")
CompareGens<-function(GGEModel,selectedG1,selectedG2,...){
  GGEPlot(GGEModel,type=5,selectedG1=selectedG1,selectedG2=selectedG2,axis_expand=1.5,...)
}