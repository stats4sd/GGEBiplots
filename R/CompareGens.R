#' Compare the performance of two genotypes across all environments
#' @param GGEModel An object of class "GGEModel" or "gge"
#' @param G1 genotype to compare. Must be a string which perfectly matches a genotype label
#' @param G2 genotype to compare. Must be a string which perfectly matches a genotype label and not equal to G1
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' CompareGens(GGE,"Unica","Canchan")
CompareGens<-function(GGEModel,G1,G2,...){
  GGEPlot(GGEModel,type=5,selectedG1=G1,selectedG2=G2,axis_expand=1.5,...)
}