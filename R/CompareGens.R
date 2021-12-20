#' Compare two genotypes biplot
#'
#' Compare the performance of two genotypes across all environments
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param G1 genotype to compare. Must be a string which matches a genotype label
#' @param G2 genotype to compare. Must be a string which matches a genotype label and not equal to G1
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' CompareGens(GGE1,"cas","luc")
CompareGens<-function(GGEModel,G1,G2,...){
  GGEPlot(GGEModel,type=5,selectedG1=G1,selectedG2=G2,...)
}
