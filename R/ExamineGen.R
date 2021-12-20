#' Examine a genotype biplot
#'
#' Ranking the environments based on the relative performance of any given cultivar
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param Gen genotype to examine. Must be a string which perfectly matches an genotype label
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' ExamineGen(GGE1,"cas")
ExamineGen<-function(GGEModel,Gen,...){
  GGEPlot(GGEModel,type=3,selectedG=Gen,...)
}
