#' Relationship between environments
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' library(GGEBiplotGUI)
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' EnvRelationship(GGE1)
EnvRelationship<-function(GGEModel,...){
  GGEPlot(GGEModel,type=4,...)
}