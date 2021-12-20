#' Which Won Where/What Biplot
#'
#' Identifying the 'best' cultivar in each environment
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' WhichWon(GGE1)
WhichWon<-function(GGEModel,...){
  GGEPlot(GGEModel,type=6,...)
}
