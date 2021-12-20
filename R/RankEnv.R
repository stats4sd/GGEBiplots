#' Ranking Environments Biplot
#'
#' Ranking environments with respect to the ideal environment
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' RankEnv(GGE1)
RankEnv<-function(GGEModel,...){
  GGEPlot(GGEModel,type=8,...)
}
