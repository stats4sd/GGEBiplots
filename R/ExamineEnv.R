#' Examine an environment
#'
#' Ranking the cultivars based on their performance in any given environment
#' @param GGEModel An object of class \code{GGEModel} or \code{gge}
#' @param Env environment to examine. Must be a string which matches an
#'   environment label
#' @param ... Other arguments sent to \code{\link[GGEBiplots]{GGEPlot}}
#' @keywords GGE
#' @export
#' @examples
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' ExamineEnv(GGE1,"WP93")
ExamineEnv<-function(GGEModel,Env,...){
  GGEPlot(GGEModel,type=2,selectedE=Env,...)
}
