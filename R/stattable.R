#' Produce a two-way summary table of results
#'
#' Transforms raw data into a simple two-way table for use in
#' \code{\link[GGEBiplots]{GGEModel}} with row names and column names. By design
#' rather than just a side-effect of combining \code{list} with \code{tapply}
#' @param rowfactor variable to be included in the rows
#' @param columnfactor variable to be included in the columns
#' @param outcome vector containing outcome values
#' @param FUN name of summary function to use
#' @param ... other arguments for \code{FUN}
#' @keywords 2way summary statistics means table GGEBiplotGUI
#' @export
#' @examples
#' simdata<-data.frame(expand.grid(Genotype=1:10,Environment=1:10,Rep=1:3),Outcome=rnorm(300))
#' meantab<-stattable(simdata$Genotype,simdata$Environment,simdata$Outcome,FUN=mean,na.rm=TRUE)
#' GGEPlot(GGEModel(meantab))
stattable<-function(rowfactor,columnfactor,outcome,FUN=mean,...){
tapply(outcome,list(rowfactor,columnfactor),FUN,...)
}
