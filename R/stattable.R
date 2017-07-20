#' Produce a 2 way summary table of results
#' 
#' Exists as a convenience function to transform raw data into a nice format for
#' other functions by design rather than as a side-effect of combining list with
#' tapply
#' @param rowfactor vector containing factor variable to be included in the rows
#' @param columnfactor vector containing facctor variable to be included in the rows
#' @param outcome vector containing numeric outcome values
#' @param FUN name of summary function to use
#' @param ... other arguments for FUN
#' @keywords 2way summary statistics means table GGEBiplotGUI
#' @export
#' @examples
#' fakedata<-data.frame(expand.grid(Genotype=1:10,Environment=1:10,Rep=1:3),Outcome=rnorm(300))
#' stattable(fakedata$Genotype,fakedata$Environment,fakedata$Outcome,na.rm=TRUE)
stattable<-function(rowfactor,columnfactor,outcome,FUN=mean,...){
tapply(outcome,list(rowfactor,columnfactor),FUN,...)
}