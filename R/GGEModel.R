#' Produces genotype plus genotype-by-environment model from a 2-way table of 
#' means
#' 
#' Calculates the GGE model where presented with a two way table of means with 
#' genotypes in rows, where genotype names are set as row names, and 
#' environments in columns, where environment names are set as column names.
#' This function serves as a command line interface to the internal code
#' contained within \code{\link[GGEBiplotGUI]{GGEBiplot}}. For dealing with
#' missing data then a better implementation is available through
#' \code{\link[gge]{gge}}.
#' 
#' @param Data a data frame or matrix containing genotype by environment means 
#'   with the genotypes in rows and the environments in columns. row names and 
#'   column names should be set to indicate the genotype names and environment 
#'   names.
#' @param centering centering method. Either "tester" for tester centered 
#'   (G+GE), "global" for global centered (E+G+GE), "double" for double centred 
#'   (GE) or "none" for no centering. Models produced without centering cannot 
#'   be used in the \code{\link[GGEBiplots]{GGEPlot}} function.
#' @param scaling scaling method. Either "sd" for standard deviation or "none" 
#'   for no scaling.
#' @param SVP method for singular value partitioning. Either 
#'   "row","column","dual" or "symmetrical".
#' @return A list of class \code{GGEModel} containing: 
#'   \item{coordgenotype}{plotting coordinates for genotypes from all
#'   components} \item{coordenviroment}{plotting coordinates for environments
#'   from all components} \item{eigenvalues}{vector of eigenvalues from each
#'   component} \item{vartotal}{overall variance} \item{varexpl}{percentage of
#'   variance explained by each component} \item{labelgen}{genotype names} 
#'   \item{labelenv}{environment names} \item{axes}{axis labels} 
#'   \item{Data}{scaled and centered input data} \item{centering}{name of
#'   centering method} \item{scaling}{name of scaling method} \item{SVP}{name of
#'   SVP method}
#' @references Yan W, Kang M (2003). \emph{GGE Biplot Analysis: A Graphical Tool
#'   for Breeders, Geneticists, and Agronomists}. CRC Press.
#' @references Yan W, Kang M (2002). \emph{Singular-Value Partitioning in Biplot
#'   Analysis of Multienvironment Trial Data}. Agronomy Journal, 94, 990-996. 
#'   \url{http://dx.doi.org/10.2134/agronj2002.0990}
#' @keywords GGE biplot ggplot2
#' @export
#' @examples
#' library(GGEBiplotGUI)
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' GGEPlot(GGE1)
#' @importFrom stats var

GGEModel <- function(Data,centering="tester",scaling="none",SVP="column") {        
  labelgen <- rownames(Data)
  labelenv <- colnames(Data)        
  Data<-as.matrix(Data)
  
  if(any(is.na(Data))){stop("missing data in input data frame")}
  if(any(apply(Data,2,is.numeric)==FALSE)){stop("not all columns are of class 'numeric'")}
  
  if(!(centering%in%c("none","tester","global","double")|centering%in%0:3)){
    warning(paste("centering method",centering,"not found; defaulting to tester centered"))
    centering="tester"
  }
  if(!(SVP%in%c("row","column","dual","symmetrical","gge")|SVP%in%0:4)){
    warning(paste("SVP method",SVP,"not found; defaulting to column metric preserving"))
    SVP="column"
  }
  if(!(scaling%in%c("sd","none")|scaling%in%0:1)){
    warning(paste("scaling method",scaling,"not found; defaulting to no scaling"))
    sd="none"
  }
  labelaxes <- paste("Component ",1:ncol(diag(svd(Data)$d)), sep = "")
  
  
  # Centering options
  if(centering==1|centering=="global"){
    meanData = mean(Data)
    Data <- Data - meanData
  }
  if(centering==2|centering=="tester"){
    meancolData = colMeans(Data)
    for (i in 1:nrow(Data)){
      for (j in 1:ncol(Data)){ 
        Data[i,j] <- Data[i, j] - meancolData[j]
      }}
  }
  if(centering==3|centering=="double"){
    meanData = mean(Data)
    meancolData = colMeans(Data)
    meanrowData = rowMeans(Data)
    for (i in 1:nrow(Data)){
      for (j in 1:ncol(Data)){
        Data[i,j] <- Data[i, j] + meanData - meancolData[j] -meanrowData[i]
      }}
  }
  
  # Scaling options
  if(scaling==1|scaling=="sd"){
    desviation <- array(, dim = ncol(Data))
    for (j in 1:ncol(Data)){ desviation[j] <- sqrt(var(Data[,j]))}
    for (i in 1:nrow(Data)){ for (j in 1:ncol(Data)){ Data[i,j] <- Data[i, j]/desviation[j]}}
  }
  
  
  # SVP options
  
  if(SVP==0|SVP=="gge"){
    coordgenotype <- svd(Data)$u * sqrt(nrow(Data)-1)
    coordenviroment  = t(Data) %*% svd(Data)$u / sqrt(nrow(Data)-1)
    d1 = (max(coordenviroment[, 1]) - min(coordenviroment[,1]))/(max(coordgenotype[, 1]) -  min(coordgenotype[, 1]))
    d2 = (max(coordenviroment[, 2]) - min(coordenviroment[,2]))/(max(coordgenotype[, 2]) -  min(coordgenotype[, 2]))
    d = max(d1, d2)
    coordenviroment <- coordenviroment/d
  }
  
  
  
  if(SVP==1|SVP=="row"){
    coordgenotype <- svd(Data)$u %*% diag(svd(Data)$d)
    coordenviroment <- svd(Data)$v
    d1 = (max(coordenviroment[, 1]) - min(coordenviroment[,1]))/(max(coordgenotype[, 1]) -  min(coordgenotype[, 1]))
    d2 = (max(coordenviroment[, 2]) - min(coordenviroment[,2]))/(max(coordgenotype[, 2]) -  min(coordgenotype[, 2]))
    d = max(d1, d2)
    coordenviroment <- coordenviroment/d
  }
  
  if(SVP==2|SVP=="column"){
    coordgenotype <- svd(Data)$u
    coordenviroment <- svd(Data)$v %*% diag(svd(Data)$d)
    d1 = (max(coordgenotype[, 1]) - min(coordgenotype[,1]))/(max(coordenviroment[, 1]) -  min(coordenviroment[, 1]))
    d2 = (max(coordgenotype[, 2]) - min(coordgenotype[,2]))/(max(coordenviroment[, 2]) -  min(coordenviroment[, 2]))
    d = max(d1, d2)
    coordgenotype <- coordgenotype/d
  }
  if(SVP==3|SVP=="dual"){
    coordgenotype <- svd(Data)$u %*% diag(svd(Data)$d)
    coordenviroment <- svd(Data)$v %*% diag(svd(Data)$d)
  }
  if(SVP==4|SVP=="symmetrical"){
    coordgenotype <- svd(Data)$u %*% diag(sqrt(svd(Data)$d))
    coordenviroment <- svd(Data)$v %*% diag(sqrt(svd(Data)$d))
  }

  eigenvalues = svd(Data)$d
  
  vartotal = round(as.numeric(sum(eigenvalues^2)),2)
  varexpl = round(as.numeric((eigenvalues^2/vartotal) *100), 2)
  
  
  GGEModel=list(coordgenotype=coordgenotype,coordenviroment=coordenviroment,
                eigenvalues=eigenvalues,vartotal=vartotal,varexpl=varexpl,labelgen=labelgen,labelenv=labelenv,labelaxes=labelaxes,Data=Data,
                centering=centering,scaling=scaling,SVP=SVP)
  class(GGEModel)<-"GGEModel"
  return(GGEModel) 
}


