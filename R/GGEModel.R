#' Produces GGE model from 2-way table of means
#' 
#' Calculates the GGE model where presented with a two way table of means with genotypes in rows (and genotype names as row names) and environments in columns (and environment names as column names)
#' Code to produce the model taken with very few modifications from internal code within \code{\link[GGEBiplotGUI]{GGEBiplot}}
#' For dealing with missing data then please use the \code{\link[gge]{gge}}
#'
#' @param Data wide data frame containing GxE means with genotypes in rows and environments in columns with rownames and colnames indicating genotype names and environment names
#' @param centering centering method. Options are "tester", "global","double" or "none" for tester centered (G+GE), global centered (E+G+GE), double centred (GE) or no centering. 
#' Models produced without centering cannot be used in the GGEPlot() function. Defaults to tester centered.
#' @param scaling scaling method. Options are "sd" or "none"
#' @param SVD method for singular value partitioning. Options are "row","column","dual" or "symmetrical". Defaults to "column"
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=TRUE)
#' GGE<-GGEModel(GxEMeans)
#' GGEPlot(GGE)
#' @importFrom stats var

GGEModel <- function(Data,centering="tester",scaling="none",SVD="column") {        
  labelgen <- rownames(Data)
  labelenv <- colnames(Data)        
  matrixdata <- matrix(Data, nrow(Data), ncol(Data))
  
  if(any(is.na(matrixdata))){stop("missing data in input data frame")}
  if(any(apply(matrixdata,2,is.numeric)==FALSE)){stop("non-numeric columns in input data frame")}
  
  if(!(centering%in%c("none","tester","global","double")|centering%in%0:3)){
    warning(paste("centering method",centering,"not found; defaulting to tester centered"))
    centering="tester"
  }
  if(!(SVD%in%c("row","column","dual","symmetrical")|SVD%in%1:4)){
    warning(paste("SVD method",SVD,"not found; defaulting to column metric preserving"))
    SVD="column"
  }
  if(!(scaling%in%c("sd","none")|scaling%in%0:1)){
    warning(paste("scaling method",scaling,"not found; defaulting to no scaling"))
    sd="none"
  }
  ejes <- paste("AXIS",1:ncol(diag(svd(matrixdata)$d)), sep = "")
  
  
  # Centering options
  if(centering==1|centering=="global"){
    meanData = mean(matrixdata)
    matrixdata <- matrixdata - meanData
  }
  if(centering==2|centering=="tester"){
    meancolData = colMeans(matrixdata)
    for (i in 1:nrow(matrixdata)){
      for (j in 1:ncol(matrixdata)){ 
        matrixdata[i,j] <- matrixdata[i, j] - meancolData[j]
      }}
  }
  if(centering==3|centering=="double"){
    meanData = mean(matrixdata)
    meancolData = colMeans(matrixdata)
    meanrowData = rowMeans(matrixdata)
    for (i in 1:nrow(matrixdata)){
      for (j in 1:ncol(matrixdata)){
        matrixdata[i,j] <- matrixdata[i, j] + meanData - meancolData[j] -meanrowData[i]
      }}
  }
  
  # Scaling options
  if(scaling==1|scaling=="sd"){
    desviation <- array(, dim = ncol(matrixdata))
    for (j in 1:ncol(matrixdata)){ desviation[j] <- sqrt(var(matrixdata[,j]))}
    for (i in 1:nrow(matrixdata)){ for (j in 1:ncol(matrixdata)){ matrixdata[i,j] <- matrixdata[i, j]/desviation[j]}}
  }
  
  
  # SVD options
  if(SVD==1|SVD=="row"){
    coordgenotype <- svd(matrixdata)$u %*% diag(svd(matrixdata)$d)
    coordenviroment <- svd(matrixdata)$v
    d1 = (max(coordenviroment[, 1]) - min(coordenviroment[,1]))/(max(coordgenotype[, 1]) -  min(coordgenotype[, 1]))
    d2 = (max(coordenviroment[, 2]) - min(coordenviroment[,2]))/(max(coordgenotype[, 2]) -  min(coordgenotype[, 2]))
    d = max(d1, d2)
    coordenviroment <- coordenviroment/d
  }
  
  if(SVD==2|SVD=="column"){
    coordgenotype <- svd(matrixdata)$u
    coordenviroment <- svd(matrixdata)$v %*% diag(svd(matrixdata)$d)
    d1 = (max(coordgenotype[, 1]) - min(coordgenotype[,1]))/(max(coordenviroment[, 1]) -  min(coordenviroment[, 1]))
    d2 = (max(coordgenotype[, 2]) - min(coordgenotype[,2]))/(max(coordenviroment[, 2]) -  min(coordenviroment[, 2]))
    d = max(d1, d2)
    coordgenotype <- coordgenotype/d
  }
  if(SVD==3|SVD=="dual"){
    coordgenotype <- svd(matrixdata)$u %*% diag(svd(matrixdata)$d)
    coordenviroment <- svd(matrixdata)$v %*% diag(svd(matrixdata)$d)
  }
  if(SVD==4|SVD=="symmetrical"){
    coordgenotype <- svd(matrixdata)$u %*% diag(sqrt(svd(matrixdata)$d))
    coordenviroment <- svd(matrixdata)$v %*% diag(sqrt(svd(matrixdata)$d))
  }

  xtext <- rbind(coordgenotype,coordenviroment)[,1]
  ytext <- rbind(coordgenotype,coordenviroment)[,2]
  
  valorespropios = svd(matrixdata)$d
  
  vartotal = round(as.numeric(sum(valorespropios^2)),2)
  varexpl = round(as.numeric((valorespropios^2/vartotal) *100), 2)
  
  
  GGEModel=list(fit="internal",coordgenotype=coordgenotype,coordenviroment=coordenviroment,xtext=xtext,ytext=ytext,
                valorespropios=valorespropios,vartotal=vartotal,varexpl=varexpl,labelgen=labelgen,labelenv=labelenv,axes=ejes,matrixdata=matrixdata,
                centering=centering,scaling=scaling,SVD=SVD)
  class(GGEModel)<-"GGEModel"
  return(GGEModel) 
}

