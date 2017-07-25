#'GGE biplots with \pkg{ggplot2}
#'
#'Produces the GGE biplot as an object of class 'ggplot' from a model produced 
#'by a call to either \code{\link[GGEBiplots]{GGEModel}} or 
#'\code{\link[gge]{gge}}. Nearly all stylistic attributes of output can either 
#'be customised within the function or disabled so that the user can customise 
#'output to their own liking.
#'
#'@param GGEModel An object of class \code{GGEModel} or \code{gge}
#'@param type type of biplot to produce. \enumerate{\item Basic biplot. \item
#'  Examine environment. See \code{\link[GGEBiplots]{ExamineEnv}} \item Examine
#'  genotype. See \code{\link[GGEBiplots]{ExamineGen}} \item Relationship among
#'  environments. See \code{\link[GGEBiplots]{EnvRelationship}} \item Compare
#'  two genotypes. See \code{\link[GGEBiplots]{CompareGens}} \item Which won
#'  where/what. See \code{\link[GGEBiplots]{WhichWon}} \item Discrimination vs.
#'  representativeness. See \code{\link[GGEBiplots]{DiscRep}} \item Ranking
#'  environments. See \code{\link[GGEBiplots]{RankEnv}} \item Mean vs.
#'  stability. See \code{\link[GGEBiplots]{MeanStability}} \item Ranking
#'  gentoypes See \code{\link[GGEBiplots]{RankGen}}}
#'@param d1 PCA component to plot on x axis. Defaults to 1
#'@param d2 PCA component to plot on y axis. Defaults to 2
#'@param selectedE name of the environment to examine when type=2. Must be a 
#'  string which matches an environment label
#'@param selectedG name of the genotype to examine when type=3. Must be a string
#'  which matches a genotype label
#'@param selectedG1 name of a genotype to compare when type=5. Must be a string 
#'  which matches a genotype label
#'@param selectedG2 name of a genotype to compare when type=5. Must be a string 
#'  which matches a genotype label and not equal to selectedG1
#'@param colGen colour for genotype attributes on biplot. Defaults to 
#'  "forestgreen"
#'@param colEnv colour for environment attributes on biplot. Defaults to "blue"
#'@param colSegment colour for segment or circle lines. Defaults to "red"
#'@param colHull colour for hull when type=6. Defaults to "black"
#'@param sizeGen text size for genotype labels. Defaults to 4
#'@param sizeEnv text size for environment labels. Defaults to 4
#'@param largeSize text size to use for larger labels where type=5, used for the
#'  two selected genotypes, and where type=6, used for the outermost genotypes.
#'  Defaults to 4.5
#'@param axis_expand multiplication factor to expand the axis limits by to 
#'  enable fitting of labels. Defaults to 1.2
#'@param axislabels logical. If TRUE then include automatically generated labels
#'  for axes
#'@param axes logical. If TRUE then include x and y axes going through the 
#'  origin
#'@param limits logical. If TRUE then automatically rescale axes
#'@param titles logical. If TRUE then include automatically generated titles
#'@param footnote logical. If TRUE then include automatically generated footbote
#'@keywords GGE Biplot
#'@return A biplot of class \code{ggplot}
#'@references Yan W, Kang M (2003). \emph{GGE Biplot Analysis: A Graphical Tool 
#'  for Breeders, Geneticists, and Agronomists}. CRC Press.
#'@export
#' @examples
#' library(GGEBiplotGUI)
#' data(Ontario)
#' GGE1<-GGEModel(Ontario)
#' GGEPlot(GGE1)
#' 
#'#using 'gge' instead
#'
#'library(gge)
#'GGE2<-gge(as.matrix(Ontario))
#'GGEPlot(GGE2)
#'    
#'@importFrom ggplot2 aes arrow coord_fixed element_text geom_abline geom_hline 
#'  geom_point geom_polygon geom_segment geom_text geom_vline ggplot ggtitle 
#'  labs layer_scales scale_color_manual scale_size_manual scale_x_continuous 
#'  scale_y_continuous theme theme_bw xlab ylab element_blank
#'@importFrom ggforce geom_arc geom_circle
#'@importFrom scales alpha squish
#'@importFrom grDevices chull
#'@importFrom grid unit

GGEPlot<-function(GGEModel,type=1,d1=1,d2=2,  selectedE=NA ,  selectedG=NA,selectedG1=NA,selectedG2=NA,
                  colGen="forestgreen",colEnv="blue",colSegment="red",colHull="black",sizeGen=4,sizeEnv=4,largeSize=4.5,axis_expand=1.2,
                  axislabels=TRUE,axes=TRUE,limits=TRUE,titles=TRUE,footnote=TRUE){
  fail<-1
  if(class(GGEModel)=="GGEModel"){

  coordgenotype=GGEModel$coordgenotype[,c(d1,d2)];coordenviroment=GGEModel$coordenviroment[,c(d1,d2)]
varexpl=GGEModel$varexpl[c(d1,d2)];labelgen=GGEModel$labelgen;
  labelenv=GGEModel$labelenv;labelaxes=GGEModel$labelaxes[c(d1,d2)];Data=GGEModel$Data
centering=GGEModel$centering;SVP=GGEModel$SVP;scaling=GGEModel$scaling

  fail<-0
  
  }
  if(class(GGEModel)=="gge"){ 
  
    coordgenotype=GGEModel$genCoord[,c(d1,d2)];coordenviroment=GGEModel$locCoord[,c(d1,d2)]
    varexpl=round(100*GGEModel$R2[c(d1,d2)],2);labelgen=rownames(GGEModel$x);
    labelenv=colnames(GGEModel$x);labelaxes=paste("AXIS",1:ncol(diag(svd(GGEModel$x)$d)), sep = "");Data=GGEModel$x
    fail<-0
    centering=ifelse(GGEModel$center==TRUE,"tester",0);
    SVP=ifelse(GGEModel$method=="svd","column",5);
    scaling=ifelse(GGEModel$scale==TRUE,"sd",0)
    
    rescale1 = (max(coordenviroment[, 1]) - min(coordenviroment[,1]))/(max(coordgenotype[, 1]) -  min(coordgenotype[, 1]))
    rescale2 = (max(coordenviroment[, 2]) - min(coordenviroment[,2]))/(max(coordgenotype[, 2]) -  min(coordgenotype[, 2]))
    d = max(rescale1, rescale2)
    coordenviroment <- coordenviroment/d
    
    
      }
  
  if(fail==1){stop(paste("Object",deparse(substitute(GGEModel)),"is not of class 'gge' or 'GGEModel'"))}
  if(centering==0|centering=="none"){stop("GGEPlot is not compatible with GGE models produced without centering")}
  
  
  plotdata<-data.frame(rbind(data.frame(coordgenotype,type="genotype",label=labelgen),data.frame(coordenviroment,type="environment",label=labelenv)))
  colnames(plotdata)[1:2]<-c("d1","d2")
  plotdata$type<-factor(plotdata$type)
  
  

  
  
  GGE1<-ggplot(data=plotdata,aes(x=d1,y=d2,group="type"))+theme(panel.grid = element_blank())+
    scale_color_manual(values=c(colGen,colEnv)) +
    scale_size_manual(values=c(sizeGen,sizeEnv))
  
  if(axislabels==TRUE){
    GGE1<-GGE1+xlab(paste(labelaxes[1],format(varexpl[1],nsmall=2), "%", sep = " "))+ylab(paste(labelaxes[2], format(varexpl[2],nsmall=2),"%", sep = " "))
  }
  if(axes==TRUE&centering!=0){
    GGE1<-GGE1+geom_hline(yintercept=0)+geom_vline(xintercept=0)
  }
  if(limits==TRUE&centering!=0){
    xlim<-c(min(plotdata$d1*axis_expand),max(plotdata$d1*axis_expand))
    ylim<-c(min(plotdata$d2*axis_expand),max(plotdata$d2*axis_expand))
  if(which(c(diff(xlim),diff(ylim))==max(c(diff(xlim),diff(ylim))))==1){
    xlim1<-xlim
    ylim1<-c(ylim[1]-(diff(xlim)-diff(ylim))/2,ylim[2]+(diff(xlim)-diff(ylim))/2)
  }
    if(which(c(diff(xlim),diff(ylim))==max(c(diff(xlim),diff(ylim))))==2){
     ylim1<-ylim
     xlim1<-c(xlim[1]-(diff(ylim)-diff(xlim))/2,xlim[2]+(diff(ylim)-diff(xlim))/2)
   }               
    
    GGE1<-GGE1+scale_x_continuous(limits=xlim1,expand=c(0,0))+
      scale_y_continuous(limits=ylim1,expand=c(0,0))+
      coord_fixed(1)     
  }
  
  

  
  if(type==1){
    
    GGE2<-GGE1+geom_segment(xend=0,yend=0,col=alpha(colEnv,0.5),data=subset(plotdata,type=="environment"))+
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE)
    if(titles==TRUE){GGE2<-GGE2+ggtitle("GGE Biplot")}
  }   
  
  if(type==2){
    if(!selectedE%in%labelenv){stop(paste(selectedE,"not in list of environment labels"))}
    venvironment<-labelenv==selectedE
    
    x1<-NULL
    for (i in 1:nrow(Data)) 
    {
      x <- solve(matrix(c(-coordenviroment[venvironment,2], coordenviroment[venvironment,1], coordenviroment[venvironment,1], coordenviroment[venvironment, 2]), nrow = 2), 
                 matrix(c(0, coordenviroment[venvironment,1] * coordgenotype[i, 1] +  coordenviroment[venvironment, 2] *coordgenotype[i, 2]), ncol = 1))
      
      x1<-rbind(x1,t(x))
    }
    
    plotdata$x1_x<-NA
    plotdata$x1_x[plotdata$type=="genotype"]<-x1[,1]
    
    plotdata$x1_y<-NA
    plotdata$x1_y[plotdata$type=="genotype"]<-x1[,2]
    
    GGE2<-GGE1+
      geom_abline(slope=coordenviroment[venvironment, 2]/coordenviroment[venvironment,1],intercept=0,col=alpha(colEnv,0.5))+
      geom_abline(slope=-coordenviroment[venvironment, 1]/coordenviroment[venvironment,2],intercept=0,col=alpha(colEnv,0.5))+
      geom_segment(xend=0,yend=0,col=alpha(colEnv,0.5),data=subset(plotdata,type=="environment"&label==selectedE),
                   arrow =arrow(ends ="first",length=unit(0.2,"inches") ),size=1.1)+
      geom_segment(aes(xend=x1_x,yend=x1_y),col=colGen,data=subset(plotdata,type=="genotype"),linetype=2)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="genotype"),col=colGen,size=sizeGen)
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle(paste("Selected Environment =",selectedE))}
    
  }
  
  
  if(type==3){
    if(!selectedG%in%labelgen){stop(paste(selectedG,"not in list of genotype labels"))} 
    vgenotype<-labelgen==selectedG
    
    x1<-NULL
    for (i in 1:ncol(Data)) 
    {
      x <- solve(matrix(c(-coordgenotype[vgenotype,2], coordgenotype[vgenotype,1], coordgenotype[vgenotype,1], coordgenotype[vgenotype, 2]), nrow = 2), 
                 matrix(c(0, coordgenotype[vgenotype,1] * coordenviroment[i, 1] +  coordgenotype[vgenotype, 2] *coordenviroment[i, 2]), ncol = 1))
      
      x1<-rbind(x1,t(x))
    }
    
    plotdata$x1_x<-NA
    plotdata$x1_x[plotdata$type=="environment"]<-x1[,1]
    
    plotdata$x1_y<-NA
    plotdata$x1_y[plotdata$type=="environment"]<-x1[,2]
    
    GGE2<-GGE1+
      geom_abline(slope=coordgenotype[vgenotype, 2]/coordgenotype[vgenotype,1],intercept=0,col=alpha(colGen,0.5),size=0.8)+
      geom_abline(slope=-coordgenotype[vgenotype, 1]/coordgenotype[vgenotype,2],intercept=0,col=alpha(colGen,0.5),size=0.8)+
      geom_segment(xend=0,yend=0,col=alpha(colGen,0.5),data=subset(plotdata,type=="genotype"&label==selectedG),
                   arrow =arrow(ends ="first",length=unit(0.15,"inches") ),size=1)+
      geom_segment(aes(xend=x1_x,yend=x1_y),col=colEnv,data=subset(plotdata,type=="environment"),linetype=2)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="environment"),col=colEnv,size=sizeEnv)
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle(paste("Selected Genotype =",selectedG))}    
  }
  
  if(type==4){        
    
    
    GGE2<-GGE1+ 
      geom_segment(xend=0,yend=0,col=alpha(colEnv,0.5),data=subset(plotdata,type=="environment"),
                   arrow =arrow(ends ="first",length=unit(0.1,"inches") ),size=1)+geom_point(x=0,y=0,size=4)+
      geom_text(aes(label=label),show.legend = FALSE,hjust="outward",vjust="outward",data=subset(plotdata,type=="environment"),col=colEnv,size=sizeEnv)
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Relationship Among Environments")}   
  }
  
  if(type==5){      
    
    if(!selectedG1%in%labelgen){stop(paste(selectedG1,"not in list of genotype labels"))} 
    if(!selectedG2%in%labelgen){stop(paste(selectedG2,"not in list of genotype labels"))} 
    if(selectedG1==selectedG2){stop(paste("Cannot compare the same genotype to itself"))}
    vgenotype1<-labelgen==selectedG1       
    vgenotype2<-labelgen==selectedG2  
    # Compara dos genotipos
    GGE2<-GGE1+geom_point(data=subset(plotdata,type=="genotype"&label%in%c(selectedG1,selectedG2)))+
      geom_segment(x=plotdata$d1[plotdata$label==selectedG1&plotdata$type=="genotype"],
                   xend=plotdata$d1[plotdata$label==selectedG2&plotdata$type=="genotype"],
                   y=plotdata$d2[plotdata$label==selectedG1&plotdata$type=="genotype"],
                   yend=plotdata$d2[plotdata$label==selectedG2&plotdata$type=="genotype"],col=colGen,size=0.8)+
      geom_abline(intercept = 0, slope = -(coordgenotype[vgenotype1, 1] - coordgenotype[vgenotype2, 1])/(coordgenotype[vgenotype1,2] - coordgenotype[vgenotype2, 2]),
                  col = colGen,size=1.2)+   
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="environment"),col=colEnv,size=sizeEnv)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="genotype"&!label%in%c(selectedG1,selectedG2)),col=alpha(colGen,0.4),size=sizeGen)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="genotype"&label%in%c(selectedG1,selectedG2)),
                col=colGen,size=largeSize,hjust="outward",vjust="outward")
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle(paste("Comparison of Genotype",selectedG1,"with Genotype",selectedG2))}  
    
  }
  
  if(type==6){ 
    # Which-won-where
    indice = c(grDevices::chull(coordgenotype[, 1], coordgenotype[,2]))
    www<-data.frame(coordgenotype[indice,])
    
    
    indice<-c(indice,indice[1])
    ######################################
    segs<-NULL
    
    
    limx<-layer_scales(GGE1)$x$limits
    limy<-layer_scales(GGE1)$y$limits
    
    i <- 1
    while (is.na(indice[i + 1]) == FALSE) 
    {
      m<-(coordgenotype[indice[i], 2] - coordgenotype[indice[i + 1], 2])/(coordgenotype[indice[i],1]-coordgenotype[indice[i + 1],1])
      mperp<--1/m
      c2<-coordgenotype[indice[i + 1], 2] - m*coordgenotype[indice[i + 1],1]
      xint<--c2/(m-mperp)
      xint<-ifelse(xint<0,min(coordenviroment[, 1],coordgenotype[, 1]), max(coordenviroment[, 1],coordgenotype[, 1]))
      yint<-mperp*xint
      
      xprop<-ifelse(xint<0,xint/limx[1],xint/limx[2])
      yprop<-ifelse(yint<0,yint/limy[1],yint/limy[2])
  
      m3<-which(c(xprop,yprop)==max(c(xprop,yprop)))
      m2<-abs(c(xint,yint)[m3])
      if(m3==1&xint<0)sl1<-(c(xint,yint)/m2)*abs(limx[1])
      if(m3==1&xint>0)sl1<-(c(xint,yint)/m2)*limx[2]
      if(m3==2&yint<0)sl1<-(c(xint,yint)/m2)*abs(limy[1])
      if(m3==2&yint>0)sl1<-(c(xint,yint)/m2)*limy[2]                                           
      
      segs<-rbind(segs,sl1)
      i <- i + 1
    }
    rownames(segs)<-NULL
    colnames(segs)<-NULL
    segs<-data.frame(segs)   
    
  
    
    colnames(www)<-c("X1","X2")
    
    winners<-plotdata[plotdata$type=="genotype",][indice[-1],]
    others<-plotdata[!rownames(plotdata)%in%rownames(winners),]
    
    GGE2<-GGE1+geom_polygon(aes(x=X1,y=X2),data=www,fill=NA,col=colHull,size=0.8)+
      geom_segment(data=segs,aes(x=X1,y=X2),xend=0,yend=0,linetype=2,col=colSegment,size=0.75) +
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE,data=others)+
      geom_text(aes(label=label,x=d1,y=d2),show.legend = FALSE,hjust="outward",vjust="outward",data=winners,col=colGen,size=largeSize,fontface="bold")
    
    
       if(titles==TRUE){GGE2<-GGE2+ggtitle("Which Won Where/What")}  
    
  }
  
  if(type==7){
   
    circles<-data.frame(x0=0,y0=0,start=0,end=pi*2,radio =1:5* max((max(coordenviroment[1, ]) - min(coordenviroment[1, ])), (max(coordenviroment[2,]) - min(coordenviroment[2, ])))/10)
    
    
    GGE2<-GGE1+geom_arc(aes(r=radio,x0=x0,y0=y0,start=start,end=end),data=circles,col=colSegment,inherit.aes=F, na.rm=TRUE)+
      geom_segment(xend=0,yend=0,x=mean(coordenviroment[,1]),y=mean(coordenviroment[,2]),arrow =arrow(ends ="first",length=unit(0.1,"inches") ),size=1,col=colEnv)+
      geom_abline(intercept=0,slope=mean(coordenviroment[, 2])/mean(coordenviroment[,1]),col=colEnv)+
      geom_segment(xend=0,yend=0,col=alpha(colEnv,0.5),data=subset(plotdata,type=="environment"),linetype=2)+
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE)
    
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Discrimination vs. representativeness")}  
    
  }
  
  if(type==8){        
 
    med1 = mean(coordenviroment[, 1])
    med2 = mean(coordenviroment[, 2])
    mod = max((coordenviroment[, 1]^2 + coordenviroment[,2]^2)^0.5)
    xcoord = sign(med1) * (mod^2/(1 + med2^2/med1^2))^0.5
    ycoord = (med2/med1) * xcoord
    
    circles<-data.frame(x0=xcoord,y0=ycoord,start=0,end=pi*2,radio=1:8*((xcoord - med1)^2 + (ycoord - med2)^2)^0.5/3)
    
    GGE2<-GGE1+ 
      geom_abline(intercept=0,slope=med2/med1,col=colEnv,size=0.8)+
      geom_abline(intercept=0,slope=-med1/med2,col=colEnv,size=0.8)+
      geom_arc(aes(r=radio,x0=x0,y0=y0,start=start,end=end),data=circles,col=colSegment,inherit.aes=F, na.rm=TRUE)+
      geom_segment(x=0, y=0,xend=xcoord,yend=ycoord,arrow =arrow(length=unit(0.15,"inches") ),size=1,col=colEnv)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="environment"),col=colEnv,size=sizeEnv)
    
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Ranking Environments")}   
  }
  
  
  if(type==9){    
    
    med1 = mean(coordenviroment[, 1])
    med2 = mean(coordenviroment[, 2])
    
    
    
    x1<-NULL
    
    for (i in 1:nrow(Data)) 
    {
      x <- solve(matrix(c(-med2, med1, med1, med2), nrow = 2), matrix(c(0, med2 * coordgenotype[i, 2] + med1 * coordgenotype[i, 1]),ncol = 1))
      x1<-rbind(x1,t(x))
      
      # segments(coordgenotype[i, dimension1], coordgenotype[i,dimension2], x[1], x[2], lty = "dotted")
    }
    plotdata$x1_x<-NA
    plotdata$x1_x[plotdata$type=="genotype"]<-x1[,1]
    
    plotdata$x1_y<-NA
    plotdata$x1_y[plotdata$type=="genotype"]<-x1[,2]
    
    GGE2<-GGE1+ 
      geom_abline(intercept=0,slope=med2/med1,col=colEnv,size=0.8)+
      geom_abline(intercept=0,slope=-med1/med2,col=colEnv,size=0.8)+
      geom_segment(aes(xend=x1_x, yend=x1_y),col=colGen,linetype=2,data=subset(plotdata,type=="genotype"))+
      geom_segment(x=0, y=0,xend=med1,yend=med2,arrow =arrow(length=unit(0.15,"inches") ),size=1,col=colEnv)+
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE)    
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Mean vs. Stability")} 
  }
  
  
  if(type==10){    
    
    
    
    med1 = mean(coordenviroment[, 1])
    med2 = mean(coordenviroment[, 2])
    coordx <- 0
    coordy <- 0
    for (i in 1:nrow(Data)) {
      x <- solve(matrix(c(-med2, med1, med1, med2),nrow = 2), matrix(c(0, med2 * coordgenotype[i,2] + med1 * coordgenotype[i, 1]),ncol = 1))
      if (sign(x[1]) == sign(med1)) {
        if (abs(x[1]) > abs(coordx)) {
          coordx <- x[1]
          coordy <- x[2]
        }
      }
    }
    
    
    circles<-data.frame(x0=coordx,y0=coordy,radio=1:10*((coordx - med1)^2 + (coordy - med2)^2)^0.5/3)
    
    GGE2<-GGE1+ 
      geom_abline(intercept=0,slope=med2/med1,col=colGen,size=0.8)+
      geom_abline(intercept=0,slope=-med1/med2,col=colGen,size=0.8)+
      geom_arc(aes(r=radio,x0=x0,y0=y0,start=0,end=pi*2),data=circles,col="gray",inherit.aes=F)+
      geom_segment(x=0, y=0,xend=coordx,yend=coordy,arrow =arrow(length=unit(0.15,"inches") ),size=1,col=colGen)
    
    if(limits==F){
      GGE2<-GGE2+ scale_x_continuous(limits=c(min(plotdata$d1*axis_expand),max(plotdata$d1*axis_expand)),oob=squish)+
        scale_y_continuous(limits=c(min(plotdata$d2*axis_expand),max(plotdata$d2*axis_expand)),oob=squish)
    }
    if(axes==TRUE){
      GGE2<-GGE2+geom_hline(yintercept=0)+geom_vline(xintercept=0)
    }
    GGE2<-GGE2+geom_text(aes(col=type,label=label,size=type),show.legend = FALSE)
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Ranking Genotypes")} 
    
    
  }

  if(footnote==T){
    centertext<-ifelse(centering==1|centering=="global","Global-Centered E+G+GE",
                       ifelse(centering==2|centering=="tester","Tester-Centered G+GE",
                              ifelse(centering==3|centering=="double","Double-Centred GE","No Centering")))
    SVPtext<-ifelse(SVP==1|SVP=="row","Row Metric Preserving SVP",
                    ifelse(SVP==2|SVP=="column","Column Metric Preserving SVP",
                           ifelse(SVP==3|SVP=="dual","Dual Metric Preserving SVP",
  ifelse(SVP==4|SVP=="symmetrical","Symmetrical SVP","NIPALS algorithm"))))
    Scalingtext<-ifelse(scaling==1|scaling=="sd","scaling by standard deviation","no scaling")
    footnotetxt=paste("\nGGE Biplot showing components ",d1," and ",d2," explaining ",sum(varexpl),"% of the total variation\nusing ",
                      SVPtext," and ",centertext," with ",Scalingtext,sep="")
    
    GGE2<-GGE2+ labs(caption = footnotetxt)+theme(plot.caption = element_text(size=8,hjust=0,face="italic"))
  }
  return(GGE2)
}
