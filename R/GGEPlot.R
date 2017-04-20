#' Main plotting function for GGEBiplots. Produces basic biplot unless otherwise specified. 
#' Most attributes of graph can either be customised within the function (text size and color) or turned off and then customised by the user using standard ggplot2 commands (titles and axes)
#'
#' @param GGEModel A GGEModel produced from a call to GGEModels()
#' @param type type of biplot to produce. 1=Basic biplot; 2=Examine environment; 3=Examine genotype; 4=Relationship among environments; 5=Compare two genotypes;
#' 6=Which won where/what; 7=Discrimination vs. representativeness; 8=Relationship among environments; 9=Mean vs. stability; 10=Relationship among genotypes.
#' Defaults to type=1. All other types have available wrapper functions
#' @param d1 component to plot on x axis. Defaults to 1
#' @param d2 component to plot on y axis. Defaults to 1
#' @param selectedE environment to examine when type=2. Must be a string which perfectly matches an environment label 
#' @param selectedG genotype to examine when type=3. Must be a string which perfectly matches a genotype label 
#' @param selectedG1 genotype to compare when type=5. Must be a string which perfectly matches a genotype label
#' @param selectedG2 genotype to compare when type=5. Must be a string which perfectly matches a genotype label and not equal to selectedG1
#' @param colGen colour for genotype attributes on biplot. Defaults to "forestgreen"
#' @param colEnv colour for environment attributes on biplot. Defaults to "blue"
#' @param sizeGen text size for genotype labels on biplot. Defaults to 4
#' @param sizeEnv text size for environment labels on biplot. Defaults to 4
#' @param largeSize text size to use for larger labels on type=5 (for the two selected genotypes) and type=6 (for the "winners"). Defaults to 4.5
#' @param axis_expand multiplication factor to expand the axis limits by to enable fitting of labels. Defaults to 1.4
#' @param axislabels TRUE/FALSE. Include axis labels
#' @param axes TRUE/FALSE. Include axes modifications
#' @param limits TRUE/FALSE. Include calls to scale_x_continuous() and scale_y_continuous()
#' @param titles TRUE/FALSE. Include title
#' @param footnote TRUE/FALSE. Include footnote
#' @keywords GGE
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' GxEMeans<-tapply(plrv$Yield,list(plrv$Genotype,plrv$Locality),mean,na.rm=T)
#' GGE<-GGEModels(GxEMeans)
#' GGEPlot(GGE)
GGEPlot<-function(GGEModel,type=1,d1=1,d2=2,  selectedE=NA ,  selectedG=NA,selectedG1=NA,selectedG2=NA,
                  colGen="forestgreen",colEnv="blue",sizeGen=4,sizeEnv=4,largeSize=4.5,axis_expand=1.4,
                  axislabels=TRUE,axes=TRUE,limits=TRUE,titles=TRUE,footnote=TRUE){
  require(ggplot2)
  require(scales)
  require(ggforce)
  coordgenotype=GGEModel$coordgenotype[,c(d1,d2)];coordenviroment=GGEModel$coordenviroment[,c(d1,d2)];xtext=GGEModel$xtext;ytext=GGEModel$ytext
  valorespropios=GGEModel$valorespropios[c(d1,d2)];vartotal=GGEModel$vartotal;varexpl=GGEModel$varexpl[c(d1,d2)];labelgen=GGEModel$labelgen;
  labelenv=GGEModel$labelenv;ejes=GGEModel$axes[c(d1,d2)];matrixdata=GGEModel$matrixdata
  
  
  plotdata<-data.frame(rbind(data.frame(coordgenotype,type="genotype",label=labelgen),data.frame(coordenviroment,type="environment",label=labelenv)))
  colnames(plotdata)[1:2]<-c("d1","d2")
  plotdata$type<-factor(plotdata$type)
  
  GGE1<-ggplot(data=plotdata,aes(x=d1,y=d2,group="type"))+theme_bw()+
    scale_color_manual(values=c(colGen,colEnv)) +
    scale_size_manual(values=c(sizeGen,sizeEnv))+
    coord_fixed()     
  
  if(axislabels==TRUE){
    GGE1<-GGE1+xlab(paste(ejes[1],varexpl[1], "%", sep = " "))+ylab(paste(ejes[2], varexpl[2],"%", sep = " "))
  }
  if(axes==TRUE){
    GGE1<-GGE1+geom_hline(yintercept=0)+geom_vline(xintercept=0)
  }
  if(limits==TRUE){
    GGE1<-GGE1+scale_x_continuous(limits=c(min(plotdata$d1*axis_expand),max(plotdata$d1*axis_expand)),expand=c(0,0))+
      scale_y_continuous(limits=c(min(plotdata$d2*axis_expand),max(plotdata$d2*axis_expand)),expand=c(0,0))
  }
  
  

  
  if(type==1){
    
    # points(coordgenotype[, dimension1], coordgenotype[,dimension2], pch = symbol_gen, col = colgenotype)
    GGE2<-GGE1+geom_segment(xend=0,yend=0,col=alpha(colEnv,0.5),data=subset(plotdata,type=="environment"))+
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE)
    if(titles==TRUE){GGE2<-GGE2+ggtitle("GGE Biplot")}
  }   
  
  if(type==2){
    if(!selectedE%in%labelenv){stop(paste(selectedE,"not in list of environment labels"))}
    venvironment<-labelenv==selectedE
    
    x1<-NULL
    for (i in 1:nrow(matrixdata)) 
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
    for (i in 1:ncol(matrixdata)) 
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
    indice = c(chull(coordgenotype[, 1], coordgenotype[,2]))
    www<-data.frame(coordgenotype[indice,])
    
    
    indice<-c(indice,indice[1])
    ######################################
    segs<-NULL
    i <- 1
    while (is.na(indice[i + 1]) == FALSE) 
    {
      m<-(coordgenotype[indice[i], 2] - coordgenotype[indice[i + 1], 2])/(coordgenotype[indice[i],1]-coordgenotype[indice[i + 1],1])
      mperp<--1/m
      c2<-coordgenotype[indice[i + 1], 2] - m*coordgenotype[indice[i + 1],1]
      xint<--c2/(m-mperp)
      xint<-ifelse(xint<0,min(coordenviroment[, 1],coordgenotype[, 1]), max(coordenviroment[, 1],coordgenotype[, 1]))
      yint<-mperp*xint
      
      m2<-max(abs(c(xint,yint)))
      m3<-which(abs(c(xint,yint))==max(abs(c(xint,yint))))
      if(m3==1&xint<0)sl1<-(c(xint,yint)/m2)*abs(min(plotdata$d1))*axis_expand
      if(m3==1&xint>0)sl1<-(c(xint,yint)/m2)*max(plotdata$d1)*axis_expand
      if(m3==2&yint<0)sl1<-(c(xint,yint)/m2)*abs(min(plotdata$d2))*axis_expand
      if(m3==2&yint>0)sl1<-(c(xint,yint)/m2)*max(plotdata$d2)*axis_expand                                                  
      
      segs<-rbind(segs,sl1)
      i <- i + 1
    }
    rownames(segs)<-NULL
    segs<-data.frame(segs)        
    
    winners<-plotdata[plotdata$type=="genotype",][indice[-1],]
    others<-plotdata[!rownames(plotdata)%in%rownames(winners),]
    
    GGE2<-GGE1+geom_polygon(aes(x=X1,y=X2),data=www,fill=NA,col="black",size=0.8)+
      geom_segment(data=segs,aes(x=X1,y=X2),xend=0,yend=0,linetype=2) +
      geom_text(aes(col=type,label=label,size=type),show.legend = FALSE,data=others)+
      geom_text(aes(label=label,x=d1,y=d2),show.legend = FALSE,hjust="outward",vjust="outward",data=winners,col=colGen,size=largeSize,fontface="bold")
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Which Won Where/What")}  
    
  }
  
  if(type==7){
   
    circles<-data.frame(x0=0,y0=0,start=0,end=pi*2,radio =1:5* max((max(coordenviroment[1, ]) - min(coordenviroment[1, ])), (max(coordenviroment[2,]) - min(coordenviroment[2, ])))/10)
    
    
    GGE2<-GGE1+geom_arc(aes(r=radio,x0=x0,y0=y0,start=start,end=end),data=circles,col="gray",inherit.aes=F, na.rm=TRUE)+
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
      geom_arc(aes(r=radio,x0=x0,y0=y0,start=start,end=end),data=circles,col="gray",inherit.aes=F, na.rm=TRUE)+
      geom_segment(x=0, y=0,xend=xcoord,yend=ycoord,arrow =arrow(length=unit(0.15,"inches") ),size=1,col=colEnv)+
      geom_text(aes(label=label),show.legend = FALSE,data=subset(plotdata,type=="environment"),col=colEnv,size=sizeEnv)
    
    
    if(titles==TRUE){GGE2<-GGE2+ggtitle("Ranking Environments")}   
  }
  
  
  if(type==9){    
    
    med1 = mean(coordenviroment[, 1])
    med2 = mean(coordenviroment[, 2])
    
    
    
    x1<-NULL
    
    for (i in 1:nrow(matrixdata)) 
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
    for (i in 1:nrow(matrixdata)) {
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
      geom_circle(aes(r=radio,x0=x0,y0=y0),data=circles,col="gray",inherit.aes=F)+
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
    centertext<-ifelse(GGEModel$centering==1|GGEModel$centering=="global","Global-Centered E+G+GE",
                       ifelse(GGEModel$centering==2|GGEModel$centering=="tester","Tester-Centered G+GE",
                              ifelse(GGEModel$centering==3|GGEModel$centering=="double","Double-Centred GE","No Centering")))
    SVPtext<-ifelse(GGEModel$SVP==1|GGEModel$SVP=="row","Row Metric Preserving",
                    ifelse(GGEModel$SVP==2|GGEModel$SVP=="column","Column Metric Preserving",
                           ifelse(GGEModel$SVP==3|GGEModel$SVP=="dual","Dual Metric Preserving","Symmetrical")))
    Scalingtext<-ifelse(GGEModel$scaling==1|GGEModel$scaling=="sd","scaling by SD","no scaling")
    footnotetxt=paste("\nGGE Biplot showing components ",d1," and ",d2," explaining ",sum(varexpl),"% of the total variation\nusing ",
                      SVPtext," SVP and ",centertext," with ",Scalingtext,sep="")
    
    GGE2<-GGE2+ labs(caption = footnotetxt)+theme(plot.caption = element_text(size=8,hjust=0,face="italic"))
  }
  return(GGE2)
}
