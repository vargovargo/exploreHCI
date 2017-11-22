#-- Load Packages -----------------------------------------------------------------------------------------------------------------------------

 
 library(classInt)
 library(RColorBrewer)
 library(shiny)

 #library(rgeos)
 library(maptools)
 library(leaflet)
library(dplyr)
 
# library(devtools)
 
 
# devtools::install_github("juba/scatterD3")
 
 library(scatterD3)
 
 
 mypath <- getwd()
 
 

#-- read Data file -----------------------------------------------------------------------------------------------

iData <- readRDS("iData.rds")

# 
# Poverty:
#  Question: Did you use the overall poverty strata?
#  Label: Percent of individuals living below the poverty level
# Alcohol Outlets
#  Question: what type of establishments are being displayed?
#  Label: Percent of individuals within 1/4 mile of alcohol outlet
# Children Abuse - substrata?
#  Question: what strata is being displayed?
#  Label: Percent of children reported with abuse/neglect
# Parks
#  Label: Percent of individuals within 1/2 mile of park/beach/open space
# Healthy Food
#  This indicator is the percentage of healthy food retailers, so higher is better. Can we make it (100-%healthy food)
#  Label: Percent of unhealthy food retailers
# 

# t1lab <- "Poverty"
# t2lab <- "Park-Beach NOT Nearby"
# t3lab <- "Retail Food"
# t4lab <- "Child Neglect"
# t5lab <- "Alcohol Outlets"


t1lab <- "Percent of individuals living below the poverty level"
t2lab <- "Percent of individuals living beyond 1/2 mile of park/beach/open space"
t3lab <- "Percent of unhealthy food retailers"
t4lab <- "Percent of children reported with abuse/neglect"
t5lab <- "Percent of individuals within 1/4 mile of alcohol outlet"


#-- read shape files ---------------------------------------------------------------------------------------------------------------------------

shape_CO  <- readShapePoly(paste0(mypath,"/Shapes/","CaliforniaCounty"))
shape_CT  <- readShapePoly(paste0(mypath,"/Shapes/","cb_2014_06_tract_500k"),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#fips      <- read.table(paste0(mypath,"/Shapes/","US_FIPS_Codes.txt"),sep=",",header=T,colClasses = "character")
#fips      <- fips[fips$State == "California",]

#-- Misc Setup -----------------------------------------------------------------------------------------------------------------------------------

lhjL     <- as.vector(sort(unique(iData$geoname[iData$geotype =="CO"]))) 

# choices = list(c("Poverty" = 1, "Parks" = 2, "Abuse" = 3), selected = 1),

 l.PPA <- c(quote(iTmp$pPov),quote(iTmp$pPark),quote(iTmp$pFood),quote(iTmp$pAbuse),quote(iTmp$pAlco))
 s.PPA <- c(quote(iTmp$sPov),quote(iTmp$sPark),quote(iTmp$sFood),quote(iTmp$sAbuse),quote(iTmp$sAlco))
 m.PPA <- c(quote(map.dat$pPov),quote(map.dat$pPark),quote(map.dat$pFood),quote(map.dat$pAbuse),quote(map.dat$pAlco))
 v.PPA <- c("pPov","pPark","pFood","pAbuse","pAlco")
 n.PPA <- c(t1lab,t2lab,t3lab,t4lab,t5lab)

 # c(AfricanAm","AIAN","Asian","Latino","Missing","Multiple","NHOPI","Other","Total","White" )
 r1name <-c("AfricanAm","Asian","Latino","White","Total")
 r0name <-c("AfricanAm","Asian","Latino","White")
 rCode <-c(3,2,4,6,9)

add.alpha <- function(col, alpha=1){
  if(missing(col)) stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) }
col2 <- add.alpha(palette(),alpha=0.6)
palette(col2) 

sPCT <-   subset(iData, geotype== "CA" & race_eth_name =="Total", select= c(pPov,pPark,pFood,pAbuse,pAlco))

#-- Default values for vars for coding -------------------------------------------------------------------------------------------------------------------------
#myGeo <- "CO"
#myV1  <- 1
#myV2  <- 2
#myRace <-"Asian"


#-- PLOTTING FUNCTIONS -------------------------------------------------------------------------------------------------------------------------
#==============================================================================
myScatter1 <- function(myV1=1,myV2=2,lim100=TRUE,myGeo="CT",myGeoname="Alameda") {
  
  myV1 <- as.numeric(myV1)
  myV2 <- as.numeric(myV2)
  
  #iTmp <- iData[iData$geotype== myGeo & iData$race_eth_name %in% r0name,]
  
  if (myGeoname=="California"){iTmp <- subset(iData, geotype== myGeo  & race_eth_name %in% r0name)
  } else {iTmp <- subset(iData, geotype== myGeo & county_name== myGeoname & race_eth_name %in% r0name)}
  
  # explore other option to function below.... junk <- "xlim=c(0,1)"
  
  if (lim100) {plot(eval(l.PPA[[myV1]]),eval(l.PPA[[myV2]]),
                    xlim=c(0,1),ylim=c(0,1),
                    col=iTmp$race_eth_code,pch=16,cex=1.9,
                    xlab=paste(n.PPA[myV1]),ylab=paste(n.PPA[myV2]))
  } else      {plot(eval(l.PPA[[myV1]]),eval(l.PPA[[myV2]]),
                    col=iTmp$race_eth_code,pch=16,cex=1.9,
                    xlab=paste(n.PPA[myV1]), ylab=paste(n.PPA[myV2]))
  }
  
  
  
  # plot_ly(iTmp, x = pPov, y = pPark, mode = "markers", color = race_eth_code)
  
  
  points(eval(l.PPA[[myV1]]),eval(l.PPA[[myV2]]),cex=1.9)
  
  legend(x="topright",legend=r1name[-5],pch=16,pt.cex=1.5,col=rCode[-5],cex=1, inset=.01)
  
  abline(v=sPCT[myV1],lwd=2,col="gray")
  abline(h=sPCT[myV2],lwd=2,col="gray")
  
  mtext(round(sPCT[myV1],2),side=1,line=1,at=sPCT[myV1],col="red")
  mtext(round(sPCT[myV2],2),side=2,line=1,at=sPCT[myV2],col="red")
  text(x=sPCT[myV1],y=par()$usr[4],"CA mean ",srt=90,adj=c(1,.3),cex=.8)
  text(x=par()$usr[2],y=sPCT[myV2],"CA mean ",adj=c(1,.3),cex=.8)
  
  
}

#myPoints1 <- function(myV1=1,myV2=2,myGeo="CO") {
#  
#  myV1 <- as.numeric(myV1)
#  myV2 <- as.numeric(myV2)
#  iTmp <- subset(iData, geotype== myGeo & race_eth_name %in% r0name)
#  nearPoint(iTmp,)
#}





myScatter2 <- function(myV1=1,myV2=2,lim100=TRUE,myGeo="CT",myGeoname="Alameda") {
  
  c1  <- add.alpha("blue",.7)
  c2  <- "blue"
  c3  <- "blue"
  cx1 <- 1.6
  
  myV1 <- as.numeric(myV1)
  myV2 <- as.numeric(myV2)
  
  
  if (myGeoname=="California"){iTmp <- subset(iData, geotype== myGeo  & race_eth_name %in% r0name)
                               z <- iTmp$county_name
  } else {iTmp <- subset(iData, geotype== myGeo & county_name== myGeoname & race_eth_name %in% r0name)
    z <- NA}
  
  
  x <- eval(l.PPA[[myV1]])
  y <- eval(l.PPA[[myV2]])
 
  x1 <- x[!is.na(x) & !is.na(y)]
  y1 <- y[!is.na(x) & !is.na(y)]
  
  c1 <- iTmp$race_eth_code[!is.na(x) & !is.na(y)]
  z1 <- z[!is.na(x) & !is.na(y)]
  
  scatterD3(x1,y1, col_var=c1,axes_font_size = 15,xlab=paste(n.PPA[myV1]), ylab=paste(n.PPA[myV2]), tooltips = TRUE,tooltip_text=z1)
 
}  






#==============================================================================

myDist1 <- function(myV1=1,myGeo="CO",myD="Density") {
  
lw <- 2.5
myV1 <- as.numeric(myV1)
iTmp <- iData[iData$geotype==myGeo & iData$race_eth_name %in% r0name,]

if (myD=="Density"){
  
   t1 <- density(eval(l.PPA[[myV1]])[iTmp$race_eth_name=="White"],na.rm=TRUE)
   t2 <- density(eval(l.PPA[[myV1]])[iTmp$race_eth_name=="AfricanAm"],na.rm=TRUE)
   t3 <- density(eval(l.PPA[[myV1]])[iTmp$race_eth_name=="Latino"],na.rm=TRUE)
   t4 <- density(eval(l.PPA[[myV1]])[iTmp$race_eth_name=="Asian"],na.rm=TRUE)
  
   plot(t1,xlim=c(0,1),ylim=c(0,max(c(t1$y,t2$y,t3$y,t4$y))),
      main=paste("County Distribution of",n.PPA[myV1],"by Race"),col=rCode[4],cex.main=1.1,
      xlab="",ylab="~Freq",lwd=lw)
mtext("percent",1,line=2)
lines(t2,col=rCode[1],lwd=lw)
lines(t3,col=rCode[3],lwd=lw)
lines(t4,col=rCode[2],lwd=lw)

legend(x="topright",legend=r1name[-5],lty=1,lwd=lw,col=rCode[-5],cex=.9, inset=.01)

}

else {
  boxplot(eval(l.PPA[[myV1]]) ~ iTmp$race_eth_name,col=rCode[1:4],
          main=paste("County Distribution of",n.PPA[myV1],"by Race"))
  }
}


myDistX <- function(myV1=1,myV2=2,myGeo="CO",myD="Density") {
  par(mfrow=c(2,1),mar=c(4,4,2,1))
  myDist1(myV1,myGeo=myGeo,myD=myD)
  myDist1(myV2,myGeo=myGeo,myD=myD)
}

#==============================================================================
myBar1 <- function(myV1=1,myGeoname="California",mCI=FALSE) {
  
  myV1 <- as.numeric(myV1)
  iTmp <- iData[iData$geotype %in% c("CO","CA") & iData$geoname == myGeoname & iData$race_eth_name %in% r0name,]
  
 # iTmp <- na.omit(iTmp[, 4])  #work around -4 gets rid of county_name column
  wDat <- 100*eval(l.PPA[[myV1]])
  sDat <-      eval(s.PPA[[myV1]])
    atx <- barplot(wDat,names.arg = iTmp$race_eth_name,col=iTmp$race_eth_code,cex.names=1.3,
                   main=paste("Race/Ethnic Difference in",n.PPA[myV1],"in",myGeoname)
                      )
    abline(h=0)        
   
    if (mCI) {arrows(atx,wDat-1.96*sDat,atx,wDat+1.96*sDat,angle=90,code=3,length=.05,lwd=2)}
    
    
    
    mtext(text="Estimate",side=1,line=3,at=0,adj=1)  
    mtext(text=round(wDat,1),side=1,line=3,at=atx)       
   
    nG <- length(wDat)
    
    xDat <- round(wDat-wDat[nG],1) 
    zDat <- xDat/(sqrt(sDat^2 + sDat[nG]^2)) 
    pDat <- pnorm(abs(zDat),lower.tail=FALSE)*2
    pDat <- format.pval(pDat,2, 0.001, nsmall = 3)
    pDat[nG] <- "-"
    
    mtext(text="Absolute Diff.",side=1,line=4,at=0,adj=1)  
    xDat[nG] <- "-"
    mtext(text=xDat,side=1,line=4,at=atx) 
    
    xDat <- round((wDat/wDat[nG]),1); xDat[nG] <- "-"
    
    xCol <- rep("black",nG)
    xCol[xDat<1] <- "blue"
    xCol[xDat>2] <- "red"
    
    mtext(text="Relative Diff.",side=1,line=5,at=0,adj=1)  
    mtext(text=xDat,side=1,line=5,at=atx,col=xCol) 
    
    
    mtext(text="p-value.",side=1,line=6,at=0,adj=1)  
    mtext(text=pDat,side=1,line=6,at=atx,cex=.8) 
    
    
    
  }
  
library(plotrix)

myBarF <- function(myV1=1,myGeoname="Alameda") {
  
  myV1 <- as.numeric(myV1)
  iTmp <- iData[iData$geotype %in% c("CO","CA") & iData$geoname == myGeoname & iData$race_eth_name %in% r0name,]
  
#  iTmp <- na.omit(iTmp[, -4])  #work around -4 gets rid of county_name column
  
  
  wDat <- 100*eval(l.PPA[[myV1]])
  
  xDat <- wDat[!is.na(wDat)]
  nDat <- iTmp$race_eth_name[!is.na(wDat)]
  
  barp(xDat,names.arg = NA,col="blue",
                 cylindrical=TRUE,shadow=TRUE,
                 main=paste("Race/Ethnic Difference in",n.PPA[myV1],"in",myGeoname)
  )

  #iTmp$race_eth_code
  mtext(nDat,at=1:length(nDat),side=1,line=1,cex=1.3)
  
    abline(h=0)        
  
  
  
}











 myBarX <- function(myV1=1,myTwo=FALSE,myGeoname="Fresno",myV2=2,mCI) {
   par(mar=c(8,8,3,3))
   if (!myTwo) { 
     par(mfrow=c(1,1))
     myBarF(myV1,myGeoname=myGeoname)
    } else {
  par(mfrow=c(2,1))
  myBar1(myV1,myGeoname=myGeoname,mCI)
  myBar1(myV2,myGeoname=myGeoname,mCI) }
 }


#--  COUNTY Mapping function ------------------------------------------------------------------------------------------------

myMap0 <- function(myV1=1,myRace,colors1,break1)      { 
  
  myV1 <- as.numeric(myV1)   
  
  tDat1  <- iData[iData$race_eth_name==myRace & iData$geotype == "CO" ,]
  mea1  <- tDat1[,v.PPA[myV1]]
  
  map.dat  <- merge(shape_CO,tDat1, by.x="NAME",by.y="county_name")
  
  plot(map.dat, col=colors1[findInterval(mea1, break1)], axes=F)
  #title(paste("%",n.PPA[myV1],"among",myRace,"by County"))
  title(paste(myRace))
  
  #legend(x=-118, y=42, legend=c(leglabs(round(break1,digits=2))), fill=colors1, bty="n", x.intersp = 1, y.intersp = 1)
  
}

myMap1 <- function(myV1){
  
  myV1 <- as.numeric(myV1) 
  
  tDat  <- iData[iData$geotype == "CO" ,]
  mea0  <- tDat[,v.PPA[myV1]]
  
  colors1 <- rev(brewer.pal(4, "RdBu"))
  breaks  <-classIntervals(mea0, n=4, style="quantile")
  break1 <- breaks$brks 
  
  par(mfrow=c(2,2),mar=c(1,1,1,1))
  myMap0(myV1,myRace="Asian",colors1,break1)
  legend(x=-118, y=42, legend=c(leglabs(round(break1,digits=2))), fill=colors1, bty="n", x.intersp = 1, y.intersp = 1,cex=1.4)
  
  
  myMap0(myV1,myRace="White",colors1,break1)
  myMap0(myV1,myRace="AfricanAm",colors1,break1)    
  myMap0(myV1,myRace="Latino",colors1,break1)
  
}  
  
    #--  CENSUS Static Mapping function ------------------------------------------------------------------------------------------------
  
  myMap2 <- function(myV1=1)      { 
    myRace="Total"
    myV1 <- as.numeric(myV1)   
    
    # if( myYear %in% c(2006,2009)) stop("Some Counties Missing Data for that Year")
    
    tDat  <- iData[iData$race_eth_name==myRace & iData$geotype == "CT" ,]
    
    map.dat  <- merge(shape_CT,tDat, by.x="GEOID",by.y="geotypevalue")
   # mea0  <- tDat[,v.PPA[myV1]]
    mea0  <- eval(m.PPA[[myV1]])
    colors1 <- rev(brewer.pal(4, "RdBu"))
    
    breaks  <-classIntervals(mea0, n=4, style="quantile")
    break1 <- breaks$brks 
    
    
    plot(map.dat, col=colors1[findInterval(mea0, break1)], axes=F)
    title(paste(n.PPA[myV1],"among",myRace,"by California Census Tract"))
    legend(x=-118, y=42, legend=c(leglabs(round(break1,digits=2))), fill=colors1, bty="n", x.intersp = 1, y.intersp = 1)
    
  }
  #--  CENSUS Leaflet Mapping function ------------------------------------------------------------------------------------------------
  
  
 # myMap3 <- function(myV1=1,myRace="Total")      { 
    myMap3 <- function(myV1=1)      { 
    myRace="Total"
    myV1 <- as.numeric(myV1)   
    
    
    tDat  <- iData[iData$race_eth_name==myRace & iData$geotype == "CT" ,]
    #mea0  <- tDat[,v.PPA[myV1]]
    
    map.dat  <- merge(shape_CT,tDat, by.x="GEOID",by.y="geotypevalue")
    
    mea0  <- eval(m.PPA[[myV1]])
    
    colors1 <- rev(brewer.pal(4, "RdBu"))
    
    breaks  <-classIntervals(mea0, n=4, style="quantile")
    break1 <- breaks$brks 
    brkLab <- paste0(round(break1[1:4],2)," - ",round(break1[2:5],2))
    map.dat$myCol <- colors1[findInterval(mea0, break1)]
    
        map3<-leaflet() %>%
      addTiles()      %>%
      addPolygons(data = map.dat, fillColor = map.dat$myCol, 
                  fillOpacity = 0.7, weight = 1, smoothFactor = 0.2) %>%
      addLegend(colors=colors1, labels=brkLab, position = "topright", 
                title = paste("Proportion",n.PPA[myV1]),
                labFormat = labelFormat(suffix = "%")) 
    map3
  }