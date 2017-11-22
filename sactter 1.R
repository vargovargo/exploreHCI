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

