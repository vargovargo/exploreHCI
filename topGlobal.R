#-- Load Packages -----------------------------------------------------------------------------------------------------------------------------
 library(openxlsx)
 library(dplyr)
 

#-- Read Data -----------------------------------------------------------------------------------------------------------------------------------
mypath <- getwd()
#mypath <- "f:/0 SHINY APPS/HCI_Viz"

t1   <- "HCI_PovertyRate_754_CT_PL_CO_RE_CA_1-22-14.xlsx"
t2   <- "ParkBeachOpen10_output4-12-13.xlsx"
t3   <- "HCI_RetailFoodEnvironment_75_CA_CO_RE_PL_CT_11-15-13.xlsx" # original raw file was .xls, manually changed to .xlsx
t4   <- "HCI_AbuseNeglectChildren_741_CT_PL_CO_RE_CA-24-4-15.xlsx"
t5a  <- "HCI_AlcoholOutletsQ_774_CA_RE_CO_CD_PL_CT-A-N-5-16-14.xlsx"
t5b  <- "HCI_AlcoholOutletsQ_774_CO_CD_PL_CT-O-Y-5-16-14.xlsx"



iD1  <- read.xlsx(paste0(mypath,"/RawDat/",t1))
iD1  <- iD1[iD1$Poverty == "Overall",]
iD1  <- iD1[iD1$reportyear == "2006-2010"      & !is.na(iD1$reportyear), ]
iD1  <- iD1[iD1$geotype %in% c("CA","CO","CT") & !is.na(iD1$geotype), ]
#iD1  <- iD1[ ! (iD1$geotype == "CT" & iD1$race_eth_name != "Total"), ]

iD2  <- read.xlsx(paste0(mypath,"/RawDat/",t2))
iD2                                 <- iD2[iD2$geotype %in% c("CA","CO","CT") & !is.na(iD2$geotype), ]
#iD2  <- iD2[ ! (iD2$geotype == "CT" & iD2$race_eth_name != "Total"), ]
iD2$geotypevalue[iD2$geotype=="CT"] <- paste0("06",iD2$geotypevalue[iD2$geotype=="CT"])
iD2$geoname[iD2$geotype=="CT"]      <- substr(iD2$geoname[iD2$geotype=="CT"],14,20)
iD2$geotype[iD2$geotype=="ST"]      <- "CA"

iD3  <- read.xlsx(paste0(mypath,"/RawDat/",t3))
iD3  <- iD3[iD3$geotype %in% c("CA","CO","CT") & !is.na(iD3$geotype), ]

iD4  <- read.xlsx(paste0(mypath,"/RawDat/",t4))
iD4 <- iD4[iD4$geotype %in% c("CA","CO","CT") & !is.na(iD4$geotype), ]
iD4 <- iD4[iD4$reportyear == "2013"      & !is.na(iD4$reportyear), ]     # ONLY 2013 has census data
iD4 <- iD4[  (iD4$geotype == "CT" &  iD4$strata_name=="All Allegations" & iD4$strata_level_name == "All Dispositions")
            |(iD4$geotype != "CT" &  iD4$strata_name=="Allegations of Abuse and Neglect" & iD4$strata_level_name == "All Dispositions") , ]

iD5a <- read.xlsx(paste0(mypath,"/RawDat/",t5a))
iD5b <- read.xlsx(paste0(mypath,"/RawDat/",t5b))
iD5  <- rbind(iD5a,iD5b)
iD5  <- iD5[iD5$geotype %in% c("CA","CO","CT") & !is.na(iD5$geotype), ]
#iD5  <- iD5[ ! (iD5$geotype == "CT" & iD5$race_eth_name != "Total"), ]
iD5  <- iD5[ iD5$license_type == "Total_licenses" , ]


vL <- c("race_eth_name","race_eth_code","geotype","geotypevalue","geoname","county_name")

iDx1  <- iD1[, c(vL,"NumPov","TotalPop","percent","percent_SE")]
iDx2  <- iD2[, c(vL,"pop_park_acc","pop2010","p_parkacc","se")]
iDx3  <- iD3[, c(vL,"mrfei")]
iDx4 <-  iD4[, c(vL,"allegations_children","total_children","percent","se")]
iDx5 <-  iD5[, c(vL,"numerator","denominator","percent","se")]

names(iDx1)[7:10] <- c("nPov","popPov","p2Pov","sPov")
names(iDx2)[7:10] <- c("nPark","popPark","p2park","sPark")
names(iDx3)[7  ] <- c("nFood")
 iDx3$popFood    <- 100
 iDx3$sFood      <- 0
names(iDx4)[7:10] <- c("nAbuse","popAbuse","p2Abuse","sAbuse")
names(iDx5)[7:10] <- c("nAlco","popAlco","p2Alco","sAlco")


park_t1 <- iDx2[iDx2$geotype == "CO",]
park_t2 <- park_t1 %>% group_by(race_eth_name,race_eth_code) %>% summarize(nPark=sum(nPark,na.rm=TRUE), popPark=sum(popPark,na.rm=TRUE) )
park_t2$geotype      <- "CA"
park_t2$geotypevalue <- "06"
park_t2$geoname      <- "California"
park_t2$county_name  <- NA
iDx2 <- merge(park_t2,iDx2, by = c("geotype","geotypevalue","geoname","county_name","race_eth_name","race_eth_code","nPark","popPark"),all=TRUE)

## CONFIRM THIS -- makes it LACK of Park Access...
iDx2[,7] <- iDx2[,8] - iDx2[,7]


#===========================================================================================================

iData <- merge(iDx1,iDx2, by = c("geotype","geotypevalue","geoname","county_name","race_eth_name","race_eth_code"),all=TRUE)
iData <- merge(iData,iDx3, by = c("geotype","geotypevalue","geoname","county_name","race_eth_name","race_eth_code"),all=TRUE)
iData <- merge(iData,iDx4, by = c("geotype","geotypevalue","geoname","county_name","race_eth_name","race_eth_code"),all=TRUE)
iData <- merge(iData,iDx5, by = c("geotype","geotypevalue","geoname","county_name","race_eth_name","race_eth_code"),all=TRUE)


iData <- mutate(iData,pPov=nPov/popPov, pPark= nPark/popPark, pFood = nFood/popFood, pAbuse=nAbuse/popAbuse, pAlco=nAlco/popAlco)

saveRDS(iData, file="iData.rds")

