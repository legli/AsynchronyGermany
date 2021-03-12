library(haven)
library(vegan)
library(tidyr)
library(rgdal)
library(codyn)

######## DATA PREPARATION

#### read Testbetriebsdaten
dfFull <- as.data.frame(read_sas("P:/egli20191219.sas7bdat"))
head(dfFull)
names(dfFull)
str(dfFull)

# remove columns not needed (fertilizer animals, separate labor)
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z0023s02","z1031s02","z2559s02","z5239s06","z5239s07","z5240s06","z5240s07","z7099s03","z7089s03","z7098s03",
                                            "z8014s02","z8015s02","z8150s02","z8153s02","z8156s02","z2539s02","z2540s02","z7099s03"))]

# adapt names
names(dfFull)[which(names(dfFull)=="key")] <- "Level"
names(dfFull)[which(names(dfFull)=="jahr")] <- "Year"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "state"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "district"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "date"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "croplandArea"
names(dfFull)

## remove regions with zero area
dfFull <- dfFull[which(dfFull$croplandArea>0),]

# only keep the regions covering 99.9% of the total cropland area 
dfCropland <- na.omit(dfFull[,c("Level","Year","croplandArea")])
dfCroplandMean <- aggregate(croplandArea~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]

dfFull <- dfFull[which(dfFull$Level%in%dfCroplandMean[1:ind,"Level"]),]

# add spatial id
dfDistrict <- read.csv("datasets/districts_farmlevel.csv",sep=";")
head(dfDistrict)

dfFull <- merge(dfFull,dfDistrict[,c("state","district","districtName","REGION_ID")],by=c("state","district"))
head(dfFull)
names(dfFull)
unique(dfFull$REGION_ID)

# only consider Year from 1.7-30.6 (same time period for all farmers)
sum(is.na(dfFull$date))
unique(substr(dfFull$date,1,4))
dfProduction <- dfFull[which(substr(dfFull$date,1,4)=="3006"|substr(dfFull$date,1,4)=="3106"|
                               substr(dfFull$date,1,3)=="306"|substr(dfFull$date,1,3)=="697"),]
sort(unique(dfProduction$date))

## remove state, district and date (not needed anymore)
dfProduction <- dfProduction[,-which(names(dfProduction)%in%c("state","district","date"))]
names(dfProduction)

# change dataset structure (frow wide to long)
dfProduction <-  dfProduction[,c(1:92,95)] %>% gather(cropVar, values,names(dfProduction)[3:92])
head(dfProduction)
dfProduction$strucpro <- "AreaHarvested"
dfProduction[which(substr(dfProduction$cropVar,7,8)=="03"),"strucpro"] <- "Production"
dfProduction$Item <- substr(dfProduction$cropVar,1,5)
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction[,-which(names(dfProduction)=="cropVar")] %>% spread(strucpro,values)
head(dfProduction)

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories
dfCalories <- read.csv("datasets/targetCrops_farmlevel.csv")
names(dfCalories)[6] <- "Item"
unique(dfCalories$cropFAO)
dfCalories <- dfCalories[which(dfCalories$cropFAO%in%c("wheat","rye","barley","maize","potatoes","sugar beets","rapeseed","sunflower seed","soybeans")),]


# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories")],by="Item")
names(dfProduction)

# change production to calories (dt to t)
dfProduction$Production <- dfProduction$Production*dfProduction$AreaHarvested*0.1*dfProduction$Calories
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","REGION_ID","Item","Year","AreaHarvested","Production")]

# target Period
dfProduction <- dfProduction[which(dfProduction$Year%in%c(1998:2017)),]
# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","REGION_ID","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","REGION_ID","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 14 crops
length(unique(dfProduction$Level)) ## 5041 farmers

nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates


#### calculate yields
sum(is.na(dfProduction))
dfYield <- aggregate(cbind(Production,AreaHarvested)~Level+REGION_ID+Year,dfProduction,sum)
head(dfYield)
dfYield$Yield <- dfYield$Production/dfYield$AreaHarvested
nrow(unique(dfYield[,c("Level","Year")])) == nrow(dfYield) # check duplicates


#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~REGION_ID+Item+Year,dfProduction,sum)
dfShannon <- aggregate(AreaHarvested~REGION_ID+Year,dfShannon,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(unique(dfShannon[,c("REGION_ID","Year")])) == nrow(dfShannon) # check duplicates


######## CALCULATE VARIABLES FOR THE 2 TIME PERIOS
# get regions across datasets
vecDistrictFinal <- Reduce(intersect,list(dfProduction$REGION_ID,dfYield$REGION_ID,dfShannon$REGION_ID))

## summarize all relevant metrics per time period 
lsAll <- lapply(vecDistrictFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1998,2008),function(yearStart){
    dfProductionLevel <- dfProduction[which(dfProduction$REGION_ID==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    dfYieldLevel <- dfYield[which(dfYield$REGION_ID==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
    dfShannonLevel <- dfShannon[which(dfShannon$REGION_ID==lev&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
    
    
    if(length(unique(dfYieldLevel$Level))>2){
      # stability
      dfProductionAgg <- aggregate(Production~Year,dfYieldLevel,sum)
      dfProductionAgg$ProductionDet <-resid(lm(Production ~ Year^2,data=dfProductionAgg)) 
      stability <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$ProductionDet,na.rm=T)
      
      # asynchrony between crops
      dfProductionCropAgg <- aggregate(cbind(Production,AreaHarvested)~Year+Item,dfProductionLevel,sum)
      lsProductionCropDet <- lapply(unique(dfProductionCropAgg$Item),function(i){
        dfCrop <- dfProductionCropAgg[which(dfProductionCropAgg$Item==i),]
        dfCrop$Production <- resid(lm(Production ~ Year^2,data=dfCrop))
        dfCrop
      })
      dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
      asynchronyB = 1-synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production") 
      
      # asynchrony within crops
      lsAsynchronyCrops <- lapply(unique(dfProductionLevel$Item),function(i){
        dfCrop <- dfProductionLevel[which(dfProductionLevel$Item==i),]
        lsCropDet <- lapply(unique(dfCrop$Level),function(l){
          dfLevel <- dfCrop[which(dfCrop$Level==l),]
          dfLevel$Production <- resid(lm(Production ~ Year^2,data=dfLevel))
          dfLevel
        })
        dfCropDet <- do.call(rbind,lsCropDet)
        data.frame(Item=i,AreaHarvested=sum(dfCropDet$AreaHarvested),asynchronyCrop= 1-synchrony(dfCropDet,time.var="Year",species.var="Level",abundance.var="Production"))
        
      })
      dfAsynchronyCrops <- do.call(rbind,lsAsynchronyCrops)
      asynchronyW <- weighted.mean(dfAsynchronyCrops$asynchronyCrop,dfAsynchronyCrops$AreaHarvested)
      
      data.frame(district=lev,timePeriod=yearStart,stabilityProduction=stability,asynchronyBetween=asynchronyB,asynchronyWithin=asynchronyW,areaHarvested=sum(dfYieldLevel$AreaHarvested),diversity=mean(dfShannonLevel$diversity))
      
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("district","timePeriod")])) == nrow(dfAll) # check duplicates
length(unique(dfAll$district))

write.csv(dfAll, "datasetsDerived/dataFinal_all_farm.csv",row.names=F)

vecFarms <- c(1:10,seq(20,500,10))
## summarize effect of crop diversity and space on asynchrony per time period
lsAll <- lapply(vecDistrictFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1998,2008),function(yearStart){
    show(yearStart)
    dfProductionLevel <- dfProduction[which(dfProduction$REGION_ID==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    
    if(length(unique(dfProductionLevel$Level))>=2){
      
      # crop-specific and region-specific harvested area
      dfAreaCrop <- aggregate(AreaHarvested~Item,dfProductionLevel,mean)
      dfAreaLevel <- aggregate(AreaHarvested~Level+Year,dfProductionLevel,sum)
      dfAreaLevel <- aggregate(AreaHarvested~Level,dfAreaLevel,mean)
      
      # asynchrony between crops
      vecCropDecreasing <-  as.character(dfAreaCrop[order(dfAreaCrop$AreaHarvested,decreasing = T),"Item"])
      lsCrops <- lapply(1:length(vecCropDecreasing),function(c1){
        dfProductionCropAgg<- aggregate(Production~Year+Item,dfProductionLevel[which(dfProductionLevel$Item%in%vecCropDecreasing[1:c1]),],sum)
        
        lsProductionCropDet <- lapply(unique(dfProductionCropAgg$Item),function(c2){
          dfCrop <- dfProductionCropAgg[which(dfProductionCropAgg$Item==c2),]
          dfCrop$Production <- resid(lm(Production ~ Year^2,data=dfCrop))
          dfCrop
        })
        dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
        asynchronyB = round(1-synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production"),10)
        data.frame(no=c1,asynchrony=asynchronyB,AreaHarvested=sum(dfAreaCrop[which(dfAreaCrop$Item%in%vecCropDecreasing[1:c1]),"AreaHarvested"]))
      })
      dfCrops <- do.call(rbind,lsCrops)
      dfCrops$propNo <- dfCrops$AreaHarvested/max(dfCrops$AreaHarvested)
      dfCrops$propAsynchrony <- dfCrops$asynchrony/max(dfCrops$asynchrony)
      dfCrops$metric <- "asynchronyBetween"
      
      # asynchrony within crops
      vecLevelDecreasing <- dfAreaLevel[order(dfAreaLevel$AreaHarvested,decreasing = T),"Level"]
      vecFarmsAdapted <- vecFarms[-which(vecFarms>length(vecLevelDecreasing))]
      lsLevel <- lapply(vecFarmsAdapted,function(l1){
        dfLevel <- dfProductionLevel[which(dfProductionLevel$Level%in%vecLevelDecreasing[1:l1]),]
        lsAsynchronyCrops <- lapply(unique(dfLevel$Item),function(c){
          dfCrop <- dfLevel[which(dfLevel$Item==c),]
          lsCropDet <- lapply(unique(dfCrop$Level),function(l2){
            dfLevel <- dfCrop[which(dfCrop$Level==l2),]
            dfLevel$Production <- resid(lm(Production ~ Year^2,data=dfLevel))
            dfLevel
          })
          dfCropDet <- do.call(rbind,lsCropDet)
          data.frame(Item=c,AreaHarvested=sum(dfCropDet$AreaHarvested),asynchronyCrop= round(1-synchrony(dfCropDet,time.var="Year",species.var="Level",abundance.var="Production"),10))
        })
        dfAsynchronyCrops <- do.call(rbind,lsAsynchronyCrops)
        asynchronyW <- weighted.mean(dfAsynchronyCrops$asynchronyCrop,dfAsynchronyCrops$AreaHarvested)
        data.frame(no=l1,asynchrony=asynchronyW,AreaHarvested=sum(dfAreaLevel[which(dfAreaLevel$Level%in%vecLevelDecreasing[1:l1]),"AreaHarvested"]))
      })
      dfLevel <- do.call(rbind,lsLevel)
      dfLevel$propNo <- dfLevel$AreaHarvested/max(dfLevel$AreaHarvested)
      dfLevel$propAsynchrony <- dfLevel$asynchrony/max(dfLevel$asynchrony)
      dfLevel$metric <- "asynchronyWithin"
      
      # collect data
      cbind(data.frame(REGION_ID=lev),timePeriod=yearStart,rbind(dfCrops,dfLevel))
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
unique(dfAll$REGION_ID)
head(dfAll)
nrow(dfAll)

write.csv(dfAll, "datasetsDerived/dataFinal_sampling_farm.csv",row.names=F)


rm(list=ls())

