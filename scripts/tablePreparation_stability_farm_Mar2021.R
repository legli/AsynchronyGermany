library(haven)
library(vegan)
library(tidyr)
library(rgdal)
library(codyn)
library(openxlsx)
library(nlme)
library(MuMIn)
library(dplyr)

######## DATA PREPARATION
set.seed(123456)
vecName <- c("Intercept","asynchronyBetween_farm","R2m","R2c","AIC")

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

# only keep the farms covering 99.9% of the total cropland area 
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
dfCalories <- read.xlsx("datasets/targetCrops_farmlevel_Nov2020.xlsx")
names(dfCalories)[6] <- "Item"
unique(dfCalories$cropZabel)


# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","cropZabel","Calories")],by="Item")
names(dfProduction)

# change production to calories (dt to t)
dfProduction$Production <- dfProduction$Production*dfProduction$AreaHarvested*0.1*dfProduction$Calories

# aggregate similar crops 
sum(is.na(dfProduction$Production))
sum(is.na(dfProduction$AreaHarvested))
dfProduction <- aggregate(cbind(Production,AreaHarvested)~Level+REGION_ID+cropZabel+Year,dfProduction,sum)
names(dfProduction)[3] <- "Item"

# calculate individual yields and remove very unrealistic values
# dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
# hist(dfProduction$Yield)
# dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","REGION_ID","Item","Year","AreaHarvested","Production")]

# target Period
dfProduction <- dfProduction[which(dfProduction$Year%in%c(1998:2018)),]

## remove farms that occur in multiple districts
dfCountDistrict <- unique(dfProduction[,c("Level","REGION_ID")])
dfCountDistrict$sum <- 1
dfCountDistrict <- aggregate(sum~Level,dfCountDistrict,sum)
dfProduction <- dfProduction[which(dfProduction$Level%in%dfCountDistrict[which(dfCountDistrict$sum==1),"Level"]),]


### function to create all datasets for different time intervals
functionTimePeriod <- function(vecIntervals,lengthInterval,nameInterval){
  # only keep crops with n entries per time period
  dfProduction$timePeriod=0
  dfProduction[dfProduction$Year%in%c(vecIntervals[1]:(vecIntervals[1]+(lengthInterval-1))),"timePeriod"] = vecIntervals[1]
  if (length(vecIntervals) > 1)
  {
    dfProduction[dfProduction$Year%in%c(vecIntervals[2]:(vecIntervals[2]+(lengthInterval-1))),"timePeriod"] = vecIntervals[2]
  }
  if (length(vecIntervals) > 2)
  {
    dfProduction[dfProduction$Year%in%c(vecIntervals[3]:(vecIntervals[3]+(lengthInterval-1))),"timePeriod"] = vecIntervals[3]
  }
  
  sum(is.na(dfProduction))
  dfProduction$sum <- 1
  dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
  head(dfCount)
  
  dfProduction <- merge(dfProduction[,c("Level","REGION_ID","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
  dfProduction <- dfProduction[which(dfProduction$sum==lengthInterval),c("Level","REGION_ID","Item","Year","timePeriod","AreaHarvested","Production")]
  show(length(unique(dfProduction$Item)))
  show(length(unique(dfProduction$Level)))
  
  show(nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction)) # check duplicates
  
  #### calculate total production
  sum(is.na(dfProduction))
  dfProductionTot <- aggregate(cbind(Production,AreaHarvested)~Level+REGION_ID+Year,dfProduction,sum)
  show(nrow(unique(dfProductionTot[,c("Level","Year")])) == nrow(dfProductionTot)) # check duplicates
  
  dfProductionDistrict <- aggregate(cbind(Production,AreaHarvested)~REGION_ID+Year+timePeriod,dfProduction,sum)
  names(dfProductionDistrict)[1] <- "district"
  write.csv(dfProductionDistrict, paste0("datasetsDerived/dataFinal_districtAnnual_",nameInterval,".csv"),row.names=F)
  
  ######## CALCULATE VARIABLES FOR THE TIME PERIOS
  # get regions across datasets
  vecDistrictFinal <- unique(dfProduction$REGION_ID)
  
  ## summarize all relevant metrics per time period 
  lsAll <- lapply(vecDistrictFinal,function(lev){
    # total production
    show(as.character(lev))
    lsAggregate <- lapply(vecIntervals,function(yearStart){
      dfProductionLevel <- dfProduction[which(dfProduction$REGION_ID==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+(lengthInterval-1))),]
      dfProductionTotLevel <- dfProductionTot[which(dfProductionTot$REGION_ID==lev&dfProductionTot$Year>=yearStart&dfProductionTot$Year<=(yearStart+(lengthInterval-1))),]

      # stability
      dfProductionAgg <- aggregate(Production~Year,dfProductionTotLevel,sum)
      dfProductionAgg$ProductionDet1 <-resid(lm(Production ~ Year^2,data=dfProductionAgg)) 
      dfProductionAgg$ProductionDet2 <- resid(loess(Production ~ Year,data=dfProductionAgg))
      
      stability0 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$Production,na.rm=T)
      stability1 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$ProductionDet1,na.rm=T)
      stability2 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$ProductionDet2,na.rm=T)
      
      # asynchrony between crops farm level
      lsFarm <- lapply(unique(dfProductionLevel$Level),function(f){
        dfProductionFarm <- dfProductionLevel[which(dfProductionLevel$Level==f),]
        lsProductionCropDet <- lapply(unique(dfProductionFarm$Item),function(i){
          dfCrop <- dfProductionFarm[which(dfProductionFarm$Item==i),]
          dfCrop$Production1 <- resid(lm(Production ~ Year^2,data=dfCrop))
          dfCrop$Production2 <- resid(loess(Production ~ Year,data=dfCrop))
          dfCrop
        })
        dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
        data.frame(Level=f,nCrops=length(unique(dfProductionFarm$Item)),
                   asynchronyB0 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production"),10),
                   asynchronyB1 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production1"),10),
                   asynchronyB2 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production2"),10), 
                   areaHarvested=sum(dfProductionFarm$AreaHarvested))
      })
      dfFarm <- do.call(rbind,lsFarm)
      minCropsFarm = min(dfFarm$nCrops)
      maxCropsFarm = max(dfFarm$nCrops)
      asynchronyBfarm0 = weighted.mean(dfFarm$asynchronyB0,dfFarm$areaHarvested)
      asynchronyBfarm1 = weighted.mean(dfFarm$asynchronyB1,dfFarm$areaHarvested) 
      asynchronyBfarm2 = weighted.mean(dfFarm$asynchronyB2,dfFarm$areaHarvested) 
      
      # asynchrony between crops regional level
      dfProductionCropAgg <- aggregate(cbind(Production,AreaHarvested)~Year+Item,dfProductionLevel,sum)
      lsProductionCropDet <- lapply(unique(dfProductionCropAgg$Item),function(i){
        dfCrop <- dfProductionCropAgg[which(dfProductionCropAgg$Item==i),]
        dfCrop$Production1 <- resid(lm(Production ~ Year^2,data=dfCrop))
        dfCrop$Production2 <- resid(loess(Production ~ Year,data=dfCrop))
        dfCrop
      })
      dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
      asynchronyB0 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production"),10) 
      asynchronyB1 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production1"),10) 
      asynchronyB2 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production2"),10)
      
      # dfCor0 <- dfProductionCropDet[,c("Year","Item","Production")]  %>% spread(Year, Production)
      # p0 <- mean(cor(dfCor0[,2:ncol(dfCor0)]))
      # dfCor1 <- dfProductionCropDet[,c("Year","Item","Production1")]  %>% spread(Year, Production1)
      # p1 <- mean(cor(dfCor1[,2:ncol(dfCor1)]))
      # dfCor2 <- dfProductionCropDet[,c("Year","Item","Production2")]  %>% spread(Year, Production2)
      # p2 <- mean(cor(dfCor2[,2:ncol(dfCor2)]))
      
      # asynchrony within crops
      lsAsynchronyCrops <- lapply(unique(dfProductionLevel$Item),function(i){
        dfCrop <- dfProductionLevel[which(dfProductionLevel$Item==i),]
        lsCropDet <- lapply(unique(dfCrop$Level),function(l){
          dfLevel <- dfCrop[which(dfCrop$Level==l),]
          dfLevel$Production1 <- resid(lm(Production ~ Year^2,data=dfLevel))
          dfLevel$Production2 <- resid(loess(Production ~ Year,data=dfLevel))
          dfLevel
        })
        dfCropDet <- do.call(rbind,lsCropDet)
        if (length(unique(dfCropDet$Level))>1) # only crops with more than one farm
        {
          # dfCor0 <- dfCropDet[,c("Year","Level","Production")]  %>% spread(Year, Production)
          # p0 <- mean(cor(dfCor0[,2:ncol(dfCor0)]))
          # dfCor1 <- dfCropDet[,c("Year","Level","Production1")]  %>% spread(Year, Production1)
          # p1 <- mean(cor(dfCor1[,2:ncol(dfCor1)]))
          # dfCor2 <- dfCropDet[,c("Year","Level","Production2")]  %>% spread(Year, Production2)
          # p2 <- mean(cor(dfCor2[,2:ncol(dfCor2)]))
          data.frame(Item=i,AreaHarvested=sum(dfCrop$AreaHarvested),
                     asynchronyCrop0= 1-round(synchrony(dfCropDet,time.var="Year",species.var="Level",abundance.var="Production"),10),
                     asynchronyCrop1= 1-round(synchrony(dfCropDet,time.var="Year",species.var="Level",abundance.var="Production1"),10),
                     asynchronyCrop2= 1-round(synchrony(dfCropDet,time.var="Year",species.var="Level",abundance.var="Production2"),10))
        }
        
      })
      dfAsynchronyCrops <- do.call(rbind,lsAsynchronyCrops)
      asynchronyW0 <- weighted.mean(dfAsynchronyCrops$asynchronyCrop0,dfAsynchronyCrops$AreaHarvested)
      asynchronyW1 <- weighted.mean(dfAsynchronyCrops$asynchronyCrop1,dfAsynchronyCrops$AreaHarvested)
      asynchronyW2 <- weighted.mean(dfAsynchronyCrops$asynchronyCrop2,dfAsynchronyCrops$AreaHarvested)
      # pW0 <- weighted.mean(dfAsynchronyCrops$p0,dfAsynchronyCrops$AreaHarvested)
      # pW1 <- weighted.mean(dfAsynchronyCrops$p1,dfAsynchronyCrops$AreaHarvested)
      # pW2 <- weighted.mean(dfAsynchronyCrops$p2,dfAsynchronyCrops$AreaHarvested)
      
      
      data.frame(district=lev,timePeriod=yearStart,nFarms=length(unique(dfProductionLevel$Level)),nCrops=length(unique(dfProductionLevel$Item)),nCropsFarmMin=minCropsFarm,nCropsFarmMax=maxCropsFarm,
                 stabilityProduction0=stability0,stabilityProduction1=stability1,stabilityProduction2=stability2,
                 asynchronyBetween_region0=asynchronyB0,asynchronyBetween_region1=asynchronyB1,asynchronyBetween_region2=asynchronyB2,
                 asynchronyBetween_farm0=asynchronyBfarm0,asynchronyBetween_farm1=asynchronyBfarm1,asynchronyBetween_farm2=asynchronyBfarm2,
                 asynchronyWithin0=asynchronyW0,asynchronyWithin1=asynchronyW1,asynchronyWithin2=asynchronyW2,
                 areaHarvested=sum(dfProductionTotLevel$AreaHarvested))
  
    })
    do.call(rbind,lsAggregate)
  })
  dfAll <- do.call(rbind,lsAll)
  show(nrow(unique(dfAll[,c("district","timePeriod")])) == nrow(dfAll)) # check duplicates
  write.csv(dfAll, paste0("datasetsDerived/dataFinal_districtSummary_",nameInterval,".csv"),row.names=F)
  

  #### Sampling
  vecFarms <- c(1:3,5,seq(10,90,10),seq(100,500,20))

  ## summarize effect of crop diversity and space on asynchrony per time period
  lsAll <- lapply(vecDistrictFinal,function(lev){
    # total production
    show(as.character(lev))
    lsAggregate <- lapply(vecIntervals,function(yearStart){
      # show(yearStart)
      dfProductionLevel <- dfProduction[which(dfProduction$REGION_ID==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+(lengthInterval-1))),]

      # if(length(unique(dfProductionLevel$Level))>=2){

        # crop-specific and region-specific harvested area
        dfAreaLevel <- aggregate(AreaHarvested~Level+Year,dfProductionLevel,sum)
        dfAreaLevel <- aggregate(AreaHarvested~Level,dfAreaLevel,mean)

        # asynchrony between crops
        vecCrops <- unique(dfProductionLevel$Item)
        lsCropsRep <- lapply(1:10,function(r){
          lsCrops <- lapply(1:length(vecCrops),function(c1){
            vecSample <- sample(vecCrops,c1)
            dfProductionCropAgg<- aggregate(Production~Year+Item,dfProductionLevel[which(dfProductionLevel$Item%in%vecSample),],sum)

            lsProductionCropDet <- lapply(unique(dfProductionCropAgg$Item),function(c2){
              dfCrop <- dfProductionCropAgg[which(dfProductionCropAgg$Item==c2),]
              dfCrop$Production1 <- resid(lm(Production ~ Year^2,data=dfCrop))
              dfCrop$Production2 <- resid(loess(Production ~ Year,data=dfCrop))
              dfCrop
            })
            dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
            asynchronyB0 = round(1-synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production"),10)
            asynchronyB1 = round(1-synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production1"),10)
            asynchronyB2 = round(1-synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production2"),10)
            data.frame(no=c1,asynchrony0=asynchronyB0,asynchrony1=asynchronyB1,asynchrony2=asynchronyB2)
          })
          dfCrops <- do.call(rbind,lsCrops)
          dfCrops$metric <- "asynchronyBetween"
          dfCrops$rep <- r
          dfCrops
        })
        dfCropsRep <- do.call(rbind,lsCropsRep)

        ## asynchrony within crops (iterate through crops and farms)
        # detrend for each farm and crop
        lsAsynchronyCrops <- lapply(unique(dfProductionLevel$Item),function(c){
          dfCrop <- dfProductionLevel[which(dfProductionLevel$Item==c),]
          lsCropDet <- lapply(unique(dfCrop$Level),function(l2){
            dfLevel <- dfCrop[which(dfCrop$Level==l2),]
            dfLevel$Production1 <- resid(lm(Production ~ Year^2,data=dfLevel))
            dfLevel$Production2 <- resid(loess(Production ~ Year,data=dfLevel))
            dfLevel
          })
          dfCropDet <- do.call(rbind,lsCropDet)
        })
        dfAsynchronyCrops <- do.call(rbind,lsAsynchronyCrops)
        
        lsLevelRep <- lapply(1:10,function(r){
          lsCrops <- lapply(vecCrops,function(c1){
            dfAsynchronyCropsCrop <- dfAsynchronyCrops[which(dfAsynchronyCrops$Item==c1),]
            if (length(unique(dfAsynchronyCropsCrop$Level))>1){
              vecFarmShuffle <- sample(unique(dfAsynchronyCropsCrop$Level)) # random sequence (kept constant for each crop)
              vecFarmsAdapted <- vecFarms
              if (max(vecFarms) > length(vecFarmShuffle))
              {vecFarmsAdapted <- vecFarms[-which(vecFarms>length(vecFarmShuffle))]}
              lsLevel <- lapply(vecFarmsAdapted,function(l1){
                dfLevel <- dfAsynchronyCropsCrop[which(dfAsynchronyCropsCrop$Level%in%vecFarmShuffle[1:l1]),]
    
                data.frame(Item=c1,no=l1,AreaHarvested=sum(dfLevel$AreaHarvested),
                           asynchronyCrop0= 1-round(synchrony(dfLevel,time.var="Year",species.var="Level",abundance.var="Production"),10),
                           asynchronyCrop1= 1-round(synchrony(dfLevel,time.var="Year",species.var="Level",abundance.var="Production1"),10),
                           asynchronyCrop2= 1-round(synchrony(dfLevel,time.var="Year",species.var="Level",abundance.var="Production2"),10))
    
              })
              do.call(rbind,lsLevel)
            }
          })
          dfCrops <- do.call(rbind,lsCrops)
          # asynchrony within crops over all crops (weighted average)
          dfSummary <- data.frame(dfCrops %>%
                                group_by(no) %>%
                                dplyr::summarise(asynchrony0 = weighted.mean(asynchronyCrop0,AreaHarvested),
                                                 asynchrony1 = weighted.mean(asynchronyCrop1,AreaHarvested),
                                                 asynchrony2 = weighted.mean(asynchronyCrop2,AreaHarvested)))
          dfSummary$rep <- r
          dfSummary
        })
        dfLevelRep <- do.call(rbind,lsLevelRep)
        dfLevelRep$metric <- "asynchronyWithin"
        
# 
#         # repeat
#         lsLevelRep <- lapply(1:10,function(r){
#           vecFarmShuffle <- sample(unique(dfAsynchronyCrops$Level))
#           vecFarmsAdapted <- vecFarms
#           if (max(vecFarms) > length(vecFarmShuffle))
#           {vecFarmsAdapted <- vecFarms[-which(vecFarms>length(vecLevelDecreasing))]}
#           lsLevel <- lapply(vecFarmsAdapted,function(l1){
#             dfLevel <- dfAsynchronyCrops[which(dfAsynchronyCrops$Level%in%vecFarmShuffle[1:l1]),]
#             lsAsynchronyCropsLevel <- lapply(unique(dfAsynchronyCrops$Item),function(c){
#               dfCrop <- dfLevel[which(dfLevel$Item==c),]
#               data.frame(Item=c,AreaHarvested=sum(dfCrop$AreaHarvested),
#                          asynchronyCrop0= 1-round(synchrony(dfCrop,time.var="Year",species.var="Level",abundance.var="Production"),10),
#                          asynchronyCrop1= 1-round(synchrony(dfCrop,time.var="Year",species.var="Level",abundance.var="Production1"),10),
#                          asynchronyCrop2= 1-round(synchrony(dfCrop,time.var="Year",species.var="Level",abundance.var="Production2"),10))
#             })
#             dfAsynchronyCropsLevel <- do.call(rbind,lsAsynchronyCropsLevel)
#             asynchronyW0 <- weighted.mean(dfAsynchronyCropsLevel$asynchronyCrop0,dfAsynchronyCropsLevel$AreaHarvested)
#             asynchronyW1 <- weighted.mean(dfAsynchronyCropsLevel$asynchronyCrop1,dfAsynchronyCropsLevel$AreaHarvested)
#             asynchronyW2 <- weighted.mean(dfAsynchronyCropsLevel$asynchronyCrop2,dfAsynchronyCropsLevel$AreaHarvested)
#             data.frame(no=l1,asynchrony0=asynchronyW0,asynchrony1=asynchronyW1,asynchrony2=asynchronyW2,
#                               AreaHarvested=sum(dfAreaLevel[which(dfAreaLevel$Level%in%vecFarmShuffle[1:l1]),"AreaHarvested"]))
#           })
#           dfLevel <- do.call(rbind,lsLevel)
#           dfLevel$propNo <- dfLevel$AreaHarvested/max(dfLevel$AreaHarvested)
#           dfLevel$propAsynchrony0 <- dfLevel$asynchrony0/max(dfLevel$asynchrony0)
#           dfLevel$propAsynchrony1 <- dfLevel$asynchrony1/max(dfLevel$asynchrony1)
#           dfLevel$propAsynchrony2 <- dfLevel$asynchrony2/max(dfLevel$asynchrony2)
#           dfLevel$metric <- "asynchronyWithin"
#           dfLevel$rep <- r
#           dfLevel
#         })
#         dfLevelRep <- do.call(rbind,lsLevelRep)

        # collect data
        cbind(data.frame(REGION_ID=lev),timePeriod=yearStart,rbind(dfCropsRep,dfLevelRep))
      # }
    })
    do.call(rbind,lsAggregate)
  })
  dfAll <- do.call(rbind,lsAll)
  write.csv(dfAll, paste0("datasetsDerived/dataFinal_districtSampling_",nameInterval,".csv"),row.names=F)

  #### summarize relevant metrics per time period at farm level
  vecFarms <- unique(dfProduction$Level)

  lsAll <- lapply(vecFarms,function(lev){
    # total production
    # show(as.character(lev))
    lsAggregate <- lapply(vecIntervals,function(yearStart){
      dfProductionLevel <- dfProduction[which(dfProduction$Level==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+(lengthInterval-1))),]
      dfProductionTotLevel <- dfProductionTot[which(dfProductionTot$Level==lev&dfProductionTot$Year>=yearStart&dfProductionTot$Year<=(yearStart+(lengthInterval-1))),]

      if (nrow(dfProductionTotLevel)==lengthInterval&(length(unique(dfProductionLevel$Item))>1)){

        # stability
        dfProductionAgg <- aggregate(Production~Year,dfProductionTotLevel,sum)
        dfProductionAgg$ProductionDet1 <-resid(lm(Production ~ Year^2,data=dfProductionAgg))
        dfProductionAgg$ProductionDet2 <-resid(loess(Production ~ Year,data=dfProductionAgg))

        stability0 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$Production,na.rm=T)
        stability1 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$ProductionDet1,na.rm=T)
        stability2 <- mean(dfProductionAgg$Production,na.rm=T)/sd(dfProductionAgg$ProductionDet1,na.rm=T)

        # asynchrony between crops farm level
        lsProductionCropDet <- lapply(unique(dfProductionLevel$Item),function(i){
          dfCrop <- dfProductionLevel[which(dfProductionLevel$Item==i),]
          dfCrop$Production1 <- resid(lm(Production ~ Year^2,data=dfCrop))
          dfCrop$Production2 <- resid(loess(Production ~ Year,data=dfCrop))
          dfCrop
        })
        dfProductionCropDet <- do.call(rbind,lsProductionCropDet)
        data.frame(farm=lev,district=unique(dfProductionTotLevel$REGION_ID),timePeriod=yearStart,nCrops=length(unique(dfProductionLevel$Item)),
                   stabilityProduction0=stability0,stabilityProduction1=stability1,stabilityProduction2=stability2,
                   asynchronyBetween0 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production"),10),
                   asynchronyBetween1 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production1"),10),
                   asynchronyBetween2 = 1-round(synchrony(dfProductionCropDet,time.var="Year",species.var="Item",abundance.var="Production2"),10),
                   areaHarvested=sum(dfProductionLevel$AreaHarvested))
      }
    })
    do.call(rbind,lsAggregate)
  })
  dfAll <- do.call(rbind,lsAll)
  show(nrow(unique(dfAll[,c("farm","timePeriod")])) == nrow(dfAll)) # check duplicates
  show(nrow(dfAll))
  show(length(unique(dfAll$farm)))

  ## normalize range to make regressions comparable
  # dfAll$stabilityProduction0 <- range01(dfAll$stabilityProduction0)
  # dfAll$stabilityProduction1 <- range01(dfAll$stabilityProduction1)
  # dfAll$stabilityProduction2 <- range01(dfAll$stabilityProduction2)
  # dfAll$stabilityProduction0 <- scale(dfAll$stabilityProduction0,center = T,scale=T)
  # dfAll$stabilityProduction1 <- scale(dfAll$stabilityProduction1,center = T,scale=T)
  # dfAll$stabilityProduction2 <- scale(dfAll$stabilityProduction2,center = T,scale=T)
  dfAll$asynchronyBetween0 <- round(dfAll$asynchronyBetween0,1)
  dfAll$asynchronyBetween1 <- round(dfAll$asynchronyBetween1,1)
  dfAll$asynchronyBetween2 <- round(dfAll$asynchronyBetween2,1)
  
  dfSummary <- rbind(data.frame(dfAll %>%
                      group_by(asynchronyBetween=asynchronyBetween0) %>%
                      dplyr::summarise(meanStabilityProduction = mean(stabilityProduction0),
                                       sdStabilityProduction = sd(stabilityProduction0),
                                       det = "none")),
                     data.frame(dfAll %>%
                                  group_by(asynchronyBetween=asynchronyBetween1) %>%
                                  dplyr::summarise(meanStabilityProduction = mean(stabilityProduction1),
                                                   sdStabilityProduction = sd(stabilityProduction1),
                                                   det = "lm")),
                     data.frame(dfAll %>%
                                  group_by(asynchronyBetween=asynchronyBetween2) %>%
                                  dplyr::summarise(meanStabilityProduction = mean(stabilityProduction2),
                                                   sdStabilityProduction = sd(stabilityProduction2),
                                                   det = "loess")))
  
  write.csv(dfSummary, paste0("datasetsDerived/dataFinal_farmSummary_",nameInterval,".csv"),row.names=F)
  
  # ## regression
  # modLME0 <- lme(stabilityProduction0~asynchronyBetween0,
  #                      random=~1|district,method = "ML",
  #                      data=dfAll)
  # modLME1 <- lme(stabilityProduction1~asynchronyBetween1,
  #                random=~1|district,method = "ML",
  #                data=dfAll)
  # modLME2 <- lme(stabilityProduction2~asynchronyBetween2,
  #                random=~1|district,method = "ML",
  #                data=dfAll)
  # 
  # functionDet <- function(modLME,det){
  #   dfMod <- data.frame(summary(modLME)$tTable)
  #   dfMod <- rbind(dfMod,data.frame(Value=r.squaredGLMM(modLME)[1],Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
  #   dfMod <- rbind(dfMod,data.frame(Value=r.squaredGLMM(modLME)[2],Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
  #   dfMod <- rbind(dfMod,data.frame(Value=AIC(modLME),Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
  #   names(dfMod) <- c("Effect","SE","DF","tVal","pVal")
  #   dfMod$Detrend <- det
  #   dfMod$nam <- factor(vecName,levels=vecName)
  #   dfMod
  # }
  # dfRegression <- rbind(functionDet(modLME1,"lm"),functionDet(modLME2,"loess"),functionDet(modLME0,"none"))
  # dfRegression$Detrend <- factor(dfRegression$Detrend ,levels=c("lm","loess","none"))
  # indLow <- which(dfRegression$pVal<0.0001)
  # dfRegression[,c(1:5)] <- round(dfRegression[,c(1:5)] ,2)
  # dfRegression$Value <- paste0(dfRegression$Effect," (",dfRegression$SE,")")
  # dfRegression[indLow,"pVal"] <- "<0.0001"
  # dfRegression <- dfRegression[,c("Value","DF","tVal","pVal")]
  # dfRegression <- cbind(dfRegression[1:5,],dfRegression[6:10,],dfRegression[11:15,])
  # colnames(dfRegression) <- rep(c("Estimate (SE)","DF","T","p-value"),3)
  # row.names(dfRegression) <- vecName
  # write.xlsx(dfRegression, paste0("results/",nameTable,".xlsx"),row.names=T)
}
functionTimePeriod(vecIntervals = c(1999,2009),lengthInterval = 10,nameInterval = "10years")
functionTimePeriod(vecIntervals = c(1999,2011),lengthInterval = 8,nameInterval = "8years")
# functionTimePeriod(vecIntervals = c(1999),lengthInterval = 20,nameInterval = "20years")

rm(list=ls())

