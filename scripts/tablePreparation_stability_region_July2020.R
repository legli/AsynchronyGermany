library(vegan)
library(rgdal)
library(tidyr)
library(countrycode)
library(codyn)
######## DATA PREPARATION

#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_europe.csv")
head(dfProduction,)
names(dfProduction)[1] <- "Item"
names(dfProduction)[3] <- "Level"
names(dfProduction)[4] <- "Year"
unique(dfProduction$Item)
unique(dfProduction$strucpro)
range(dfProduction$Year)


# only keep the regions covering 99.9% of the total cropland area 
dfCropland <- dfProduction[which(dfProduction$Item=="UAA"),]
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(values~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$values/sum(dfCroplandMean$values)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$Level%in%dfCroplandMean[1:ind,"Level"]),] # this are the target regions
vecLevel <- unique(dfCropland$Level)

# only keep area harvested and production
dfProduction <- dfProduction[which(dfProduction$strucpro%in%c("AR","PR")),]
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevel),] # only keep current regions
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction %>% spread(strucpro, values)
head(dfProduction)
names(dfProduction)[4] <- "AreaHarvested"
names(dfProduction)[5] <- "Production"

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories and make crops consistent with target crop file
dfCalories <- read.csv("datasets/targetCrops_europe.csv")
head(dfCalories) # Group2 is the target crop name
names(dfCalories)[1] <- "Item"
dfCalories <- dfCalories[which(!is.na(dfCalories$Item)&!is.na(dfCalories$Calories)),]
dfCalories <- dfCalories[which(dfCalories$NameFAO%in%c("WHEAT","RYE","BARLEY","MAIZE","SORGHUM","RICE PADDY","POTATOES","SUGAR BEETS","RAPESEED","SUNFLOWER SEEDS","SOYBEANS","BUCKWHEAT, MILLET, CANARY SEED")),]
setdiff(dfProduction$Item,dfCalories$Item)

# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories")],by="Item")
names(dfProduction)

# change production to calories (from 1000t to t)
dfProduction$Production <- dfProduction$Production*dfProduction$Calories
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","Year","AreaHarvested","Production")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1978:1987),"timePeriod"] = 1978
dfProduction[dfProduction$Year%in%c(1988:1997),"timePeriod"] = 1988
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 12 crops

nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates

#area to ha
dfProduction$AreaHarvested <- dfProduction$AreaHarvested*1000


### add cntry code and NUTS names
dfProduction$ISO2 <- substr(dfProduction$Level,1,2)
dfProduction$Country <- countrycode(dfProduction$ISO2, 'iso2c', 'iso3c') 
dfProduction[which(dfProduction$ISO2=="EL"),"ISO2"] <- "GR"
dfProduction[which(dfProduction$ISO2=="UK"),"ISO2"] <- "GB"
dfProduction$Country <- countrycode(dfProduction$ISO2, 'iso2c', 'iso3c') 
head(dfProduction)


#### calculate yields
sum(is.na(dfProduction))
dfYield <- aggregate(cbind(Production,AreaHarvested)~Level+Country+Year,dfProduction,sum)
head(dfYield)
dfYield$Yield <- dfYield$Production/dfYield$AreaHarvested
nrow(unique(dfYield[,c("Level","Year")])) == nrow(dfYield) # check duplicates


#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Country+Item+Year,dfProduction,sum)
dfShannon <- aggregate(AreaHarvested~Country+Year,dfShannon,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(unique(dfShannon[,c("Country","Year")])) == nrow(dfShannon) # check duplicates


######## CALCULATE VARIABLES FOR THE 4 TIME PERIOS

# get regions across datasets
vecCountryFinal <- Reduce(intersect,list(dfProduction$Country,dfYield$Country,dfShannon$Country))

## summarize all relevant metrics per time period 
lsAll <- lapply(vecCountryFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){
    dfProductionLevel <- dfProduction[which(dfProduction$Country==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    dfYieldLevel <- dfYield[which(dfYield$Country==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
    dfShannonLevel <- dfShannon[which(dfShannon$Country==lev&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
    
    if(length(unique(dfYieldLevel$Level))>=2){
      
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
      
      data.frame(Country=lev,timePeriod=yearStart,stabilityProduction=stability,asynchronyBetween=asynchronyB,asynchronyWithin=asynchronyW,areaHarvested=sum(dfYieldLevel$AreaHarvested),diversity=mean(dfShannonLevel$diversity))
      
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Country","timePeriod")])) == nrow(dfAll) # check duplicates
unique(dfAll$Country)
head(dfAll)
nrow(dfAll)

write.csv(dfAll, "datasetsDerived/dataFinal_all_region.csv",row.names=F)



## summarize effect of crop diversity and space on asynchrony per time period
lsAll <- lapply(vecCountryFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){
    show(yearStart)
    dfProductionLevel <- dfProduction[which(dfProduction$Country==lev&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]

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
      lsLevel <- lapply(1:length(vecLevelDecreasing),function(l1){
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
      cbind(data.frame(Country=lev),timePeriod=yearStart,rbind(dfCrops,dfLevel))
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
unique(dfAll$Country)
head(dfAll)
nrow(dfAll)

write.csv(dfAll, "datasetsDerived/dataFinal_sampling_region.csv",row.names=F)






rm(list=ls())
