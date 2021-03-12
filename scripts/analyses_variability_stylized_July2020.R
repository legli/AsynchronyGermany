library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(codyn)
library(NLMR)
library(landscapetools)
library(raster)

vecCrops <- c("barley","cassava","groundnut","maize","millet",
              "potato","rapeseed","rice","rye","sorghum",
              "soybean","sugarbeet","sunflower","wheat")

# open climate response data
dfTemp <- read.xlsx("datasets/climateResponse/dfSuitabilityTemperature.xlsx")
head(dfTemp)
dfTemp <- dfTemp[,c("climate",vecCrops)]

dfPrec <- read.xlsx("datasets/climateResponse/dfSuitabilityPrecipitation.xlsx")
dfPrec <- dfPrec[,c("climate",vecCrops)]
head(dfPrec)

dfTempG <- dfTemp
names(dfTempG)<- c("temp",1:14)
dfTempG <- dfTempG %>% gather (crop,suitabilityTemp,"1":"14")
dfTempG$crop <- as.numeric(dfTempG$crop)
dfPrecG <- dfPrec
names(dfPrecG)<- c("prec",1:14)
dfPrecG <- dfPrecG %>% gather (crop,suitabilityPrec,"1":"14")
dfPrecG$crop <- as.numeric(dfPrecG$crop)

## plot reaction
figS1a <- ggplot(data=dfTempG, aes(x=climate, y=suitability, fill=crop)) +
  geom_line() +
  theme(legend.position = "none") 

figS1b <- ggplot(data=dfPrecG, aes(x=climate, y=suitability, fill=crop)) +
  geom_line() +
  theme(legend.position = "none") 


##################################### Management
meanTemp <- 24
meanPrec <- 750
devTemp <- 10
devPrec <- 550


# vecSize <- c(9,25,49,81,121,169,255,1089)
# vecSizePar <- c(4,6,8,10,12,14,16,35)
vecSize <- c(25,81,1089)
vecSizePar <- c(6,10,35)

set.seed(9999)
lsDev <- lapply(1:10,function(run){
  print(run)
  lsSize <- lapply(1:length(vecSize),function(s){
    print(s)
    lsLandscape <- lapply(c("specialized","fragmented","diversified"),function(l){
      lsCrops <- lapply(1:14,function(c){
        if (l!="diversified")
        {
          rough <- 0
          if (l=="fragmented") {rough <- 1}
          rAllocation <- nlm_mpd(vecSizePar[s],vecSizePar[s],roughness = rough)
          rAllocation <- util_classify(rAllocation, weighting = rep(1/c,c))
          dfAllocation <- cbind(data.frame(id=1:vecSize[s]),as.data.frame(rAllocation))
          names(dfAllocation)[2] <- "crop"
        }
        
        if (l=="diversified")
        {
          lsAllocation <- lapply(1:c,function(c){
            data.frame(id=1:vecSize[s],crop=c)
          })
          dfAllocation <- do.call(rbind,lsAllocation)
        }
        
        lsYears <-lapply(1:10,function(y){
          
          # simulate climate
          devTempActual <- runif(1,0,devTemp)
          rTempDist <-  round((nlm_mpd(vecSizePar[s],vecSizePar[s],roughness = 0))*(2*devTempActual)+(meanTemp-devTempActual))
          dfTempDist <- as.data.frame(rTempDist)
          names(dfTempDist) <- "temp"
          dfTempDist$id <- 1:vecSize[s]
          devPrecActual <- runif(1,0,devPrec)
          rPrecDist <-  round((nlm_mpd(vecSizePar[s],vecSizePar[s],roughness = 0))*(2*devPrecActual)+(meanPrec-devPrecActual))
          dfPrecDist <- as.data.frame(rPrecDist)
          names(dfPrecDist) <- "prec"
          dfPrecDist$id <- 1:vecSize[s]
          
          # combine landscape and climate
          dfAll <- merge(dfAllocation,dfTempDist)
          dfAll <- merge(dfAll,dfPrecDist)
          
          # add suitability for actual climate
          dfAll <- merge(dfAll,dfTempG)
          dfAll <- merge(dfAll,dfPrecG)
          dfAll$suitability <- apply(dfAll[,c("suitabilityTemp","suitabilityPrec")],1,min)
          dfAll$year <- y
          dfAll[,c("id","year","crop","suitability")]
        })
        dfYears <- do.call(rbind,lsYears)
        head(dfYears)
        
        dfAgg <- aggregate(suitability~year,dfYears,sum)
        
        dfYearsAgg <- aggregate(suitability~year+crop,dfYears,sum)
        aB <- round(1-synchrony(dfYearsAgg,time.var="year",species.var="crop",abundance.var="suitability"),10)
        
        vecAW <-sapply(unique(dfYears$crop),function(c){
          round(1-synchrony(dfYears[which(dfYears$crop==c),],time.var="year",species.var="id",abundance.var="suitability"),10)
        })
        vecAW[is.na(vecAW)] <- 0
        dfResult <- data.frame(cv=sd(dfAgg$suitability)/mean(dfAgg$suitability),asynchronyB=aB,asynchronyW=mean(vecAW))
        dfResult$diversity <- c
        dfResult
      })
      dfCrops <- do.call(rbind,lsCrops)
      dfCrops$landscape <- l
      dfCrops
    })
    dfLandscape <- do.call(rbind,lsLandscape)
    dfLandscape$size <- s
    dfLandscape
  })
  dfSize <- do.call(rbind,lsSize)
  dfSize$rep <- run
  dfSize
})
dfDev <- do.call(rbind,lsDev)
head(dfDev)
write.csv(dfDev,"datasetsDerived/dataFinal_all_stylized.csv",row.names=F)




rm(list=ls())
