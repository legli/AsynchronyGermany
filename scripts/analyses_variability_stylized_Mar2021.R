library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(codyn)
library(NLMR)
library(landscapetools)
library(raster)
library(viridis)
library(cowplot)

vecCrops <- c("winterwheat","barley","rapeseed","rye","sugarbeet",
              "maize","potato","sunflower","summerwheat","soybean") ## ordered by mean harvested area in the farms considered


# open climate response data
dfTemp <- read.xlsx("datasets/climateResponse/dfSuitabilityTemperature_Mar2021.xlsx")
head(dfTemp)
dfTemp <- dfTemp[,c("climate",vecCrops)]

dfPrec <- read.xlsx("datasets/climateResponse/dfSuitabilityPrecipitation_Mar2021.xlsx")
dfPrec <- dfPrec[,c("climate",vecCrops)]
head(dfPrec)

dfTempG <- dfTemp
names(dfTempG)<- c("temp",1:10)
dfTempG <- dfTempG %>% gather (crop,suitabilityTemp,"1":"10")
dfPrecG <- dfPrec
names(dfPrecG)<- c("prec",1:10)
dfPrecG <- dfPrecG %>% gather (crop,suitabilityPrec,"1":"10")

## change to numeric
dfTempG$crop <- as.numeric(dfTempG$crop)
dfPrecG$crop <- as.numeric(dfPrecG$crop)

############################# Method figures
#### show how simulation was done
set.seed(123)

rGradient <- nlm_mpd(10,10,roughness = 0)
gradient_spdf <- as(rGradient, "SpatialPixelsDataFrame")
dfGradient <- as.data.frame(gradient_spdf)
colnames(dfGradient) <- c("value", "x", "y")

rRandom <- nlm_mpd(10,10,roughness = 1)
random_spdf <- as(rRandom, "SpatialPixelsDataFrame")
dfRandom <- as.data.frame(random_spdf)
colnames(dfRandom) <- c("value", "x", "y")

a1 <- ggplot() +  
  geom_tile(data=dfGradient, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="none") 
# theme(legend.key.width=unit(2, "cm"))

a2 <- ggplot() +  
  geom_tile(data=dfRandom, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="none") 
# theme(legend.key.width=unit(2, "cm"))

jpeg("results/FigS1.jpeg", width = 8, height = 4, units = 'cm', res = 600)
ggarrange(a1, a2, 
          labels = c(letters[1:2]),font.label=list(size=8),
          ncol = 2, nrow = 1)
dev.off()


#### show suitability functions
figS1a <- ggplot(data=dfTempG, aes(x=temp, y=suitabilityTemp, colour=as.factor(crop))) +
  geom_line() +
  theme_classic()+
  theme(legend.position = "none") +
  xlab("Temperature (°C)")+
  ylab("Suitability")

figS1b <- ggplot(data=dfPrecG, aes(x=prec, y=suitabilityPrec, colour=as.factor(crop))) +
  geom_line() +
  theme_classic()+
  theme(legend.position = "none") +
  xlab("Precipitation (mm)")+
  ylab("Suitability")

jpeg("results/FigS2.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(figS1a, figS1b, 
            labels = c(letters[1:2]),font.label=list(size=8),
            ncol = 2, nrow = 1)
dev.off()

##################################### Management
meanTemp <- 20
meanPrec <- 600
devTemp <- 15
devPrec <- 450


# vecSize <- c(9,25,49,81,121,169,255,1089)
# vecSizePar <- c(4,6,8,10,12,14,16,35)
# vecSize <- c(25,81,1089,9801)
# vecSizePar <- c(6,10,35,100)
vecSize <- c(25,81,1089)
vecSizePar <- c(6,10,35)

set.seed(9999)
lsDev <- lapply(1:100,function(run){
  print(run)
  lsSize <- lapply(1:length(vecSize),function(s){
    print(s)
    lsLandscape <- lapply(c("specialized","fragmented","diversified"),function(l){
      lsCrops <- lapply(1:10,function(c){
        if (l!="diversified")
        {
          rough <- 0
          if (l=="fragmented") {rough <- 1}
          rAllocation <- nlm_mpd(vecSizePar[s],vecSizePar[s],roughness = rough)
          # rAllocation <- util_classify(rAllocation, weighting = rep(1/c,c))
          rAllocation <- util_classify(rAllocation, weighting = rep(1/c,c),level_names=sample(1:10,c))
          
          dfAllocation <- cbind(data.frame(id=1:vecSize[s]),as.data.frame(rAllocation))
          names(dfAllocation)[2] <- "crop"
        }
        
        if (l=="diversified")
        {
          lsAllocation <- lapply(sample(1:10,c),function(cd){
            data.frame(id=1:vecSize[s],crop=cd)
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
write.csv(dfDev,"datasetsDerived/dataFinal_stylized.csv",row.names=F)




rm(list=ls())
