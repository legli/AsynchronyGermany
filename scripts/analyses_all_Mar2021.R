#### libraries
library(nlme)
library(MuMIn)
library(car)
library(ggplot2)
library(rgdal)
library(grid)
library(countrycode)
library(dplyr)
library(tidyr)
library(ggpubr)
library(openxlsx)
library(sf)
library(scales)
library(raster)
library(fitdistrplus)

#### functions
addSmallLegend <- function(myPlot, pointSize = 1, textSize = 6, spaceLegend = 0) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

#### load datasets
dfDistrictAnnual10 <- read.csv("datasetsDerived/dataFinal_districtAnnual_10years.csv")
dfDistrictAnnual8 <- read.csv("datasetsDerived/dataFinal_districtAnnual_8years.csv")
dfDistrictSummary10 <- read.csv("datasetsDerived/dataFinal_districtSummary_10years.csv")
dfDistrictSummary8 <- read.csv("datasetsDerived/dataFinal_districtSummary_8years.csv")
dfDistrictSampling10 <- read.csv("datasetsDerived/dataFinal_districtSampling_10years.csv")
dfDistrictSampling8 <- read.csv("datasetsDerived/dataFinal_districtSampling_8years.csv")
dfFarmsummary10 <- read.csv("datasetsDerived/dataFinal_farmSummary_10years.csv")
dfFarmsummary8 <- read.csv("datasetsDerived/dataFinal_farmSummary_8years.csv")
mapDistrict <- st_as_sf(readOGR("spatial/vg2500_rbz_bld.shp"))
dfStylized <- read.csv("datasetsDerived/dataFinal_stylized.csv")

############################# Relationship between stability and shocks
vecDet <- c("lm","loess")
vecDetStability <- c("stabilityProduction1","stabilityProduction2")
funShock <- function(df1, df2,leg){
  dfSummary <- df1
  lsDet <- lapply(1:2,function(det){
    vecQuant <- as.numeric(quantile(dfSummary[,vecDetStability[det]]))
    dfSummary$bin <- 1
    dfSummary[which(dfSummary[,vecDetStability[det]]>vecQuant[2]),"bin"] <- 2
    dfSummary[which(dfSummary[,vecDetStability[det]]>vecQuant[3]),"bin"] <- 3
    dfSummary[which(dfSummary[,vecDetStability[det]]>vecQuant[4]),"bin"] <- 4
    show(table(dfSummary$bin))
    dfSummaryMean <- aggregate(stabilityProduction1~bin,dfSummary,mean)
    
    dfAnnual <- df2
    dfAnnualMean <- aggregate(Production~district+timePeriod,dfAnnual,mean)
    names(dfAnnualMean)[3] <- "ProductionMean"
    dfAnnual <- merge(dfAnnual,dfAnnualMean)
    dfAnnual <- merge(dfAnnual,dfSummary[,c("district","timePeriod","bin")],by=c("district","timePeriod"))
    head(dfAnnual)
    dfAnnual$sumShock <- 0
    dfAnnual[which(dfAnnual$Production<(dfAnnual$ProductionMean*0.9)),"sumShock"] <- 1
    dfShock <- aggregate(sumShock~bin,dfAnnual,function(i){sum(i)/length(i)})
    dfFinal <- merge(dfShock,dfSummaryMean,by="bin")
    dfFinal$det <- vecDet[det]
    names(dfFinal)[2:3] <- c("probabilityShock","stability")
    dfFinal[,2:4]
  })
  dfShockFinal <- do.call(rbind,lsDet)
  
  fig <- ggplot(data=dfShockFinal, aes(x=stability, y=probabilityShock,fill=det,shape=det)) +
    geom_point(size=2)+
    theme_classic() +  
    xlab("Stability") +
    ylab("Shock frequency") +
    theme(axis.title=element_text(size=8)) +
    theme(axis.text.x = element_text(size=6))+
    theme(axis.text.y = element_text(size=6))+
    scale_fill_grey(start = 0.3, end = 0.7) +
    scale_shape_manual(values = 21:23)+
    theme(legend.position = "none")
  
  if (leg){
    fig <- fig +
      theme(legend.position = c(0.8,0.8))+
      labs(shape="Detrend model",fill="Detrend model")
  }
  fig
    
}

figS3a <- funShock(dfDistrictSummary10,dfDistrictAnnual10,F) 
figS3b <- addSmallLegend(funShock(dfDistrictSummary8,dfDistrictAnnual8,T))

jpeg("results/FigS3.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(figS3a, figS3b, 
            labels = c(letters[1:2]),font.label=list(size=8),
            ncol = 2, nrow = 1)
dev.off()


############################# spatial
funMaps <- function(dfInput,variable,map,level,colVec,tit,lab,leg)
{
  
  ## get df
  dfTarget <- dfInput
  variable2 <- gsub("1", "2",variable)
  b <- seq(min(c(dfTarget[,variable],dfTarget[,variable2])),max(c(dfTarget[,variable],dfTarget[,variable2])),length.out = 11)
  labScale <- b
  
  # restrict df (for right colors)
  dfTarget[which(dfTarget[,variable]>max(b)),variable] <- max(b)
  dfTarget[which(dfTarget[,variable]<min(b)),variable] <- min(b)
  dfTarget$dim <- dfTarget[,variable]
  
  
  ## join to map
  mapsBivariate <- fortify(map,region=level)
  mapsBivariate = merge(mapsBivariate, dfTarget[,c(level,"dim")], by=level)
  
  ## plot
  fig = ggplot() + 
    geom_sf(data = mapsBivariate, aes(fill = dim),size=0.1) +
    scale_fill_gradientn(colours=colVec,
                         # values=rescale(b),
                         na.value="white", guide="colourbar",
                         name=tit,limits=c(min(b),max(b)),breaks=b, 
                         labels=round(labScale,2))+
    theme(legend.position = c(0.85, 0.5))+
    theme_void() +
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4]) +
    guides(fill = guide_colorbar(barheight = 5,barwidth = 0.2,title.position = "left"))+
    theme(legend.title = element_text(size=6,angle = 90),legend.text = element_text(size=6))+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
  
    if (isFALSE(leg))
    {
      fig <- fig + theme(legend.position = "none")
    }
  fig
}

dfDistrictAgg10 <- aggregate(cbind(stabilityProduction1,asynchronyBetween_farm1,asynchronyBetween_region1,asynchronyWithin1,
                                   stabilityProduction2,asynchronyBetween_farm2,asynchronyBetween_region2,asynchronyWithin2)~
                               district,dfDistrictSummary10,function(i){mean(i,na.rm=T)})

a2 <- funMaps(dfDistrictAgg10,variable="stabilityProduction1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Stability","",F)
b2 <- funMaps(dfDistrictAgg10,variable="asynchronyWithin1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Within crops","",F)
c2 <- funMaps(dfDistrictAgg10,variable="asynchronyBetween_region1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (district)","",F)
d2 <- funMaps(dfDistrictAgg10,variable="asynchronyBetween_farm1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (farm)","",F)
e2 <- funMaps(dfDistrictAgg10,variable="stabilityProduction2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Stability","",T)
f2 <- funMaps(dfDistrictAgg10,variable="asynchronyWithin2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Within crops","",T)
g2 <- funMaps(dfDistrictAgg10,variable="asynchronyBetween_region2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (district)","",T)
h2 <- funMaps(dfDistrictAgg10,variable="asynchronyBetween_farm2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (farm)","",T)

jpeg("results/Fig2.jpeg", width = 16.9, height = 10, units = 'cm', res = 600)
  ggarrange(a2,b2,c2,d2,
          e2,f2,g2,h2,
          labels = c(letters[1:8]),
          font.label=list(size=8),
          ncol = 4, nrow = 2)
dev.off()


dfDistrictAgg8 <- aggregate(cbind(stabilityProduction1,asynchronyBetween_farm1,asynchronyBetween_region1,asynchronyWithin1,
                                  stabilityProduction2,asynchronyBetween_farm2,asynchronyBetween_region2,asynchronyWithin2)~
                              district,dfDistrictSummary8,function(i){mean(i,na.rm=T)})
a2 <- funMaps(dfDistrictAgg8,variable="stabilityProduction1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Stability","",F)
b2 <- funMaps(dfDistrictAgg8,variable="asynchronyWithin1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Within crops","",F)
c2 <- funMaps(dfDistrictAgg8,variable="asynchronyBetween_region1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (district)","",F)
d2 <- funMaps(dfDistrictAgg8,variable="asynchronyBetween_farm1",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (farm)","",F)
e2 <- funMaps(dfDistrictAgg8,variable="stabilityProduction2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Stability","",T)
f2 <- funMaps(dfDistrictAgg8,variable="asynchronyWithin2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Within crops","",T)
g2 <- funMaps(dfDistrictAgg8,variable="asynchronyBetween_region2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (district)","",T)
h2 <- funMaps(dfDistrictAgg8,variable="asynchronyBetween_farm2",mapDistrict,"district",c("#FFFFE5","#78C679","#004529"),"Between crops (farm)","",T)

jpeg("results/FigS4.jpeg", width = 16.9, height = 10, units = 'cm', res = 600)
  ggarrange(a2,b2,c2,d2,
            e2,f2,g2,h2,
            labels = c(letters[1:8]),
            font.label=list(size=8),
            ncol = 4, nrow = 2)
dev.off()


############################# Regression analysis: effect of asynchrony on stability
sum(dfDistrictSummary10$nFarms)
sum(dfDistrictSummary8$nFarms)
range(dfDistrictSummary10$nFarms)
range(dfDistrictSummary8$nFarms)
range(dfDistrictSummary10$nCrops)
min(dfDistrictSummary10$nCropsFarmMin)
max(dfDistrictSummary10$nCropsFarmMax)
min(dfDistrictSummary8$nCropsFarmMin)
max(dfDistrictSummary8$nCropsFarmMax)

# run regressions, extract tables
vecName <- c("Intercept","asynchronyWithin","asynchronyBetween_region","asynchronyBetween_farm","R2m","R2c","AICc")
funTime <- function(dfInput){
  # check distribution and adapt if necesary
  aicNorm1 <- AICcmodavg::AICc(fitdist(dfInput$stabilityProduction1,"norm"))
  aicLog1 <- AICcmodavg::AICc(fitdist(dfInput$stabilityProduction1,"lnorm")) 
  show(aicNorm1)
  show(aicLog1)
  if (aicLog1 < (aicNorm1-2)){dfInput$stabilityProduction1 <- log(dfInput$stabilityProduction1)}
  aicNorm2 <- AICcmodavg::AICc(fitdist(dfInput$stabilityProduction2,"norm"))
  aicLog2 <- AICcmodavg::AICc(fitdist(dfInput$stabilityProduction2,"lnorm")) 
  show(aicNorm2)
  show(aicLog2)
  if (aicLog2 < (aicNorm2-2)){dfInput$stabilityProduction2 <- log(dfInput$stabilityProduction2)}
  
  ## linear regression
  modLME1 <- lme(stabilityProduction1~asynchronyWithin1+asynchronyBetween_region1+asynchronyBetween_farm1,
                 random=~1|district,method = "ML",
                 data=dfInput)
  modLME2 <- lme(stabilityProduction2~asynchronyWithin2+asynchronyBetween_region2+asynchronyBetween_farm2,
                 random=~1|district,method = "ML",
                 data=dfInput)
  
  
  functionDet <- function(modLME,det){
    
    show(vif(modLME))
    plot(modLME)
    qqnorm(modLME)
    show(shapiro.test(modLME[['residuals']]))
    plot(density(modLME[['residuals']]))
  
    dfMod <- data.frame(summary(modLME)$tTable)
    dfMod <- rbind(dfMod,data.frame(Value=r.squaredGLMM(modLME)[1],Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
    dfMod <- rbind(dfMod,data.frame(Value=r.squaredGLMM(modLME)[2],Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
    dfMod <- rbind(dfMod,data.frame(Value=AICc(modLME),Std.Error=NA,DF=NA,t.value=NA,p.value=NA))
    names(dfMod) <- c("Effect","SE","DF","tVal","pVal")
    dfMod$Detrend <- det
    dfMod$nam <- factor(vecName,levels=vecName)
    dfMod
  }
  dfRegression <- rbind(functionDet(modLME1,"lm"),functionDet(modLME2,"loess"))
  dfRegression$Detrend <- factor(dfRegression$Detrend ,levels=c("lm","loess"))
  dfRegression
}
dfRegression10 <- funTime(dfDistrictSummary10)
dfRegression8 <- funTime(dfDistrictSummary8)

funFig <- function(dfInput){
  ggplot(data=dfInput[which(dfInput$nam%in%vecName[2:4]),], aes(x=nam, y=Effect,fill=Detrend)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +  
  xlab("") +
  ylab("Regression coefficient") +
  theme(axis.title.y=element_text(size=8),axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(size=6))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.position = c(0.8,0.8))+
  labs(fill="Detrend model")+
  geom_hline(yintercept=0)+
  scale_x_discrete(labels=c(gsub("  ", "\n",c("Asynchrony within  crops")),gsub("  ", "\n",c("Asynchrony between  crops (district level)")),gsub("  ", "\n",c("Asynchrony between  crops (farm level)"))))+
  scale_fill_grey(start = 0.3, end = 0.7) 
}
jpeg("results/Fig3.jpeg", width = 8, height = 8, units = 'cm', res = 600)
  addSmallLegend(funFig(dfRegression10))  
dev.off()
jpeg("results/FigS5.jpeg", width = 8, height = 8, units = 'cm', res = 600)
  addSmallLegend(funFig(dfRegression8))  
dev.off()

##### TABLE 3
funTable <- function(dfInput){
  dfTable <- dfInput
  indLow <- which(dfTable$pVal<0.0001)
  dfTable[,c(1:2,4:5)] <- round(dfTable[,c(1:2,4:5)] ,2)
  dfTable$Value <- paste0(dfTable$Effect," (",dfTable$SE,")")
  dfTable[indLow,"pVal"] <- "<0.0001"
  dfTable <- dfTable[,c("Value","DF","tVal","pVal")]
  dfTable <- cbind(dfTable[1:7,],dfTable[8:14,])
  colnames(dfTable) <- rep(c("Estimate (SE)","DF","T","p-value"),2)
  row.names(dfTable) <- vecName
  dfTable
}
write.xlsx(funTable(dfRegression10),"results/Table2.xlsx",row.names=T)
write.xlsx(funTable(dfRegression8),"results/TableS2.xlsx",row.names=T)


############################# effects of crops diversity and no of cells on national asynchrony between and within crops
funSampling <- function(dfInput){
    # only keep entries with at least 3 districts
  vecCount1 <- as.numeric(names(which(table(dfInput[which(dfInput$metric=="asynchronyBetween"),"no"])>=30)))
  vecCount2 <- as.numeric(names(which(table(dfInput[which(dfInput$metric=="asynchronyWithin"),"no"])>=30)))
  dfInput <- dfInput[which(dfInput$metric=="asynchronyBetween"&dfInput$no%in%vecCount1|dfInput$metric=="asynchronyWithin"&dfInput$no%in%vecCount2),]

  dfSamplingSummary <- 
    rbind(
      data.frame(dfInput %>%
      group_by(no,metric) %>%
      dplyr::summarise(mean = mean(asynchrony1,na.rm=T),
                sd = sd(asynchrony1,na.rm=T),
                det="lm")),
      data.frame(dfInput %>%
                   group_by(no,metric) %>%
                   dplyr::summarise(mean = mean(asynchrony2,na.rm=T),
                                    sd = sd(asynchrony2,na.rm=T),
                                    det="loess"))
    )
  
  
  Fig5a <- ggplot(data=dfSamplingSummary[which(dfSamplingSummary$metric=="asynchronyWithin"),], aes(x=no, y=mean,colour=det,fill=det)) +
    geom_line() +
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3,colour=NA)+
    theme_classic()+
    ylim(-0.05,1.14)+
    xlab("Farms")+
    ylab("Asynchrony within crops")+
    theme(legend.position = "none")+
    scale_fill_grey(start = 0.3, end = 0.7)+
    scale_colour_grey(start = 0.3, end = 0.7)
    
  Fig5b <- ggplot(data=dfSamplingSummary[which(dfSamplingSummary$metric=="asynchronyBetween"),], aes(x=no, y=mean,colour=det,fill=det)) +
    geom_line() +
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3,colour=NA)+
    theme_classic()+
    ylim(-0.05,1.14)+
    xlab("Number of crops")+
    ylab("Asynchrony between crops")+
    theme(legend.position = c(0.8,0.8))+
    labs(fill="Detrend model",colour="Detrend model")+
    scale_fill_grey(start = 0.3, end = 0.7)+
    scale_colour_grey(start = 0.3, end = 0.7)+
    scale_x_continuous(breaks = seq(2, 9, by = 2))

    ggarrange(Fig5a, addSmallLegend(Fig5b), 
              labels = c(letters[1:2]),font.label=list(size=8),
              ncol = 2, nrow = 1)
  
}


jpeg("results/Fig4.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  funSampling(dfDistrictSampling10) 
dev.off()

jpeg("results/FigS6.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  funSampling(dfDistrictSampling8)
dev.off()

############################# management stylized 
dfStylizedStat1 <- aggregate(cbind(asynchronyB,asynchronyW)~size+landscape+diversity,dfStylized,mean)
dfStylizedStat1 <- dfStylizedStat1 %>% gather(asynchrony, mean, "asynchronyB":"asynchronyW")
dfStylizedStat2 <- aggregate(cbind(asynchronyB,asynchronyW)~size+landscape+diversity,dfStylized,sd)
dfStylizedStat2 <- dfStylizedStat2 %>% gather(asynchrony, sd, "asynchronyB":"asynchronyW")
dfStylizedStat <-merge(dfStylizedStat1,dfStylizedStat2)

asynchrony.labs <-  c("Between crops","Within crops")
names(asynchrony.labs) <- c("asynchronyB", "asynchronyW")

size.labs <- c("5x5","9x9","33x33")
names(size.labs) <- c("1", "2","3")

dfStylizedStat$asynchrony <- factor(dfStylizedStat$asynchrony,levels = c("asynchronyW","asynchronyB"))

Fig5 <- ggplot(data=dfStylizedStat, aes(x=diversity, y=mean, fill=landscape)) +
  facet_grid(asynchrony~size,labeller = labeller(asynchrony = asynchrony.labs, size = size.labs))+
  geom_line(aes(color=landscape)) +
  geom_ribbon(aes(fill=landscape,ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  theme_bw() +
  xlab("Number of crops")+
  ylab("Asynchrony")+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())+
  scale_color_manual(values=c("#4daf4a","#045A8D", "#ff7f00"))+
  scale_fill_manual(values=c("#4daf4a","#045A8D", "#ff7f00"))+
  scale_x_continuous(breaks=seq(0,10,2))+
  # theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(legend.text=element_text(size=8))


jpeg("results/Fig5.jpeg", width = 16.9, height = 14, units = 'cm', res = 600)
  Fig5
dev.off()

############################# farm level 
FigS7a <- ggplot(data=dfFarmsummary10[which(dfFarmsummary10$det%in%c("lm","loess")),], aes(x=asynchronyBetween, y=meanStabilityProduction,colour=det,fill=det)) +
  geom_line() +
  geom_ribbon(aes(ymin=meanStabilityProduction-sdStabilityProduction,ymax=meanStabilityProduction+sdStabilityProduction),alpha=0.3,colour=NA)+
  theme_classic()+
  ylim(0,35)+
  xlab("Asynchrony between crops")+
  ylab("Stability")+
  theme(legend.position = "none")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_colour_grey(start = 0.3, end = 0.7)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))

FigS7b <- ggplot(data=dfFarmsummary8[which(dfFarmsummary8$det%in%c("lm","loess")),], aes(x=asynchronyBetween, y=meanStabilityProduction,colour=det,fill=det)) +
  geom_line() +
  geom_ribbon(aes(ymin=meanStabilityProduction-sdStabilityProduction,ymax=meanStabilityProduction+sdStabilityProduction),alpha=0.3,colour=NA)+
  theme_classic()+
  ylim(0,40)+
  xlab("Asynchrony between crops")+
  ylab("Stability")+
  theme(legend.position = c(0.77,0.8))+
  labs(fill="Detrend model",colour="Detrend model")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  scale_colour_grey(start = 0.3, end = 0.7)+    
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))

  

jpeg("results/FigS7.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(FigS7a, addSmallLegend(FigS7b), 
            labels = c(letters[1:2]),font.label=list(size=8),
            ncol = 2, nrow = 1)
dev.off()



rm(list=ls())
