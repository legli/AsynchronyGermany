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

############################# Regression region and farm level

dfFarm <- read.csv("datasetsDerived/dataFinal_all_farm_Nov2020.csv")
names(dfFarm)
range(dfFarm$nFarms)
sum(dfFarm$nFarms)
range(dfFarm$nCrops)
min(dfFarm$nCropsFarmMin)
max(dfFarm$nCropsFarmMax)

cor(dfFarm[,7:10],method='s')


## linear regression
mod <- lm(stabilityProduction~asynchronyWithin+asynchronyBetween_region+asynchronyBetween_farm,
          data=dfFarm)
modFarm <- lme(stabilityProduction~asynchronyWithin+asynchronyBetween_region+asynchronyBetween_farm,
               random=~1|district,method = "ML",
               data=dfFarm)
r.squaredGLMM(modFarm)
summary(modFarm)
vif(modFarm)
dfRandom <- random.effects(modFarm)
anova(modFarm,mod)

dfModFarm <- data.frame(summary(modFarm)$tTable)[2:4,c(1,2,5)]
names(dfModFarm) <- c("Effect","SE","pVal")
# dfTextFarm <- data.frame(xpos=1:2,ypos=c(30,30),lab=c("***","***"))
colnames(dfModFarm)
dfModFarm$nam <- factor(row.names(dfModFarm),levels=c("asynchronyWithin","asynchronyBetween_region","asynchronyBetween_farm"))


Fig3 <- ggplot(data=dfModFarm, aes(x=nam, y=Effect)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  # geom_text(data=dfTextFarm,aes(x=xpos,y=ypos,label=lab),size=3)+
  theme_classic() +  
  xlab("") +
  # scale_y_continuous(expand = c(-3, 0),limit=c(0,35))+
  ylab("Regression coefficient") +
  theme(axis.title.y=element_text(size=8),axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(size=6))+
  theme(axis.text.y = element_text(size=8))+
  scale_x_discrete(labels=c(gsub("  ", "\n",c("Asynchrony within  crops")),gsub("  ", "\n",c("Asynchrony between  crops (district level)")),gsub("  ", "\n",c("Asynchrony between  crops (farm level)"))))


jpeg("results/Fig3.jpeg", width = 8, height = 5, units = 'cm', res = 600)
  ggarrange(Fig3,  
            # labels = c(letters[1:2]),
            font.label=list(size=8),
            ncol = 1, nrow = 1)
dev.off()


##### TABLE 3

dfTab <- data.frame(summary(modFarm)$tTable[1:4,c(1,2,4,5)])
indLow <- which(dfTab$p.value<0.0001)
dfTab <- round(dfTab,2)
row.names(dfTab) <- c("Intercept","Asynchrony within crops","Asynchrony between crops (region)","Asynchrony between crops (farm)")
dfTab$Value <- paste0(dfTab$Value," (",dfTab$Std.Error,")")
dfTab[indLow,4] <- "<0.0001"
dfTab <- dfTab[,c(1,3,4)]
colnames(dfTab) <- c("Estimate (SE)","T","p-value") 
dfTab2 <- data.frame(est=c(r.squaredGLMM(modFarm),AIC(modFarm)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA))
dfTab2$est <- round(dfTab2$est,2)
rownames(dfTab2) <- c("R2m","R2c","AIC")
colnames(dfTab2) <- colnames(dfTab)
dfTabFarm <- rbind(dfTab,dfTab2)

write.xlsx(dfTabFarm,"results/Table3.xlsx")


############################# effects of crops diversity and no of cells on national asynchrony between and within crops
dfTargetFarm <- read.csv("datasetsDerived/dataFinal_sampling_farm_Nov2020.csv")
head(dfTargetFarm)

dfTargetFarmAbs <- data.frame(dfTargetFarm %>%
  group_by(no,metric) %>%
  dplyr::summarise(mean = mean(asynchrony,na.rm=T),
            sd = sd(asynchrony,na.rm=T)))
dfTargetFarmAbs[is.na(dfTargetFarmAbs)] <- 0


Fig3a <- ggplot(data=dfTargetFarmAbs[which(dfTargetFarmAbs$metric=="asynchronyWithin"),], aes(x=no, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  ylim(-0.05,1.14)+
  xlab("Farms")+
  ylab("Asynchrony within crops")+
  theme_classic()

Fig3b <- ggplot(data=dfTargetFarmAbs[which(dfTargetFarmAbs$metric=="asynchronyBetween"),], aes(x=no, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  ylim(-0.05,1.14)+
  xlab("Diversity")+
  ylab("Asynchrony between crops")+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme_classic()

jpeg("results/Fig4.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)

  ggarrange(Fig4a, Fig4b, 
            labels = c(letters[1:2]),font.label=list(size=8),
            ncol = 2, nrow = 1)

dev.off()




############################# management stylized 
dfStylized <- read.csv("datasetsDerived/dataFinal_all_stylized_Nov2020.csv")


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
  xlab("Diversity")+
  ylab("Asynchrony")+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())+
  scale_color_manual(values=c("#4daf4a","#045A8D", "#ff7f00"))+
  scale_fill_manual(values=c("#4daf4a","#045A8D", "#ff7f00"))+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(legend.text=element_text(size=8))

jpeg("results/Fig5.jpeg", width = 16.9, height = 10, units = 'cm', res = 600)
  Fig4
dev.off()


## show how simulation was done
library(NLMR)
library(viridis)
library(cowplot)
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

jpeg("results/Fig1.jpeg", width = 8, height = 4, units = 'cm', res = 600)

ggarrange(a1, a2, 
          labels = c(letters[1:2]),font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()

## spatial
mapDistrict <- st_as_sf(readOGR("spatial/vg2500_rbz_bld.shp"))

funMaps <- function(dfInput,variable,map,level,b,colVec,tit,lab)
{
  
  ## get df
  dfTarget <- dfInput
  
  b <- seq(min(dfTarget[,variable]),max(dfTarget[,variable]),length.out = 11)
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
                         values=rescale(b),
                         na.value="white", guide="colourbar",
                         name=tit,limits=c(min(b),max(b)),breaks=b, 
                         labels=round(labScale,2))+
    theme(legend.position = c(0.85, 0.5)) +
    theme_void() +
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4]) +
    guides(fill = guide_colorbar(barheight = 5,barwidth = 0.2,title.position = "left"))+
    theme(legend.title = element_text(size=6,angle = 90),legend.text = element_text(size=6))+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
  
  fig
}

dfFarmAgg <- aggregate(cbind(stabilityProduction,asynchronyBetween_farm,asynchronyBetween_region,asynchronyWithin)~district,dfFarm,function(i){mean(i,na.rm=T)})
dfRandom$district <- row.names(dfRandom)
dfFarmAgg <- merge(dfFarmAgg,dfRandom,by="district")
a2 <- funMaps(dfFarmAgg,variable="stabilityProduction",mapDistrict,"district",seq(0,30,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","")
b2 <- funMaps(dfFarmAgg,variable="asynchronyWithin",mapDistrict,"district",seq(0,1,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Within crops","")
c2 <- funMaps(dfFarmAgg,variable="asynchronyBetween_region",mapDistrict,"district",seq(0,1,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Between crops (region)","")
d2 <- funMaps(dfFarmAgg,variable="asynchronyBetween_farm",mapDistrict,"district",seq(0,1,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Between crops (farm)","")
e2 <- funMaps(dfFarmAgg,variable="(Intercept)",mapDistrict,"district",seq(0,1,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Intercept","")

jpeg("results/Fig2.jpeg", width = 16.9, height = 5, units = 'cm', res = 600)
ggarrange(a2,b2,c2,d2,  
          labels = c(letters[1:4]),
          font.label=list(size=8),
          ncol = 4, nrow = 1)
dev.off()



rm(list=ls())
