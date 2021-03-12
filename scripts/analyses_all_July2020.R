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


############################# Regression region and farm level

dfRegion <- read.csv("datasetsDerived/dataFinal_all_region.csv")
cor(dfRegion[,2:7],method='s')
# ## scale predictors for standardized regression
# dfRegionPredictors=sapply(dfRegion[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
# dfRegion=data.frame(Country=dfRegion[,1],cvCurrent=dfRegion[,2],dfRegionPredictors)


dfFarm <- read.csv("datasetsDerived/dataFinal_all_farm.csv")
cor(dfFarm[,2:7],method='s')
# ## scale predictors for standardized regression
# dfFarmPredictors=sapply(dfFarm[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
# dfFarm=data.frame(district=dfFarm[,1],cvCurrent=dfFarm[,2],dfFarmPredictors)


mod <- lm(stabilityProduction~asynchronyBetween+asynchronyWithin+timePeriod+areaHarvested,data=dfRegion)
summary(mod)
modRegion <- lme(stabilityProduction~asynchronyBetween+asynchronyWithin,
                 random=~1|Country,method = "ML",
                 data=dfRegion)
r.squaredGLMM(modRegion)
summary(modRegion)

dfModRegion <- data.frame(summary(modRegion)$tTable)[2:3,c(1,2,5)]
names(dfModRegion) <- c("Effect","SE","pVal")
dfTextRegion <- data.frame(xpos=1:2,ypos=c(20,20),lab=c("*","NS"))
colnames(dfModRegion)
dfModRegion$nam <- row.names(dfModRegion)

modFarm <- lme(stabilityProduction~asynchronyBetween+asynchronyWithin,
               random=~1|district,method = "ML",
               data=dfFarm)
r.squaredGLMM(modFarm)
summary(modFarm)



dfModFarm <- data.frame(summary(modFarm)$tTable)[2:3,c(1,2,5)]
names(dfModFarm) <- c("Effect","SE","pVal")
dfTextFarm <- data.frame(xpos=1:2,ypos=c(30,30),lab=c("***","***"))
colnames(dfModFarm)
dfModFarm$nam <- row.names(dfModFarm)



Fig1a <- ggplot(data=dfModFarm, aes(x=nam, y=Effect)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfTextFarm,aes(x=xpos,y=ypos,label=lab),size=3)+
  theme_classic() +  
  xlab("") +
  scale_y_continuous(expand = c(0, 0),limit=c(0,32))+
  ylab("") +
  theme(axis.title.y=element_text(size=8),axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_x_discrete(labels=c(gsub("  ", "\n",c("Asynchrony between  crops")),gsub("  ", "\n",c("Asynchrony within  crops"))))

Fig1b <- ggplot(data=dfModRegion, aes(x=nam, y=Effect)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfTextRegion,aes(x=xpos,y=ypos,label=lab),size=3)+
  theme_classic() +  
  xlab("") +
  scale_y_continuous(expand = c(0, 0),limit=c(0,32))+
  ylab("Regression coefficient") +
  theme(axis.title.y=element_text(size=8),axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_x_discrete(labels=c(gsub("  ", "\n",c("Asynchrony between  crops")),gsub("  ", "\n",c("Asynchrony within  crops"))))

jpeg("results/Fig1.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
ggarrange(Fig1a, Fig1b, 
          labels = c(letters[1:2]),font.label=list(size=8),
          ncol = 2, nrow = 1)
dev.off()


##### TABLE 3

dfTab <- data.frame(summary(modRegion)$tTable[1:3,c(1,2,4,5)])
indLow <- which(dfTab$p.value<0.0001)
dfTab <- round(dfTab,2)
row.names(dfTab) <- c("Intercept","Asynchrony between crops","Asynchrony within crops")
dfTab$Value <- paste0(dfTab$Value," (",dfTab$Std.Error,")")
dfTab[indLow,4] <- "<0.0001"
dfTab <- dfTab[,c(1,3,4)]
colnames(dfTab) <- c("Estimate (SE)","T","p-value") 
dfTab2 <- data.frame(est=c(r.squaredGLMM(modRegion),AIC(modRegion)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA))
dfTab2$est <- round(dfTab2$est,2)
rownames(dfTab2) <- c("R2m","R2c","AIC")
colnames(dfTab2) <- colnames(dfTab)
dfTabReg <- rbind(dfTab,dfTab2)

dfTab <- data.frame(summary(modFarm)$tTable[1:3,c(1,2,4,5)])
indLow <- which(dfTab$p.value<0.0001)
dfTab <- round(dfTab,2)
row.names(dfTab) <- c("Intercept","Asynchrony between crops","Asynchrony within crops")
dfTab$Value <- paste0(dfTab$Value," (",dfTab$Std.Error,")")
dfTab[indLow,4] <- "<0.0001"
dfTab <- dfTab[,c(1,3,4)]
colnames(dfTab) <- c("Estimate (SE)","T","p-value") 
dfTab2 <- data.frame(est=c(r.squaredGLMM(modFarm),AIC(modFarm)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA))
dfTab2$est <- round(dfTab2$est,2)
rownames(dfTab2) <- c("R2m","R2c","AIC")
colnames(dfTab2) <- colnames(dfTab)
dfTabFarm <- rbind(dfTab,dfTab2)

write.xlsx(cbind(dfTabFarm,dfTabReg),"results/Table3.xlsx")


############################# effects of crops diversity and no of cells on national asynchrony between and within crops
dfTargetRegion <- read.csv("datasetsDerived/dataFinal_sampling_region.csv")
head(dfTargetRegion)

dfTargetFarm <- read.csv("datasetsDerived/dataFinal_sampling_farm.csv")
head(dfTargetFarm)

dfTargetFarmAbs <- data.frame(dfTargetFarm %>%
  group_by(no,metric) %>%
  dplyr::summarise(mean = mean(asynchrony,na.rm=T),
            sd = sd(asynchrony,na.rm=T)))
dfTargetFarmAbs[is.na(dfTargetFarmAbs)] <- 0

dfTargetRegionAbs <- data.frame(dfTargetRegion %>%
                                group_by(no,metric) %>%
                                dplyr::summarise(mean = mean(asynchrony,na.rm=T),
                                                 sd = sd(asynchrony,na.rm=T)))
dfTargetRegionAbs[is.na(dfTargetRegionAbs)] <- 0

Fig2a <- ggplot(data=dfTargetFarmAbs[which(dfTargetFarmAbs$metric=="asynchronyBetween"),], aes(x=no, y=mean)) +
    geom_line() +
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
    ylim(-0.05,1.14)+
    xlab("Diversity")+
    ylab("Asynchrony between crops")+
    theme_classic()

Fig2b <- ggplot(data=dfTargetRegionAbs[which(dfTargetRegionAbs$metric=="asynchronyBetween"),], aes(x=no, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  ylim(-0.05,1.14)+
  xlab("Diversity")+
  ylab("")+
  theme_classic()

Fig2c <- ggplot(data=dfTargetFarmAbs[which(dfTargetFarmAbs$metric=="asynchronyWithin"),], aes(x=no, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  ylim(-0.05,1.14)+
  xlab("Farms")+
  ylab("Asynchrony within crops")+
  theme_classic()

Fig2d <- ggplot(data=dfTargetRegionAbs[which(dfTargetRegionAbs$metric=="asynchronyWithin"),], aes(x=no, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha=0.3)+
  ylim(-0.05,1.14)+
  xlab("Regions")+
  ylab("")+
  theme_classic()

jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9, units = 'cm', res = 600)

  ggarrange(Fig2a, Fig2b, Fig2c, Fig2d,
            labels = c(letters[1:4]),font.label=list(size=8),
            ncol = 2, nrow = 2)

dev.off()




############################# management stylized 
dfStylized <- read.csv("datasetsDerived/dataFinal_all_stylized.csv")


dfStylizedStat1 <- aggregate(cbind(asynchronyB,asynchronyW)~size+landscape+diversity,dfStylized,mean)
dfStylizedStat1 <- dfStylizedStat1 %>% gather(asynchrony, mean, "asynchronyB":"asynchronyW")
dfStylizedStat2 <- aggregate(cbind(asynchronyB,asynchronyW)~size+landscape+diversity,dfStylized,sd)
dfStylizedStat2 <- dfStylizedStat2 %>% gather(asynchrony, sd, "asynchronyB":"asynchronyW")
dfStylizedStat <-merge(dfStylizedStat1,dfStylizedStat2)

asynchrony.labs <-  c("Between crops","Within crops")
names(asynchrony.labs) <- c("asynchronyB", "asynchronyW")

size.labs <- c("5x5","9x9","33x33")
names(size.labs) <- c("1", "2","3")

Fig3 <- ggplot(data=dfStylizedStat, aes(x=diversity, y=mean, fill=landscape)) +
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
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(legend.text=element_text(size=8))

jpeg("results/Fig3.jpeg", width = 16.9, height = 10, units = 'cm', res = 600)
  Fig3
dev.off()




rm(list=ls())
