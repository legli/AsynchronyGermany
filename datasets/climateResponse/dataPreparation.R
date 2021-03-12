library(openxlsx)
library(imputeTS)

lines <- readLines("datasets/climateResponse/clim.param")
vecLines <- c(seq(4,116,8),132)
lsSuitTemp <- lapply(vecLines,function(l){
  show(l)
  cropName <- lines[l]
  vecTemp <- as.numeric(unlist(strsplit(gsub("temp,","",lines[l+3]), ',')))
  vecTempSuit <- as.numeric(unlist(strsplit(gsub("temp,","",lines[l+4]), ',')))
  vecPrec <- as.numeric(unlist(strsplit(gsub("prec,","",lines[l+6]), ',')))
  vecPrecSuit <- as.numeric(unlist(strsplit(gsub("prec,","",lines[l+7]), ',')))
  dfTemp <- data.frame(crop=cropName,climate=vecTemp,suitability=vecTempSuit)
  dfTemp <- merge(data.frame(crop=cropName,climate=1:45),dfTemp,all=T)
  dfTemp[,cropName] <- na_interpolation(dfTemp$suitability, option = "linear")
  dfTemp[,c(2,4)]
})
dfSuitTemp <- do.call(cbind,lsSuitTemp)
dfSuitTemp <- dfSuitTemp[,-(seq(3,46,2))]
names(dfSuitTemp)[13] <- "soybean"
write.xlsx(dfSuitTemp,"datasets/climateResponse/dfSuitabilityTemperature.xlsx",row.names = F)


lsSuitPrec <- lapply(vecLines,function(l){
  show(l)
  cropName <- lines[l]
  vecPrec <- as.numeric(unlist(strsplit(gsub("prec,","",lines[l+6]), ',')))
  vecPrecSuit <- as.numeric(unlist(strsplit(gsub("prec,","",lines[l+7]), ',')))
  dfPrec <- data.frame(crop=cropName,climate=vecPrec,suitability=vecPrecSuit)
  dfPrec <- merge(data.frame(crop=cropName,climate=1:5000),dfPrec,all=T)
  dfPrec[,cropName] <- na_interpolation(dfPrec$suitability, option = "linear")
  dfPrec[,c(2,4)]
})
dfSuitPrec <- do.call(cbind,lsSuitPrec)
dfSuitPrec <- dfSuitPrec[,-(seq(3,46,2))]
names(dfSuitPrec)[13] <- "soybean"
write.xlsx(dfSuitPrec,"datasets/climateResponse/dfSuitabilityPrecipitation.xlsx",row.names = F)


rm(list=ls())
