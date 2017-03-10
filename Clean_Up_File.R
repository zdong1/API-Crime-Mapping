# STAT 554 Final Project
#setting the working directory
setwd("~/Desktop/R/stat_554_project")
#setting the projection
proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
###############################
#Loading Required packages
library(sp)
library(splancs)
library(maps)
library(shapefiles)
library(maptools)
library(raster)
library(geoR)
library(dplyr)
#Load the dataset, and transfer the census tract into the correct format

#############################################################################################
# Very Messy below:
############################################################################################{
robbery<-read.csv(file="sea_rob.csv",header=TRUE)
robbery$ct<-as.numeric(as.character(robbery$Census.Tract))
robbery$ct<-trunc(robbery$ct,digits=0)
robbery$ct<-(robbery$ct/100)
robbery$control<-1
robbery.df<-as.data.frame(table(robbery$ct))
names(robbery.df)[1] = 'tract'
names(robbery.df)[2] = 'rob.n'
homicide<-read.csv(file="sea_homi.csv",header=TRUE)
homicide$ct<-as.numeric(as.character(homicide$Census.Tract))
homicide$ct<-trunc(homicide$ct,digits=0)
homicide$ct<-(homicide$ct/100)
homicide$control<-1
homicide.df<-as.data.frame(table(homicide$ct))
names(homicide.df)[1] = 'tract'
names(homicide.df)[2] = 'hom.n'
assault<-read.csv(file="sea_ass.csv",header=TRUE)
assault$ct<-as.numeric(as.character(assault$Census.Tract))
assault$ct<-trunc(assault$ct,digits=0)
assault$ct<-(assault$ct/100)
assault$control<-1
assault.df<-as.data.frame(table(assault$ct))
names(assault.df)[1] = 'tract'
names(assault.df)[2] = 'ass.n'
autotheft<-read.csv(file="sea_auto.csv",header=TRUE)
autotheft$ct<-as.numeric(as.character(autotheft$Census.Tract))
autotheft$ct<-trunc(autotheft$ct,digits=0)
autotheft$ct<-(autotheft$ct/100)
autotheft$control<-1
autotheft.df<-as.data.frame(table(autotheft$ct))
names(autotheft.df)[1] = 'tract'
names(autotheft.df)[2] = 'auto.n'
burglary<-read.csv(file="sea_burg.csv",header=TRUE)
burglary$ct<-as.numeric(as.character(burglary$Census.Tract))
burglary$ct<-trunc(burglary$ct,digits=0)
burglary$ct<-(burglary$ct/100)
burglary$control<-1
burglary.df<-as.data.frame(table(burglary$ct))
names(burglary.df)[1] = 'tract'
names(burglary.df)[2] = 'burg.n'
harass<-read.csv(file="sea_har.csv",header=TRUE)
harass$ct<-as.numeric(as.character(harass$Census.Tract))
harass$ct<-trunc(harass$ct,digits=0)
harass$ct<-(harass$ct/100)
harass$control<-1
harass.df<-as.data.frame(table(harass$ct))
names(harass.df)[1] = 'tract'
names(harass.df)[2] = 'har.n'
damage<-read.csv(file="sea_prop.csv",header=TRUE)
damage$ct<-as.numeric(as.character(damage$Census.Tract))
damage$ct<-trunc(damage$ct,digits=0)
damage$ct<-(damage$ct/100)
damage$control<-1
damage.df<-as.data.frame(table(damage$ct))
names(damage.df)[1] = 'tract'
names(damage.df)[2] = 'pdam.n'
shoplifting<-read.csv(file="sea_shop.csv",header=TRUE)
shoplifting$ct<-as.numeric(as.character(shoplifting$Census.Tract))
shoplifting$ct<-trunc(shoplifting$ct,digits=0)
shoplifting$ct<-(shoplifting$ct/100)
shoplifting$control<-1
shoplifting.df<-as.data.frame(table(shoplifting$ct))
names(shoplifting.df)[1] = 'tract'
names(shoplifting.df)[2] = 'shop.n'
df1<-merge(homicide.df,robbery.df, by = "tract", all=TRUE)
df2<-merge(assault.df, burglary.df, by = "tract", all=TRUE)
df3<-merge(harass.df, autotheft.df, by= "tract", all=TRUE)
df4<-merge(shoplifting.df, damage.df, by ="tract", all=TRUE)
df5<-merge(df1,df2, by ="tract", all=TRUE)
df6<-merge(df3,df4, by ="tract", all=TRUE)
tot<-merge(df5,df6, by ="tract", all=TRUE)
rm(df1,df2,df3,df4,df5,df6,assault.df,burglary.df,autotheft.df,homicide.df,
   robbery.df,damage.df,shoplifting.df,harass.df)
tot$tract<-as.numeric(as.character(tot$tract))
tot[is.na(tot)] <- 0
tot<-arrange(tot,tract)
tot[19,2:9]<- tot[19,2:9]+(tot[18,2:9]/2)
tot[20,2:9]<- tot[20,2:9]+(tot[18,2:9]/2)
tot[45,2:9]<- tot[45,2:9]+(tot[44,2:9]/2)
tot[46,2:9]<- tot[46,2:9]+(tot[44,2:9]/2)
tot[79,2:9]<- tot[79,2:9]+(tot[78,2:9]/2)
tot[80,2:9]<- tot[80,2:9]+(tot[78,2:9]/2)
tot[109,2:9]<- tot[109,2:9]+(tot[108,2:9]/2)
tot[110,2:9]<- tot[110,2:9]+(tot[108,2:9]/2)
tot[115,2:9]<- tot[115,2:9]+(tot[114,2:9]/2)
tot[116,2:9]<- tot[116,2:9]+(tot[114,2:9]/2)
tot[125,2:9]<- tot[125,2:9]+(tot[124,2:9]/2)
tot[126,2:9]<- tot[126,2:9]+(tot[124,2:9]/2)
tot[132,2:9]<- tot[132,2:9]+(tot[131,2:9]/2)
tot[133,2:9]<- tot[133,2:9]+(tot[131,2:9]/2)
tot<-tot[-c(18,44,78,108,114,124,131),]
tot<-arrange(tot,tract)
tot<-tot[-c(134,135),]
tot[115,2:9]<- tot[115,2:9]+(tot[114,2:9]/2)
tot[116,2:9]<- tot[116,2:9]+(tot[114,2:9]/2)
tot<-tot[-c(114),]
tot<-arrange(tot,tract)
#############################################################################################
# Finish the messy part
############################################################################################}
############################################### 
#Loading the demographic data
demo <-read.csv(file="sea_demog.csv",header=TRUE)

#match the crime data and demographic data by tract,calling the new dataset 'merged'
merged<-merge(demo,tot,by="tract", all=TRUE)


########################################
#Calculating crime rates: messy
merged$hom.p <- merged$hom.n/merged$pop*1000
merged$rob.p <- merged$rob.n/merged$pop*1000
merged$ass.p <- merged$ass.n/merged$pop*1000
merged$pdam.p <- merged$pdam.n/merged$pop*1000
merged$auto.p <- merged$auto.n/merged$pop*1000
merged$shop.p <- merged$shop.n/merged$pop*1000
merged$burg.p <- merged$burg.n/merged$pop*1000
merged$har.p <- merged$har.n/merged$pop*1000
## Read the spatial data
seattle <- readShapePoly("seattle.shp")
class(seattle)
plot(seattle)

#Edit a variable ct in the seattle shapefile to let the census tract match the two digits
#format in the crime data
seattle$NAME10<-as.numeric(as.character(seattle$NAME10))
seattle$NAME10<-round(seattle$NAME10,digits = 2)
seattle$tract<-seattle$NAME10
head(seattle$tract)

seattle@data = data.frame(seattle@data, merged[match(seattle@data[,"tract"], merged[,"tract"]),])
