
# Setting the working directory
setwd("~/Desktop/R/stat_554_project")
# Setting the projection, for future use
# proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
########################################################################
# Loading Required packages
library(sp)
library(splancs)
library(maps)
library(shapefiles)
library(maptools)
library(raster)
library(geoR)
library(dplyr)
#Load the dataset, and transfer the census tract into the correct format

#########################################################################
# Very Messy below:
#########################################################################
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
burglary<-read.csv(file="sea_burg.csv",header=TRUE)
burglary$ct<-as.numeric(as.character(burglary$Census.Tract))
burglary$ct<-trunc(burglary$ct,digits=0)
burglary$ct<-(burglary$ct/100)
burglary$control<-1
burglary.df<-as.data.frame(table(burglary$ct))
names(burglary.df)[1] = 'tract'
names(burglary.df)[2] = 'burg.n'
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
df3<-merge(shoplifting.df, damage.df, by ="tract", all=TRUE)
df4<-merge(df1,df2, by ="tract", all=TRUE)
tot<-merge(df3,df4, by ="tract", all=TRUE)
rm(df1,df2,df3,df4,assault.df,burglary.df,homicide.df,
   robbery.df,damage.df,shoplifting.df)
tot$tract<-as.numeric(as.character(tot$tract))
tot[is.na(tot)] <- 0
tot<-arrange(tot,tract)
tot[19,2:7]<- tot[19,2:7]+(tot[18,2:7]/2)
tot[20,2:7]<- tot[20,2:7]+(tot[18,2:7]/2)
tot[45,2:7]<- tot[45,2:7]+(tot[44,2:7]/2)
tot[46,2:7]<- tot[46,2:7]+(tot[44,2:7]/2)
tot[79,2:7]<- tot[79,2:7]+(tot[78,2:7]/2)
tot[80,2:7]<- tot[80,2:7]+(tot[78,2:7]/2)
tot[109,2:7]<- tot[109,2:7]+(tot[108,2:7]/2)
tot[110,2:7]<- tot[110,2:7]+(tot[108,2:7]/2)
tot[115,2:7]<- tot[115,2:7]+(tot[114,2:7]/2)
tot[116,2:7]<- tot[116,2:7]+(tot[114,2:7]/2)
tot[125,2:7]<- tot[125,2:7]+(tot[124,2:7]/2)
tot[126,2:7]<- tot[126,2:7]+(tot[124,2:7]/2)
tot[132,2:7]<- tot[132,2:7]+(tot[131,2:7]/2)
tot[133,2:7]<- tot[133,2:7]+(tot[131,2:7]/2)
tot<-tot[-c(18,44,78,108,114,124,131),]
tot<-arrange(tot,tract)
tot<-tot[-c(134,135),]
tot[115,2:7]<- tot[115,2:7]+(tot[114,2:7]/2)
tot[116,2:7]<- tot[116,2:7]+(tot[114,2:7]/2)
tot<-tot[-c(114),]
tot<-arrange(tot,tract)
#############################################################################################
# Finish the messy part
#############################################################################
# Loading the demographic data
demo <-read.csv(file="sea_demog.csv",header=TRUE)

# Match the crime data and demographic data by tract,calling the 
# new dataset 'merged'
merged<-merge(demo,tot,by="tract", all=TRUE)


##############################################################################
# Messy: Calculating crime rates
merged$hom.p <- merged$hom.n/merged$pop*1000
merged$rob.p <- merged$rob.n/merged$pop*1000
merged$ass.p <- merged$ass.n/merged$pop*1000
merged$pdam.p <- merged$pdam.n/merged$pop*1000
merged$shop.p <- merged$shop.n/merged$pop*1000
merged$burg.p <- merged$burg.n/merged$pop*1000
merged$tot.n <- merged$hom.n+merged$rob.n+merged$ass.n+merged$pdam.n+merged$shop.n+merged$burg.n
merged$tot.p <- merged$tot.n/merged$pop*1000
################################################################################

## Read the spatial data
seattle <- readShapePoly("seattle.shp")
class(seattle)
plot(seattle)

# Edit a variable ct in the seattle shapefile to let the census tract match the two digits
# format in the crime data
seattle$NAME10<-as.numeric(as.character(seattle$NAME10))
seattle$NAME10<-round(seattle$NAME10,digits = 2)
seattle$tract<-seattle$NAME10
head(seattle$tract)

#########################################################################
# Very messy part again, creating expected counts for all types of crime
# versus the observed counts
merged$Y1 <-Y1 <- merged$rob.n
merged$E1 <- E1<- sum(Y1)*merged$pop/sum(merged$pop)

merged$Y2<-Y2 <- merged$burg.n
merged$E2<- E2<- sum(Y2)*merged$pop/sum(merged$pop)

merged$Y3<-Y3 <- merged$shop.n
merged$E3<- E3<-sum(Y3)*merged$pop/sum(merged$pop)

merged$Y4<-Y4 <- merged$pdam.n
merged$E4<- E4 <-sum(Y4)*merged$pop/sum(merged$pop)

merged$Y5<-Y5 <- merged$ass.n
merged$E5<- E5<-sum(Y5)*merged$pop/sum(merged$pop)

merged$Y6<- Y6 <- merged$hom.n
merged$E6<- E6<-sum(Y6)*merged$pop/sum(merged$pop)

# Calculating the SMR, treating mortality as an incidence of crime happened...
merged$smr1<-merged$Y1/merged$E1
merged$smr2<-merged$Y2/merged$E2
merged$smr3<-merged$Y3/merged$E3
merged$smr4<-merged$Y4/merged$E4
merged$smr5<-merged$Y5/merged$E5
merged$smr6<-merged$Y6/merged$E6


pov<- as.numeric(as.character(merged$pov))
pov[55]=19.000
nohs<-as.numeric(as.character(merged$nohs))
het<- as.numeric(as.character(merged$het))
#########################################################################

# Finally, we have the SPDF file with everything we want!
seattle@data = data.frame(seattle@data, merged[match(seattle@data[,"NAME10"], merged[,"tract"]),])

#########################################################################
# Part 2. Let us see the clustering of census tract crime data
#########################################################################
# Transfer Longitude and latitude into UTM
oldCRS<-data.frame(seattle$INTPTLON10, seattle$INTPTLAT10)
newCRS<-read.csv(file="utm_seattle.csv",header=TRUE) # Upload the converter file
seattle$northing<- newCRS$northing
seattle$easting<- newCRS$easting
# Load the required package, and see difference of census tracts
# in crime rate.
library(SpatialEpi)
library(spdep)
nb_q<-poly2nb(seattle)
brks<-c(0,20,40,80,160,320,640,1280,3500)
spplot(seattle,zcol="tot.p",at = brks)
brks3<-c(0,2,6,12,24,40,60)
spplot(seattle,zcol="rob.p",at=brks3)

brks2<-c(0,2,6,12,24,40,60,120,240,480,960)
spplot(seattle,zcol="ass.p", at=brks2)
spplot(seattle,zcol="burg.p", at=brks3)
hist(seattle$tot.p, breaks=200, xlim=c(0,1200),
     col="lightblue",main="Seattle Crime Rate in Census Tract",
     xlab="Crime Rate (Counts per 1000)",freq=TRUE)
coords<-coordinates(seattle)
plot(nb_q,coords, col="pink")

# Monte Carlo Test for Overdispersion
kappaval <- function(Y,fitted,df){
  sum((Y-fitted)^2/fitted)/df}
mod <- glm(Y1 ~ 1, offset=log(E1),family="quasipoisson"(link="log"))
kappaest <- kappaval(Y1,mod$fitted,mod$df.resid)
nMC <- 1000
ncts <- length(E1)
yMC <- matrix(rpois(n=nMC*ncts,lambda=E1),
              nrow=ncts,ncol=nMC)
kappaMC <- NULL
for (i in 1:nMC){
  modMC <- glm(yMC[,i]~ 1,offset=log(E1), data=seattle,family="quasipoisson")
  kappaMC[i] <- kappaval(yMC[,i],modMC$fitted,modMC$df.resid)
}
summary(modMC)
summary(kappaMC)
#########################################################################
# Using the poly2nb to create neighborhoods for seattle
library(classInt)
library(RColorBrewer)
library(maps)
library(SpatialEpi)
library(maptools)
sea.nb <-poly2nb(seattle)
col.W<-nb2listw(sea.nb, style="W", zero.policy = TRUE)
col.B<-nb2listw(sea.nb, style="B", zero.policy = TRUE)
col.S <- nb2listw(sea.nb, style="S", zero.policy=TRUE)

#########################################################################
# Messy part again, for quasipoisson models testing the observed 
# counts against the expected count
mod1 <- (glm(Y1~1+offset(log(E1)),family="quasipoisson"))
residual1<-residuals(mod1,type="pearson")

mod2 <- (glm(Y2~1+offset(log(E2)),family="quasipoisson"))
residual2<-residuals(mod2,type="pearson")

mod3 <- (glm(Y3~1+offset(log(E3)),family="quasipoisson"))
residual3<-residuals(mod3,type="pearson")

mod4 <- (glm(Y4~1+offset(log(E4)),family="quasipoisson"))
residual4<-residuals(mod4,type="pearson")

mod5 <- (glm(Y1~1+offset(log(E5)),family="quasipoisson"))
residual5<-residuals(mod5,type="pearson")

mod6 <- (glm(Y2~1+offset(log(E6)),family="quasipoisson"))
residual6<-residuals(mod6,type="pearson")



moran.test(residual1, col.W) # Robbery
moran.test(residual2, col.W) # Burglary
moran.test(residual3, col.W) # Shoplifting
moran.test(residual4, col.W) # Property Damage
moran.test(residual5, col.W) # Assault
moran.test(residual6, col.W) # Homicide



moran.test(residual1, col.B) # Robbery
moran.test(residual2, col.B) # Burglary
moran.test(residual3, col.B) # Shoplifting
moran.test(residual4, col.B) # Property Damage
moran.test(residual5, col.B) # Assault
moran.test(residual6, col.B) # Homicide




moran.test(residual1, col.S) # Robbery
moran.test(residual2, col.S) # Burglary
moran.test(residual3, col.S) # Shoplifting
moran.test(residual4, col.S) # Property Damage
moran.test(residual5, col.S) # Assault
moran.test(residual6, col.S) # Homicide

geary.test(residual1, col.W) # Robbery
geary.test(residual2, col.W) # Burglary
geary.test(residual3, col.W) # Shoplifting
geary.test(residual4, col.W) # Property Damage
geary.test(residual5, col.W) # Assault
geary.test(residual6, col.W) # Homicide


geary.test(residual1, col.B) # Robbery
geary.test(residual2, col.B) # Burglary
geary.test(residual3, col.B) # Shoplifting
geary.test(residual4, col.B) # Property Damage
geary.test(residual5, col.B) # Assault
geary.test(residual6, col.B) # Homicide

geary.test(residual1, col.S) # Robbery
geary.test(residual2, col.S) # Burglary
geary.test(residual3, col.S) # Shoplifting
geary.test(residual4, col.S) # Property Damage
geary.test(residual5, col.S) # Assault
geary.test(residual6, col.S) # Homicide

quasipmod1 <- glm(Y1 ~ easting + northing, offset = log(E1),
                  data = seattle, family = quasipoisson())
quasipmod2 <- glm(Y2 ~ easting + northing, offset = log(E2),
                  data = seattle, family = quasipoisson())
quasipmod3 <- glm(Y3 ~ easting + northing, offset = log(E3),
                  data = seattle, family = quasipoisson())
quasipmod4 <- glm(Y4 ~ easting + northing, offset = log(E4),
                  data = seattle, family = quasipoisson())
quasipmod5 <- glm(Y5 ~ easting + northing, offset = log(E5),
                  data = seattle, family = quasipoisson())
quasipmod6 <- glm(Y6 ~ easting + northing, offset = log(E6),
                  data = seattle, family = quasipoisson())

sidsres1 <- residuals(quasipmod1, type = "pearson")
seattle$resB1 <- sidsres1
sidsres2 <- residuals(quasipmod2, type = "pearson")
seattle$resB2 <- sidsres2
sidsres3 <- residuals(quasipmod3, type = "pearson")
seattle$resB3 <- sidsres3
sidsres4 <- residuals(quasipmod4, type = "pearson")
seattle$resB4 <- sidsres4
sidsres5 <- residuals(quasipmod5, type = "pearson")
seattle$resB5 <- sidsres5
sidsres6 <- residuals(quasipmod6, type = "pearson")
seattle$resB6 <- sidsres6


par(mfrow=c(1,3))
spplot(seattle,"resB1")
spplot(seattle,"resB2")
spplot(seattle,"resB3")
spplot(seattle,"resB4")
summary(quasipmod1)

coords <- coordinates(seattle)
plot(seattle)
text(coords, cex = 0.75)

# Test of local influence

par(mfrow=c(1,1))
lisa <- moran.plot(seattle$resB1, col.W, labels = seattle$OBJECTID,
                   xlab = "Residuals", ylab = "Spatially Lagged Residuals")


lm1 <- localmoran(seattle$resB1, col.W)
gry <- c(rev(brewer.pal(8, "Reds")[1:6]), brewer.pal(6,
                                                     "Blues"))
seattle$localM1 <- lm1[, 1]
yellow <- c(rev(brewer.pal(8, "Oranges")[1:6]), brewer.pal(6,
                                                     "Greens"))
lm5 <- localmoran(seattle$resB5, col.W)
lm6 <- localmoran(seattle$resB6, col.W)
seattle$localM5 <- lm5[, 1]
seattle$localM6 <- lm6[, 1]
spplot(seattle, zcol = "localM1", at = c(-2.5, -1.4,
                                         -0.6, -0.2, 0, 0.2, 0.6, 1.4, 2.5), col.regions = colorRampPalette(gry)(8))
spplot(seattle, zcol = "localM5", at = c(-2.5, -1.4,
                                        -0.6, -0.2, 0, 0.2, 0.6, 1.4, 2.5), col.regions = colorRampPalette(yellow)(8))

spplot(seattle, zcol = "localM6", at = c(-5.5, -1.4,
                                         -0.6, -0.2, 0, 0.2, 0.6, 1.4, 5.5), col.regions = colorRampPalette(yellow)(8))

####################################################################
# Part 3: Clustering and Smoothing
####################################################################
se1 <- sqrt(Y1/E1)
plot(Y1 ~ se1, xlab = "Standard Error", ylab = "Observed Robbery Rate",
     col = "blue", cex = 0.7, pch = 5, ylim=c(0,100))
se5 <- sqrt(Y5/E5)
plot(Y5 ~ se5, xlab = "Standard Error", ylab = "Observed Assault Rate",
     col = "pink", cex = 0.7, pch = 5, ylim=c(0,500),xlim=c(0,2.5))



# Gamma random effects model： Robbery Case
ebresults <- eBayes(Y1, E1)
ebresults$alpha #the estimate of alpha in our model
exp(ebresults$beta) 

# the RRs are greater in the study region than in
# the reference region
mean(Y1/E1)
summary(ebresults$RR)
# Now let's examine the gamma assumption
egamma <- qgamma(seq(0.5, length(Y1), 1)/length(Y1),
                 ebresults$alpha, ebresults$alpha)
par(mfrow = c(1, 2))
# First plot is the estimates from the gamma model
plot(egamma, exp(-ebresults$beta) * sort(Y1/E1), xlim = c(0,5), 
     ylim = c(0, 5), pch= 3, col="pink", 
     xlab = "Exp Order Stat", ylab = "Obs Order Stat (SMR)")
abline(0, 1) #Y1/E1 here = SMR on health statistics...
# Second plot is the Robbery estimates
plot(egamma, exp(-ebresults$beta) * sort(ebresults$RR),
     xlim = c(0, 5), ylim = c(0, 5), xlab = "Exp Order Stat",
     ylab = "Obs Order Stat (Gamma)", pch=2,col="gray")
abline(0, 1)

# This looks pretty reasonable...

# Considers the Smoother Model now
# Below are "Zero Adjustment"...
Ystar1 <- ifelse(Y1 == 0, 0.5, Y1)
Estar1 <- ifelse(Y1 == 0, E1 + 0.5, E1)
SMRstar1 <- Ystar1/Estar1
alphastar1 <- log(SMRstar1)
varalphastar1 <- 1/(SMRstar1 * Estar1)
SMRlower1 <- exp(alphastar1 - 1.96 * sqrt(varalphastar1))
SMRupper1 <- exp(alphastar1 + 1.96 * sqrt(varalphastar1))
SMRwidth1 <- SMRupper1 - SMRlower1

smr1<-Y1/E1
sesmr1 <- sqrt(smr1/E1)
seEBests <- sqrt((ebresults$beta+Y1)*exp(2*ebresults$beta)/
                   (ebresults$alpha+E1*ebresults$beta)^2)
par(mfrow=c(1,2))
plot(SMRstar1,ebresults$RR,xlab="SMR",ylab="EB estimates",
     xlim=c(0,max(SMRstar1)),ylim=c(0,max(SMRstar1)),pch=6,col="purple")
abline(0,1)
plot(log(SMRstar1),log(ebresults$RR),xlim=c(-3,3),
     ylim=c(-3,3),xlab="log(SMR)",ylab="log(EB estimates)",pch=5,col="green")
abline(0,1)

# The shrunk in EB Estimates is pretty obvious
seEBests <- sqrt((ebresults$alpha+Y1)*exp(2*ebresults$beta)/
                   (ebresults$alpha+E1*ebresults$beta)^2)
par(mfrow=c(1,2))
plot(se1,smr1,ylim=c(0,max(smr1)),xlim=c(0,3),
     xlab="SE SMR",ylab="SMR",pch=8,col="pink")
plot(seEBests,ebresults$RR,ylim=c(0,max(smr1)),
     xlim=c(0,7),xlab="SE Emp Bayes",ylab="Emp Bayes",pch=7,col="orange")
par(mfrow=c(1,1))
plot(seEBests ~ se1, ylab = "SE Emp Bayes", xlab = "SE SMR",
     xlim = c(0, 4), ylim = c(0, 8), pch = 1, col = "purple")
abline(0, 1, col = "green")
# Uncertainty estimates of EB estimates

apost1 <- ebresults$alpha+Y1
bpost1 <- (ebresults$alpha+E1*exp(ebresults$beta))/exp(ebresults$beta)
EBlower1 <- qgamma(0.025,apost1,bpost1)
EBupper1 <- qgamma(0.975,apost1,bpost1)
EBwidth1 <- EBupper1-EBlower1

# Comparison of Interval: EB is generally better
par(mfrow = c(1, 3))
plot(EBlower1 ~ SMRlower1, col = "orange")
abline(0, 1, col = "blue")
plot(EBupper1 ~ SMRupper1, col = "orange")
abline(0, 1, col = "blue")
plot(EBwidth1 ~ SMRwidth1, col = "orange")
abline(0, 1, col = "blue")

# Very Obvious that EB incurs smaller interval
####################################################################
ebresultsX <- eBayes(Y1,E1,pov)
ebresultsX$alpha 
ebresults$alpha
# note the reduction in excess-Poisson variation
# compared to the no covariate model
ebresultsX$beta
ebresultsX <- eBayes(Y1,E1,pov)
par(mfrow=c(1,1))
plot(pov,smr1,type="n")
text(pov,smr1)
xval <- seq(0,max(pov),.01)
lines(xval,exp(ebresultsX$beta[1]+ebresultsX$beta[2]*xval))

ebresultsX <- eBayes(Y1,E1,pov)
par(mfrow=c(1,1))
plot(pov,smr1,type="n",xlab="Proportion under Poverty",ylab="SMR Robbery")
text(pov,smr1,col="pink",cex=.4)
xval <- seq(0,max(pov),.01)
lines(xval,exp(ebresultsX$beta[1]+ebresultsX$beta[2]*xval),col="orange")

# 0.47/0.27 with pov, means variability explained by the covariance.

x0 <- rep(0,length(Y1))
x1 <- rep(1,length(Y1))
x2 <- rep(2,length(Y1))
plot(x0,smr1,xlim=c(0,2),type="n",
     ylab="Relative risks",
     xlab="",axes=F)
axis(2)
axis(1,at=c(0,1,2))
text(x0,smr1,cex=.5)
text(x1,ebresults$RR,cex=.5)
text(x2,ebresultsX$RR,cex=.5)
for (i in 1:length(Y1)){
  lines(c(0,1,2),c(smr1[i],ebresults$RR[i],ebresultsX$RR[i]),
        lty=2,col="grey")}; abline(1,0,col="red")

# Lognormal Model
# We now consider an alternative lognormal model for the relative risks, 
# but still independent. We specify the 5\% and 
# 95\% points of the relative risk associated with $\beta$ as  1 and 5.


lnprior <- LogNormalPriorCh(1,5,0.5,0.95)
lnprior
plot(seq(0,7,.1),dlnorm(seq(0,7,.1),meanlog=lnprior$mu,sdlog=lnprior$sigma),
     type="l",xlab=expression(theta),ylab="LogNormal Density")

library(INLA)
library(rgdal)
library(RColorBrewer)

spplot(seattle, c("smr1"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "Blues")))
spplot(seattle, c("smr2"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "PuRd")))
spplot(seattle, c("smr3"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "Purples")))
spplot(seattle, c("smr4"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "Oranges")))
spplot(seattle, c("smr5"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "PuBuGn")))
spplot(seattle, c("smr6"),at=c(0, 0.25, 0.5, 1, 2, 4, 7, 12, 20),
       col.regions=(brewer.pal(8, "GnBu")))

sea.fit1 <- inla(Y1 ~ 1 + f(tract, model ="iid", param=c(1,0.026)), 
                 data=merged, family="poisson", E=E1, 
                 control.predictor=list(compute=T)) 
summary(sea.fit1)
plot(sea.fit1, plot.hyperparameter=FALSE, 
     plot.random.effects=TRUE, plot.fixed.effects=FALSE, prefix="logmodplot1", postscript=T)
sea.fit1$summary.fixed

# Comparison of lognormal and gamma models
# First illustrate how to extract the intercept
# (beta0)
lnorminter1 <- sea.fit1$summary.fixed[4]
# Now extract the medians of the random effects
# (which are centered around alpha)
lnormREs1 <- exp(sea.fit1$summary.random$tract[5])

lnormRRs1 <- as.double(exp(lnorminter1))*lnormREs1[,1]
plot(ebresults$RR,lnormRRs1,xlim=c(0,4.5),ylim=c(0,4.5),
     xlab="Gamma RRs",ylab="Lognormal RRs")
abline(0,1)



# See the difference of smoother

seattle$RRgam <- ebresults$RR
spplot(seattle, c("RRgam"),
       col.regions=colorRampPalette(rev(brewer.pal(8, "RdBu")))(50) )

seattle$RRlnorm1 <- lnormRRs1
spplot(seattle, c("RRlnorm1"),
       col.regions=colorRampPalette(rev(brewer.pal(8, "RdBu")))(50) )

## Uncertainty estimates of Lognormal estimates

LNlower1 <- exp(sea.fit1$summary.linear.predictor["0.025quant"])
LNupper1 <- exp(sea.fit1$summary.linear.predictor["0.975quant"])
LNwidth1 <- LNupper1[,1]-LNlower1[,1]

par(mfrow = c(1, 3))
plot(LNlower1[, 1] ~ SMRlower1, col = "cyan")
abline(0, 1, col = "pink")
plot(LNupper1[, 1] ~ SMRupper1, col = "cyan")
abline(0, 1, col = "pink")
plot(LNwidth1 ~ SMRwidth1, col = "cyan")
abline(0, 1, col = "pink")

## Lognormal model with covariates
merged$pov<-as.numeric(as.character(merged$pov))
sea.fit1X <- inla(Y1 ~1+I(pov)+f(tract, model="iid", 
                                       param=c(1,0.014)),data=merged, family="poisson",E=E1)
summary(sea.fit1X)

sea.fit1X$summary.fixed[2,]
exp(sea.fit1X$summary.fixed[2,])

sea.fit1X$summary.fixed

## Lognormal spatial model with covariates

## We now add spatial (ICAR) random effects to the model.

## We need a graph file containing the neighbors.
nb2INLA("seasss.graph",sea.nb)


## For INLA, the region unique identifier must be INTEGER. So, we have to
## to temporarily create an identifier variable here.
merged$region <- c(1:132)
merged$region2 <- merged$region
sea.fit2 <- inla(Y1 ~ 1 + I(pov) + 
                        f(region,model="iid",param=c(1,0.014)) + 
                        f(region2,model="besag",graph=
                            "seasss.graph",param=c(1,0.68)),data=merged,
                 family="poisson",E=E1,control.predictor=list(compute=TRUE))
summary(sea.fit2)

REsnonspat1 <- exp(sea.fit2$summary.random$region[5])
REsspat1 <- exp(sea.fit2$summary.random$region2[5])
seattle$REsnonspat <- REsnonspat1[,1]
seattle$REsspat <- REsspat1[,1]
spplot(seattle, c("REsnonspat"),
       col.regions=colorRampPalette(rev(brewer.pal(8, "RdBu")))(50) )


## Lognormal spatial model with covariates: non-spatial random effects


spplot(seattle, c("REsnonspat"),
       col.regions=colorRampPalette(rev(brewer.pal(8, "RdBu")))(50) )


## Lognormal spatial model with covariates: spatial random effects
spplot(seattle, c("REsspat"),
       col.regions=colorRampPalette(rev(brewer.pal(8, "RdBu")))(50) )


## Comparison of spatial lognormal and gamma fits: some differences
plot(ebresults$RR,sea.fit2$summary.fitted.values[,4],
     xlab="Gamma fitted",ylab="Spatial fitted")
abline(0,1)


## Proportion of Variation that is Spatial

mat.marg <- matrix(NA, nrow = 132, ncol = 1000)
m <- sea.fit2$marginals.random$region2
for (i in 1:132) {
  Sre <- m[[i]]
  mat.marg[i, ] <- inla.rmarginal(1000, Sre)
}
var.Sre <- apply(mat.marg, 2, var)
var.eps <- inla.rmarginal(1000, inla.tmarginal(function(x) 1/x
                                               ,sea.fit2$marginals.hyper$"Precision for region"))
mean(var.Sre)
mean(var.eps)
perc.var.Sre <- mean(var.Sre/(var.Sre + var.eps))
perc.var.Sre

## Uncertainty estimates of spatial estimates

LN2lower <- exp(sea.fit2$summary.linear.predictor["0.025quant"])
LN2upper <- exp(sea.fit2$summary.linear.predictor["0.975quant"])
LN2width <- LN2upper[,1]-LN2lower[,1]


par(mfrow=c(1,3))
plot(LN2lower[,1]~SMRlower1,col="green")
abline(0,1,col="blue")
plot(LN2upper[,1]~SMRupper1,col="green")
abline(0,1,col="blue")
plot(LN2width~SMRwidth1,col="green")
abline(0,1,col="blue")

##########################################################################
# Part 4 Spatial Regression and GWR
##########################################################################
# Spatial Regression, OLS
# 
seattle$pov<-as.numeric(as.character(seattle$pov))
sea.ols1 <-lm(rob.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols1)

sea.ols2 <-lm(shop.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols2)

sea.ols3 <-lm(burg.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols3)

sea.ols4 <-lm(pdam.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols4)

sea.ols5 <-lm(hom.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols5)

sea.ols6 <-lm(ass.p~ pov+hetero+nohs, data=seattle@data)
summary(sea.ols6)

# Creating Queen-Bound Neighborhoods
list.queen<-poly2nb(seattle, queen=TRUE) # share a side or an edge
W<-nb2listw(list.queen, style="W", zero.policy=TRUE) 
W
plot(W,coordinates(seattle))
coords<-coordinates(seattle)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)

moran.lm<-lm.morantest(sea.ols1, W, alternative="two.sided")
print(moran.lm)


moran.lm<-lm.morantest(sea.ols2, W, alternative="two.sided")
print(moran.lm)

moran.lm<-lm.morantest(sea.ols3, W, alternative="two.sided")
print(moran.lm)

moran.lm<-lm.morantest(sea.ols4, W, alternative="two.sided")
print(moran.lm)

moran.lm<-lm.morantest(sea.ols5, W, alternative="two.sided")
print(moran.lm)

moran.lm<-lm.morantest(sea.ols6, W, alternative="two.sided")
print(moran.lm)

#Lagrange-Multiplier Test
LM1 <-lm.LMtests(sea.ols1, W, test="all")
print(LM1)

LM2 <-lm.LMtests(sea.ols2, W, test="all")
print(LM2)

LM3 <-lm.LMtests(sea.ols3, W, test="all")
print(LM3)

LM4 <-lm.LMtests(sea.ols3, W, test="all")
print(LM4)

LM5 <-lm.LMtests(sea.ols3, W, test="all")
print(LM5)

LM6 <-lm.LMtests(sea.ols3, W, test="all")
print(LM6)



seattle$pov[89]<-32.101 # So strange this data turns to NA

sar.sea1 <-lagsarlm(rob.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.sea1)
impacts(sar.sea1, listw=W)

sar.sea2 <-lagsarlm(shop.p ~ pov+nohs+hetero,data=seattle@data, W,zero.policy=TRUE)
summary(sar.sea2)
impacts(sar.sea2, listw=W)


sar.sea3 <-lagsarlm(burg.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.sea3)
impacts(sar.sea3, listw=W)


sar.sea4 <-lagsarlm(pdam.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.sea4)
impacts(sar.sea4,listw=W)

sar.sea5 <-lagsarlm(hom.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.sea5)
impacts(sar.sea5,listw=W)


sar.sea6 <-lagsarlm(ass.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.sea6)
impacts(sar.sea6,listw=W)


# Spatial Error Model

sar.err1 <-errorsarlm(rob.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.err1)

sar.err2 <-errorsarlm(shop.p ~ pov+nohs+hetero,data=seattle@data, W,zero.policy=TRUE)
summary(sar.err2)

sar.err3 <-errorsarlm(burg.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.err3)

sar.err4 <-errorsarlm(pdam.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.err4)

sar.err5 <-errorsarlm(hom.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.err5)

sar.err6 <-errorsarlm(ass.p ~ pov+nohs+hetero,data=seattle@data, W)
summary(sar.err6)

