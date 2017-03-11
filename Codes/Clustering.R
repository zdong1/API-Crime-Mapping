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
brks<-c(0,20,40,80,160,320,640,1280,2560)
spplot(seattle,zcol="tot.p",at = brks)
hist(seattle$tot.p, breaks=200, xlim=c(0,1200),
     col="lightblue",main="Seattle Crime Rate in Census Tract",
     xlab="Crime Rate (Counts per 1000)",freq=TRUE)
coords<-coordinates(seattle)
plot(nb_q,coords, col="pink")
#########################################################################
# Very messy part again, creating expected counts for all types of crime
# versus the observed counts
Y1 <- merged$rob.n
E1 <- sum(Y1)*merged$pop/sum(merged$pop)

Y2<- merged$burg.n
E2<- sum(Y2)*merged$pop/sum(merged$pop)

Y3<- merged$shop.n
E3<-sum(Y3)*merged$pop/sum(merged$pop)

Y4<- merged$auto.n
E4<-sum(Y4)*merged$pop/sum(merged$pop)

Y5 <- merged$ass.n
E5<-sum(Y5)*merged$pop/sum(merged$pop)

Y6 <- merged$hom.n
E6<-sum(Y6)*merged$pop/sum(merged$pop)

Y7 <- merged$pdam.n
E7<-sum(Y7)*merged$pop/sum(merged$pop)

Y8 <- merged$har.n
E8<-sum(Y8)*merged$pop/sum(merged$pop)

#########################################################################
# Monte Carlo Test for Overdispersion
kappaval <- function(Y,fitted,df){
  sum((Y-fitted)^2/fitted)/df}
mod <- glm(Y1~ 1, offset=log(E1), data="seattle", family="quasipoisson"(link="log"))
kappaest <- kappaval(Y1,mod$fitted,mod$df.resid)
nMC <- 1000
ncts <- length(E1)
yMC <- matrix(rpois(n=nMC*ncts,lambda=E1),
              nrow=ncts,ncol=nMC)
kappaMC <- NULL
for (i in 1:nMC){
  modMC <- glm(yMC[,i]~ northing+easting,offset=log(E1), data=seattle,family="quasipoisson")
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

mod7 <- (glm(Y3~1+offset(log(E7)),family="quasipoisson"))
residual7<-residuals(mod7,type="pearson")

mod8 <- (glm(Y4~1+offset(log(E8)),family="quasipoisson"))
residual8<-residuals(mod8,type="pearson")


moran.test(residual1, col.W) # Robbery
moran.test(residual2, col.W) # Burglary
moran.test(residual3, col.W) # Shoplifting
moran.test(residual4, col.W) # Auto Theft
moran.test(residual5, col.W) # Assault
moran.test(residual6, col.W) # Homicide
moran.test(residual7, col.W) # Property Damage
moran.test(residual8, col.W) # Harrassment


moran.test(residual1, col.B) # Robbery
moran.test(residual2, col.B) # Burglary
moran.test(residual3, col.B) # Shoplifting
moran.test(residual4, col.B) # Auto Theft
moran.test(residual5, col.B) # Assault
moran.test(residual6, col.B) # Homicide
moran.test(residual7, col.B) # Property Damage
moran.test(residual8, col.B) # Harrassment



moran.test(residual1, col.S) # Robbery
moran.test(residual2, col.S) # Burglary
moran.test(residual3, col.S) # Shoplifting
moran.test(residual4, col.S) # Auto Theft
moran.test(residual5, col.S) # Assault
moran.test(residual6, col.S) # Homicide
moran.test(residual7, col.S) # Property Damage
moran.test(residual8, col.S) # Harrassment

geary.test(residual1, col.W) # Robbery
geary.test(residual2, col.W) # Burglary
geary.test(residual3, col.W) # Shoplifting
geary.test(residual4, col.W) # Auto Theft
geary.test(residual5, col.W) # Assault
geary.test(residual6, col.W) # Homicide
geary.test(residual7, col.W) # Property Damage
geary.test(residual8, col.W) # Harrassment

geary.test(residual1, col.B) # Robbery
geary.test(residual2, col.B) # Burglary
geary.test(residual3, col.B) # Shoplifting
geary.test(residual4, col.B) # Auto Theft
geary.test(residual5, col.B) # Assault
geary.test(residual6, col.B) # Homicide
geary.test(residual7, col.B) # Property Damage
geary.test(residual8, col.B) # Harrassment

geary.test(residual1, col.S) # Robbery
geary.test(residual2, col.S) # Burglary
geary.test(residual3, col.S) # Shoplifting
geary.test(residual4, col.S) # Auto Theft
geary.test(residual5, col.S) # Assault
geary.test(residual6, col.S) # Homicide
geary.test(residual7, col.S) # Property Damage
geary.test(residual8, col.S) # Harrassment

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
quasipmod7 <- glm(Y7 ~ easting + northing, offset = log(E7),
                  data = seattle, family = quasipoisson())
quasipmod8 <- glm(Y8 ~ easting + northing, offset = log(E8),
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
sidsres7 <- residuals(quasipmod7, type = "pearson")
seattle$resB7 <- sidsres7
sidsres8 <- residuals(quasipmod7, type = "pearson")
seattle$resB8 <- sidsres8

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
lm7 <- localmoran(seattle$resB7, col.W)
seattle$localM7 <- lm1[, 1]

spplot(seattle, zcol = "localM1", at = c(-2.5, -1.4,
                                         -0.6, -0.2, 0, 0.2, 0.6, 1.4, 2.5), col.regions = colorRampPalette(gry)(8))
spplot(seattle, zcol = "localM7", at = c(-2.5, -1.4,
                                        -0.6, -0.2, 0, 0.2, 0.6, 1.4, 2.5), col.regions = colorRampPalette(yellow)(8))

