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
sea.fit2 <- inla(Y1 ~ 1 + I(nohs) + 
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

