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

#Comparison of Interval: EB is generally better
par(mfrow = c(1, 3))
plot(EBlower1 ~ SMRlower1, col = "orange")
abline(0, 1, col = "blue")
plot(EBupper1 ~ SMRupper1, col = "orange")
abline(0, 1, col = "blue")
plot(EBwidth1 ~ SMRwidth1, col = "orange")
abline(0, 1, col = "blue")

# Very Obvious that EB incurs smaller interval
#######################
