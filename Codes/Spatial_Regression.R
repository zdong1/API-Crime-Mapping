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
