##R --vanilla
require(raster)
e <- extent(-61.869,-61.005,5.453,5.750)
r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000.tif")
r1 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_lossyear.tif")
fbsq <- vbsq <- crop(r0,e)
pbsq <- crop(r1,e)

r2 <- vbsq*area(pbsq)

x <- values(r2)
y <- values(pbsq)

rslt <- aggregate(x,list(y),sum)

bsq <- sum(rslt$x)-cumsum(c(0,rslt$x[-1]))
fch <- 2000:2019
prd <- factor(fch<2012)

mdl0 <- lm(bsq~fch,subset=fch<2012)
mdl1 <- lm(bsq~fch,subset=fch>2014)
mdl2 <- lm(bsq~poly(fch,2))
plot(bsq~fch)
matlines(fch,
   predict(mdl0,data.frame(fch=2000:2019),interval="prediction"),col=2,lty=c(1,2,2))
matlines(fch,
   predict(mdl1,data.frame(fch=2000:2019),interval="prediction"),col=3,lty=c(1,2,2))
matlines(fch,
   predict(mdl2,data.frame(fch=2000:2019),interval="prediction"),col=4,lty=c(1,2,2))

teo.prd <- predict(mdl0,data.frame(fch=c(2000,2010,2020,2030,2040,2050)),interval="prediction")

## best estimate: 3.6 % decline in 50 years (2000 to 2050 assuming a linear trend between 2000 and 2012)
1-(teo.prd[6,1]/teo.prd[1,1])
## 95% interval: 3.4 -- 3.8
1-(teo.prd[6,2]/teo.prd[1,3])
 1-(teo.prd[6,3]/teo.prd[1,2])

fut.prd <- predict(mdl2,data.frame(fch=c(2000,2010,2020,2030,2040,2050)),interval="prediction")

## best estimate: 11.5 % decline in 50 years (2000 to 2050 assuming a quadratic trend)
1-(fut.prd[6,1]/fut.prd[1,1])
## 95% interval: 9.6 -- 13.5
1-(fut.prd[6,2]/fut.prd[1,3])
 1-(fut.prd[6,3]/fut.prd[1,2])

mdl1 <- glm(bsq~fch*prd,family=quasipoisson(log),)
plot(bsq~fch)
lines(fch,predict(mdl1,type="response"))

plot(rslt$x[-1])
