##R --vanilla
library(ggplot2)
require(raster)

work.dir <- Sys.getenv("WORKDIR")
e <- extent(-61.869,-61.005,5.453,5.750)
r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000.tif")
r1 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_lossyear.tif")
fbsq <- vbsq <- crop(r0,e)
pbsq <- crop(r1,e)

sum(values(r2))
sum(values(area(vbsq)))
 sum(values(r2))/sum(values(area(vbsq)))

r2 <- vbsq*area(pbsq)/100

x <- values(r2)
y <- values(pbsq)

rslt <- aggregate(x,list(y),sum)

summary(rslt$x[-1])

dts <- data.frame(bsq=sum(rslt$x)-cumsum(c(0,rslt$x[-1])),
   fch = 2000:2019)


prd <- dts$fch<2012

mdl0 <- lm(bsq~fch,dts,subset=fch<2012)
prd0 <- predict(mdl0,data.frame(fch=2000:2019),interval="prediction")

ggplot(dts,aes(x = fch, y = bsq)) +
geom_smooth(color="red", fill="pink",aes(y=prd0[,1],ymin = prd0[,2], ymax = prd0[,3]),
    stat = "identity") +
geom_point(color="slateblue4") + xlab("Year") +
  ylab("Tree cover in km²")

mdl2 <- lm(bsq~poly(fch,2),dts)
prd2 <- predict(mdl2,data.frame(fch=2000:2019),interval="prediction")

fig.res = 300
inch.to.mm = 25.4
fig.width = 6
fig.height = 5
point.size = 12

svg(sprintf('%s/Tree-cover-change.svg',work.dir), width= fig.width, height=fig.height, pointsize=point.size)

ggplot(dts,aes(x = fch, y = bsq)) +
geom_smooth(color="red", fill="pink",aes(y=prd2[,1],ymin = prd2[,2], ymax = prd2[,3]),
    stat = "identity") +
geom_point(color="slateblue4") + xlab("Year") +
  ylab("Tree cover in km²") + theme_classic()

dev.off()


pdf(sprintf('%s/Tree-cover-change.pdf',work.dir), width= fig.width, height=fig.height, pointsize=point.size)

ggplot(dts,aes(x = fch, y = bsq)) +
geom_smooth(color="red", fill="pink",aes(y=prd2[,1],ymin = prd2[,2], ymax = prd2[,3]),
    stat = "identity") +
geom_point(color="slateblue4") + xlab("Year") +
  ylab("Tree cover in km²") + theme_classic()

dev.off()


# mdl1 <- lm(bsq~fch,dts,subset=fch>2014)




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

#mdl1 <- glm(bsq~fch*prd,family=quasipoisson(log),)
#plot(bsq~fch)
#lines(fch,predict(mdl1,type="response"))

#plot(rslt$x[-1])


nd







# Library

# Create dummy data
data <- data.frame(
  cond = rep(c("condition_1", "condition_2"), each=10),
  my_x = 1:100 + rnorm(100,sd=9),
  my_y = 1:100 + rnorm(100,sd=16)
)

# Basic scatter plot.
p1 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point( color="#69b3a2") +
  theme_ipsum()

# with linear trend
p2 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# linear trend + confidence interval
p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
