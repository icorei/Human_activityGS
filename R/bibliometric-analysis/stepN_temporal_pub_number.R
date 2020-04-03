##
#Tenporal pattern in publication
###

#How many publications by year?
#Create binomial variables for topics
my.data$camera <- ifelse(my.data$my.topic %in% "cameratrap",1,0)
my.data$planning <- ifelse(my.data$my.topic %in% "consplan",1,0)

###
#How many years cover our review?
range(my.data$PY,na.rm=T)
#31 years from 1984 to 2020

dts <- with(my.data,aggregate(data.frame(camera,planning),by=list(year=PY),sum))
dts$total <- dts$camera+dts$planning

#Plot to vizualize temporal publication pattern
plot(total~year, data=dts, pch=NA, xlim=c(1984,2019), ylim=c(0.0,sum(dts$total)),
ylab= "Accumaleted publications", xlab= "Year")
lines(cumsum(camera)~year, data=dts, col= "black", lwd= 1, lty = 1)
lines(cumsum(planning)~year, data=dts, col= "blue", lwd= 1, lty = 2)
legend(1984,2500,c("Camera traps","Conservation planning"),lty=c(1,2),
     col=c("black","blue"), cex=.9)
