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





######
## por ordenar


#
#

#Topic 1
M3$topic_1 <- 0
M3$topic_2 <- 0
M3$topic_3 <- 0
M3$topic_4 <- 0
M3$topic_5 <- 0
M3$topic_6 <- 0
M3$topic_7 <- 0
M3$topic_8 <- 0
M3$topic_9 <- 0
M3$topic_10 <- 0

M3$topic_1 <- grepl(1, M3$lda_topic)
M3$topic_1[M3$lda_topic==c("TRUE")]<-1
M3$topic_1[M3$lda_topic==c("FALSE")]<-0

M3$topic_2 <- grepl(2, M3$lda_topic)
M3$topic_2[M3$lda_topic==c("TRUE")]<-1
M3$topic_2[M3$lda_topic==c("FALSE")]<-0

M3$topic_3 <- grepl(3, M3$lda_topic)
M3$topic_3[M3$lda_topic==c("TRUE")]<-1
M3$topic_3[M3$lda_topic==c("FALSE")]<-0

M3$topic_4 <- grepl(4, M3$lda_topic)
M3$topic_4[M3$lda_topic==c("TRUE")]<-1
M3$topic_4[M3$lda_topic==c("FALSE")]<-0

M3$topic_5 <- grepl(5, M3$lda_topic)
M3$topic_5[M3$lda_topic==c("TRUE")]<-1
M3$topic_5[M3$lda_topic==c("FALSE")]<-0

M3$topic_6 <- grepl(6, M3$lda_topic)
M3$topic_6[M3$lda_topic==c("TRUE")]<-1
M3$topic_6[M3$lda_topic==c("FALSE")]<-0

M3$topic_7 <- grepl(7, M3$lda_topic)
M3$topic_7[M3$lda_topic==c("TRUE")]<-1
M3$topic_7[M3$lda_topic==c("FALSE")]<-0

M3$topic_8 <- grepl(8, M3$lda_topic)
M3$topic_8[M3$lda_topic==c("TRUE")]<-1
M3$topic_8[M3$lda_topic==c("FALSE")]<-0

M3$topic_9 <- grepl(9, M3$lda_topic)
M3$topic_9[M3$lda_topic==c("TRUE")]<-1
M3$topic_9[M3$lda_topic==c("FALSE")]<-0

M3$topic_10 <- grepl(10, M3$lda_topic)
M3$topic_10[M3$lda_topic==c("TRUE")]<-1
M3$topic_10[M3$lda_topic==c("FALSE")]<-0

###
#How many years cover our review?
M3$PY <- as.character(M3$PY)
tmp <- table(unlist(strsplit(M3$PY," :: ")))
length(tmp)
#23 years from 1984 to 2020

topic_1<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_1 <- rbind(topic_1, data.frame(my.topic_1 = sum(M3$topic_1[grep(aa, M3$PY)])))

}

topic_2<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_2 <- rbind(topic_2, data.frame(my.topic_2 = sum(M3$topic_2[grep(aa, M3$PY)])))

}

topic_3<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_3 <- rbind(topic_3, data.frame(my.topic_3 = sum(M3$topic_3[grep(aa, M3$PY)])))

}

topic_4<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_4 <- rbind(topic_4, data.frame(my.topic_4 = sum(M3$topic_4[grep(aa, M3$PY)])))

}

topic_5<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_5 <- rbind(topic_5, data.frame(my.topic_5 = sum(M3$topic_5[grep(aa, M3$PY)])))

}


topic_6<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  topic_6 <- rbind(topic_6, data.frame(my.topic_6 = sum(M3$topic_6[grep(aa, M3$PY)])))

}

topic_7<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
topic_7 <- rbind(topic_7, data.frame(my.topic_7 = sum(M3$topic_7[grep(aa, M3$PY)])))

}

topic_8<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
topic_8 <- rbind(topic_8, data.frame(my.topic_8 = sum(M3$topic_8[grep(aa, M3$PY)])))

}

topic_9<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
topic_9 <- rbind(topic_9, data.frame(my.topic_9 = sum(M3$topic_9[grep(aa, M3$PY)])))

}

topic_10<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
topic_10 <- rbind(topic_10, data.frame(my.topic_10 = sum(M3$topic_10[grep(aa, M3$PY)])))

}


tmp1<-as.data.frame(tmp)
fch1<-as.numeric(levels(tmp1$Var1))[tmp1$Var1]
total.reg<-tmp1$Freq
t.ints<-data.frame(cbind(fch1,total.reg, topic_1, topic_2, topic_3, topic_4,
topic_5, topic_6, topic_7, topic_8, topic_9, topic_10))
#Accumulated number of publications
for (k in 2:12) {
  t.ints[,k] <- cumsum(t.ints[,k])
}

#Proportion of total
t.ints$p.topic_1 <- c((t.ints$my.topic_1*100)/t.ints$total.reg)
t.ints$p.topic_2 <- c((t.ints$my.topic_2*100)/t.ints$total.reg)
t.ints$p.topic_3 <- c((t.ints$my.topic_3*100)/t.ints$total.reg)
t.ints$p.topic_4 <- c((t.ints$my.topic_4*100)/t.ints$total.reg)
t.ints$p.topic_5 <- c((t.ints$my.topic_5*100)/t.ints$total.reg)
t.ints$p.topic_6 <- c((t.ints$my.topic_6*100)/t.ints$total.reg)
t.ints$p.topic_7 <- c((t.ints$my.topic_7*100)/t.ints$total.reg)
t.ints$p.topic_8 <- c((t.ints$my.topic_8*100)/t.ints$total.reg)
t.ints$p.topic_9 <- c((t.ints$my.topic_9*100)/t.ints$total.reg)
t.ints$p.topic_10<- c((t.ints$my.topic_10*100)/t.ints$total.reg)

colSums(t.ints)

#Plot to vizualize temporal publication pattern
plot(t.ints$fch1, t.ints$total.reg, pch=NA, xlim=c(1994,2020), ylim=c(0.0,100),
ylab= "% of publications", xlab= "Year")
lines(t.ints$fch1, t.ints$p.topic_1, col= "black", lwd= 2, lty = 1, pch = 3, type="b", cex = 1) #Mammals
lines(t.ints$fch1, t.ints$p.topic_2, col= "grey20", lwd= 2, pch = 15, type="b", cex = 1) #Home range & abundance
lines(t.ints$fch1, t.ints$p.topic_3, col= "grey50", lwd= 2, pch = 17, type="b") #Protected area & threatened spp
lines(t.ints$fch1, t.ints$p.topic_4, col= "black", lwd= 2, pch = 18, type="b") #Use trap & Prey
lines(t.ints$fch1, t.ints$p.topic_5, col= "grey70", lwd= 2, pch= 19, type="b", cex = 1) #Density estimation & Methods
lines(t.ints$fch1, t.ints$p.topic_6, col= "red", pch = 3,type="b") #Richness of mammals spp
lines(t.ints$fch1, t.ints$p.topic_7, col= "blue", pch =15, type="b",  cex = 1, lwd= 3)
lines(t.ints$fch1, t.ints$p.topic_8, col= "green", pch = 17, type="b", ,  cex = 1, lwd= 3)
lines(t.ints$fch1, t.ints$p.topic_9, col= "orange", pch = 18, type="b", ,  cex = 1, lwd= 3)
lines(t.ints$fch1, t.ints$p.topic_10, col= "pink", pch = 19,  type="b",  cex = 1, lwd= 3)

legend(2000, 90,c("topic 1 (Nature reserve for large mammals)","topic 2 (Diversity)",
"topic 3 (Density estimates)", "topic 4 (Conservation & Threatened spp)",
"topic 5 (Protected area)", "topic 6 (Prey – Predator)","topic 7 (Human activities)",
"topic 8 (Activity pattern)", "topic 9 (Occupancy models)",
"topic 10 (Wildlife management & Monitoring)")
,pch =c (3,15,17,18,19, 3,15,17,18,19),
col=c("black","grey20", "grey50", "black", "grey70", "red","blue", "green", "orange", "pink"), cex=1 )


#Now for the 2nd approach we will use lme4
#using a Poisson model with a log link
#But firts we will extract the colums of number of cite by paper from
#the biblioAnalysis function and add to the data frame t.ints.
#We will use this colunm as predictive variable
tmp002 <- biblioAnalysis(M3, sep = ";")
tmp003 <- data.frame(tmp002$MostCitedPapers$`Paper         `, tmp002$TCperYear, tmp002$TotalCitation)
colnames(tmp003)<- c("SR", "TC_year", "TC")

#Now, merge with M3
tmp004 <- data.frame(SR = M3$SR, fch1 = M3$PY, topic = M3$lda_topic)
t.ints2 <- merge(tmp004, tmp003, by ="SR")
str(t.ints2)
t.ints2$fch1 <- as.numeric(as.character(t.ints2$fch1))

#Now we can fit a mixed modelsto fit a unique intercept
#(i.e., mean number of publications if the predictor variable is centered)
#and slope (i.e., rate of change in number of publications) for each topic.
#the predictor variable is time
library(lme4)
my.model1 <- glmer(TC_year ~ fch1 + (fch1|topic),
data = t.ints2, family = poisson)
tt <-as.data.frame(ranef(my.model1)$topic)
colnames(tt) <- c("intercept", "slope")
tt$topic <- c(1:10)
#Plot
p <- ggplot(tt, aes(intercept, slope, label = topic))
p + geom_point(cex = 7)+ geom_text(hjust = 0, nudge_x = 0.01, cex = 7)
```
#In such a model, topics with positive random intercepts (i.e., u > 0)
#can be interpreted as having higher- than-average numbers of articles written
#about them in the period. Similarly, topics with positive random slopes
#have higher-than-average growth in publications during the same period.



## save to a Rdata object:
#  save(file=sprintf("%s/....rda",Rdata.dir), ...)

## That's it!, we are ready for the next step.
