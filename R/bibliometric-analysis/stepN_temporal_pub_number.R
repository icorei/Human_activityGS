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



  #Network analysis
  #We will analyse the complete data set to evaluate
  #how frequently are camera trap cited in conservation planning papers
  #We will use "histNetwork" fuction creates a historical citation network
  #from a bibliographic data frame.
  #Historiograph - Direct citation linkages

  #Crate the data frame ISI.topic.df with all literature review (CT and CP)
  #plus the vector with LDA topics

#Or we should create a Reference co-citation network?

NetMatrix <- biblioNetwork(ISI.topic.df, analysis = "co-citation", network = "references", sep = ";")

net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5)

#NetMatrix <- biblioNetwork(ISI.topic.df, analysis = "co-citation",
 #                          network = "references", sep = ";")
#networkPlot(NetMatrix, normalize = NULL,
 #               weighted= TRUE, n = 100, label = "",
  #          labelsize=0.7,
   #             edgesize = 5,
    #            curved=TRUE, Title = "A Co-citation Network of references",
     #           type = "mds", size= TRUE, remove.multiple=TRUE)

#net <- NetMatrix

#Now convert to igraph object
my.net <-graph_from_adjacency_matrix(net, mode = "directed")
#Examine attributes
E(my.net)$weight
edge_attr(my.net)
#vertex_attr(my.net)
vertex_attr_names(my.net)

#To add a new attribute: topics
#1. Iterate through the vector of vertices V(g)
#You can't iterate the vector of vertices, you'll need to
#iterate through a vertex attribute that all the vertices have.
#2.Filter the data frame down to the row that matches each vector
#3.Return the value of interest
#But first create new table of attributes
tmp001 <- data.frame(id = ISI.topic.df$SR,
                     my.topic = as.character(ISI.topic.df$search.group),
                     lda.topic = as.character(ISI.topic.df$lda_topic))

str(tmp001)

my.net2 <- my.net %>%
set_vertex_attr(.,
                name = 'my.topic',
                index = V(my.net),
                value = sapply(V(my.net)$name, function(x){
                  tmp001 %>%
                    filter(id == x) %>%
                    .$my.topic
                 }))

#This result in a list of topics, we need transform this list
#in a vector and add as vertex attribute in the matrix
#Asign the new atribute as a vector
my.vector1 <- unlist(V(my.net2)$my.topic)
vertex_attr(my.net2, "my.topic2", index = V(my.net2)) <-sapply(my.vector1, paste0, collapse="")
str(V(my.net2)$my.topic2)

my.net2 <- my.net %>%
set_vertex_attr(.,
                name = 'lda.topic',
                index = V(my.net),
                value = sapply(V(my.net)$name, function(x){
                  tmp001 %>%
                    filter(id == x) %>%
                    .$lda.topic
                }))

my.vector2 <- unlist(V(my.net2)$lda.topic)
vertex_attr(my.net2, "lda.topic2", index = V(my.net2)) <-sapply(my.vector2, paste0, collapse="")#unlist(c(my.vector2))
str(V(my.net2)$lda.topic2)

#Now calculate network metrix
#Degree by vertex
tmp002 <- centr_degree(my.net2, mode="all", normalized=T, loops = FALSE)
my.degree <- tmp002$res
#Betw by vertex
tmp003 <- centr_betw(my.net2, directed=F, normalized=T)
my.betw <- tmp003$res

#Asign the new atribute as a vector
vertex_attr(my.net2, "degree", index = V(my.net2)) <-unlist(c(my.degree))
str(V(my.net2)$degree)

vertex_attr(my.net2, "betw", index = V(my.net2)) <-unlist(c(my.betw))
str(V(my.net2)$betw)

#Plot the network
# Remove loops
my.net2 <- simplify(my.net2, remove.multiple = T, remove.loops = T)
#2263 vertex
#Delete isolated vertices
table(degree(my.net2))
Isolated = which(degree(my.net2)<10)
#1688 isolated vertex!!!!
my.net3 = delete.vertices(my.net2, Isolated)
#575 vertex only

# Set vertex color based on topic research:
igraph :: V(my.net3)$color = igraph :: V(my.net3)$my.topic2
igraph :: V(my.net3)$color = gsub("camera_trap", "black", igraph :: V(my.net3)$color)
igraph :: V(my.net3)$color = gsub("cons_plan", "grey70",igraph :: V(my.net3)$color)

# Set vertex color frame based on topic research:
igraph :: V(my.net3)$frame.color = igraph :: V(my.net3)$my.topic2
igraph :: V(my.net3)$frame.color = gsub("camera_trap", "black", igraph :: V(my.net3)$frame.color)
igraph :: V(my.net3)$frame.color = gsub("cons_plan", "grey70",igraph :: V(my.net3)$frame.color)

# Set vertex shape based on my.topic research:
#igraph :: V(my.net3)$shape = igraph :: V(my.net3)$lda.topic2

#change arrow size and edge color:
igraph :: E(my.net3)$arrow.size <- .3
igraph :: E(my.net3)$edge.color <- "blue"
igraph :: E(my.net3)$width <-  1

#Label characteristics
igraph :: V(my.net3)$label = igraph :: V(my.net3)$lda.topic2
V(my.net3)$label.cex = .6 #Label size
V(my.net3)$size = 4

#The best option
plot(my.net3, edge.curved=.1,
     layout=layout_nicely, edge.color = "lightblue",
     display.isolates= TRUE,
     vertex.label = ifelse(degree(my.net3) < 10, NA, V(my.net3)$name))
#ifelse(V(my.net3)$lda.topic2 == "cp" ,NA, V(my.net3)$name)
#vertex.label = V(my.net3)$lda.topic2
#vertex.label = ifelse(degree(my.net3) > 10, V(my.net3)$label, NA)

#Community detection
#Hierarchical agglomerative method that is designed to run well in large networks.
my.ceb <- edge.betweenness.community(my.net3, modularity = TRUE)
modularity(my.ceb)
#0.01862102
#the modularity score Q, which ranged from -1 to 1 and measured how strong
#the division was: the more positive the value of Q, the more significant
#the grouping. By definition, the entire network (as one community) had Q = 0

#How many paper by groups?
membership(my.ceb)
str(my.ceb)
#How many nodes by group
with(my.ceb, tapply(names, membership, function(x) length(unique(x))))

plot(my.ceb, my.net3,
   vertex.label = NA,
   layout = layout_nicely)
#As histogtram
plot_dendrogram(my.ceb, use.modularity = T,
              mode="hclust",
              cex = 0.15,
              lwd= 0.3)
