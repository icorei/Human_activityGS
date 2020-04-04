setwd("~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/R/bibliometric-analysis")
data.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/documents/ISI-20191211"
require(lubridate)
library(stringr)
library(caret)
library(sna)
library(igraph)
library(ggplot2)
library(plyr)
library(scales)
library(tidyr)


  ######
  #STEP 4: NETWORK ANALYSIS
  ######

  #Network analysis
  #We will analyse the complete data set to evaluate
  #how frequently are camera trap cited in conservation planning papers
  #We will use "histNetwork" fuction creates a historical citation network
  #from a bibliographic data frame.
  #Historiograph - Direct citation linkages

  #Crate the data frame n3 with all literature review (CT and CP)
  #plus the vector with LDA topics
  str(my.rslt)
  my.rslt$lda_topic <- as.character(my.rslt$lda_topic)
  n3 <- merge(my.data, my.rslt, by = "UT", all.x = T)
  dim(n3)
  str(n3)
  table(n3$my.topic, n3$lda_topic)

  #Unify lda topic for CP papers, lad-topics = NA should be "cp"
  n3$lda_topic2 <-n3$lda_topic
  str(n3)
  n3$lda_topic2[is.na(n3$lda_topic)] <- "cp"
  table(n3$my.topic, n3$lda_topic2)

  n3 <- within(n3, lda_topic2[lda_topic2 == 'cp' & my.topic == 'camera_trap'] <- 'sin_lda')
  table(n3$my.topic, n3$lda_topic2)
  #There are 283 document of CT without LDA classification

  #####
  histResults2 <- histNetwork(n3, min.citations = 3, sep = ";")

  #Extract the network matrix from the object so we can have more control
  #of plotting parameters
  net <- histResults2$NetMatrix
  str(net)

  #Or we should create a Reference co-citation network?
  #NetMatrix <- biblioNetwork(n3, analysis = "co-citation",
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
  tmp001 <- data.frame(id = n3$SR,
                       my.topic = as.character(n3$my.topic),
                       lda.topic = as.character(n3$lda_topic2))

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
