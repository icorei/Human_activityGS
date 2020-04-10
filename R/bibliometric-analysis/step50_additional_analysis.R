
######
# ADDITIONAL ANALYSIS
######
my.way <-data.frame(lda.topic = V(my.net2)$lda.topic2,
                    betw = V(my.net2)$betw,
                    degree = V(my.net2)$degree,
                    my.topic = V(my.net2)$my.topic2,
                    my.name = V(my.net2)$name)

str(my.way)
table(my.way$my.topic, my.way$lda.topic)

#Create a data frame with memberships
my.memb <- as.data.frame(as.table(membership(my.ceb)))
str(my.memb)
colnames(my.memb) <- c("my.name", "membership")

#Combine both data frames
my.way2 <- merge(my.way, my.memb, by = "my.name", all.x = T)
dim(my.way2)
str(my.way2)
table(my.way2$my.topic)

table(my.way2$membership, my.way2$my.topic)
table(my.way2$membership, my.way2$lda.topic)

boxplot(my.way2$degree ~ my.way2$lda.topic)
####
sp <- ggplot(my.way2, aes(degree, betw, fill = my.topic, size = 3))
sp <- sp + geom_point(aes(color = my.topic)) +
  theme(legend.position = "top") +
  facet_grid(membership ~ .) +
  scale_color_grey()+
  geom_text(
    label= my.way2$my.name,
    nudge_x = -0.1, nudge_y = -0.1, cex = 3,
    check_overlap = T
  )
sp

####
#END
####
