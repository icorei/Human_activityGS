

ggplot(data=cam.data, aes(y=dcon,x=tree_1000m,colour=hunting,size=duration)) +
geom_point() +  theme_bw() +
labs(y="Distance to conuco (m)", x="Forest cover (%)",colour="Hunting",size="Duration (days)") +
#stat_smooth(aes(color = hunting, fill = hunting),
#              method = "loess", alpha = 0.2,span=1.8,show.legend=F)
stat_smooth(aes(color = hunting, fill = hunting),
              method = "lm", formula=y~poly(x,2),alpha = 0.2,show.legend=F)

ggsave(sprintf("%s/documentation/figures/Fig-2.pdf",script.dir),width=8,height=5)

pdf(file=sprintf("%s/documentation/figures/Fig-4.pdf",script.dir))
ccs  %>% filter(grepl('dcon',rownames(ccs))) %>% dplyr::arrange(coef.mavg..full...F.) -> ss
par(mar=c(4,8,3,1))
plot(ss$coef.mavg.,1:nrow(ss),xlim=c(-3.8,3.8), pch=19,xlab=expression(hat(beta)),ylab='',axes=F,main='Distance to nearest conuco')
segments(ss$X2.5..,   1:nrow(ss),   ss$X97.5..,1:nrow(ss))
axis(1)
axis(2,1:nrow(ss),gsub("\\.",". ",ss$spp),las=2,font=3)
box()
abline(v=0,lty=2,lwd=2,col=2)
 dev.off()



 Hv <- c('C.paca'=6.336,'C.alector'=4.630, 'D.leporina'=2.681, 'T.terrestris'=2.681,'T.major'=1.949, 'M.gouazoubira'=0.731,'M.americana'=0.731, 'D.kappleri'=0.244, 'D.novemcinctus'=0.244)
 mtz <- data.frame()

 for (k in spps[spps %in% names(Hv)]) {
   if (spp %in% with.quad.term) {
       mtz <- rbind(mtz,data.frame(species=k,abundance=predict(get(sprintf("mavg03.%s",k)),type='state')$fit,hunting=UMF@siteCovs$hunting))
     } else {
       mtz <- rbind(mtz,data.frame(species=k,abundance=predict(get(sprintf("mavg01.%s",k)),type='state')$fit,hunting=UMF@siteCovs$hunting))
     }
 }

 # grouped boxplot
 ggplot(mtz %>% filter(), aes(x=species, y=abundance, fill=hunting)) +
     geom_boxplot(notch=F) + theme_bw() +# or notch=T
     ##labs(title="Model prediction of abundance at sites with and without hunting") +
     labs(y=expression( hat(lambda)), x="",caption="Species",fill="Hunting") +
     theme(axis.text.x = element_text( size = 7, hjust = .5, vjust=.5, face = "italic"),
     plot.margin = unit(c(1,1,2,1), "lines")) +
    ## scale_y_continuous(trans='sqrt') +
     coord_cartesian(clip="off")


ggsave(sprintf("%s/documentation/figures/Fig-5A.pdf",script.dir),width=8,height=5)

 exc <- c('M.tridactyla','E.barbara')

 mtz <- data.frame()

 for (k in spps[!(spps %in% names(Hv)) & !(spps %in% exc)]) {
   if (spp %in% c('C.alector','L.rufaxilla','T.tetradactyla')) {
       mtz <- rbind(mtz,data.frame(species=k,abundance=predict(get(sprintf("mavg03.%s",k)),type='state')$fit,hunting=UMF@siteCovs$hunting))
     } else {
       mtz <- rbind(mtz,data.frame(species=k,abundance=predict(get(sprintf("mavg01.%s",k)),type='state')$fit,hunting=UMF@siteCovs$hunting))
     }
 }

 # grouped boxplot
 ggplot(mtz %>% filter(), aes(x=species, y=abundance, fill=hunting)) +
     geom_boxplot(notch=F) + theme_bw() +## or notch=T
     labs(y=expression( hat(lambda)), x="",fill="Hunting") +
 theme(axis.text.x = element_text( size = 9, hjust = .5, vjust=.5, face = "italic"))

ggsave(sprintf("%s/documentation/figures/Fig-5B.pdf",script.dir),width=8,height=5)
