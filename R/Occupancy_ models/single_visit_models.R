require(detect)

eventos <- read.csv2(sprintf("%s/input/fieldwork/Eventos_GS_CAM_RAS_2019.csv",script.dir))
camaras <- read.csv2(sprintf("%s/input/fieldwork/Camaras_GS_short_2019.csv",script.dir))


plot(vbsq)
plot(grd,add=T,border="maroon")
points(lat~long,data=eventos,subset=!camara %in% "RAS",col=4,pch=19)
points(lat~long,data=eventos,subset=camara %in% "RAS",col=5,pch=3)


with(dts,
     text(x,y+0.036,
          sprintf("h=%0.1f%% H=%0.2f",hs*100,fd-1),
          font=2,cex=.7,col=1))
points(frs.c,pch=3,cex=.3,col=2)

table(eventos$species,eventos$camara %in% "RAS")

pa <- eventos$species %in% "P.concolor"

#dts <- aggregate(data.frame(pa=eventos$species %in% "P.concolor"),by=eventos[,c("bloque","periodo","camara")],max)
left_join(eventos,camaras,by=c('bloque'='bloque','periodo'='period','camara'='camera')) %>%
   group_by(bloque,periodo,camara) %>%
   summarise(lon=mean(long),lat=mean(lat.x), pa=max(species %in% "P.concolor"),effort=replace_na(mean(dias.de.trabajo),5)) %>%
   mutate(rastro=camara %in% "RAS") -> dts

dts$bosque <- extract(vbsq,dts[,c("lon","lat")]) ##> 50
dts$bosque.cell <- extract(rbsq,dts[,c("lon","lat")]) ##> 50
## distribucion bimodal, pero funciona mejor con la variable cuantitativa que con una binomal
##hist(dts$bosque)
dts$v1 <- ( dts$bosque.cell - mean(dts$bosque.cell) ) / sd(dts$bosque.cell)
dts$v2 <- ( dts$bosque - mean(dts$bosque) ) / sd(dts$bosque)
dts$e1 <- ( dts$effort - mean(dts$effort) ) / sd(dts$effort)

dim(dts)

## even after installing dclone and dcmle it throws errors in dc methods...
fit <- svocc(pa ~ bosque + bosque.cell | effort+rastro, data=dts, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "dc"))

fit <- svocc(pa ~ v1+v2 | e1+rastro, data=dts, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
summary(fit)


GS <- merge(EventosGranSabana_mam,camerasGS,by.x=c("bloque","periodo","camara"),
            by.y=c("bloque","period","camera"),all.y=T)
tt <- table(paste(GS$bloque,GS$periodo,GS$camara),GS$species)
head(tt)
