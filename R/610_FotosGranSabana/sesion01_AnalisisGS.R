##R --vanilla
## cargar paquetes necesarios
require(chron)
require(gdata)
require(vegan)
source("~/Dropbox/Mapoteca/inc/inc00_funciones.R")

## establecer carpeta de trabajo (donde se guardan las imagenes)
setwd("~/tmp")
camaras <- read.xls("~/Dropbox/Mapoteca/data/600_FragGranSabana/2019/Camaras_GS_2019.xlsx",as.is=T,encoding="utf8")
 eventos <- read.xls("~/Dropbox/Mapoteca/data/600_FragGranSabana/2019/Eventos_GS_CAM_RAS_2019.xlsx",as.is=T,encoding="utf8")

eventos$bloque <- sub("B010","B10",eventos$bloque)




plot(lat~lon,camaras)


table(camaras$bloque,camaras$period)

camaras$f1 <- chron(dates.=camaras$fecha.act,times.=ifelse(nchar(camaras$hora.act)==5,paste0(camaras$hora.act,":00"),camaras$hora.act),
                    format = c(dates = "d/m/y", times = "h:m:s"))
camaras$f2 <- chron(dates.=camaras$fecha.desact.real,times.=ifelse(nchar(camaras$hora.desact.real)==5,paste0(camaras$hora.act,":00"),camaras$hora.desact.real),
                    format = c(dates = "d/m/y", times = "h:m:s"))

camaras$effort <- abs(camaras$f2-camaras$f1)
camaras[camaras$camera %in% "IS10","effort"] <- camaras[camaras$camera %in% "IS10","effort"]/3
## camara en una misma localidad en los tres periodos
subset(camaras,camera %in% "IS10")

with(camaras,aggregate(effort,list(bloque,period),sum))

evts <- subset(eventos,!camara %in% "RAS" & !(species %in% c("vaca","raton")))
evts$cdg <- paste(evts$bloque,evts$periodo)

mtz <- with(evts,tapply(evento,list(cdg,species),length))

mtz[is.na(mtz)] <- 0

mtz.GS <- mtz[-grep(" P4$",rownames(mtz)),]
mtz.GS <- mtz.GS[,colSums(mtz.GS)>0]

plot(specaccum(mtz,condition=F))
plot(specaccum(mtz.GS,condition=F),add=T,col=2)

d0 <- vegdist(mtz,"chao")

h0 <- hclust(d0)

layout(matrix(1:2,ncol=1))
par(mar=c(1,9,2,1))

plot(h0,hang=-1)

par(mar=c(1,10,0,2))
image(sqrt(mtz[h0$labels[h0$order],order(colSums(mtz))]),col=rev(heat.colors(10)),axes=F)

axis(2,(0:(ncol(mtz)-1))/(ncol(mtz)-1),colnames(mtz)[order(colSums(mtz))],las=2)





##archs <- dir("~/SIG/mapas/Venezuela/250m_16_days_EVI","tif$",full.names=T)
##archs <- dir("~/SIG/mapas/Venezuela/250m_16_days_NDVI","tif$",full.names=T)
archs <- dir("/media/jferrer/Elements/SIG/mapas/Venezuela/250m_16_days_NDVI.006","tif$",full.names=T)
qrchs <- dir("/media/jferrer/Elements/SIG/mapas/Venezuela/250m_16_days_VI_Quality.006","tif$",full.names=T)

qry <- matrix(nrow=nrow(camaras),ncol=length(archs),
                      dimnames=list(c(apply(unique(camaras[,1:3]),1,paste,collapse=":")),
                          sapply(archs,function(x) strsplit(x,"\\.")[[1]][3])))

for (aa in archs) {
    qa <- strsplit(aa,"\\.")[[1]][3]
    qry[,qa] <- 
        as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",camaras$lon,camaras$lat),collapse="\n"),aa),intern=T))
}
 ndvi.camara <- mSSt(qry)

## quality of NDVI measurement
## https://gis.stackexchange.com/questions/163457/handling-mod13q1-ndvi-product-quality-assessment-qa-flags
## https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
qry <- matrix(nrow=nrow(camaras),ncol=length(qrchs),
                      dimnames=list(c(apply(unique(camaras[,1:3]),1,paste,collapse=":")),
                          sapply(archs,function(x) strsplit(x,"\\.")[[1]][3])))

for (aa in qrchs) {
    qa <- strsplit(aa,"\\.")[[1]][3]
    qry[,qa] <- 
        as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",camaras$lon,camaras$lat),collapse="\n"),aa),intern=T))
}
 viq.camara <- R.utils::intToBin(qry)

## VI quality (pixels 0 and 1) 00 produced good quality, 01 produced check QA, 10 produced but cloudy...
## usefulness (pixels 2-5) 0000 highest -- 1100 lowest -- 1111 not useful

table(as.vector(substring(viq.camara,15,16)))

qry <- ndvi.camara
qry[!substring(viq.camara,15,16)%in% "00"] <- NA

head(viq.camara)
## si usamos solo 2112...
##sort(table(as.vector(viq.camara)))

table(subset(eventos,camara %in% "RAS")$species)

hdvi <- hclust(dist(qry))
plot(hdvi)

require(cluster)
hdvi <- fanny(dist(qry),k=3)
hdvi <- pam(dist(qry),k=3)
table(hdvi$clustering)
hdvi$silinfo$avg.width

hdvi <- hclust(dist(qry))
plot(hdvi)
hdvi <- agnes(daisy(qry),method="complete")
plot(hdvi)
## con k 4 el ancho de silueta es mejor que con cinco...
hdvi <- fanny(dist(qry),k=4)
table(hdvi$clustering)


grp <- hdvi$clustering
grp2 <- substr(camaras$charact_SBOF,0,1)
table(grp,grp2)


mtz <- with(evts,tapply(evento,list(cdg,species),length))
evts <- subset(eventos,!camara %in% "RAS" & !(species %in% c("vaca","raton")))
evts$cdg <- paste(evts$bloque,evts$periodo)
evts$cdg <- apply(evts[,c("bloque","periodo","camara")],1,paste,collapse=":")

mtz <- with(evts,tapply(evento,list(cdg,species),function(x) length(unique(x))))
mtz[is.na(mtz)] <- 0
mtz <- mtz[rownames(mtz) %in% rownames(ndvi.camara),]
mtz <- mtz[,colSums(mtz)>0]
dim(ndvi.camara)
 dim(mtz)


require(labdsv)
IV.GS <- indval(as.matrix(mtz),grp[match(rownames(mtz),names(grp))])
IV.G2 <- indval(as.matrix(mtz),grp2[match(rownames(mtz),rownames(ndvi.camara))])

IV.G2$maxcls[IV.G2$pval<0.05]


##png(file="~/Escritorio/EjemploGS_IndVal.png",width=1200,height=900)
layout(matrix(1:4,ncol=2))
slc <- grep("A201[5-7]",colnames(qry))
x <- as.numeric(substring(colnames(qry)[slc],2,5)) + (as.numeric(substring(colnames(qry)[slc],6,8))/365)


for (k in 1:3) {
    matplot(x,t(ndvi.camara[grp %in% k,slc]),type="p",col=rgb(.5,.5,.5,.45),ylim=c(0,1),ylab="NDVI",xlab="meses",pch=1,axes=F)
    axis(2)
    axis(1,2015:2018,las=2,col.axis="grey35",lty=0,cex.axis=.8,line=-2)

    axis(1,seq(2015+(15/365),2016,by=30/365),month.abb,las=2,cex.axis=.65)
    axis(1,seq(2016+(15/365),2017,by=30/365),month.abb,las=2,cex.axis=.65)
    axis(1,seq(2017+(15/365),2018,by=30/365),month.abb,las=2,cex.axis=.65)
        matpoints(x,t(qry[grp %in% k,slc]),type="p",col=1,ylim=c(0,1),ylab="NDVI",xlab="meses",pch=19)
    ##matplot(1:length(slc),t(apply(ndvi.camara[grp %in% k,slc],2,quantile,c(.1,.5,.9))),type="l",col=rgb(.5,.5,.5,1),lty=c(3,1,3),lwd=2,ylim=c(0,1),ylab="NDVI",xlab="meses")

    txt <- sprintf("%s IV=%0.3f (p=%0.3f)",
                   names(IV.GS$indcls[IV.GS$maxcls %in% k & IV.GS$pval < 0.05 ]),
                   IV.GS$indcls[IV.GS$maxcls %in% k & IV.GS$pval < 0.05 ],
                   IV.GS$pval[IV.GS$maxcls %in% k & IV.GS$pval < 0.05 ])
    
    text(2015.2,0.21,paste(txt,collapse="\n"),adj=-.1,cex=1)
}
##dev.off()



## usar NDVI (probar EVI)
## periodo tiempo para mostrar (Mayo 2015 -- Mayo 2016; Mayo -- Julio 2018)
## verificar calidad de las mediciones
## tabla de valor indicador.
## escribir métodos



hdvi <- hclust(dist(ndvi.camara))
plot(hdvi)
grp <- cutree(hdvi,k=4)
grp2 <- substr(camaras$charact_SBOF,0,1)
table(grp,grp2)




table(camaras$bloque,cutree(hdvi,k=4))
plot(lat~lon,camaras,col=cutree(hdvi,k=4))

camaras$grp <- cutree(hdvi,k=4)

mtz <- with(evts,tapply(evento,list(,species),length))

mtz[is.na(mtz)] <- 0



require(knitr)
mi.arch <- "~/Dropbox/Mapoteca/doc/610_FotosGranSabana/IndiceIndicador.Rmd"
rmarkdown::render(mi.arch,"all")
