##R --vanilla

## nombre de la carpeta de trabajo
setwd("D:/PROJECTS/Gran Sabana/Metodologia") 

##paquetes necesarios
require(raster)
require(gdata)
require(foreign)
require(fractaldim)


##leer shapefiles (cambiar nombres de la carpeta)
grd <- shapefile("D:/PROJECTS/Gran Sabana 2018/KavaGrid.shp")
izza <- shapefile("D:/PROJECTS/Gran Sabana 2018/area_Kav.shp")


## limites del area de estudio
e <- extent(izza)

##cargar archivos con datos (cambiar nombre de carpeta)

frs <- shapefile("D:/PROJECTS/Gran Sabana 2018/fire_clip.shp")

## cortar capa de fuego, usar solo datos con CONFIDENCE mayor a 40
frs.c <- subset(frs,frs@data$CONFIDENCE>40)


##bosque y fragmentacion
vbsq <- raster("D:/PROJECTS/Gran Sabana 2018/bosque_clip.tif")
bbsq <- vbsq>40

## calcular dimension fractal en cada cuadrado del diseño

fds <- hs <- c()
for (k in 7:10){
  ptch <- crop(vbsq,extent(subset(grd,cuadrado==k)),
               snap="near")>40
  if (nrow(ptch)!=ncol(ptch))
    ptch <- crop(vbsq,extent(subset(grd,cuadrado==k)),
                 snap="out")>40
  
  hs <- c(hs,sum(values(ptch),na.rm=T)/ncell(ptch))
  mat <- matrix(values(ptch),ncol=ncol(ptch),byrow=T)+0
  if (nrow(ptch)==ncol(ptch)) {
    fds <- c(fds,(fd.estim.filter1(mat,p.index=2))$fd)
  } else {
    if (nrow(mat)>ncol(mat)) {
      mat <- mat[-(1:2),]
    }
    if (ncol(mat)>nrow(mat))
      mat <- mat[,-(1:2)]
    fds <- c(fds,(fd.estim.filter1(mat))$fd)
  }
  plot(hs,fds)
  
}

## ALTERNATIVA ##Funcion para calcular diemension fractal dentro del bloque segun Gneiting 2012.
####

#patch
#k=1
#crop(vbsq,extent(subset(grd,Id==k)))

#ClassStat(ptch, latlon=TRUE)[2,"mean.frac.dim.index"]                   

fds <- hs <- c()
for (k in 7:10){
  ptch <- crop(vbsq,extent(subset(grd,Id==k)),
               snap="near")>40
  
  
  hs <- c(hs,sum(values(ptch),na.rm=T)/ncell(ptch))
  fds <- c(fds,ClassStat(ptch, latlon=TRUE)[2,"perimeter.area.frac.dim"])
  
  plot(hs,fds)
  
}

fds <- hs <- c()
for (Id in 1:6){
  ptch <- crop(vbsq,extent(subset(grd,cuadrado==Id)),
               snap="near")
  if (nrow(ptch)!=ncol(ptch))
    ptch <- crop(vbsq,extent(subset(grd,cuadrado==k)),
                 snap="out")
  
  hs <- c(hs,sum(values(ptch),na.rm=T)/ncell(ptch))
  mat <- matrix(values(ptch),ncol=ncol(ptch),byrow=T)+0
  if (nrow(ptch)==ncol(ptch)) {
    fds <- c(fds,(fd.estim.filter1(mat))$fd)
  } else {
    if (nrow(mat)>ncol(mat))
      mat <- mat[-1,]
    if (ncol(mat)>nrow(mat))
      mat <- mat[,-1]
    fds <- c(fds,(fd.estim.filter1(mat))$fd)
  }
  plot(hs,fds)
  
}


##mapa del area de estudio con diseño de cuadrados seleccionados
## h: proporcion de bosque en el paisaje
## H: dimension fractal del bosque en el paisaje

plot(vbsq)
plot(grd,add=T,border="maroon")
with(aggregate(coordinates(grd),by=list(grd$cuadrado),median),
     text(V1,V2+0.036,
          sprintf("h=%0.1f%% H=%0.2f",hs*100,fds-1),
          font=2,cex=.7,col=1))
points(frs.c,pch=3,cex=.3,col=2)


## clasificacion de las celdas 

rgrd <- rasterize(grd,vbsq)
dt1 <- aggregate(data.frame(mu=values(vbsq)),
                 list(cdg=values(rgrd)),sum,na.rm=T)
dt2 <- aggregate(data.frame(sd=values(vbsq)),
                 list(cdg=values(rgrd)),sd,na.rm=T)
dts <- merge(dt1,dt2,by="cdg",all=T)
plot(sd~mu,dts)
tt <- table(extract(rgrd,frs.c))
dts <- merge(dts,tt,by.x="cdg",by.y="Var1",all=T)
dts$Freq[is.na(dts$Freq)] <- 0


## frecuencia de celdas con diferentes condiciones de bosque y fuego

## frecuencia de celdas con diferentes condiciones de bosque y fuego
table(cut(dts$mu,c(-1,median(dts$mu,na.rm=T),max(dts$mu,na.rm=T)+1)),
      cut(dts$sd,c(-1,median(dts$sd,na.rm=T),max(dts$sd,na.rm=T)+1)),
      cut(dts$Freq,c(-1,0,1,100)))

tmp <- as.numeric(cut(dts$mu,c(-1,median(dts$mu,na.rm=T),max(dts$mu,na.rm=T)+1)))*10+as.numeric(cut(dts$sd,c(-1,median(dts$sd,na.rm=T),max(dts$sd,na.rm=T)+1)))
dts$bosque <- "B"
dts$bosque[tmp == 11] <- "S"
dts$bosque[tmp == 12] <- "s"
dts$bosque[tmp == 22] <- "b"
tmp <- as.numeric(cut(dts$Freq,c(-1,0,1,100)))
dts$fuego <- c("0","f","F")[tmp]


##resumen de las categorias de bosque y fuego para todas las celdas
with(dts,table(bosque,fuego))

##resumen de las categorias de bosque y fuego por cuadrado

with(dts,table(bosque,fuego,grd@data$cuadrado))


##mapa del area de estudio con diseño de cuadrados seleccionados
## h: proporcion de bosque en el paisaje
## H: dimension fractal del bosque en el paisaje
## B: Bosque continuo
## b: fragmentado con predominio de bosque
## s: fragmentado con predominio de sabana
## S: Sabana
## 0: sin fuego
## f: un evento de fuego en los ultimos 10 años
## F: varios eventos de fuego en los ultimos 10 años

plot(crop(bbsq,grd))
plot(grd,add=T,border="maroon")
with(aggregate(coordinates(grd),by=list(grd$cuadrado),median),
     text(V1,V2+0.036,
          sprintf("h=%0.1f%% H=%0.2f",hs*100,fds),
          font=2,cex=.7,col=1))
points(frs.c,pch=3,cex=.3,col=2)

text(grd,paste(dts$bosque,dts$fuego),cex=.50)
