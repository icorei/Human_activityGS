#!R --vanilla
##paquetes necesarios
require(dplyr)
require(tidyr)
require(raster)
##require(gdata)
require(foreign)
require(fractaldim)
require(SDMTools)
require(chron)

## ubicación de la carpeta de trabajo y el repositorio local

if (Sys.getenv("WORKDIR") == "") {
  ## for Izza:
  work.dir <- "D:/PROJECTS/Gran Sabana/Metodologia"
  script.dir <- "???"
} else {
  ## for JR:
  work.dir <- Sys.getenv("WORKDIR")
  script.dir <- Sys.getenv("SCRIPTDIR")
}

## set working directory
setwd(work.dir)

# unzip shapefiles from local repository to working directory
unzip(sprintf("%s/input/sampling/grid2km_Kavanayen.zip",script.dir))
unzip(sprintf("%s/input/sampling/grid2km_Warapata.zip",script.dir))

## read both grids and join them
grd1 <- shapefile("grid2km_Warapata.shp")
grd2 <- shapefile("grid2km_Kavanayen.shp")
grd <- rbind(grd1,grd2)

##unzip(sprintf("%s/input/sampling/Canaima.zip",script.dir))
##unzip(sprintf("%s/input/sampling/GS_teritory.zip",script.dir))
##unzip(sprintf("%s/input/sampling/area_powierzchnia.zip",script.dir))
##unzip(sprintf("%s/input/sampling/area_Kav.zip",script.dir))

vbsq <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000.tif")
vbsq <- crop(vbsq,grd)
plot(vbsq)
plot(grd,add=T)


##########
## bosque y fragmentación: dimensión fractal
###########
dts <- data.frame()
for (k in 1:10){
  xy <- apply(coordinates(subset(grd,cuadrado==k)),2,median)
  r0 <- crop(vbsq,extent(subset(grd,cuadrado==k)),
               snap="near")
  ptch <- r0>40
  mat <- matrix(values(ptch),ncol=ncol(ptch),byrow=T)+0
  idx <- min(dim(ptch)[1:2])
  mat <- mat[1:idx,1:idx]

  dts <- rbind(dts,
    data.frame(k,x=xy[1],y=xy[2],
      mu=mean(values(r0)),sg=sd(values(r0)),
      hs=sum(values(ptch),na.rm=T)/ncell(ptch),
      fd=fd.estim.filter1(mat,p.index=2)$fd,
      fda=ClassStat(ptch, latlon=TRUE)[2,"perimeter.area.frac.dim"]
    )
  )
}
plot(fd~hs,dts)
text(dts$hs,dts$fd,1:10,adj=2)

## ALTERNATIVA ##Funcion para calcular diemension fractal dentro del bloque segun Gneiting 2012.
## Esta se comporta como una función de hs
plot(fda~hs,dts)
text(dts$hs,dts$fda,1:10,adj=2)


##########
## fuego
###########

frs <- shapefile("GS_M6_FIRES.shp")
## cortar capa de fuego, usar solo datos con CONFIDENCE mayor a 40
frs.c <- subset(frs,frs@data$confidence>40)
frs.c@data$fch <- chron(dates.=frs.c@data$acq_date,format="y/m/d")
plot(months(frs.c@data$fch))
plot(years(frs.c@data$fch))
table(months(frs.c@data$fch),years(frs.c@data$fch))

##mapa del area de estudio con diseño de cuadrados seleccionados
## h: proporcion de bosque en el paisaje
## H: dimension fractal del bosque en el paisaje

plot(vbsq)
plot(grd,add=T,border="maroon")
with(dts,
     text(x,y+0.036,
          sprintf("h=%0.1f%% H=%0.2f",hs*100,fd-1),
          font=2,cex=.7,col=1))
points(frs.c,pch=3,cex=.3,col=2)



########
## estratificación de las celdas
########

rgrd <- rasterize(grd,vbsq)
cell.dts <- data.frame(cell=values(rgrd),bosque=values(vbsq))
tt <- extract(rgrd,frs.c)


cell.dts %>% group_by(cell) %>% # group by cell
  summarize(mu=mean(bosque),sg=sd(bosque)) %>% # summarise mean and sd bosque
  left_join(data.frame(cell=as.numeric(names(table(tt))),fire.freq=as.numeric(table(tt))),by=c("cell")) %>% # left join with fire freq table
  mutate(fire.freq=replace_na(fire.freq,0)) -> # replace nulls with zeros
  cell.smr # This is the output data.frame


plot(sg~mu,cell.smr)

### POR HACER...
## Falta calcular frecuencia de celdas con diferentes condiciones de bosque y fuego
## frecuencia de celdas con diferentes condiciones de bosque y fuego

# table(cut(dts$mu,c(-1,median(dts$mu,na.rm=T),max(dts$mu,na.rm=T)+1)),
#       cut(dts$sd,c(-1,median(dts$sd,na.rm=T),max(dts$sd,na.rm=T)+1)),
#       cut(dts$Freq,c(-1,0,1,100)))
#
# tmp <- as.numeric(cut(dts$mu,c(-1,median(dts$mu,na.rm=T),max(dts$mu,na.rm=T)+1)))*10+as.numeric(cut(dts$sd,c(-1,median(dts$sd,na.rm=T),max(dts$sd,na.rm=T)+1)))
# dts$bosque <- "B"
# dts$bosque[tmp == 11] <- "S"
# dts$bosque[tmp == 12] <- "s"
# dts$bosque[tmp == 22] <- "b"
# tmp <- as.numeric(cut(dts$Freq,c(-1,0,1,100)))
# dts$fuego <- c("0","f","F")[tmp]
#
#
# ##resumen de las categorias de bosque y fuego para todas las celdas
# with(dts,table(bosque,fuego))
#
# ##resumen de las categorias de bosque y fuego por cuadrado
#
# with(dts,table(bosque,fuego,grd@data$cuadrado))
#
#
# ##mapa del area de estudio con diseño de cuadrados seleccionados
# ## h: proporcion de bosque en el paisaje
# ## H: dimension fractal del bosque en el paisaje
# ## B: Bosque continuo
# ## b: fragmentado con predominio de bosque
# ## s: fragmentado con predominio de sabana
# ## S: Sabana
# ## 0: sin fuego
# ## f: un evento de fuego en los ultimos 10 años
# ## F: varios eventos de fuego en los ultimos 10 años
#
# plot(crop(bbsq,grd))
# plot(grd,add=T,border="maroon")
# with(aggregate(coordinates(grd),by=list(grd$cuadrado),median),
#      text(V1,V2+0.036,
#           sprintf("h=%0.1f%% H=%0.2f",hs*100,fds),
#           font=2,cex=.7,col=1))
# points(frs.c,pch=3,cex=.3,col=2)
#
# text(grd,paste(dts$bosque,dts$fuego),cex=.50)
