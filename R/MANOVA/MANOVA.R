 

require(dismo)
require(vegan)
require(raster)
require(gdata)
require(foreign)

##load data (change the name of archive, look for location on the computer)
#spacial data
load("D:/PROJECTS/Gran Sabana/Metodologia/redisenomuestral/rasters_GS.rda")
load("D:/PROJECTS/Gran Sabana/Metodologia/redisenomuestral/fuegos_GS.rda")

#data
grid2km_Kavanayen.shp2("D:/PHD_statistic/EventosGranSabana/Eventos_GS_CAM_RAS_2019.csv")
camerasGS <- read.csv2("D:/PHD_statistic/EventosGranSabana/Camaras_GS_short_2019.csv")

#bosque y fragmentacion
vbsq <- raster("D:/PROJECTS/Gran Sabana/Metodologia/GS_studyarea_Izza/TREE/MOD44B.2010.GS.TREE.tif")
grd6 <- shapefile("D:/PROJECTS/Gran Sabana/Metodologia/grids/grid2km_8_1salida.shp")


rgrd <- rasterize(grd6,vbsq)
dt1 <- aggregate(data.frame(mu=values(vbsq)),
                 list(cdg=values(rgrd)),sum,na.rm=T)
dt2 <- aggregate(data.frame(sd=values(vbsq)),
                 list(cdg=values(rgrd)),sd,na.rm=T)
dts <- merge(dt1,dt2,by="cdg",all=T)

camerasGS$bosque <- extract(vbsq,camerasGS[,c("lon","lat")])
camerasGS$grid <- extract(rgrd,camerasGS[,c("lon","lat")])


## merge de los archivos con registros de mamiferos y el de los datos de camaras trampas
GS <- merge(EventosGranSabana_mam_ras,camerasGS,by.x=c("bloque","periodo","camara"),
            by.y=c("bloque","period","camera"),all.y=T)
tt <- table(paste(GS$bloque,GS$periodo,GS$camara),GS$species)
head(tt)

tt <- with(GS,tapply(paste(dieta,periodo,camara),list(species),function(x)length(unique(x))))
tt[is.na(tt)] <- 0
estimateR(tt)


#calculos de chao index (1) por evento (2) por numeros de fotos por

tt <- with(GS,tapply(evento,list(charact_SBOF,species),function(x)length(unique(x))))
tt <- with(GS,tapply(paste(bloque,periodo,camara),list(charact_SBOF,species),function(x)length(unique(x))))
tt[is.na(tt)] <- 0


tt <- table(paste(GS$bloque,GS$periodo,GS$camara),GS$species)

tt<- tt[rowSums(tt)>0,]

t2 <-  camerasGS[match(rownames(tt),with(camerasGS,paste(bloque,period,camera))),]

###calculating MANOVA
head(tt)

t1 <- vegdist(tt,"chao")

#caseria
adonis(t1~caza.celda2+conuco.bloque,t2)
adonis(t1~caza.bloque+H*h,t2)
adonis(t1~h+caza.bloque+conuco.bloque+conuco.dist.m,t2)
adonis(t1~caza.bloque+conuco.bloque+conuco.dist.m,t2)

