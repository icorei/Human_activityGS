##R --vanilla
## cargar paquetes necesarios
require(chron)

## establecer carpeta de trabajo (donde se guardan las imagenes)
setwd("~/tmp")

##respaldar archivos
##source("~/Dropbox/Mapoteca/doc/610_FotosGranSabana/inc00_respaldoarchivos.R")
##source("~/Dropbox/Mapoteca/doc/610_FotosGranSabana/inc01_respaldoarchivos_Izza.R")

## establecer carpeta donde se encuentran las fotos a leer
## organizar camaras 

##source("~/Dropbox/Mapoteca/doc/610_FotosGranSabana/inc00_listaEventos.R")



require(RMySQL)
archivo.sql <- "~/lib/sql/acceso.cnf"
mi.sql <- dbDriver("MySQL")
c.nm1 <- dbConnect(mi.sql, group="MV",default.file=archivo.sql)
tmp1 <- dbGetQuery(c.nm1,"SET NAMES 'utf8'")
tmp1 <- dbGetQuery(c.nm1,"SET CHARACTER SET utf8")

ft2 <- dbReadTable(c.nm1,"fotos")
nt2 <- dbReadTable(c.nm1,"ANOTACIONES")
ft2$fecha <- chron(dates.=substr(ft2$fecha,2,9),times.=substr(ft2$fecha,11,18),
      format=c(dates="y-m-d", times = "h:m:s"))

table(duplicated(ft2[,c("filename","createdate","FileSize")]))

length(unique(ft2$evento))
dim(ft2)
boxplot(ft2$fecha~ft2$camara)
boxplot(ft2$fecha~ft2$Directory)
dim(dts)
head(dts$fotos/((minutes(dts$duracion)*60) + seconds(dts$duracion)+1))
with(dts,aggregate(evento,list(año,mes,dia,camara),function(x) length(unique(x)))) 
hist(dts$fecha1[dts$año %in% 2015])
hist(hours(dts$fecha1[dts$año %in% 2015]))


lugar <- "Municipio Gran Sabana, sector 5, estado Bolívar. \nI. Stachowicz, Lab. Ecol. Esp. Permiso MPPEA N 1421 3/33/2015"
dstdir <- "/var/www/html/proyectos/MonitoreoVertebrados/GS"

for (j in sample(unique(ft2[ft2$bloque %in% "B06","evento"]))) {
##for (j in  subset(nt2,anotación == "Algo")$evento) {

    SS <- subset(ft2,evento %in% j)
    destino <- unique(sprintf("%s/%s",dstdir,SS$evento))
    if (!file.exists(destino))
        system(sprintf("mkdir -p %s",destino))
        for (oo in unique(sample(unique(SS$filename),4,replace=T))) {
    ##    for (oo in unique(SS$filename)) {
        origen <- sprintf("%s/%s",unique(SS$Directory),oo)
        rslt <- sprintf("%s/%s",destino,gsub(".JPG",".jpg",oo))
        if (!file.exists(rslt)) {
            source("~/Dropbox/Mapoteca/doc/610_FotosGranSabana/inc02_JPGweb.R")
        }
    }
}

##write.csv(dts,"tablaEventos.csv")
##write.csv(dts,"tablaEventosOAEC.csv")

##os <- 42+(15.21/24)
##ft2[ft2$camara=="100MFCAM","fecha"] <- ft2[ft2$camara=="100MFCAM","fecha"]+os

dstdir <- "/var/www/html/proyectos/MonitoreoVertebrados/GS"
##dstdir <- "OAECgif2"
for (mie in unique(ft2$evento)) {
    source("inc01_hacerGIFs.R")
}

##mie=ft2$evento[301]
##source("inc01_GIFcambios.R")


for (j in  subset(nt2,anotación == "Puma")$evento) {

}



require(raster)
j <- "B06.P3.IS10_E31" ## un evento con un puma
j <- "B03.P3.LEE04_E12" ## oso palmero
## alternativa con mad

if (file.exists("pruebaImgAna.rda")) {
    load("pruebaImgAna.rda")
} else {
    img.ana <- data.frame()
}
for (j in unique(ft2[ft2$bloque %in% "B01" & ft2$periodo %in% "P3"
                    ,"evento"])) {
    if (!j %in% img.ana$evento) {
        SS <- subset(ft2,evento %in% j)
        if (nrow(SS) < 20) {
            ## reducir tamaño
            s0 <- aggregate(stack(  sprintf("%s/%s",SS$Directory,SS$filename)),10)
            
            s1 <- s2 <- s3 <- raster(s0,1)
            if (nlayers(s0)>3) {
                ks <- grep(sprintf(".%s$",1),names(s0))
                values(s1) <- apply(values(s0)[,ks],1,mad)
                ks <- grep(sprintf(".%s$",2),names(s0))
                values(s2) <- apply(values(s0)[,ks],1,mad)
                ks <- grep(sprintf(".%s$",3),names(s0))
                values(s3) <- apply(values(s0)[,ks],1,mad)
                s1 <- stack(s1,s2,s3)
            } else {
                s1 <- s0
            }
            s1 <- max(values(s1))-s1
            plotRGB(s1,scale=max(values(s1)))
            co <- max(values(s1))*.9 ## escoger un punto de corte informativo
            cdif <- clump(raster(s1,1)<co)
        t0 <- table(values(cdif))
            
            
            print(rev(sort(t0))[1:10])
            ## aparentemente un valor > 100000 indica un elemento importante
        ## los cuadros 1-3 son informativos            
            
            wo <- which.max(t0)
            idx <- values(cdif) %in% wo
            sum(idx)
        
            rsm <- apply(xyFromCell(cdif,(1:ncell(cdif))[idx]),2,quantile,c(.05,.5,.95))
            polygon(rsm[c(1,3,3,1),1],rsm[c(1,1,3,3),2])
            if (file.exists(sprintf("%s/%s/",dstdir,j))) {
                dev.copy(png,file=sprintf("%s/%s/MAD.png",dstdir,j))
                dev.off()
            }
            img.ana <-
                rbind(img.ana,
                      data.frame(evento=j,banda=1,
                                 ndif=length(t0),
                                 tdif=sum(t0),
                                 mdif=median(t0),f0=t0[wo],
                                 xmin=rsm[1,1],xmed=rsm[2,1],xmax=rsm[3,1],
                                 ymin=rsm[1,2],ymed=rsm[2,2],ymax=rsm[3,2]))
            
            print(tail(img.ana,3))
            save(file="pruebaImgAna.rda",img.ana)
        }
    }
}

qry <- "SELECT * FROM ANOTACIONES"
rslt <- dbGetQuery(c.nm1,qry)

img.nt <- rslt[match(img.ana$evento,rslt$evento),"anotación"]
save(file="pruebaImgAna.rda",img.nt,img.ana)

plot(ndif~tdif,img.ana,col = ifelse (!is.na(img.nt),1,-1) + 
         ifelse ((img.nt %in% "Nada"),1,2))
y <- img.nt %in% "Nada"
ss <- !is.na(img.nt)
require(rpart)
mdl0 <- rpart(y[ss]~ndif+tdif, data=img.ana[ss,])
mdl1 <- rpart(y[ss]~ndif+tdif+mdif+f0+xmin+xmed+xmax+ymin+ymed+ymax, data=img.ana[ss,])

ypred <- predict(mdl0,img.ana[!ss,])
yduda <- predict(mdl0,img.ana[img.nt %in% "Nada",])

plot(ndif~tdif,img.ana,col = ifelse (!is.na(img.nt),1,-1) + 
         ifelse ((img.nt %in% "Nada"),1,2),pch=4,cex=.5)
text(img.ana[!ss,"tdif"],img.ana[!ss,"ndif"],
     ifelse(ypred>.5,"N","A"),cex=.6)

boxplot(predict(mdl0,img.ana)~paste(is.na(img.nt),(img.nt %in% "Nada")),varwidth=T)
boxplot(predict(mdl1,img.ana)~paste(is.na(img.nt),(img.nt %in% "Nada")),varwidth=T)




## posiblemente haya algo
img.ana[!ss,][ypred<.05,"evento"]

## posible error 
img.ana[img.nt %in% "Nada",][yduda<.05,"evento"]


img.ana <- data.frame()
for (j in unique(ft2[ft2$bloque %in% "B02" &
                         ft2$periodo %in% "P2","evento"])) {

    SS <- subset(ft2,evento %in% j)
    if (nrow(SS) < 20) {
        ## reducir tamaño
        s0 <- aggregate(stack(  sprintf("%s/%s",SS$Directory,SS$filename)),10)

        for (band in 1:3) {
            ks <- grep(sprintf(".%s$",band),names(s0))

            s1 <- raster(s0,1)
            values(s1) <- apply(values(s0)[,ks],1,median)
            
            for (k in 1:length(ks)) {
                if (k>1) {
                    rdif <- raster(s0,ks[k])-raster(s0,ks[k-1])
                    ## probar con cambios en una dirección 
                    ##values(rdif)[values(rdif)<0] <- 0
                    ## probar con cambios absolutos
                    rdif <- abs(rdif)
                    plot(rdif)
                    
                    co <- quantile(values(rdif),.9) ## escoger un punto de corte informativo
                    cdif <- clump(rdif>co)
                    t0 <- table(values(cdif))
                    
                    
                    print(rev(sort(t0))[1:10])
                    ## aparentemente un valor > 100000 indica un elemento importante
                    ## los cuadros 1-3 son informativos            
                    
                    wo <- which.max(t0)
                    idx <- values(cdif) %in% wo
                    sum(idx)
                    
                    rsm <- apply(xyFromCell(cdif,(1:ncell(cdif))[idx]),2,quantile,c(.05,.5,.95))
                    polygon(rsm[c(1,3,3,1),1],rsm[c(1,1,3,3),2])
                    
                    img.ana <-
                        rbind(img.ana,
                              data.frame(evento=j,cuadro=k,banda=band,
                                         ndif=length(t0),
                                         tdif=sum(t0),
                                         mdif=median(t0),f0=t0[wo],
                                         xmin=rsm[1,1],xmed=rsm[2,1],xmax=rsm[3,1],
                                         ymin=rsm[1,2],ymed=rsm[2,2],ymax=rsm[3,2]))
                    
                    print(tail(img.ana,3))
                }
            }
        }
    }
}
