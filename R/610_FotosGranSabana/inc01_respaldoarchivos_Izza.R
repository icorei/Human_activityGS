bkp <- "/media/jferrer/OneTouch4/GS_cameratraps/"
bkp <- "/media/jferrer/TOSHIBA EXT/CT_GS/"
dst <- "~/fototeca"
dst <- "/media/jferrer/TOSHIBA EXT/GS/"
periodos <- dir(bkp)
names(periodos)[grep("2",periodos)] <- "P1"
names(periodos)[grep("3",periodos)] <- "P2"
names(periodos)[grep("4",periodos)] <- "P3"

tds <- dts <- data.frame()
##for (pp in names(periodos)) {
for (pp in c("P1","P2","P3")) {
    bloques <- dir(sprintf("%s%s",bkp,periodos[pp]))
    bloques <- grep("bloque",bloques,value=T)
    names(bloques) <- gsub("_","",gsub("bloque","B0",gsub("^[0-9\\._]+","",bloques)))
    names(bloques) <- substr(names(bloques),0,3)

    ##  for (bb in names(bloques)) {
    for (bb in c("B01","B02","B03","B04","B05","B06")) {
        camaras <- dir(sprintf("%s%s/%s",bkp,periodos[pp],bloques[bb]))
        names(camaras) <- gsub("\\.","",gsub("_[0-9\\.\\-]+$","",camaras))
        names(camaras) <- toupper( names(camaras))

        for (cc in names(camaras)) {
            system(sprintf("mkdir -p '%s%s/%s/%s'", dst,bb,pp,cc))
             buscar <- sprintf("%s%s/%s/%s",bkp,periodos[pp],bloques[bb],camaras[cc])
            lista <- sprintf("lista.%s.%s.%s.txt",bb,pp,cc)

            if (!file.exists(lista)) {
                for (fotos in dir(buscar,full.names=T)) {
                    if (!grepl("\\([0-9]\\)",fotos))
                        system(sprintf("exiftool -T -r -filename -createdate -Directory -FileSize -ImageDescription -Make -CameraModelName -ShutterSpeedValue -ApertureValue -ExposureCompensation -LightSource -Flash -Aperture -ImageSize -ShutterSpeed '%s' >> %s",fotos,lista))
                }
            }
            if (file.exists(lista)) {
            
            fts <- read.table(lista,sep="\t",stringsAsFactors=F)
            
            ## colocamos nombres a las columnas de "fts", ordenamos la tabla
            colnames(fts) <- c("filename","createdate","Directory","FileSize","ImageDescription","Make","CameraModelName","ShutterSpeedValue","ApertureValue","ExposureCompensation","LightSource","Flash","Aperture","ImageSize","ShutterSpeed")
            
                ## guardamos datos de la fecha y hora en un formato apropiado
                if (fts$createdate %in% "-") {
                    fts$fecha <- NA
                    fts$hora <- NA
                } else {
                    fts$fecha <- chron(dates.=substr(fts$createdate,1,10),
                                       times.=substr(fts$createdate,12,100),
                                       format=c(dates="y:m:d", times="h:m:s"),
                                       out.format=c(dates="Y-m-d",times="h:m:s"))
                    fts$hora <- substr(fts$createdate,12,100)
                }

                
                    fts <- subset(fts,!is.na(fts$fecha))

            if (nrow(fts)>0) {

                fts$camara <- cc
                fts$bloque <- bb
                fts$periodo <- pp
                
                fts <- fts[order(fts$camara,fts$createdate),]
                ## agrupamos la lista de fotos en eventos, un "evento" es una secuencia de fotos separadas por menos de un minuto, de esta forma es posible analizar los datos de forma mas ordenada
                fts$ev <- NA
                fts$ev[1] <- 1
                for (k in 2:nrow(fts)) {
                    if (diff(fts$fecha[(k-1):k])<0.001) {
                        fts$ev[k] <- fts$ev[k-1]
                    } else {
                        fts$ev[k] <- fts$ev[k-1]+1
                    }
                }
                
                
                for (cc in unique(fts$camara)) {
                    ss <- fts$camara %in% cc
                    fts$ev[ss] <- fts$ev[ss]-min(fts$ev[ss])+1
                }
                
                fts$evento <- paste(fts$camara,fts$ev,sep="_E")
                
                length(unique(fts$evento))
                
                
                for (j in 1:nrow(fts)) {
                    if (file.exists(sprintf("%s/%s",fts[j,"Directory"],
                                       fts[j,"filename"]
                                      ))) {
                        system(sprintf("mkdir -p '%s%s/%s/%s/%s'", dst,
                                       fts[j,"bloque"],
                                       fts[j,"periodo"],
                                       fts[j,"camara"],                               
                                       fts[j,"evento"]))
                        system(sprintf("mv '%s/%s' '%s%s/%s/%s/%s'",
                                       fts[j,"Directory"],
                                       fts[j,"filename"],
                                       dst,
                                       fts[j,"bloque"],
                                       fts[j,"periodo"],
                                       fts[j,"camara"],
                                       fts[j,"evento"]))
                    }
                    
                }
            
            
            ## ahora hacemos una tabla con los datos resumidos por evento y lo grabamos en el archivo "tablaEventos.csv", ese archivo lo leemos despues en un programa de hojas de calculo y le agregamos la informacion adicional

                tds <- rbind(tds,fts)
                for (mie in unique(fts$evento)) {
                    SS <- subset(fts,evento==mie)
                    
                    dts <- rbind(dts,data.frame(bloque=unique(SS$bloque),
                                                periodo=unique(SS$periodo),
                                                camara=unique(SS$camara),
                                                evento=unique(SS$evento),
                                                fotos=nrow(SS),
                                                dia=min(days(SS$fecha)),
                                                mes=min(months(SS$fecha)),
                                                año=min(years(SS$fecha)),
                                                hora.ini=min(SS$hora),
                                                hora.fin=max(SS$hora),
                                                fecha1=min(SS$fecha),
                                                fecha2=max(SS$fecha),
                                                duracion=diff(range(SS$fecha))))
                    
                }
            }
        }
    }
}
}
write.csv(file="tablaEventosGranSabanaIzza.csv",dts)
save(file="~/Dropbox/Mapoteca/Rdata/tablaEventosGranSabanaIzza.rda",dts,tds)


table(fts$bloque,fts$periodo)

system(sprintf("cp -rnv %s%s/%s/%s/* %s%s/%s/%s",
               bkp,periodos[pp],bloques[bb],camaras[cc],
               dst,bb,pp,cc))

buscar <- sprintf('%s%s/%s/%s',dst,bb,pp,cc)
