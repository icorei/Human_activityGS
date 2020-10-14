# Global Forest Change

## Referencia
> Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. *High-Resolution Global Maps of 21st-Century Forest Cover Change.* **Science** 342 (15 November): 850–53. [Data available on-line](http://earthenginepartners.appspot.com/science-2013-global-forest).

## Repositorio de los datos

```sh
export GISDATA=/opt/gisdb/extra-gisdata/
export SRC="https://storage.googleapis.com/earthenginepartners-hansen"
```

## Descarga de los datos:

Para todas las versiones desde v1.0 (2013) hasta v1.7 usamos un script en bash para descargar todas las capas:

```sh

for VRS in GFC-2019-v1.7 GFC-2018-v1.6 GFC-2017-v1.5 GFC-2016-v1.4 GFC-2015-v1.3 GFC2015 GFC2014 GFC2013
do
    mkdir -p $GISDATA/sensores/Landsat/$VRS
    cd $GISDATA/sensores/Landsat/$VRS
    for VAR in gain lossyear treecover2000
    do
	     ##Venezuela
	     curl -O -C - $SRC/$VRS/Hansen_${VRS}_${VAR}_[1-2]0N_0[6-8]0W.tif
    done
done
```


Creamos un VRT (Virtual Dataset) para facilitar acceso a los diferentes archivos

```sh
export VRS=GFC-2019-v1.7
export VAR=treecover2000

cd $GISDATA/sensores/Landsat/
for VRS in GFC-2019-v1.7
do
  for VAR in gain lossyear treecover2000
  do
    gdalbuildvrt index_${VRS}_${VAR}.vrt $GISDATA/sensores/Landsat/$VRS/Hansen_${VRS}_${VAR}_*.tif
  done
done

```


Para la Gran Sabana (bbox/: -63.1 4.6 -60.5 6.6) podemos extraer los datos asi:

```sh
export VRS=GFC-2019-v1.7
cd $WORKDIR
mkdir -p $WORKDIR/$VRS
for VAR in gain lossyear treecover2000
do
  ## use -co "COMPRESS=LZW" for highest compression lossless ration
  gdalwarp -te -63.1 4.6 -60.5 6.6 -co "COMPRESS=LZW" $GISDATA/sensores/Landsat/index_${VRS}_${VAR}.vrt $WORKDIR/$VRS/Hansen_${VRS}_${VAR}.tif
done

```
