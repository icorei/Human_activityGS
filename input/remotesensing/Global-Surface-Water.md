# Global Surface Water

https://global-surface-water.appspot.com/#data

#### Referencia
> Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (doi:10.1038/nature20584)


## Repositorio de los datos

https://global-surface-water.appspot.com/download
https://storage.cloud.google.com/global-surface-water/downloads_ancillary/DataUsersGuidev2018.pdf

## Descarga de los datos:

```sh
mkdir -p $GISDATA/sensores/Landsat/GSW/
cd $GISDATA/sensores/Landsat/GSW/

## install download script
##python -m pip install download_water_data
##download_water_data occurrence -d $GISDATA/sensores/Landsat/GSW/2018
##nohup download_water_data seasonality -d $GISDATA/sensores/Landsat/GSW/2018 &
## but some areas are lacking data, maybe incomplete downloads?
## I move this to /opt/gisdb/extra-gisdata/sensores/

# try other python scripts
cd $GISDATA/sensores/Landsat/GSW/
mv ~/Downloads/downloads_ancillary_downloadWaterData* $GISDATA/sensores/Landsat/GSW/
 unzip downloads_ancillary_downloadWaterDatasets2018.zip


## this downloads v1.1 files:
python2 downloadWaterData.py $GISDATA/sensores/Landsat/GSW/v1.1/occurrence "occurrence"
python2 downloadWaterData.py $GISDATA/sensores/Landsat/GSW/v1.1/seasonality "seasonality"
python2 downloadWaterData.py $GISDATA/sensores/Landsat/GSW/v1.1/transitions "transitions"

```


Creamos un VRT (Virtual Dataset) para facilitar acceso a los diferentes archivos

```sh

cd $GISDATA/sensores/Landsat/GSW/
for VRS in v1.1
do
  for VAR in occurrence seasonality transitions
  do
    gdalbuildvrt index_${VRS}_${VAR}.vrt $GISDATA/sensores/Landsat/GSW/$VRS/$VAR/*.tif
  done
done

```


Para la Gran Sabana (bbox/: -63.1 4.6 -60.5 6.6) podemos extraer los datos asi:

```sh
export VRS=v1.1
cd $WORKDIR
mkdir -p $WORKDIR/$VRS
for VAR in occurrence
do
  ## use -co "COMPRESS=LZW" for highest compression lossless ration
  gdalwarp -te -63.1 4.6 -60.5 6.6 -co "COMPRESS=LZW" $GISDATA/sensores/Landsat/GSW/index_${VRS}_${VAR}.vrt $WORKDIR/$VRS/GSW_${VRS}_${VAR}.tif
done

```
