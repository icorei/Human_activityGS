# Version 1 VIIRS Day/Night Band Nighttime Lights

Página web: https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html


## Referencia
> When using the data please credit the product generation to the Earth Observation Group, NOAA National Centers for Environmental Information (NCEI).

## Descarga

> Files with extensions "avg_rade9" contain floating point radiance values with units in nanoWatts/cm2/sr. Note that the original DNB radiance values have been multiplied by 1E9. Files with extension "cf_cvg" are integer counts of the number of cloud-free coverages, or observations, that went in to constructing the average radiance image. Files with extension “cvg” are integer counts of the number of coverages or total observations available (regardless of cloud-cover).


```sh
mkdir -p $GISDATA/sensores/VIIRS/DNB_NightLights/
cd $GISDATA/sensores/VIIRS/DNB_NightLights/

export SERVER=https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10/
wget $SERVER --output-document List.html
html2text List.html |cut -d" " -f1 | grep $20  > fechas
 for FECHA in $(grep 201 fechas)
 do
   export FF=$(echo ${FECHA} | sed -s "s/\\///g")
   wget ${SERVER}${FECHA}  --output-document tmp${FF}.html
   for DATASET in $(html2text tmp${FF}.html | grep ^vcm | cut -d" " -f1)
   do
       wget ${SERVER}${FECHA}${DATASET}  --output-document tmpfinal${FF}.html
       grep tgz tmpfinal${FF}.html > enlaces${FF}
       wget --continue -i enlaces${FF} --force-html --base=${SERVER}${FECHA}${DATASET}
       rm tmpfinal${FF}.html
       rm enlaces${FF}
   done
   rm tmp${FF}.html
done

```


Para la Gran Sabana (bbox/: -63.1 4.6 -60.5 6.6):

```sh
mkdir -p $WORKDIR/input/nightlights
cd $WORKDIR/input/nightlights

for k in $(ls $GISDATA/sensores/VIIRS/DNB_NightLights/*75N180W_vcmcfg*tgz)
do
  tar -xzvf $k
  export m=$(basename $k | cut -d "." -f1)
  export j=$(basename $k | cut -d "_" -f1,2,3,5,6)
  gdalwarp -te -63.1 4.6 -60.5 6.6 -co "COMPRESS=LZW" $m.avg_rade9h.tif $j.tif
  rm *.avg_rade9h.tif *.cf_cvg.tif
done


```
