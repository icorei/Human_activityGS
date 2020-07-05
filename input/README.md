## Input data folder

This folder contains data from the field survey.

### Remote sensing data (_input/remotesensing_)

Scripts for data download and preparation of external data used in the analysis.

### Sampling design (_input/sampling_)

Spatial vector files in shapefile format used to define the area of study and sampling design.

### Field work (_input/fieldwork_)

Files and data on mammals and birds occurance and camera trap location.

```sh
cd $WORKDIR

ogrinfo $SCRIPTDIR/input/fieldwork/GS_track_all.gpx ## waypoints routes tracks route_points track_points
ogrinfo $SCRIPTDIR/input/fieldwork/GS_track_all.gpx tracks

ogr2ogr tracks.shp $SCRIPTDIR/input/fieldwork/GS_track_all.gpx tracks
ogr2ogr track_points.shp $SCRIPTDIR/input/fieldwork/GS_track_all.gpx track_points
```

```R
require(raster)
s0 <- shapefile("tracks.shp")
 plot(s0)

```
