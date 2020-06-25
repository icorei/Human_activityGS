# MODIS and VIIRS datasets on active fires

We used the new interface for selecting and downloading data: https://firms.modaps.eosdis.nasa.gov/download/create.php

The MODIS data is available from November 2000 (for Terra) and from July 2002 (for Aqua) to the present. VIIRS 375 m data is available from January 2012 to the present.

User documentation and complementary data in https://cdn.earthdata.nasa.gov/conduit/upload/10575/MODIS_C6_Fire_User_Guide_B.pdf
 also FAQ in https://earthdata.nasa.gov/faq/firms-faq#ed-user-guides

## Spatial database and query

We downloaded the global layers and added them to a local postGIS database. Then we use *ogr2ogr* to export this to shapefiles for the target region. We will only use the Modis time series since it covers a larger period of time (almost 20 years).

```sh
cd $WORKDIR
export xmin="-63.1"
export ymin="4.6"
export xmax="-60.5"
export ymax="6.6"

ogr2ogr -f "ESRI Shapefile" GS_M6_FIRES.shp  PG:"host=localhost user=jferrer dbname=gisdata" -sql "SELECT * FROM firms.fire_archive_m6 WHERE ST_Contains(ST_makeEnvelope(${xmin}, ${ymin}, ${xmax}, ${ymax}, 4326),wkb_geometry)"

ogr2ogr -f "ESRI Shapefile" GS_V1_FIRES.shp  PG:"host=localhost user=jferrer dbname=gisdata" -sql "SELECT * FROM firms.fire_archive_v1 WHERE ST_Contains(ST_makeEnvelope(${xmin}, ${ymin}, ${xmax}, ${ymax}, 4326),wkb_geometry)"

tar -cjvf fires_GS.tar.bz2 GS*FIRES.*

```

**Note** that the user guide suggests:
| the near real-time MODIS fire locations should not be used for time series analyses or long-term studies; for such purposes the standard MCD14ML product is more appropriate

So we should explore a different data set for this.

## References
* Giglio, L., Descloitres, J., Justice, C. O. and Kaufman, Y. 2003. An enhanced contextual fire detection algorithm for MODIS. Remote Sensing of Environment 87:273-282. doi: 10.1016/S0034-4257(03)00184-6
* Schroeder, W., Oliva, P., Giglio, L., & Csiszar, I. A. (2014). The New VIIRS 375m active fire detection data product: algorithm description and initial assessment. Remote Sensing of Environment, 143, 85-96. doi: 10.1016/j.rse.2013.12.008
