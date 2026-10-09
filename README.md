# PA_Effectiveness
Project aimed at understanding where, when, and under what conditions are protected areas most effective at preventing forest loss globally.

Code and documentation for the Global Protected Area Effectiveness project.

## Data-processing workflow

The project is currently being built as a reproducible processing pipeline. The main workflow begins with construction of the Global Forest Loss Cube (GFLC), a global ~1-km raster dataset derived from Hansen Global Forest Change data.

### 1. Generate the Global Forest Loss Cube in Google Earth Engine

See:

`GFLC_Main_Script.txt`

This is the master Google Earth Engine production script for GFLC v0.2. It:

- uses Hansen Global Forest Change 2025 v1.13;
- aggregates the native Hansen data to a ~1-km MODIS sinusoidal grid;
- produces baseline forest-cover variables and annual forest-loss fractions for 2001–2025;
- processes the globe in spatial tiles, including specialized high-latitude handling; and
- exports 27-band GeoTIFFs to Google Drive.

After all Earth Engine tasks are complete, download the resulting `.tif` files and place them together in a single local directory:

`Tiff_GFC/`

### 2. Prepare the GeoTIFFs for ArcGIS

See:

`GFLC v0.2 — POST-DOWNLOAD ARCGIS PREPROCESSING`

Run this R script after downloading the Earth Engine outputs.

The script performs three post-processing steps:

1. Converts masked/NaN cells to explicit GeoTIFF NoData values of `-9999`.
2. Assigns the correct MODIS spherical Sinusoidal CRS metadata (`R = 6371007.181 m`) without reprojecting or resampling the raster data.
3. Removes one empty 1-km western-edge column from two dateline GeoTIFF chunks that otherwise fall just outside the valid MODIS sinusoidal coordinate domain in ArcGIS.

The processed ArcGIS-ready rasters are written to:

`Tiff_GFC_ArcGIS/`

The source TIFFs are not modified.

### 3. Build the ArcGIS mosaic

The processed TIFFs can then be added to a single ArcGIS Pro mosaic dataset.

Current mosaic specifications:

- Pixel type: `32_BIT_FLOAT`
- Bands: `27`
- Coordinate system: MODIS spherical Sinusoidal
- Source NoData: `-9999`
- Raster type: Raster Dataset

Statistics, overviews, symbology, and subsequent analytical preprocessing are documented here as the workflow develops.
