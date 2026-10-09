# =============================================================================
# GFLC v0.2 — POST-DOWNLOAD ARCGIS PREPROCESSING
# =============================================================================
#
# PURPOSE
#
# Convert raw Google Earth Engine GFLC GeoTIFF exports into ArcGIS-ready
# GeoTIFFs without changing the scientific raster grid or valid cell values.
#
# This script performs three required post-download operations:
#
#   1. Encode NA / NaN as explicit GeoTIFF NoData = -9999.
#
#   2. Correct the CRS metadata to the MODIS spherical Sinusoidal CRS:
#
#        +proj=sinu
#        +lon_0=0
#        +x_0=0
#        +y_0=0
#        +R=6371007.181
#        +units=m
#        +no_defs
#
#      IMPORTANT:
#      This is CRS METADATA ASSIGNMENT ONLY.
#      There is NO reprojection and NO resampling.
#
#   3. Repair the western-dateline ArcGIS domain issue affecting two physical
#      chunks of:
#
#        GFLC_v02_latm30_0_lonm180_m150
#
#      Those chunks extend one empty 1-km column west of the valid MODIS
#      Sinusoidal domain. The script detects this condition, verifies that
#      the offending column contains NO valid forest_fraction_30pct cells,
#      and removes only that empty column.
#
# INPUT
#
#   /Users/alexrogers/Desktop/Tiff_GFC
#
# OUTPUT
#
#   /Users/alexrogers/Desktop/Tiff_GFC_ArcGIS
#
# The input archive is NEVER modified.
#
# =============================================================================


suppressPackageStartupMessages({
  library(terra)
})


# =============================================================================
# 0. SETTINGS
# =============================================================================

SOURCE_DIR <-
  "/Users/alexrogers/Desktop/Tiff_GFC"

OUTPUT_DIR <-
  "/Users/alexrogers/Desktop/Tiff_GFC_ArcGIS"


# Explicit ArcGIS-friendly NoData sentinel.
#
# IMPORTANT:
# 0 is scientifically valid throughout the GFLC and MUST NOT be used as NoData.

NODATA_VALUE <- -9999


# Correct MODIS sphere.

MODIS_RADIUS_M <- 6371007.181


# Correct CRS for the existing GFLC x/y coordinates.
#
# Assignment of this CRS DOES NOT move or resample cells.

MODIS_CRS <-
  paste(
    "+proj=sinu",
    "+lon_0=0",
    "+x_0=0",
    "+y_0=0",
    paste0("+R=", MODIS_RADIUS_M),
    "+units=m",
    "+no_defs"
  )


# The theoretical western x-limit of the global MODIS Sinusoidal domain.

MODIS_XMIN <-
  -pi * MODIS_RADIUS_M


# Small numerical tolerance in metres.
#
# We only care about approximately whole 1000-m columns outside the domain.

DOMAIN_TOLERANCE_M <- 0.01


# Logical standard tile known to have produced the two physical TIFF chunks
# extending one empty column beyond the western MODIS domain.

DATELINE_TILE_ID <-
  "GFLC_v02_latm30_0_lonm180_m150"


# =============================================================================
# 1. LOCATE INPUT TIFFS
# =============================================================================

if (!dir.exists(SOURCE_DIR)) {
  stop(
    "Input directory does not exist:\n",
    SOURCE_DIR
  )
}


dir.create(
  OUTPUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)


files <- list.files(
  SOURCE_DIR,
  pattern = "\\.(tif|tiff)$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)


if (length(files) == 0) {
  stop(
    "No TIFF files found in:\n",
    SOURCE_DIR
  )
}


# Because all TIFFs are being placed in one flat output directory,
# duplicated filenames would be unsafe.

if (anyDuplicated(basename(files))) {
  stop(
    "Duplicate TIFF filenames detected in the input archive.\n",
    "Resolve duplicate names before running this script."
  )
}


cat("\n============================================================\n")
cat("GFLC POST-DOWNLOAD PREPROCESSING\n")
cat("============================================================\n")

cat("\nInput folder:\n")
cat(SOURCE_DIR, "\n")

cat("\nOutput folder:\n")
cat(OUTPUT_DIR, "\n")

cat("\nTIFFs found:", length(files), "\n")

cat("\nMODIS sphere radius:", MODIS_RADIUS_M, "m\n")
cat("MODIS western x-limit:", MODIS_XMIN, "m\n")
cat("NoData output value:", NODATA_VALUE, "\n")

cat("\n============================================================\n\n")


# =============================================================================
# 2. PROCESSING LOG
# =============================================================================

processing_log <-
  vector(
    "list",
    length(files)
  )


# =============================================================================
# 3. PROCESS EVERY TIFF
# =============================================================================

for (i in seq_along(files)) {
  
  f <- files[i]
  
  fname <-
    basename(f)
  
  
  cat("\n------------------------------------------------------------\n")
  cat(
    sprintf(
      "[%d/%d] %s\n",
      i,
      length(files),
      fname
    )
  )
  cat("------------------------------------------------------------\n")
  
  
  # ---------------------------------------------------------------------------
  # 3A. Read source
  # ---------------------------------------------------------------------------
  
  r <-
    rast(f)
  
  
  # ---------------------------------------------------------------------------
  # 3B. Basic GFLC structure checks
  # ---------------------------------------------------------------------------
  
  if (nlyr(r) != 27) {
    stop(
      "\nUnexpected band count in:\n",
      fname,
      "\nExpected 27; found ",
      nlyr(r)
    )
  }
  
  
  expected_first_bands <-
    c(
      "mean_treecover2000",
      "forest_fraction_30pct",
      "loss_cellfrac_2001"
    )
  
  
  if (!identical(
    names(r)[1:3],
    expected_first_bands
  )) {
    
    stop(
      "\nUnexpected GFLC band names/order in:\n",
      fname
    )
  }
  
  
  if (!isTRUE(
    all.equal(
      res(r),
      c(1000, 1000)
    )
  )) {
    
    stop(
      "\nUnexpected raster resolution in:\n",
      fname,
      "\nFound: ",
      paste(res(r), collapse = " x ")
    )
  }
  
  
  # Record original geometry before making any intended change.
  
  original_extent <-
    ext(r)
  
  original_resolution <-
    res(r)
  
  original_origin <-
    origin(r)
  
  original_ncol <-
    ncol(r)
  
  original_nrow <-
    nrow(r)
  
  original_names <-
    names(r)
  
  
  trimmed_columns <- 0
  
  valid_before_trim <- NA_real_
  
  valid_after_trim <- NA_real_
  
  
  # ===========================================================================
  # 3C. REPAIR WESTERN DATELINE DOMAIN EDGE
  # ===========================================================================
  #
  # In the validated production archive, exactly two physical TIFF chunks from:
  #
  #   GFLC_v02_latm30_0_lonm180_m150
  #
  # began at approximately:
  #
  #   xmin = -20,016,109.354 m
  #
  # while the MODIS sphere has a theoretical western limit of approximately:
  #
  #   xmin = -20,015,109.356 m
  #
  # Thus those TIFFs contained exactly one completely empty 1-km column beyond
  # the valid ArcGIS coordinate domain.
  #
  # We detect the geometry rather than relying on the physical chunk suffix.
  # ===========================================================================
  
  
  outside_western_domain <-
    xmin(r) <
    (
      MODIS_XMIN -
        DOMAIN_TOLERANCE_M
    )
  
  
  if (outside_western_domain) {
    
    
    # Safety check:
    #
    # We only expect this condition for the known -180 to -150 standard tile.
    # If some other TIFF violates the MODIS domain, stop rather than silently
    # modifying an unexpected raster.
    
    if (!grepl(
      DATELINE_TILE_ID,
      fname,
      fixed = TRUE
    )) {
      
      stop(
        "\nUnexpected raster extends west of the MODIS Sinusoidal domain:\n",
        fname,
        "\nxmin = ",
        xmin(r),
        "\n\nNo automatic trimming was performed."
      )
    }
    
    
    cat(
      "Western-domain issue detected.\n"
    )
    
    cat(
      "Original xmin:",
      xmin(r),
      "\n"
    )
    
    
    # Count valid forest-support cells before trimming.
    #
    # This is used to prove that the repair does not remove scientific data.
    
    valid_before_trim <-
      global(
        !is.na(
          r[["forest_fraction_30pct"]]
        ),
        "sum",
        na.rm = TRUE
      )[1, 1]
    
    
    # There should currently be one offending column, but the while-loop
    # makes the operation robust to any future export that extends by more
    # than one empty column.
    
    while (
      xmin(r) <
      (
        MODIS_XMIN -
        DOMAIN_TOLERANCE_M
      )
    ) {
      
      
      # -----------------------------------------------------------------------
      # Inspect EXACTLY the westernmost raster column
      # -----------------------------------------------------------------------
      
      west_col <-
        crop(
          r[["forest_fraction_30pct"]],
          ext(
            xmin(r),
            xmin(r) + xres(r),
            ymin(r),
            ymax(r)
          ),
          snap = "near"
        )
      
      
      west_vals <-
        values(
          west_col,
          mat = FALSE
        )
      
      
      n_valid_west <-
        sum(
          !is.na(west_vals)
        )
      
      
      n_forest_west <-
        sum(
          west_vals > 0,
          na.rm = TRUE
        )
      
      
      cat(
        "Westernmost column valid cells:",
        n_valid_west,
        "\n"
      )
      
      cat(
        "Westernmost column forest cells:",
        n_forest_west,
        "\n"
      )
      
      
      # NEVER remove a column containing valid GFLC data.
      
      if (n_valid_west != 0) {
        
        stop(
          "\nDateline repair safety check failed.\n",
          "The westernmost out-of-domain column contains valid data in:\n",
          fname,
          "\n\nNo scientific cells will be deleted."
        )
      }
      
      
      # -----------------------------------------------------------------------
      # Remove exactly one empty 1000-m western column
      #
      # No reprojection.
      # No resampling.
      # Remaining cells retain their original coordinates.
      # -----------------------------------------------------------------------
      
      r <-
        crop(
          r,
          ext(
            xmin(r) + xres(r),
            xmax(r),
            ymin(r),
            ymax(r)
          ),
          snap = "near"
        )
      
      
      trimmed_columns <-
        trimmed_columns + 1
      
    }
    
    
    # -------------------------------------------------------------------------
    # Verify that all valid forest-support cells were retained
    # -------------------------------------------------------------------------
    
    valid_after_trim <-
      global(
        !is.na(
          r[["forest_fraction_30pct"]]
        ),
        "sum",
        na.rm = TRUE
      )[1, 1]
    
    
    if (!identical(
      as.numeric(valid_before_trim),
      as.numeric(valid_after_trim)
    )) {
      
      stop(
        "\nValid-cell count changed during dateline repair for:\n",
        fname
      )
    }
    
    
    cat(
      "Columns trimmed:",
      trimmed_columns,
      "\n"
    )
    
    cat(
      "New xmin:",
      xmin(r),
      "\n"
    )
    
    cat(
      "Valid cells preserved:",
      valid_before_trim,
      "->",
      valid_after_trim,
      "\n"
    )
    
  }
  
  
  # ===========================================================================
  # 3D. ASSIGN THE CORRECT MODIS SPHERICAL SINUSOIDAL CRS
  # ===========================================================================
  #
  # IMPORTANT:
  #
  # This is NOT:
  #   project()
  #   resample()
  #   warp()
  #
  # The existing x/y cell coordinates are retained exactly.
  #
  # We are correcting only the CRS metadata used to interpret those coordinates.
  # ===========================================================================
  
  crs(r) <-
    MODIS_CRS
  
  
  # ===========================================================================
  # 3E. WRITE ARCGIS-READY TIFF
  # ===========================================================================
  #
  # terra recognizes the GEE NaN cells as NA.
  #
  # NAflag = -9999 causes those missing cells to be physically encoded in the
  # output GeoTIFF using an explicit -9999 NoData sentinel.
  #
  # Valid zeros remain valid zeros.
  # ===========================================================================
  
  out_file <-
    file.path(
      OUTPUT_DIR,
      fname
    )
  
  
  writeRaster(
    r,
    out_file,
    overwrite = TRUE,
    datatype = "FLT4S",
    NAflag = NODATA_VALUE,
    gdal = c(
      "COMPRESS=DEFLATE",
      "TILED=YES",
      "BIGTIFF=IF_SAFER"
    )
  )
  
  
  # ===========================================================================
  # 3F. REOPEN AND QA OUTPUT
  # ===========================================================================
  
  check <-
    rast(out_file)
  
  
  # Band structure.
  
  if (nlyr(check) != 27) {
    
    stop(
      "\nOutput band-count QA failed for:\n",
      fname
    )
  }
  
  
  if (!identical(
    names(check),
    original_names
  )) {
    
    stop(
      "\nOutput band-name QA failed for:\n",
      fname
    )
  }
  
  
  # Resolution must remain exactly the original 1-km grid.
  
  if (!isTRUE(
    all.equal(
      res(check),
      original_resolution
    )
  )) {
    
    stop(
      "\nResolution changed during processing for:\n",
      fname
    )
  }
  
  
  # Grid origin must remain unchanged.
  
  if (!isTRUE(
    all.equal(
      origin(check),
      original_origin
    )
  )) {
    
    stop(
      "\nGrid origin changed during processing for:\n",
      fname
    )
  }
  
  
  # Rows are never expected to change.
  
  if (nrow(check) != original_nrow) {
    
    stop(
      "\nRaster row count changed unexpectedly for:\n",
      fname
    )
  }
  
  
  # Number of columns should change ONLY by the explicitly validated
  # western-dateline trim.
  
  expected_ncol <-
    original_ncol -
    trimmed_columns
  
  
  if (ncol(check) != expected_ncol) {
    
    stop(
      "\nUnexpected output column count for:\n",
      fname
    )
  }
  
  
  # If no dateline repair was required, extent must remain unchanged.
  
  if (
    trimmed_columns == 0 &&
    !isTRUE(
      all.equal(
        as.vector(ext(check)),
        as.vector(original_extent)
      )
    )
  ) {
    
    stop(
      "\nRaster extent changed unexpectedly for:\n",
      fname
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # GDAL metadata QA
  # ---------------------------------------------------------------------------
  #
  # Verify:
  #   - MODIS sphere radius is actually written
  #   - -9999 is registered as NoData for all 27 bands
  # ---------------------------------------------------------------------------
  
  gdal_info <-
    terra::describe(
      out_file
    )
  
  
  sphere_ok <-
    any(
      grepl(
        "6371007.181",
        gdal_info,
        fixed = TRUE
      )
    )
  
  
  nodata_band_count <-
    sum(
      grepl(
        "NoData Value=-9999",
        gdal_info,
        fixed = TRUE
      )
    )
  
  
  if (!sphere_ok) {
    
    stop(
      "\nMODIS sphere CRS was not written correctly for:\n",
      fname
    )
  }
  
  
  if (nodata_band_count != 27) {
    
    stop(
      "\nExpected -9999 NoData metadata on 27 bands but found ",
      nodata_band_count,
      " in:\n",
      fname
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Log successful processing
  # ---------------------------------------------------------------------------
  
  processing_log[[i]] <-
    data.frame(
      file = fname,
      bands = nlyr(check),
      xres_m = xres(check),
      yres_m = yres(check),
      rows = nrow(check),
      cols_before = original_ncol,
      cols_after = ncol(check),
      dateline_columns_trimmed = trimmed_columns,
      valid_cells_before_trim = valid_before_trim,
      valid_cells_after_trim = valid_after_trim,
      nodata_value = NODATA_VALUE,
      modis_radius_m = MODIS_RADIUS_M,
      stringsAsFactors = FALSE
    )
  
  
  cat(
    "SUCCESS\n"
  )
  
}


# =============================================================================
# 4. FINAL ARCHIVE QA
# =============================================================================

output_files <-
  list.files(
    OUTPUT_DIR,
    pattern = "\\.(tif|tiff)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )


if (length(output_files) != length(files)) {
  
  stop(
    "\nInput/output TIFF count mismatch.\n",
    "Input: ",
    length(files),
    "\nOutput: ",
    length(output_files)
  )
}


log_df <-
  do.call(
    rbind,
    processing_log
  )


log_file <-
  file.path(
    OUTPUT_DIR,
    "GFLC_postdownload_processing_log.csv"
  )


write.csv(
  log_df,
  log_file,
  row.names = FALSE
)


# =============================================================================
# 5. REPORT DATELINE REPAIR
# =============================================================================

trimmed <-
  log_df[
    log_df$dateline_columns_trimmed > 0,
  ]


cat("\n\n============================================================\n")
cat("GFLC POST-DOWNLOAD PROCESSING COMPLETE\n")
cat("============================================================\n")

cat("\nInput TIFFs: ", length(files), "\n")
cat("Output TIFFs:", length(output_files), "\n")

cat("\nOutput directory:\n")
cat(OUTPUT_DIR, "\n")

cat("\nProcessing log:\n")
cat(log_file, "\n")


cat("\nDateline TIFFs repaired:", nrow(trimmed), "\n")


if (nrow(trimmed) > 0) {
  
  print(
    trimmed[
      ,
      c(
        "file",
        "dateline_columns_trimmed",
        "valid_cells_before_trim",
        "valid_cells_after_trim"
      )
    ],
    row.names = FALSE
  )
  
}


cat("\nExpected for the validated production archive:\n")
cat("  2 western-dateline physical TIFFs repaired\n")
cat("  1 empty 1000-m column removed from each\n")
cat("  0 valid GFLC cells lost\n")

cat("\nAll output TIFFs:\n")
cat("  27 Float32 bands\n")
cat("  1000-m grid retained\n")
cat("  MODIS spherical Sinusoidal CRS assigned\n")
cat("  NoData = -9999 on all 27 bands\n")

cat("\nNO reprojection or resampling was performed.\n")

cat("\n============================================================\n")