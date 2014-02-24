# (c) 2014 Jussi Jousimo, jvj@iki.fi
# MIT licensed
#
# Downloads 1 km x 1 km population grid over Finland from Statistics Finland with R.
# Contains total population, males, females, 0-14, 15-64 and >65 years old. If there
# are less than 10 people on a square, only total population is returned. Note that
# it takes a while to download and process the data. Once completed, you may want to
# save the data on your local disk for later access by specifying the 'outputFile'
# argument. If you want to transform the coordinate system, give 'crs' argument.
# Currently, years 2005 and 2010-2012 are available and the year is specified with
# the 'year' argument. Year is matched with the closest available one. The result is
# returned as a raster object. Zero population and no-data cells are currently both
# marked with NA.
#
# Lataa Suomen väestömäärän 1 km x 1 km -ruudukkona Tilastokeskuksesta.
# Lisätietoja: http://www.paikkatietohakemisto.fi/catalogue/ui/metadata.html?lang=fi&metadataresourceuuid=a901d40a-8a6b-4678-814c-79d2e2ab130c

queryStatFiPopulationGrid <- function(year, outputFile, crs) {
  library(sp)
  library(rgdal)
  library(raster)
  
  # Find data for the closest available year
  yearsAvailable <- c(2005, 2010, 2011, 2012) # Needs to be updated when more years become available
  diff <- abs(year - yearsAvailable)
  closestYear <- yearsAvailable[which.min(diff)]
  if (!any(diff == 0))
    warning("No data available for year ", year, ". Using the closest year ", closestYear, ".")
  
  # Download the shapefile
  query <- paste("http://www.stat.fi/tup/rajapintapalvelut/vaki", closestYear, "_1km.zip", sep="")
  baseDir <- tempdir()
  fileName <- file.path(baseDir, paste("statfi", year, sep=""))
  
  # Cache files for each year
  if (!file.exists(fileName)) {
    success <- download.file(query, fileName, "internal")
    if (success!=0) stop("Query failed.")
    unzip(fileName, exdir=baseDir)
  }
  
  x <- readOGR(baseDir, paste("vaki", closestYear, "_1km", sep=""))  
  x$GRD_ID <- NULL # Remove useless(?) variables
  x$ID_NRO <- NULL
  x$XKOORD <- NULL
  x$YKOORD <- NULL
  x@data[x@data<0] <- NA # Set missing values  
  y <- SpatialPixelsDataFrame(coordinates(x), x@data, proj4string=x@proj4string)
  if (missing(outputFile)) outputFile <- tempfile()
  z <- brick(y) # Convert sp object to raster object
  z <- writeRaster(z, filename=outputFile)
  if (!missing(crs)) # Project raster if requested
    z <- projectRaster(z, crs=crs, filename=outputFile, overwrite=T)
  
  return(z)
}

# Get human population density on a 1 km x 1 km area at given points 'xy' in year 'year'
getStatFiPopulationDensity <- function(xy, year, variable="VAESTO") {
  populationGrid <- queryStatFiPopulationGrid(year)
  xy.trans <- spTransform(xy, CRS(proj4string(populationGrid)))
  population <- extract(populationGrid[[variable]], xy.trans)
  
  # As zero population cells are marked with NA, set them zero.
  # Does not check if the coordinates are outside Finland where it would be more appropriate to return NA.
  population[is.na(population)] <- 0
  population <- round(population)
  
  return(population)
}
