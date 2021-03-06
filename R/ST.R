# (c) 2013 Jussi Jousimo, jvj@iki.fi
# MIT licensed
#

# Interpolates spatial points to a raster object using thin plate spline regression.
rasterInterpolate <- function(xyz, templateRaster, transform=identity, inverseTransform=identity) {
  library(fields)
  library(raster)
  
  if (missing(xyz)) stop("xyz missing")
  if (missing(templateRaster)) stop("templateRaster missing")
  
  message("Interpolating data, minmax = ", min(xyz[,3], na.rm=T), " ", max(xyz[,3], na.rm=T), ", sd = ", sd(xyz[,3], na.rm=T), "...")
  
  z <- transform(xyz[,3])
  fit <- Tps(xyz[,1:2], z)
  predicted <- interpolate(templateRaster, fit)
  final <- calc(predicted, inverseTransform)
  
  message("Result raster, minmax = ", cellStats(final, min), " ", cellStats(final, max), ", sd = ", cellStats(final, sd))
  
  return(final)
}

# Interpolates a set spatial points to a set of raster objects.
multiRasterInterpolate <- function(xyzt, variables, templateRaster, transform=identity, inverseTransform=identity) {
  library(plyr)
  library(raster)
  
  if (missing(xyzt)) stop("xyzt missing")
  if (missing(variables)) stop("variables missing")  
  if (missing(templateRaster)) stop("templateRaster missing")
  
  message("Template raster:")
  print(extent(templateRaster))
  
  rasterList <- dlply(.data=xyzt, .variables=variables, .fun=function(xyz, templateRaster, transform, inverseTransform) {
    xyz <- xyz[complete.cases(xyz),]
    resultRaster <- rasterInterpolate(xyz=xyz, templateRaster=templateRaster, transform=transform, inverseTransform=inverseTransform)
    return(resultRaster)
  }, templateRaster=templateRaster, transform=transform, inverseTransform=inverseTransform, .parallel=TRUE)
  
  return(stack(rasterList))
}

