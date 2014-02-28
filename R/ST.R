# (c) 2013 Jussi Jousimo, jvj@iki.fi
# MIT licensed
#

# Interpolates spatial points to a raster object using thin plate spline regression.
rasterInterpolate <- function(xyz, templateRaster, transform=identity, inverseTransform=identity) {
  library(fields)
  library(raster)
  
  message("Interpolating data, minmax = ", min(xyz[,3], na.rm=T), " ", max(xyz[,3], na.rm=T), "...")
  
  z <- transform(xyz[,3])
  fit <- Tps(xyz[,1:2], z)
  predicted <- interpolate(templateRaster, fit)
  final <- calc(predicted, inverseTransform)
  
  message("Result raster, minmax = ", cellStats(final, min), " ", cellStats(final, max))
  
  return(final)
}

# Interpolates a set spatial points to a set of raster objects.
multiRasterInterpolate <- function(xyz, variables, templateRaster, transform=identity, inverseTransform=identity) {
  library(plyr)
  library(raster)
  
  rasterList <- dlply(.data=xyz, .variables=variables, .fun=function(xyz, templateRaster, transform, inverseTransform) {
    xyz <- xyz[complete.cases(xyz),]
    resultRaster <- rasterInterpolate(xyz, templateRaster, transform, inverseTransform)
    return(resultRaster)
  }, templateRaster=templateRaster, transform=transform, inverseTransform=inverseTransform, .parallel=F)
  
  return(stack(rasterList))
}

