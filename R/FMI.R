# (c) 2013 Jussi Jousimo, jvj@iki.fi
# MIT licensed
#
# Functions to query and parse the Finnish Meteorological Institute's open access
# data. For more information about queries and data, see
# http://en.ilmatieteenlaitos.fi/open-data-manual.
#
# Kyselyfunktio Suomen ilmatieteen laitoksen avoimeen dataan sekä funktio
# säähavaintoasema-aineiston parsimiseksi. Lisätietoja kyselyistä ja
# aineistoista: https://ilmatieteenlaitos.fi/avoin-data.

# Executes stored queries from the FMI open data service. Saves the XML response to a file.
# See http://en.ilmatieteenlaitos.fi/open-data-manual for more information of the queries.
queryFMIData <- function(apiKey, queryStored, parameters) {
  x <- lapply(seq_along(parameters), function(i) paste(names(parameters)[[i]], parameters[[i]], sep="="))
  queryParameters <- paste(x, collapse="&")
  query <- paste("http://data.fmi.fi/fmi-apikey/", apiKey, "/wfs?request=getFeature&storedquery_id=", queryStored, "&", queryParameters, sep="")
  fileName <- tempfile()
  success <- download.file(query, fileName, "internal")
  if (success != 0) stop("Query failed.")
  return(fileName)
}

# Parses result of the query fmi::observations::weather::daily::multipointcoverage
# in SAX fashion. Do not add "parameters" parameter in the query or the parsing would fail.
# Also, query EPSG should be 4326.
parseFMIWeatherStationMultipointCoverage <- function(fileName, proj4string=CRS("+init=epsg:4326")) {
  library(XML)
  library(sp)
  library(rgdal)
  
  positionsTag <- F
  weatherTag <- F
  positions <- ""
  weather <- ""
  
  startElement <- function(tag, attrs) {
    if (tag == "gmlcov:positions") positionsTag <<- T
    else if (tag == "gml:doubleOrNilReasonTupleList") weatherTag <<- T
  }
  
  text <- function(content) {
    if (positionsTag) positions <<- paste(positions, content, sep="")
    else if (weatherTag) weather <<- paste(weather, content, sep="")
  }
  
  endElement <- function(tag) {
    if (tag == "gmlcov:positions") positionsTag <<- F
    else if (tag == "gml:doubleOrNilReasonTupleList") weatherTag <<- F
  }
  
  handlers <- list(startElement=startElement, text=text, endElement=endElement)
  xmlEventParse(fileName, handlers, useTagName=FALSE, addContext=FALSE, trim=FALSE)
  
  str <- strsplit(positions, "\\s+", fixed=F)[[1]]
  str <- str[nchar(str) > 0]
  y <- as.numeric(str[seq(1, length(str), by=3)])
  x <- as.numeric(str[seq(2, length(str), by=3)])
  epoch <- as.integer(str[seq(3, length(str), by=3)])
  date <- as.POSIXct(epoch, origin = "1970-01-01")
  
  str <- strsplit(weather, "\\s+", fixed=F)[[1]]
  str <- str[nchar(str) > 0]
  numVariables <- 5
  
  rrday <- str[seq(1, length(str), by=numVariables)]
  rrday <- as.numeric(rrday)
  rrday[rrday < 0] <- 0
  rrday[is.nan(rrday)] <- NA
  
  tday <- str[seq(2, length(str), by=numVariables)]
  tday <- as.numeric(tday)
  tday[is.nan(tday)] <- NA
  
  snow <- str[seq(3, length(str), by=numVariables)]
  snow <- as.numeric(snow)
  snow[snow < 0] <- 0
  snow[is.nan(snow)] <- NA

  tmin <- str[seq(4, length(str), by=numVariables)]
  tmin <- as.numeric(tmin)
  tmin[is.nan(tmin)] <- NA
  
  tmax <- str[seq(5, length(str), by=numVariables)]
  tmax <- as.numeric(tmax)
  tmax[is.nan(tmax)] <- NA
  
  weather <- SpatialPointsDataFrame(coords=cbind(x, y),
                                    data=data.frame(date=date, rrday=rrday, tday=tday, snow=snow, tmin=tmin, tmax=tmax),
                                    proj4string=CRS("+init=epsg:4326"))
  weather <- spTransform(weather, proj4string)
  
  return(weather)
}
