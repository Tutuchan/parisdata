#' \code{getPolygons} retrieve the boundaries for towns and returns a 
#' SpatialPolygonsDataFrame.
#' 
#' @param lCommunes a named list of numerics (the INSEE codes for each town).
#' The names of the list are used if union is TRUE to give a name to the
#' grouped towns.
#' @param union a logical. If TRUE, all towns of each group will be melted 
#' together.
#' @param quiet a logical. If FALSE, most inner functions will report progress.
#' @param cleanup a logical. If TRUE, temporary files will be deleted.
#' @param outDir a character. The path to the output directory.

getPolygons = function(lCommunes = NULL, union = FALSE, quiet = TRUE, cleanup = FALSE, outDir = "communes/"){
  if (is.null(lCommunes)) stop("'lCommunes' must be a numeric vector or a named list of numerics")
  if ("list" %in% class(lCommunes) & is.null(names(lCommunes))) stop("'lCommunes' must be a numeric vector or a named list")
  if (!dir.exists(outDir)) dir.create(outDir)
  
  if (!union) vecIDs <- numeric(length(lCommunes))
  if (!"list" %in% class(lCommunes)) lCommunes <- list(aux = lCommunes)
  
  lPolygons <- lapply(1:length(lCommunes), function(i) {
    communes <- lCommunes[[i]]
    communesP <- paste(communes, collapse = "+OR+")
    fullURL <- paste0("http://data.iledefrance.fr/explore/dataset/les-communes-generalisees-dile-de-france/download/?format=shp&q=",
                      communesP, "&rows=", length(communes))
    auxDir <- paste0(outDir, names(lCommunes)[i])
    auxZipFile <- paste0(auxDir, ".zip")
    download.file(fullURL, auxZipFile, quiet = quiet)
    unzip(auxZipFile, exdir = auxDir)
    unlink(auxZipFile)
    polygon <- rgdal::readOGR(paste0(auxDir, "/les-communes-generalisees-dile-de-france.shp"), "les-communes-generalisees-dile-de-france", verbose = !quiet)
    if (union) {
      uPolygon <- rgeos::gUnaryUnion(polygon)
      res <- sp::SpatialPolygonsDataFrame(uPolygon, data = data.frame(name = names(lCommunes)[i], stringsAsFactors = FALSE))
      res@polygons[[1]]@ID <- as.character(i)
    } else {
      vecIDs[i] <<- nrow(polygon@data)
      if (i > 1) {
        vecIDs[i] <<- vecIDs[i] + vecIDs[i-1]
        rownames <- as.character(vecIDs[i-1]:(vecIDs[i]-1))
        row.names(polygon@data) = rownames
        for (j in 1:length(polygon@polygons)) polygon@polygons[[j]]@ID <- rownames[j]
      }
      res <- polygon
    }
    if (cleanup) unlink(auxDir, recursive = TRUE, force = TRUE)
    res
  })
  totalPolygon <- Reduce(maptools::spRbind, lPolygons)
  polygonWGS <- sp::spTransform(totalPolygon, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))  
  outFile <- paste0(outDir, "polygon.kml")
  rgdal::writeOGR(polygonWGS, dsn=outFile, layer="polygonWGS", driver="KML")
  cat(paste(".kml file is in ", outFile))
  polygonWGS
}