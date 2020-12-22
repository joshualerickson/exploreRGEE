
#' Leaflet with USGS basemaps
#'
#' @return A leaflet map with USGS basemaps
#' @import ggplot2
#' @examples
viz_A <- function() {

  grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
           "USGS Shaded Relief", "Hydrography")
  att <- paste0("<a href='https://www.usgs.gov/'>",
                "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }

  map <- leaflet::leaflet()
  map <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                              group = grp[1], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSImageryOnly"),
                              group = grp[2], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSImageryTopo"),
                              group = grp[3], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSShadedReliefOnly"),
                              group = grp[4], attribution = att, layers = "0")

  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[5], options = opt, layers = "0")
  map <- leaflet::hideGroup(map, grp[5])
  opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map <- leaflet::addLayersControl(map, baseGroups = grp[1:4],
                                   overlayGroups = grp[5], options = opt)
  map %>%
    leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

}



