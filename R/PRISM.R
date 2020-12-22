
#' Visualize PRISM data from GEE
#'
#' @description This function takes a lot of arguments but in the end is very efficient to view
#' PRISM data. At a minimum, just providing a sf object will generate the Norm81m grid
#' and print out numerous statistics, e.g. mean, max, min, stdDev and sum. Grids are generated using PRISM (Parameter-elevation Regressions on Independent Slopes Model) and
#' Google Earth Engine via the \link[rgee]{rgee-package}. This makes it fast to explore these image collections
#' in R alongside your data analysis.
#'
#' @param aoi A sf object.
#' @param method \code{character}. 'Norm81m'(default), 'AN81m', 'AN81d'.
#' @param param \code{character}. 'ppt' (default), tmin, tmax, tmean, tdmean (deg C); vpdmin, vpdmax (hPa)
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#'
#' @note It's important to note that you need a GEE account and also this function depends
#' on \link[rgee]{rgee-package}. The time parameter options 'AN81m' and 'AN81d' can take a long time to run unless the date range is
#' changed. So be aware of time when running these for
#' large areas, e.g. gt 50 sq.miles can take up to 30 seconds to process.
#'
#' @return A leaflet map with selected parameters
#' @import rgee
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples

viz_PRISM <- function(aoi, method = "Norm81m", param = 'ppt', startDate = "1981-01-01", endDate = "2010-01-01",
                      window = FALSE, w.low = NULL, w.high = NULL, c.low = 1, c.high = 12){

  if(method == "Norm81m") {

    startDate = "1981-01-01"
    endDate = "2010-01-01"
  }

  bb <-  sf::st_bbox(aoi)
  geom <- ee$Geometry$Rectangle(bb)

  reducers <-  ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$min(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$max(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$sum(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$median(),
    sharedInputs = TRUE
  )

  coll_name <- paste0("OREGONSTATE/PRISM/",method)
  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

    prism <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate, endDate)
    prism <- prism$filter(colFilter)

range <- prism$reduceColumns(
      ee$Reducer$minMax(),
      list("system:time_start")
    )

    col_min <- eedate_to_rdate(range$get("min"))
    col_max <- eedate_to_rdate(range$get("max"))

    if(method == "Norm81m") {
      cat("Year range is always 1981-2010 for 'Norm81m'; however, months selected are: ",
          stringr::str_remove(as.character(col_min), "1981-")," to ",
          stringr::str_remove(as.character(col_max), "1981-"), "\n", "\n")

      } else {

    cat("Date range: ", "Years: ", lubridate::year(as.character(col_min)),"-", lubridate::year(as.character(col_max)), ";and Months:",
        lubridate::month(as.character(col_min)),"-", lubridate::month(as.character(col_max)),"\n", "\n")
}
    prism <- prism$median()

    if(window == TRUE) {

      prism_m <- prism$gt(w.low)$And(prism$lt(w.high))

      prism <- prism$updateMask(prism_m)
    }




cen <- as.numeric(AOI::bbox_coords({{aoi}}))
Map$setCenter(cen[1], cen[2], 7)

 #mapping stuff

GetURL <- function(service, host = "basemap.nationalmap.gov") {
      sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
    }

    grp = "Hydrography"
    opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)

  if(!window == TRUE){

    stats <- prism$reduceRegions(
      reducer = reducers,
      collection = geom,
      scale = 800
    )

    print(stats$getInfo()$features[[1]]$properties)
    min <-stats$getInfo()$features[[1]]$properties$min
    max <- stats$getInfo()$features[[1]]$properties$max


    if(param == 'ppt'){

      mLayer <- Map$addLayer(prism$clip(geom),
                   visParams = list(min = min, max = max, palette = c('black','red',
                                                                      'orange', 'white',
                                                                      'green', 'forestgreen',
                                                                      'blue')), paste0("PRISM: ", method, "; " ,startDate, " - ", endDate))

      mLayer@map %>%
        leaflet::addWMSTiles(GetURL("USGSTopo"),
                             group = "Topo", layers = "0") %>%
      leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                           group = grp, options = opt, layers = "0") %>%
      leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                               "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                overlayGroups = c(paste0("PRISM: ", method, "; " , startDate, " - ", endDate),grp))



    } else {


      mLayer <- Map$addLayer(prism$clip(geom),
                   visParams = list(min = min, max = max, palette = rev(c('black','red',
                                                                          'orange', 'white',
                                                                          'green', 'forestgreen',
                                                                          'blue'))), paste0("PRISM: ", method, "; " , startDate, " - ", endDate))

      mLayer@map %>%
        leaflet::addWMSTiles(GetURL("USGSTopo"),
                             group = "Topo", layers = "0") %>%
        leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                             group = grp, options = opt, layers = "0") %>%
        leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                                 "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                  overlayGroups = c(paste0("PRISM: ", method, "; " , startDate, " - ", endDate), grp))


    }
  } else {

    if(param == 'ppt'){




      mLayer <- Map$addLayer(prism$clip(geom),
                   visParams = list(min = w.low, max = w.high,
                                    palette = c('black','red',
                                                'orange', 'white',
                                                'green', 'forestgreen',
                                                'blue')),
                   paste0( "<b>","PRISM: ", method,"</b>", "; Range: ", w.low, " (mm) - ", w.high, " (mm)" ))

      mLayer@map  %>%
        leaflet::addWMSTiles(GetURL("USGSTopo"),
                             group = "Topo", layers = "0") %>%
        leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                             group = grp, options = opt, layers = "0") %>%
        leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                                 "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                  overlayGroups = c(paste0( "<b>","PRISM: ", method,"</b>", "; Range: ", w.low, " (mm) - ", w.high, " (mm)" ), grp))

       } else {


         mLayer <- Map$addLayer(prism$clip(geom),
                   visParams = list(min = w.low, max = w.high,
                                    palette = rev(c('black','red',
                                                    'orange', 'white',
                                                    'green', 'forestgreen',
                                                    'blue'))),
                   paste0( "<b>","PRISM: ", method,"</b>", "; Range: ", w.low, " (mm) - ", w.high, " (mm)" ))


         mLayer@map    %>%
           leaflet::addWMSTiles(GetURL("USGSTopo"),
                                group = "Topo", layers = "0") %>%
           leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                                group = grp, options = opt, layers = "0") %>%
           leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                                    "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                     overlayGroups = c(paste0( "<b>","PRISM: ", method,"</b>", "; Range: ", w.low, " (mm) - ", w.high, " (mm)" ), grp))

    }

  }

}


#' Get PRISM Reduced Regions
#' @description This function uses PRISM and the USGS Watershed Boundary Dataset of Basins to
#' reduce regions (wb) to commmon statistics, e.g. mean, max, min, median, stdDev and sum.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param method \code{character}. 'Norm81m'(default), 'AN81m', 'AN81d'.
#' @param param \code{character}. 'ppt' (default), tmin, tmax, tmean, tdmean (deg C); vpdmin, vpdmax (hPa)
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param crs \code{numeric} crs code
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param leaflet \code{logical} indicating to view leaflet map. default (TRUE)
#'
#' @return An sf object.

#'

rr_PRISMwb <- function(aoi, wb, method = "Norm81m",
                     param = 'ppt', startDate = "1981-01-01",
                     endDate = "2010-01-01",c.low = 1, c.high = 12, crs = 4326,
                     scale = 800, leaflet = TRUE) {

  if(method == "Norm81m") {

    startDate = "1981-01-01"
    endDate = "2010-01-01"
  }

if(missing(wb)){stop("Need watershed boundary")}

    h_size <- paste0("USGS/WBD/2017/", wb)

     huc <- ee$FeatureCollection(h_size)

  bb <-  sf::st_bbox(aoi)
  geom <- ee$Geometry$Rectangle(bb)

  huc_c <- huc$filterBounds(geom)
  coll_name <- paste0("OREGONSTATE/PRISM/", method)
  prism <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate, endDate)

  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

  prism <- prism$filter(colFilter)
  reducers <-  ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$min(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$max(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$sum(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$median(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )

  # Create ImageCollection using prism

  if(method == "Norm81m"){


    prism <- ee$ImageCollection("OREGONSTATE/PRISM/Norm81m")$select(param)$filterBounds(geom)$median()

  } else if (method == "AN81m") {

    prism <- ee$ImageCollection("OREGONSTATE/PRISM/AN81m")
    prism <- prism$filterDate(startDate, endDate)$select(param)$filterBounds(geom)$median()

  } else if (method == "AN81d") {

    prism <- ee$ImageCollection("OREGONSTATE/PRISM/AN81d")
    prism <- prism$filterDate(startDate, endDate)$select(param)$filterBounds(geom)$median()

  }

  stats <- prism$reduceRegions(
    reducer = reducers,
    collection = huc_c,
    scale = scale
  )

  # huclist <- stats$getInfo()$features
  #
  # region_df <- data.frame()
  #
  # for(i in 1:length(huclist)) {
  #
  #   it_stat <- huclist[[i]]$properties %>% flatten() %>% as.data.frame()
  #   it_geo <- huclist[[i]]$geometry %>% as.data.frame() %>%
  #     dplyr::mutate(coords = c('lat', 'lon')) %>%
  #     dplyr::relocate(coords) %>% dplyr::select(-type)
  #
  #   it_geo <-  t(it_geo)
  #
  #   rownames(it_geo) <- NULL
  #
  #   it_geo <- it_geo %>% as.data.frame() %>% dplyr::slice(-1) %>% dplyr::rename(lat = 'V1', lon = 'V2')
  #
  #   it_geo <- it_geo %>%
  #     sf::st_as_sf(coords = c("lat", "lon"), crs = crs) %>%
  #     dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  #     sf::st_cast("POLYGON")
  #
  #   it_region <- dplyr::bind_cols(it_stat, it_geo)
  #
  #   region_df <- plyr::rbind.fill(region_df, it_region)
  # }


  region_df <- rgee::ee_as_sf(stats)

  if(leaflet == TRUE) {
if(param == "ppt"){
    prismPal <- leaflet::colorNumeric("RdBu", region_df$mean)
    plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                 fillOpacity = .75,
                                                 fillColor = ~prismPal(mean),
                                                 popup = paste0("<b>","HUC Name: ","</b>", region_df$name,
                                                                "<br>", "<b>", "Parameter: ", "</b>",param,
                                                                "<br>", "<b>", "Method: ", "</b>",method,
                                                                "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                                "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,1),
                                                                "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,1),
                                                                "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,1),
                                                                "<br>", "<b>", "Median: ", "</b>",round(region_df$median,1),
                                                                "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,1),
                                                                "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,1),
                                                                "<br>",
                                                                '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

    print(plot)
} else {

  prismPal <- leaflet::colorNumeric("RdBu", region_df$mean, reverse = TRUE)
  plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                            fillOpacity = .75,
                                            fillColor = ~prismPal(mean),
                                            popup = paste0("<b>","HUC Name: ","</b>", region_df$name,
                                                           "<br>", "<b>", "Parameter: ", "</b>",param,
                                                           "<br>", "<b>", "Method: ", "</b>",method,
                                                           "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                           "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,1),
                                                           "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,1),
                                                           "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,1),
                                                           "<br>", "<b>", "Median: ", "</b>",round(region_df$median,1),
                                                           "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,1),
                                                           "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,1),
                                                           "<br>",
                                                           '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

  print(plot)
}
  }

  return(region_df)

}



#' Get PRISM Reduced Regions
#' @description This function uses PRISM and any sf object or USGS Watershed Boundary Dataset of Basins feature collection to
#' reduce regions to commmon statistics: mean, max, min, median, stdDev and sum for selected date ranges.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param method \code{character}. 'Norm81m'(default), 'AN81m', 'AN81d'.
#' @param param \code{character}. 'ppt' (default), tmin, tmax, tmean, tdmean (deg C); vpdmin, vpdmax (hPa)
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param crs \code{numeric} crs code
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param leaflet \code{logical} indicating to view leaflet map. default (TRUE)
#' @importFrom dplyr across
#' @importFrom stringr str_remove
#' @return An sf object.
#' @export
#'

rr_PRISM <- function(aoi, wb, method = "Norm81m",
                     param = 'ppt', startDate = "1981-01-01",
                     endDate = "2010-01-01",c.low = 1, c.high = 12, crs = 4326,
                     scale = 800, leaflet = TRUE){

  if(missing(aoi)){stop("Need aoi to use this function")}
  if(missing(aoi) & missing(wb)){stop("Need at least aoi or wb with argument to use this function")}

  if(method == "Norm81m") {

    startDate = "1981-01-01"
    endDate = "2010-01-01"
  }

  if(missing(wb)) {

    if(is.atomic(aoi)) {

      clat <- aoi[[1]]
      clng <- aoi[[2]]
      aoi <- data.frame(clat = clat, clng = clng)
      aoi <- sf::st_as_sf(aoi, coords = c("clat", "clng")) %>% sf::st_set_crs(crs)

      bb <-  sf::st_bbox(aoi)
      geom <- ee$Geometry$Rectangle(bb)

      reg <- rgee::sf_as_ee(aoi)

    } else {

      bb <-  sf::st_bbox(aoi)
      geom <- ee$Geometry$Rectangle(bb)
      aoi <- aoi %>% dplyr::mutate(across(lubridate::is.Date, as.character))
      reg <- rgee::sf_as_ee(aoi)
    }
  } else {

   region_df <- rr_PRISMwb(aoi = {{aoi}}, wb = {{wb}}, method = {{method}},
               param = {{param}}, startDate = {{startDate}},
               endDate = {{endDate}},c.low = {{c.low}}, c.high = {{c.high}}, leaflet = {{leaflet}}, crs = {{ crs }})
  }

  if(missing(wb)){
  coll_name <- paste0("OREGONSTATE/PRISM/", method)
  prism <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate, endDate)

  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

  prism <- prism$filter(colFilter)

  prism <- prism$median()

  reducers <-  ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$min(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$max(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$sum(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$median(),
    sharedInputs = TRUE
  )$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )

if(class(sf::st_geometry(aoi)[[1]])[[2]] == "POINT") {

  stats <- prism$reduceRegions(
    reducer = ee$Reducer$mean(),
    collection = reg,
    scale = scale
  )
} else {

  stats <- prism$reduceRegions(
    reducer = reducers,
    collection = reg,
    scale = scale
  )
}
#   reglist <- stats$getInfo()$features
#
#   region_df <- data.frame()
#
#   for(i in 1:length(reglist)) {
#
#     it_stat <- reglist[[i]]$properties %>% flatten() %>% as.data.frame()
#     it_stat <- it_stat %>% dplyr::slice(1)
#     it_geo <- reglist[[i]]$geometry %>% as.data.frame() %>%
#       dplyr::mutate(coords = c('lat', 'lon')) %>%
#       dplyr::relocate(coords) %>% dplyr::select(-type)
#
#     it_geo <-  t(it_geo)
#
#     rownames(it_geo) <- NULL
#
#     it_geo <- it_geo %>% as.data.frame() %>%
#       dplyr::slice(-1) %>% dplyr::rename(lat = 'V1', lon = 'V2')
#
#
# if(reglist[[1]]$geometry$type != "Point"){
#     it_geo <- it_geo %>%
#       sf::st_as_sf(coords = c("lat", "lon"), crs = crs) %>%
#       dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
#       sf::st_cast("POLYGON")
# } else {
#     it_geo <- it_geo %>%
#       sf::st_as_sf(coords = c("lat", "lon"), crs = crs)
# }
#
#     it_region <- dplyr::bind_cols(it_stat, it_geo)
#
#     region_df <- plyr::rbind.fill(region_df, it_region)
#   }
#

  region_df <- rgee::ee_as_sf(stats) %>%
    dplyr::mutate(dplyr::across(c('max', 'mean', 'median', 'min','stdDev', 'sum'), as.numeric))

  if(leaflet == TRUE) {
if(class(region_df$geometry[[1]])[[2]] != "POINT") {
    if(param == "ppt"){
      prismPal <- leaflet::colorNumeric("RdBu", region_df$mean)
      plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                fillOpacity = .75,
                                                fillColor = ~prismPal(mean),
                                                popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                               "<br>", "<b>", "Method: ", "</b>",method,
                                                               "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                               "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,1),
                                                               "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,1),
                                                               "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,1),
                                                               "<br>", "<b>", "Median: ", "</b>",round(region_df$median,1),
                                                               "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,1),
                                                               "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,1),
                                                               "<br>",
                                                               '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

      print(plot)
    } else {

      prismPal <- leaflet::colorNumeric("RdBu", region_df$mean, reverse = TRUE)
      plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                fillOpacity = .75,
                                                fillColor = ~prismPal(mean),
                                                popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                               "<br>", "<b>", "Method: ", "</b>",method,
                                                               "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                               "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,1),
                                                               "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,1),
                                                               "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,1),
                                                               "<br>", "<b>", "Median: ", "</b>",round(region_df$median,1),
                                                               "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,1),
                                                               "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,1),
                                                               "<br>",
                                                               '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

      print(plot)
    }
} else {


  if(param == "ppt"){
    prismPal <- leaflet::colorNumeric("RdBu", region_df$mean)
    plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                              fillOpacity = .75,
                                              fillColor = ~prismPal(mean),
                                              popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                             "<br>", "<b>", "Method: ", "</b>",method,
                                                             "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                             "<br>", "<b>", "Value: ", "</b>",round(region_df$mean,1),
                                                             "<br>",
                                                             '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

    print(plot)
  } else {

    prismPal <- leaflet::colorNumeric("RdBu", region_df$mean, reverse = TRUE)
    plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                              fillOpacity = .75,
                                              fillColor = ~prismPal(mean),
                                              popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                             "<br>", "<b>", "Method: ", "</b>",method,
                                                             "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                             "<br>", "<b>", "Value: ", "</b>",round(region_df$mean,1),
                                                             "<br>",
                                                             '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'))

    print(plot)
  }


  }



}
  return(region_df)

  }
  return(region_df)
}






#' Extract Regions by Date
#' @description This function is similar to \link[exploreRGEE]{rr_PRISM} but instead of reducing the region
#' post date range (only returning one statistic value) this function keeps the dates selected, e.g. 'months' and returns the median of each month. It uses only the 'AN81m' or 'AN81d' image
#' collections for processing. This function is good if your wanting a time series within a region.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param id A \code{vector} indentifying each region.
#' @param method \code{character}. 'AN81m'(default), 'AN81d'.
#' @param param \code{character}. 'ppt' (default), tmin, tmax, tmean, tdmean (deg C); vpdmin, vpdmax (hPa)
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param plot \code{logical} TRUE/FALSE.
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param crs \code{numeric} crs value
#' @note Might change this function in the future by using map() server-side to speed up computations. Better with points or centroids of polygons.
#' @return A \code{data.frame} and a side-effect plot; unless using \code{save.plot}.
#' @export
#'
#' @examples

band_PRISM <- function(aoi,id,  wb,  method = "AN81m",
                       param = 'ppt', startDate = "1981-01-01",
                       endDate = "2010-01-01",c.low = 1, c.high = 12,
                       plot = TRUE, save.plot = "FALSE", scale = 800,
                       crs = 4326) {

  if(missing(aoi)){stop("Need aoi to use this function")}
if(!missing(aoi) & missing(id) & missing(wb)){stop("Need at least id or wb with argument to use this function")}
  if(!missing(id) & !missing(wb)){stop("Can't have both id and wb set.")}


  if(missing(wb)) {
    if(is.atomic(aoi)) {

      clat <- aoi[[1]]
      clng <- aoi[[2]]
      aoi <- data.frame(clat = clat, clng = clng)
      aoi <- sf::st_as_sf(aoi, coords = c("clat", "clng")) %>% sf::st_set_crs(crs)

      bb <-  sf::st_bbox(aoi)
      geom <- ee$Geometry$Rectangle(bb)

      region <- rgee::sf_as_ee(aoi)

    } else {
      aoi <- aoi %>% dplyr::mutate(across(lubridate::is.Date, as.character))

      bb <-  sf::st_bbox(aoi)
      geom <- ee$Geometry$Rectangle(bb)

      region <- rgee::sf_as_ee(aoi)

    }

  } else {

    bb <-  sf::st_bbox(aoi)
    geom <- ee$Geometry$Rectangle(bb)

    h_size <- paste0("USGS/WBD/2017/", wb)

    region <- ee$FeatureCollection(h_size)$filterBounds(geom)
    feat <- reg$getInfo()$features

}
  coll_name <- paste0("OREGONSTATE/PRISM/", method)
  prism <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate, endDate)

  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

  prism <- prism$filter(colFilter)

  tB <- prism$toBands()

  prism_tb <- rgee::ee_extract(tB, region, fun = ee$Reducer$median(), scale = scale)

  if(missing(wb)){

  prism_tb <- prism_tb %>% dplyr::mutate(ID = {{ id }})

  } else {
    vect_prtib <- vector()
  for(i in 1:length(feat)){

    prtib <- feat[[i]]$properties$name

    vect_prtib <- append(prtib, vect_prtib)

  }

    prism_tb <- prism_tb %>% mutate(ID = vect_prtib)
}
  proc <- prism_tb %>% pivot_longer(-ID, names_to = "Date")

  param_name <- paste0("_",param)
if(method == "AN81m"){
  proc <- proc %>% mutate(Date = stringr::str_remove_all(Date, "X"),
                          Date = stringr::str_remove_all(Date, param_name),
                          Date = stringr::str_replace(Date,"(\\d{4})", "\\1-"),
                          Date = paste0(Date, "-01"),
                          Date = lubridate::as_date(Date))

} else if (method == "AN81d") {

  proc <- proc %>% mutate(Date = stringr::str_remove_all(Date, "X"),
                          Date = stringr::str_remove_all(Date, param_name),
                          Date = stringr::str_replace(Date,"(\\d{4})", "\\1-"),
                          Date = lubridate::as_date(Date))

}

  if(plot == "TRUE"){

  plot_proc <- proc %>% ggplot(aes(Date, value, color = ID)) +
    geom_line() +
    geom_smooth(alpha = 0.3) +
    theme_bw() +
    labs(title = paste0(method, " ", param, " values for date range: "),
         subtitle = paste0("Years: ",str_remove(startDate,"(-).*"), " - ", str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
         y = paste0(param, ' values')) +
    facet_wrap(~ID)

  print(plot_proc)

    }
if(save.plot == "TRUE"){
    return(list(proc = proc, plot_proc = plot_proc))

  }
  proc

}
