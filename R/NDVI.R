

#' Visualize Landsat
#'
#' @param aoi A sf object
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
viz_Landsat <- function(aoi, param = "NDVI", startDate = '1986-04-01', endDate = '2020-10-30',
                        window = FALSE, w.low = NULL, w.high = NULL, c.low = 1, c.high = 12, scale = 250){

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

  # Bring in image collections from landsat 5,7,8
  oliCol = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterBounds(geom)
  etmCol = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterBounds(geom)
  tmCol = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$filterBounds(geom)

  colFilter = ee$Filter$lt('CLOUD_COVER', 50)$lt('GEOMETRIC_RMSE_MODEL', 10)$Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

  # Filter collections and prepare them for merging.
  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm))
  tmCol = tmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm))

  #Merge the collections.

  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

  #filter by cal range
 calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
 collection <- collection$filter(calRange)


  collection <- collection$filterDate(startDate, endDate)

  cat("Date range: ", "Years: ", lubridate::year(startDate),"-", lubridate::year(endDate), ";and Months:", c.low, " - ", c.high,"\n", "\n")


  col_med <- collection$median()

  col_med <- col_med$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'nd', 'nd_1'), c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'NDVI', 'NDWI'))

  col_med <- col_med$select(param)

  cen <- as.numeric(AOI::bbox_coords(aoi))
  Map$setCenter(cen[1], cen[2], 7)

  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }

  grp = "Hydrography"
  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)

  if(!window == TRUE){

    stats <- col_med$reduceRegions(
      reducer = reducers,
      collection = geom,
      scale = 800
    )

    cat("Band: ", param, "\n")

print(stats$getInfo()$features[[1]]$properties)

st_max <- stats$getInfo()$features[[1]]$properties$max

st_min <- stats$getInfo()$features[[1]]$properties$min

      mLayer <- Map$addLayer(col_med$select(param)$clip(geom),
                             visParams = list(min = st_min, max = st_max, palette = c(
                               "#d73027", "#f46d43", "#fdae61",
                               "#fee08b", "#d9ef8b", "#a6d96a",
                               "#66bd63", "#1a9850"
                             )), paste0("Landsat: ", param, "; " ,startDate, " - ", endDate))

      mLayer@map %>%
        leaflet::addWMSTiles(GetURL("USGSTopo"),
                             group = "Topo", layers = "0") %>%
        leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                             group = grp, options = opt, layers = "0") %>%
        leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                                 "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                  overlayGroups = c(paste0("Landsat: ", param, "; " , startDate, " - ", endDate), grp))

  } else {


      col_m <- col_med$gt(w.low)$And(col_med$lt(w.high))

      col_med <- col_med$updateMask(col_m)

      mLayer <- Map$addLayer(col_med$select(param)$clip(geom),
                             visParams = list(min = w.low, max = w.high,
                                              palette =  c(
                                                "#d73027", "#f46d43", "#fdae61",
                                                "#fee08b", "#d9ef8b", "#a6d96a",
                                                "#66bd63", "#1a9850")),
                             paste0( "<b>","Landsat: ", param,"</b>", "; Range: ", w.low, " - ", w.high))

      mLayer@map  %>%
        leaflet::addWMSTiles(GetURL("USGSTopo"),
                             group = "Topo", layers = "0") %>%
        leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                             group = grp, options = opt, layers = "0") %>%
        leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                                 "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                  overlayGroups = c(paste0( "<b>","Landsat: ", param,"</b>", "; Range: ", w.low, " - ", w.high ), grp))


  }

}




#' Get Landsat Reduced Regions
#' @description This function uses landsat and any sf object or USGS Watershed Boundary Dataset of Basins to
#' reduce regions to commmon statistics: mean, max, min, median, stdDev and sum for selected date ranges.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param leaflet \code{logical} indicating to view leaflet map. default (TRUE)
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#' @param crs \code{numeric}
#' @return An sf object and leaflet map.
#' @export
#'

rr_Landsat <- function(aoi, wb, param = 'NDVI',
                       startDate = "1984-04-01",
                       endDate = "2020-01-01",c.low = 1, c.high = 12,
                       leaflet = TRUE, scale = 250, crs = 4326){

  if(missing(aoi)){stop("Need aoi to use this function")}
  if(missing(aoi) & missing(wb)){stop("Need at least aoi or wb with argument to use this function")}

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

   bb <-  sf::st_bbox(aoi)
   geom <- ee$Geometry$Rectangle(bb)

    h_size <- paste0("USGS/WBD/2017/", wb)$filterBounds(geom)

    reg <- ee$FeatureCollection(h_size)

 }


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

  # Bring in image collections from landsat 5,7,8
  oliCol = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterBounds(geom)
  etmCol = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterBounds(geom)
  tmCol = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$filterBounds(geom)

  colFilter = ee$Filter$lt('CLOUD_COVER', 50)$lt('GEOMETRIC_RMSE_MODEL', 10)$Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

  # Filter collections and prepare them for merging.
  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(prepEtm)
  tmCol = tmCol$filter(colFilter)$map(prepEtm)

  #Merge the collections.

  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

  #filter by cal range
  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
  collection <- collection$filter(calRange)


  collection <- collection$filterDate(startDate, endDate)

  col_med <- collection$median()

  col_med <- col_med$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'nd', 'nd_1'), c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'NDVI', 'NDWI'))

col_med <- col_med$select(param)
    #get stats

    stats <- col_med$reduceRegions(
      reducer = reducers,
      collection = reg,
      scale = scale
    )

    # reglist <- stats$getInfo()$features
    #
    # region_df <- data.frame()
    #
    # for(i in 1:length(reglist)) {
    #
    #   it_stat <- reglist[[i]]$properties %>% flatten() %>% as.data.frame()
    #   it_stat <- it_stat %>% dplyr::slice(1)
    #   it_geo <- reglist[[i]]$geometry %>% as.data.frame() %>%
    #     dplyr::mutate(coords = c('lat', 'lon')) %>%
    #     dplyr::relocate(coords) %>% dplyr::select(-type)
    #
    #   it_geo <-  t(it_geo)
    #
    #   rownames(it_geo) <- NULL
    #
    #   it_geo <- it_geo %>% as.data.frame() %>%
    #     dplyr::slice(-1) %>% dplyr::rename(lat = 'V1', lon = 'V2')
    #
    #   if(reglist[[1]]$geometry$type != "Point"){
    #     it_geo <- it_geo %>%
    #       sf::st_as_sf(coords = c("lat", "lon"), crs = crs) %>%
    #       dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
    #       sf::st_cast("POLYGON")
    #   } else {
    #     it_geo <- it_geo %>%
    #       sf::st_as_sf(coords = c("lat", "lon"), crs = crs)
    #   }
    #
    #   it_region <- dplyr::bind_cols(it_stat, it_geo)
    #
    #   region_df <- plyr::rbind.fill(region_df, it_region)
    # }

    region_df <- rgee::ee_as_sf(stats)

    region_df <- sf::st_as_sf(region_df) %>%
      dplyr::mutate(param = {{param}}, month_range = paste0(c.low," - ", c.high), year_range = paste0(stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"))) %>%
      dplyr::mutate(dplyr::across(c('max', 'mean', 'median', 'min','stdDev', 'sum'), as.numeric))

    if(leaflet == "TRUE") {

      if(class(region_df$geometry[[1]])[[2]] != "POINT") {
        ldPal <- leaflet::colorNumeric("RdYlGn", region_df$mean)
        plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                  fillOpacity = .75,
                                                  fillColor = ~ldPal(mean),
                                                  popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                                 "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                                 "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,3),
                                                                 "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,3),
                                                                 "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,3),
                                                                 "<br>", "<b>", "Median: ", "</b>",round(region_df$median,3),
                                                                 "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,3),
                                                                 "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,3),
                                                                 "<br>",
                                                                 '<a href = "https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-quality-assessment?qt-science_support_page_related_con=0#qt-science_support_page_related_con"> More Info </a>'))

        print(plot)

      } else {

        ldPal <- leaflet::colorNumeric("RdYlGn", region_df$mean)
        plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                                       fillOpacity = .75,
                                                       fillColor = ~ldPal(mean),
                                                       popup = paste0("<b>", "Parameter: ", "</b>",param,
                                                                      "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                                      "<br>", "<b>", "Value: ", "</b>",round(region_df$mean,3),
                                                                      "<br>",
                                                                      '<a href = "https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-quality-assessment?qt-science_support_page_related_con=0#qt-science_support_page_related_con"> More Info </a>'))

        print(plot)
        }

    }

    return(region_df)

  }

#' Extract Regions by Date
#' @description This function is similar to \link[exploreRGEE]{rr_Landsat} but instead of reducing the region
#' for entire date range this function keeps the dates selected ('year' and 'months') and returns the median of each month.
#' This function can take a while since it's compiling every image to a band and then exporting to a data.frame.
#'  This function is good if your wanting a time series within a region(s).
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param id A \code{vector} identifying each region. \code{optional} if using \code{wb}
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#' @param crs \code{numeric} crs code
#' @param plot \code{logical} TRUE/FALSE.
#' @param save.plot \code{logical} TRUE/FALSE whether to save plot and data.frame
#' @importFrom dplyr across mutate
#' @importFrom tidyr pivot_longer
#' @note Might change this function in the future by using map() server-side to speed up computations.
#'
#' @return A \code{data.frame} and a side-effect plot; unless using \code{save.plot}.
#' @export
#'
#' @examples
band_Landsat <- function(aoi, wb, id,
                       param = 'NDVI', startDate = "1984-04-01",
                       endDate = "2020-10-01",c.low = 1, c.high = 12,
                       scale = 250, crs = 4326,
                       plot = TRUE, save.plot = FALSE) {

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

      reg <- rgee::sf_as_ee(aoi)

    } else {

      bb <-  sf::st_bbox(aoi)
      geom <- ee$Geometry$Rectangle(bb)
      aoi <- aoi %>% dplyr::mutate(across(lubridate::is.Date, as.character))
      reg <- rgee::sf_as_ee(aoi)
    }

  } else {

    bb <-  sf::st_bbox(aoi)
    geom <- ee$Geometry$Rectangle(bb)

    h_size <- paste0("USGS/WBD/2017/", wb)

    reg <- ee$FeatureCollection(h_size)$filterBounds(geom)
    feat <- reg$getInfo()$features

  }
  # Bring in image collections from landsat 5,7,8
  oliCol = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterBounds(geom)
  etmCol = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterBounds(geom)
  tmCol = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$filterBounds(geom)

  colFilter = ee$Filter$lt('CLOUD_COVER', 50)$lt('GEOMETRIC_RMSE_MODEL', 10)$Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

  # Filter collections and prepare them for merging.
  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(prepEtm)
  tmCol = tmCol$filter(colFilter)$map(prepEtm)

  #Merge the collections.

  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

#filter by cal range
  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
  collection <- collection$filter(calRange)
collection <- collection$filterDate(startDate, endDate)

  collection <- collection$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'nd', 'nd_1'), c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'NDVI', 'NDWI'))

  collection <- collection$select(param)

  tB <- collection$toBands()

  ld_tb <- rgee::ee_extract(tB, reg, fun = ee$Reducer$median(), scale = scale)

  if(missing(wb)){

    ld_tb <- ld_tb %>% dplyr::mutate(ID = {{ id }})

  } else {
    vect_prtib <- vector()
    for(i in 1:length(feat)){

      prtib <- feat[[i]]$properties$name

      vect_prtib <- append(prtib, vect_prtib)

    }

    ld_tb <- ld_tb %>% mutate(ID = vect_prtib)
  }
  proc <- ld_tb %>% pivot_longer(-ID, names_to = "Date")

  param_name <- paste0("_",param)

  proc <- proc %>% mutate(Date = stringr::str_sub(Date, start = 18),
                          Date = stringr::str_remove_all(Date, param_name),
                          Date = lubridate::as_date(Date))
  proc <- proc %>%
    mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(ID, Date) %>% dplyr::summarise(value = mean(value, na.rm = TRUE))

  if(plot == "TRUE"){

    plot_proc <- proc %>% ggplot(aes(Date, value, color = ID)) +
      geom_line() +
      geom_smooth(alpha = 0.3) +
      geom_point() +
      theme_bw() +
      labs(title = paste0(param, " values for date range: "),
           subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
           y = paste0(param, ' values')) +
      facet_wrap(~ID)

    print(plot_proc)

  }
  if(save.plot == "TRUE"){
    return(list(proc = proc, plot_proc = plot_proc))

  }
  proc

}



