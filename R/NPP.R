

#' Visualize Net Annual NPP
#'
#' @description The Landsat Net Primary Production (NPP) CONUS dataset
#' estimates NPP using Landsat Surface Reflectance for CONUS.
#' NPP is the amount of carbon captured by plants in an ecosystem,
#' after accounting for losses due to respiration.
#'  NPP is calculated using the MOD17 algorithm (see MOD17 User Guide)
#'  with Landsat Surface Reflectance, gridMET, and the National Land Cover Database.
#'
#' @param aoi A sf object
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
viz_NPP <- function(aoi, startDate = '1986-01-01', endDate = '2019-01-01',
                        window = FALSE, w.low = NULL, w.high = NULL, scale = 250){

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
  collection = ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

  collection <- collection$filterDate(startDate, endDate)

  cat("Date range: ", "Years: ", lubridate::year(startDate),"-", lubridate::year(endDate), "\n", "\n")

  #Create a mask for the QC (clouds)


    collection = collection$map(maskcloud_npp)

  col_med <- collection$median()

  col_med <- col_med$select('annualNPP')

  cen <- as.numeric(AOI::bbox_coords({{aoi}}))
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
      scale = scale
    )

    cat("Band: ", "annualNPP", "\n")

    print(stats$getInfo()$features[[1]]$properties)

    st_max <- stats$getInfo()$features[[1]]$properties$max

    st_min <- stats$getInfo()$features[[1]]$properties$min

    mLayer <- Map$addLayer(col_med$clip(geom),
                           visParams = list(min = st_min, max = st_max, palette = c(
                             "#d73027", "#f46d43", "#fdae61",
                             "#fee08b", "#d9ef8b", "#a6d96a",
                             "#66bd63", "#1a9850"
                           )), paste0("<b>","annualNPP","</b>", "; " ,startDate, " - ", endDate))

    mLayer@map %>%
      leaflet::addWMSTiles(GetURL("USGSTopo"),
                           group = "Topo", layers = "0") %>%
      leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                           group = grp, options = opt, layers = "0") %>%
      leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                               "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                overlayGroups = c(paste0("<b>","annualNPP","</b>", "; " , startDate, " - ", endDate), grp))

  } else {


    col_m <- col_med$gt(w.low)$And(col_med$lt(w.high))

    col_med <- col_med$updateMask(col_m)

    mLayer <- Map$addLayer(col_med$clip(geom),
                           visParams = list(min = w.low, max = w.high,
                                            palette =  c(
                                              "#d73027", "#f46d43", "#fdae61",
                                              "#fee08b", "#d9ef8b", "#a6d96a",
                                              "#66bd63", "#1a9850"
                                            )),
                           paste0( "<b>","annualNPP","</b>", "; Range: ", w.low, " - ", w.high))

    mLayer@map  %>%
      leaflet::addWMSTiles(GetURL("USGSTopo"),
                           group = "Topo", layers = "0") %>%
      leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                           group = grp, options = opt, layers = "0") %>%
      leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                               "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                overlayGroups = c(paste0( "<b>","annualNPP","</b>","</b>", "; Range: ", w.low, " - ", w.high ), grp))

  }

}


#' Get Landsat Reduced Regions
#' @description This function uses NPP and any sf object or USGS Watershed Boundary Dataset of Basins to
#' reduce regions to commmon statistics: mean, max, min, median, stdDev and sum for selected date ranges.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param leaflet \code{logical} indicating to view leaflet map. default (TRUE)
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#' @param crs \code{numeric}
#' @return An sf object.
#' @export
#' @importFrom dplyr across
#'

rr_NPP <- function(aoi, wb,
                           startDate = "1986-01-01",
                           endDate = "2019-01-01",
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

    h_size <- paste0("USGS/WBD/2017/", wb)

    reg <- ee$FeatureCollection(h_size)$filterBounds(geom)

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


  collection = ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

  collection <- collection$filterDate(startDate, endDate)

  cat("Date range: ", "Years: ", lubridate::year(startDate),"-", lubridate::year(endDate))

  #Create a mask for the QC (clouds)

  collection = collection$map(maskcloud_npp)

  col_med <- collection$median()

  col_med <- col_med$select('annualNPP')
  #get stats

  stats <- col_med$reduceRegions(
    reducer = reducers,
    collection = reg,
    scale = scale
  )

  region_df <- rgee::ee_as_sf(stats)

  region_df <- region_df %>%
    dplyr::mutate(param = 'Annual NPP',year_range = paste0(stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"))) %>%
  dplyr::mutate(dplyr::across(c('max', 'mean', 'median', 'min','stdDev', 'sum'), as.numeric))

  if(leaflet == "TRUE") {

    if(class(region_df$geometry[[1]])[[2]] != "POINT") {
      ldPal <- leaflet::colorNumeric("RdYlGn", region_df$mean)
      plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                fillOpacity = .75,
                                                fillColor = ~ldPal(mean),
                                                popup = paste0("<b>", "Parameter: ", "</b>","Annual NPP",
                                                               "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*")),
                                                               "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,3),
                                                               "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,3),
                                                               "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,3),
                                                               "<br>", "<b>", "Median: ", "</b>",round(region_df$median,3),
                                                               "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,3),
                                                               "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,3),
                                                               "<br>",
                                                               '<a href = "http://www.ntsg.umt.edu/files/modis/MOD17UsersGuide2015_v3.pdf"> More Info </a>'))

      print(plot)

    } else {

      ldPal <- leaflet::colorNumeric("RdYlGn", region_df$mean)
      plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                                     fillOpacity = .75,
                                                     fillColor = ~ldPal(mean),
                                                     popup = paste0("<b>", "Parameter: ", "</b>","Annual NPP",
                                                                    "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*")),
                                                                    "<br>", "<b>", "Value: ", "</b>",round(region_df$mean,3),
                                                                    "<br>",
                                                                    '<a href = "http://www.ntsg.umt.edu/files/modis/MOD17UsersGuide2015_v3.pdf"> More Info </a>'))


      print(plot)
    }

  }

  return(region_df)

}






#' Extract Regions by Date
#' @description This function is similar to \link[exploreRGEE]{rr_NPP} but instead of reducing the region
#' for entire date range this function keeps the dates selected ('year' and 'months') and returns the median of each month.
#'  This function is good if your wanting a time series within a region(s).
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param id A \code{vector} identifying each region. \code{optional} if using \code{wb}
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 10 (m) default.
#' @param crs \code{numeric} crs code
#' @param plot \code{logical} TRUE/FALSE.
#' @param save.plot \code{logical} TRUE/FALSE whether to save plot and data.frame
#'
#' @return A \code{data.frame} and a side-effect plot; unless using \code{save.plot}.
#' @export
#'
#' @examples
band_NPP <- function(aoi, wb, id,startDate = "1986-01-01",
                         endDate = "2019-01-01",scale = 30, crs = 4326,
                         plot = TRUE, save.plot = "FALSE") {
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

  collection = ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

  collection <- collection$filterDate(startDate, endDate)

  cat("Date range: ", "Years: ", lubridate::year(startDate),"-", lubridate::year(endDate))

  #Create a mask for the QC (clouds)
  collection = collection$map(maskcloud_npp)
  collection = collection$select('annualNPP')

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

  param_name <- paste0("_annualNPP")

  proc <- proc %>% mutate(Date = stringr::str_remove(Date, "X"),
                          Date = stringr::str_remove_all(Date, param_name),
                          Date = as.numeric(Date))

  if(plot == "TRUE"){

    plot_proc <- proc %>% ggplot(aes(Date, value, color = ID)) +
      geom_line() +
      geom_smooth(alpha = 0.3) +
      geom_point() +
      theme_bw() +
      labs(title = paste0("Annnual NPP (Kg*C/m^2*yr)", " values for date range: "),
           subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*")),
           y = paste0("Annnual NPP (Kg*C/m^2*yr)", ' values')) +
      facet_wrap(~ID)

    print(plot_proc)

  }
  if(save.plot == "TRUE"){
    return(list(proc = proc, plot_proc = plot_proc))

  }
  proc

}





