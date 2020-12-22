
#' Visualize DEMs
#' @description This function takes the USGS NED layer and generates a handfull of terrain visualizations. This is good for
#' getting a quick peak into an unknown area or just masking out certain ranges, e.g. slope > 45%.
#' @param aoi A sf object
#' @param param \code{character} indicating terrain type, e.g. 'slope', 'TPI', 'sin', 'hillshade', 'complete'.
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param stats \code{logical} whether to compute stats or not, slope only.
#' @param az \code{numeric} The illumination azimuth in degrees from north. o
#' @param el \code{numeric} The illumination elevation in degrees. optional
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 10 (m) default.
#' @return
#' @export
#'

viz_Terrain <- function(aoi, param = "slope",
                        window = FALSE, w.low = NULL, w.high = NULL, stats = FALSE, az = 270, el = 45,
                        scale = 10){

  bb <-  sf::st_bbox(aoi)
  geom <- ee$Geometry$Rectangle(bb)

dem <- ee$Image("USGS/NED")

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

if(param == "slope"){
  if(!window == "TRUE"){

slope <- ee$Terrain$slope(dem)

stats <- slope$reduceRegions(
  reducer = reducers,
  collection = geom,
  scale = scale
)
if(stats == "TRUE"){
max <- stats$getInfo()$features[[1]]$properties$max
mean <- stats$getInfo()$features[[1]]$properties$mean
min <- stats$getInfo()$features[[1]]$properties$min

cat(" Max:",tan(max*base::pi/180)*100, " %", "\n", "Mean:", tan(mean*base::pi/180)*100, " %", "\n", "Min:", tan(min*base::pi/180)*100, " %")
}
cen <- as.numeric(AOI::bbox_coords({{aoi}}))
Map$setCenter(cen[1], cen[2], 7)
mlay <- Map$addLayer(slope$clip(geom), visParams = list(min = 0, max = 60))
return(mlay@map)

  } else {

    slow <- atan(w.low/100)*(180/base::pi)
    shigh <- atan(w.high/100)*(180/base::pi)

    slope <- ee$Terrain$slope(dem)

    slope_m <- slope$gt(slow)$And(slope$lt(shigh))

    slope <- slope$updateMask(slope_m)
    cen <- as.numeric(AOI::bbox_coords({{aoi}}))
    Map$setCenter(cen[1], cen[2], 7)
    mlay <- Map$addLayer(slope$clip(geom), visParams = list(min = slow, max = shigh))
    return(mlay@map)
}
}



if(param == "sin"){


 #Get the aspect (in degrees).
  aspect = ee$Terrain$aspect(dem)

  #Convert to radians, compute the sin of the aspect.

  sinImage = aspect$divide(180)$multiply(pi)$sin()

  cen <- as.numeric(AOI::bbox_coords({{aoi}}))

  Map$setCenter(cen[1], cen[2], 7)
  mlay <- Map$addLayer(sinImage$clip(geom), visParams = list(min = -1, max = 1))

  return(mlay@map)
}

if(param == "hillshade"){



hillshade = ee$Terrain$hillshade(dem, az)



cen <- as.numeric(AOI::bbox_coords({{aoi}}))

Map$setCenter(cen[1], cen[2], 7)
mlay <- Map$addLayer(hillshade$clip(geom), visParams = list(min = 0, max = 256))
return(mlay@map)
}

if(param == "TPI"){

 boxcar <- ee$Kernel$square(8, "pixels", T)

# Smooth the image by convolving with the boxcar kernel.
smooth <- dem$convolve(boxcar)

tpi <- dem$subtract(smooth)

  #Convert to radians, compute the sin of the aspect.

  cen <- as.numeric(AOI::bbox_coords({{aoi}}))


  Map$setCenter(cen[1], cen[2], 7)
  mlay <- Map$addLayer(tpi$clip(geom), visParams = list(min = -30, max = 30))
  return(mlay@map)
}


if(param == "TRI"){

  boxcar <- ee$Kernel$square(8, "pixels", T)

  # Smooth the image by convolving with the boxcar kernel.
  smooth <- dem$convolve(boxcar)

  tri <- dem$subtract(smooth)$abs()$divide(8)
  stats <- tri$reduceRegions(
    reducer = reducers,
    collection = geom,
    scale = 800
  )
  min <-stats$getInfo()$features[[1]]$properties$min
  max <- stats$getInfo()$features[[1]]$properties$max

  Map$setCenter(cen[1], cen[2], 7)
  mlay <- Map$addLayer(tri$clip(geom), visParams = list(min = 0, max = 1))
  return(mlay@map)
}

if(param == "complete"){

  stats <- dem$reduceRegions(
    reducer = reducers,
    collection = geom,
    scale = 800
  )

    max <- stats$getInfo()$features[[1]]$properties$max
    min <- stats$getInfo()$features[[1]]$properties$min


  complete <- ee$Terrain$products(dem)

  cen <- as.numeric(AOI::bbox_coords({{aoi}}))

  Map$setCenter(cen[1], cen[2], 7)

  mlay <- Map$addLayer(complete$clip(geom), visParams = list(bands = "hillshade", min = 0, max = 256), opacity = 0.45)
  mlay2 <- Map$addLayer(complete$clip(geom), visParams = list(bands = "elevation", min = min, max = max, palette = c('green','yellow','grey','red','black')), opacity = 0.45)
  mlay3 <- Map$addLayer(complete$clip(geom), visParams = list(bands = "slope", min = 0, max = 45, palette = c('white','grey','black','red','yellow')))
  return(mlay3 + mlay2 + mlay )
}

}



#' Get slope Reduced Regions
#' @description This function uses USGS NED and any sf object or USGS Watershed Boundary Dataset of Basins to
#' reduce regions to commmon statistics: mean, max, min, median, stdDev and sum for selected date ranges.
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param leaflet \code{logical} TRUE/FALSE whether to include leaflet map
#' @param crs \code{numeric}
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#'
#' @return  An sf object and leaflet map.
#' @export

rr_Terrain <- function(aoi, wb, leaflet = TRUE, crs = 4326, scale = 30) {

  if(missing(aoi)){stop("Need aoi to use this function")}
  if(missing(aoi) & missing(wb)){stop("Need at least aoi or wb with argument to use this function")}




  if(missing(wb)) {

 bb <-  sf::st_bbox(aoi)

  geom <- ee$Geometry$Rectangle(bb)

    region <- rgee::sf_as_ee(aoi)

  } else {
    bb <-  sf::st_bbox(aoi)
    geom <- ee$Geometry$Rectangle(bb)
    h_size <- paste0("USGS/WBD/2017/", wb)

    region <- ee$FeatureCollection(h_size)$filterBounds(geom)
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

  dem <- ee$Image("USGS/NED")

      slope <- ee$Terrain$slope(dem)

      stats <- slope$reduceRegions(
        reducer = reducers,
        collection = region,
        scale = scale
      )

#       reglist <- stats$getInfo()$features
#
#       region_df <- data.frame()
#
#       for(i in 1:length(reglist)) {
#
#         it_stat <- reglist[[i]]$properties %>% flatten() %>% as.data.frame()
#         it_stat <- it_stat %>% dplyr::slice(1)
#         it_geo <- reglist[[i]]$geometry %>% as.data.frame() %>%
#           dplyr::mutate(coords = c('lat', 'lon')) %>%
#           dplyr::relocate(coords) %>% dplyr::select(-type)
#
#         it_geo <-  t(it_geo)
#
#         rownames(it_geo) <- NULL
#
#         it_geo <- it_geo %>% as.data.frame() %>%
#           dplyr::slice(-1) %>% dplyr::rename(lat = 'V1', lon = 'V2')
#
# it_geo <- it_geo %>% dplyr::mutate(lat = ifelse(lat == lon, NA, lat),
#                                    lon = ifelse(lon == lat, NA, lon)) %>% stats::na.omit()
#
#
#         it_geo <- it_geo %>%
#           sf::st_as_sf(coords = c("lat", "lon"), crs = crs) %>%
#           dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
#           sf::st_cast("POLYGON")
#         it_region <- dplyr::bind_cols(it_stat, it_geo)
#
#         region_df <- plyr::rbind.fill(region_df, it_region)
#       }

      region_df <- rgee::ee_as_sf(stats)

# region_df <- region_df %>%
#   tidyr::pivot_longer(cols = starts_with("X"), names_to = "metric") %>%
#   dplyr::mutate(metric = stringr::str_sub(metric, 4)) %>%
#   tidyr::pivot_wider(names_from = metric) %>% sf::st_as_sf()

      if(leaflet == "TRUE") {
        if(class(region_df$geometry[[1]])[[2]] != "POINT") {
        comPal <- leaflet::colorNumeric("RdYlGn", region_df$mean, reverse = TRUE)
       plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                  fillOpacity = .75,
                                                  fillColor = ~comPal(mean),
                                                  popup = paste0("<b>", "Maximum Slope: ", "</b>",paste0(round(tan(region_df$max*base::pi/180)*100,3), " %"),
                                                                 "<br>", "<b>", "Minimum Slope: ", "</b>",paste0(round(tan(region_df$min*base::pi/180)*100,3), " %"),
                                                                 "<br>", "<b>", "Mean Slope: ", "</b>",paste0(round(tan(region_df$mean*base::pi/180)*100,3), " %"),
                                                                 "<br>", "<b>", "Median Slope: ", "</b>",paste0(round(tan(region_df$median*base::pi/180)*100,3), " %"),
                                                                 "<br>", "<b>", "Standard Deviation Slope: ", "</b>",paste0(round(tan(region_df$stdDev*base::pi/180)*100,3), " %"),
                                                                 "<br>",
                                                                 '<a href = "https://www.usgs.gov/core-science-systems/national-geospatial-program/national-map"> More Info </a>'))

print(plot)
      } else {

        ldPal <- leaflet::colorNumeric("RdYlGn", region_df$mean)
        plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                                       fillOpacity = .75,
                                                       fillColor = ~ldPal(mean),
                                                       popup = paste0("<b>", "Parameter: ", "</b>","Slope",
                                                                      "<br>", "<b>", "Value: ", "</b>",paste0(round(tan(region_df$min*base::pi/180)*100,3), " %"),
                                                                      "<br>",
                                                                      '<a href = "https://www.usgs.gov/core-science-systems/national-geospatial-program/national-map"> More Info </a>'))


        print(plot)
      }
}
      return(region_df)
}
