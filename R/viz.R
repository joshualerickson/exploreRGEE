#' Viz-ualize GEE Image/ImageCollections
#' @description This function allows users to quickly view a previously created get_*() object or ee.image.Image.
#' @param x A previously created get_* object or ee.image.Image
#' @param scale A \code{numeric} value indicating scale in meters. 250 (default)
#' @param band A \code{character} indicating what bands/type to visualize.
#'  Can select more than one, e.g. c('Red', 'Green', 'Blue').
#' @param palette \code{vector} of a color palette.
#' @param range \code{vector} indicating a numerical range to set min/max visualization.
#' @param gamma \code{numeric} gamma correction factor.
#' @param opacity \code{numeric} transparent display value.
#' @param ... additional arguments for mapview.
#' @note This function uses a scale argument which is used to generate a min and max value for viewing.
#'  Because this uses getInfo(), it can take a while
#' depending on the scale. Since this is used for viewing, I would suggest to go bigger on the scale. If a user selects more than one band,
#' the 'up-to' three bands will be overlayed like earth engine.
#' @return A leaflet map.
#' @export
#'
#' @examples \dontrun{
#'
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
#'                   endDate = '2018-12-31', c.low = 6, c.high = 11)
#'
#' ld8 %>% ee_viz(scale = 30, band = 'NDVI', palette = 'RdYlGn')
#'
#' # or without scale, when min and max are used scale is irrelevant.
#'
#' ld8 %>% ee_viz(min = 0, max = 1, band = 'NDVI', palette = 'RdYlGn')
#'
#' }
#'
#'

ee_viz <- function(x, ...) {

  UseMethod('ee_viz')

}


#' @name ee_viz
#' @param geom A sf object to use as 'aoi/geom' for an 'ee.image.Image'.
#' @param ... extra args to pass on
#' @export

ee_viz.ee.image.Image <- function(x,
                                  scale = 1000,
                                  geom = NULL,
                                  band = NULL,
                                  palette = hcl.colors(11, 'RdBu'),
                                  range = NULL,
                                  gamma = NULL,
                                  opacity = NULL,
                                  ...){



  # error catching

  if(missing(x))stop({"Need an 'ee.image.Image'"})

  if(is.null(band))stop({"Need to provide band(s)."})

  stopifnot(inherits(x, "ee.image.Image"))

  if(inherits(geom, c('sf', 'sfc'))){

    geom <- setup(geom)

  }

  viz_helper(x, geom, band, scale, range, gamma, opacity, palette)

}

#' @name ee_viz
#' @param ... extra args to pass on
#' @export

ee_viz.exploreList <- function(x,
                                  scale = 1000,
                                  band = NULL,
                                  palette = hcl.colors(11, 'RdBu'),
                                  range = NULL,
                                  gamma = NULL,
                                  opacity = NULL,
                                  ...){



  # error catching

  if(missing(x))stop({"Need an 'exploreList' class"})

  if(is.null(band))stop({"Need to provide band(s)."})

  stopifnot(inherits(x, "exploreList"))

  geom <- x$geom

  x <- x$image

  viz_helper(x, geom, band, scale, range, gamma, opacity, palette)

}


#'
#' @title viz_helper
#' @param x image
#' @param geom geometry of image
#' @param band a band(s) input
#' @param scale what to scale the min/max getInfo() by
#' @param range min/max vector
#' @param gamma gamma value
#' @param opacity opacity of image
#' @param palette palette to use
#'
#'
viz_helper <- function(x, geom, band, scale, range, gamma, opacity, palette){

  image = x$select(band)

  image_tag <- image$bandNames()$getInfo()

  if(is.null(range)){
    range <- reducers_min_max(image, geom, scale)
  }

  image <- if(is.null(geom)){image} else {image$clip(geom)}

  if(!is.null(geom)){
    bbox <- geom$getInfo()
    lon <- bbox[["coordinates"]][[1]][[1]][[1]]
    lat <- bbox[["coordinates"]][[1]][[1]][[2]]
    rgee::Map$setCenter(lon, lat, 8)
  }

  if(length(band) > 1){

    map <- rgee::Map$addLayer(image,
                              visParams = list(min = range[1], max = range[2],
                              gamma = gamma),
                              paste(image_tag, collapse = ', '), opacity = opacity)

  } else {

    map <- rgee::Map$addLayer(image,
                              visParams = list(min = range[1], max = range[2],
                                               palette = palette,
                              gamma = gamma),
                              image_tag, opacity = opacity)

  }


  map
}
