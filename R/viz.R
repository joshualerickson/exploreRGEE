#' Viz-ualize GEE Image/ImageCollections
#' @description This function allows users to quickly view a previously created get_*() object.
#' @param data A previously created get_* object or ee.image.Image
#' @param scale A \code{numeric} value indicating scale in meters. 250 (default)
#' @param band A \code{character} indicating what bands/type to use when you have more than one. Can select more than one, e.g. c('Red', 'Green', 'Blue').
#' @param palette \code{character} color palette using colorBrewer format, e.g. "RdBu" (default), "RdYlGn", etc.
#' @param n_pal \code{numeric} indicating levels of colors in palette. 6 (default).
#' @param reverse \code{logical} TRUE/FALSE whether to reverse palette or not, FALSE (default).
#' @param min \code{numeric} indicating lowest value for viewing.
#' @param max \code{numeric} indicating highest value for viewing.
#' @param gamma \code{numeric} gamma correction factor.
#' @param opacity \code{numeric} transparent display value.
#' @param user_shape A sf object to use as 'aoi/geom' for an 'ee.image.Image'.
#' @param ... additional arguments for mapview.
#' @note This function uses a scale argument which is used to generate a min and max value for viewing. Because this uses getInfo(), it can take a while
#' depending on the scale. Since this is used for viewing, I would suggest to go bigger on the scale. If a user selects more than one band, the 'up-to' three bands will be overlayed like earth engine. When visualizing a
#' \link[exploreRGEE]{get_diff} object, black is towards 0, red is negative (-) and green/blue is positive (+).
#' @return A leaflet map.
#' @export
#'
#' @examples \dontrun{
#'
#' # Load Libraries
#'
#' library(rgee)
#' rgee::ee_intialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
#'                   endDate = '2018-12-31', c.low = 6, c.high = 11)
#'
#' ld8 %>% viz(scale = 30, band = 'NDVI', palette = 'RdYlGn')
#'
#' # or without scale, when min and max are used scale is irrelevant.
#'
#' ld8 %>% viz(min = 0, max = 1, band = 'NDVI', palette = 'RdYlGn')
#'
#' }
viz <- function(data, scale = 250, band = NULL, palette = "RdBu", n_pal = 6, reverse = FALSE, min = NULL, max = NULL, gamma = NULL, opacity = NULL, user_shape = NULL, ...){


  if(missing(data))stop({"Need a previously created get_* object or 'ee.image.Image' as 'data'."})

  #if(!is.null(user_shape)){stop("Can't have both user_shape and ee.image.Image or get_*() objects. Try to pass it (user_shape) along to viz(), e.g. %>%")}

# dissecting the passed get_*() object, not a huge fan of it but it works for now....

  if(class(data)[[1]] == "ee.image.Image"){
    image <- data
    aoi <- user_shape
    geom <- setup(aoi)
    method <- 'user Image'
    param <- NULL
    stat <- NULL
    startDate <- NULL
    endDate <- NULL
    bbox <- as.numeric(sf::st_bbox(aoi))

  } else {

    image <- data$data
    geom <- data$geom
    method <- data$method
    param <- data$param
    stat <- data$stat
    startDate <- data$startDate
    endDate <- data$endDate
    bbox <- data$bbox
  }


    if(is.null(param) && is.null(band))stop({"Need to choose a band name."})

    if(is.null(param)){

     image = image$select(band)
     param <- band

    }

    if(length(param) > 1 || class(data) == 'diff_list' || class(data) == 'linear_list'){

      id_tag <- paste0(method, ' - ',stat, "; " ,startDate, " - ", endDate)

      m1 <- leaf_call(data = data, image = image, geom = geom, min = min, max = max, palette = NULL,
                      id_tag = id_tag, bbox = bbox, reverse = NULL, n_pal = NULL, bands = param, gamma = gamma, opacity = opacity)

    } else {


      reducers <- rgee::ee$Reducer$min()$combine(
                  reducer2 = rgee::ee$Reducer$max(),
                  sharedInputs = TRUE)

      stats <- image$reduceRegions(
               reducer = reducers,
               collection = geom,
               scale = scale)

    #need to make better

    if(is.null(min) || is.null(max)){

      min <- stats$getInfo()$features[[1]]$properties$min
      max <- stats$getInfo()$features[[1]]$properties$max

    }



  if(class(data) == 'terrain_list' && param == "complete"){

    mlay <- rgee::Map$addLayer(image$clip(geom), visParams = list(bands = "hillshade", min = 0, max = 256), opacity = 0.45)
    mlay2 <- rgee::Map$addLayer(image$clip(geom), visParams = list(bands = "elevation", min = min, max = max, palette = c('green','yellow','grey','red','black')), opacity = 0.45)
    mlay3 <- rgee::Map$addLayer(image$clip(geom), visParams = list(bands = "slope", min = 0, max = 45, palette = c('white','grey','black','red','yellow')))

    m1 <-  mlay3 + mlay2 + mlay

  } else {

    id_tag <- paste0(method, ' - ', param, ' ',stat, "; " ,startDate, " - ", endDate)

    m1 <- leaf_call(data = data, image = image, min = min, max = max, palette = palette,bands = param, id_tag = id_tag, bbox = bbox, geom = geom, reverse = reverse, n_pal = n_pal, gamma = gamma, opacity = opacity)

  }

}
    print(m1)
}



# Palette function

Pal <- function(pal, reverse, n_pal) {

  if(isTRUE(reverse)){

  rev(grDevices::hcl.colors(n = n_pal ,palette = pal))

  } else {

    grDevices::hcl.colors(n = n_pal ,palette = pal)
}

}


# leaflet mapping function

leaf_call <- function(data,image, geom, min, max, palette, id_tag, bbox, reverse, n_pal, bands, gamma, opacity){

    rgee::Map$setCenter(bbox[1], bbox[2], 6)

    GetURL <- function(service, host = "basemap.nationalmap.gov") {
      sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
    }

    grp = "Hydrography"
    opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)

    if(is.null(palette)){

if(length(bands) > 1){

  mLayer <- rgee::Map$addLayer(image$clip(geom), visParams = list(bands = bands, min = min, max = max, gamma = gamma), id_tag, opacity = opacity)


} else {


  mLayer <- rgee::Map$addLayer(image$clip(geom)$sldStyle(sld_intervals(data, min, max)), visParams = list(), id_tag, opacity = opacity)

}

mLayer

    } else {

    mLayer <- rgee::Map$addLayer(image$clip(geom),
                           visParams = list(min = min, max = max, palette = Pal(palette, reverse, n_pal)), id_tag, opacity = opacity)
}
    m1 <- mLayer %>%
      leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                           group = grp, options = opt, layers = "0") %>%
      leaflet::hideGroup(group = grp) %>%
      leaflet::addLayersControl(baseGroups = c("CartoDB.Positron", "CartoDB.DarkMatter",
                                               "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                                overlayGroups = c(id_tag, grp))
}


# Sld styles for diff_list

# this will get better with min/max args

sld_intervals <- function(data, min, max){


    if(class(data) == 'met_list' || class(data) == 'npp_list'){
  paste0(
    "<RasterSymbolizer>",
    '<ColorMap  type="ramp" extended="false" >',

    '<ColorMapEntry color="#B22222" quantity="-400" />',
    '<ColorMapEntry color="#B22222" quantity="-150" />',
    '<ColorMapEntry color="#B22222" quantity="-30" />',
    '<ColorMapEntry color="#FF0000" quantity="-15" />',
    '<ColorMapEntry color="#000000" quantity="0" />',
    '<ColorMapEntry color="#008000" quantity="15" />',
    '<ColorMapEntry color="#0000CD" quantity="30" />',
    '<ColorMapEntry color="#B22222" quantity="150" />',
    '<ColorMapEntry color="#B22222" quantity="400" />',
    "</ColorMap>",
    "</RasterSymbolizer>"
  )

  } else if (class(data) == 'linear_list'){

    if (min >= -1 && max <= 1){
      paste0(
        "<RasterSymbolizer>",
        '<ColorMap  type="ramp" extended="false" >',
        '<ColorMapEntry color="#A51122" quantity="-1" />',
        '<ColorMapEntry color="#D94300" quantity="-0.5" />',
        '<ColorMapEntry color="#E78200" quantity="-0.25" />',
        '<ColorMapEntry color="#EFB446" quantity="-0.1" />',
        '<ColorMapEntry color="#F6DE90" quantity="-0.05" />',
        '<ColorMapEntry color="#000000" quantity="0" />',
        '<ColorMapEntry color="#D7E88D" quantity="0.05" />',
        '<ColorMapEntry color="#A2CC5B" quantity="0.1" />',
        '<ColorMapEntry color="#6AAB44" quantity="0.25" />',
        '<ColorMapEntry color="#338738" quantity="0.5" />',
        '<ColorMapEntry color="#006228" quantity="1" />',
        "</ColorMap>",
        "</RasterSymbolizer>"
      )

    } else {
       paste0(
      "<RasterSymbolizer>",
      '<ColorMap  type="ramp" extended="false" >',
      '<ColorMapEntry color="#A51122" quantity="-300" />',
      '<ColorMapEntry color="#D94300" quantity="-200" />',
      '<ColorMapEntry color="#E78200" quantity="-100" />',
      '<ColorMapEntry color="#EFB446" quantity="-25" />',
      '<ColorMapEntry color="#F6DE90" quantity="-.5" />',
      '<ColorMapEntry color="#000000" quantity="0" />',
      '<ColorMapEntry color="#D7E88D" quantity="0.5" />',
      '<ColorMapEntry color="#A2CC5B" quantity="25" />',
      '<ColorMapEntry color="#6AAB44" quantity="100" />',
      '<ColorMapEntry color="#338738" quantity="200" />',
      '<ColorMapEntry color="#006228" quantity="300" />',
      "</ColorMap>",
      "</RasterSymbolizer>"
    )

    }

  } else {
    paste0(
      "<RasterSymbolizer>",
      '<ColorMap  type="ramp" extended="false" >',
      '<ColorMapEntry color="#A51122" quantity="-30" />',
      '<ColorMapEntry color="#D94300" quantity="-20" />',
      '<ColorMapEntry color="#E78200" quantity="-10" />',
      '<ColorMapEntry color="#EFB446" quantity="-2.5" />',
      '<ColorMapEntry color="#F6DE90" quantity="-.5" />',
      '<ColorMapEntry color="#000000" quantity="0" />',
      '<ColorMapEntry color="#D7E88D" quantity="0.5" />',
      '<ColorMapEntry color="#A2CC5B" quantity="2.5" />',
      '<ColorMapEntry color="#6AAB44" quantity="10" />',
      '<ColorMapEntry color="#338738" quantity="20" />',
      '<ColorMapEntry color="#006228" quantity="30" />',
      "</ColorMap>",
      "</RasterSymbolizer>"
    )

}

}
