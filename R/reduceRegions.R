#' Title
#' @description This function allows the user to pass a previously created get_*() object to get
#' reduceRegions() by using \link[rgee]{ee_as_sf} function.
#' @param data A previously created get_* object
#' @param geeFC A known GEE FeatureCollection or asset, e.g. "USGS/WBD/2017/HUC12"
#' @param scale A \code{numeric} value indicating scale in meters
#' @param tileScale \code{numeric} what to reduce regions by, 1 (default). Higher means slower but less memory, e.g. 5.
#' @param band A \code{character} indicating what bands/type to use when you have more than one.
#' @param lazy \code{logical} whether to run a future or not.
#' @param variable \code{character} indicating what to label features in leaflet map. Need to know ahead of time.
#' @param leaflet \code{logical}. TRUE/FALSE whether to view map. FALSE (default).
#' @param palette \code{character} color palette using colorBrewer format, e.g. "RdBu" (default), "RdYlGn", etc.
#' @param n_pal \code{numeric} indicating levels of colors in palette. 11 is max and (default).
#' @param reverse \code{logical} TRUE/FALSE whether to reverse palette or not, FALSE (default).
#'
#' @note If lazy is TRUE, the function will be run in the background. If the pixel size is big then please adjust tileScale
#' to account for memory. This will not effect zonal stats (pixel size) but will just take longer.
#' @importFrom rgee ee Map
#' @return A leaflet map (leaflet = TRUE) and always a sf object.
#' @export
#'
#' @examples \dontrun{
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
#' # without leaflet save to object
#' ld8_rr <- ld8 %>% rr(scale = 30, band = 'NDVI')
#'
#' # with leaflet as side-effect
#' ld8 %>% rr(scale = 30, band = 'NDVI', leaflet = TRUE, variable = 'name')
#'
#' # or side-effect leaflet and get sf object.
#' ld8_ts <- ld8 %>% band(scale = 500, band = 'NDVI', leaflet = TRUE, variable = 'name')
#'
#' }
rr <- function(data, geeFC = NULL, scale, tileScale = 1, band = NULL, lazy = FALSE, variable = NULL, leaflet = FALSE, palette = "RdBu", n_pal = 11, reverse = FALSE){

  if(missing(scale))stop({"Please provide a scale to reduce region(s)."})
  if(missing(data))stop({"Need a previously created get_* object as 'data'."})

  # dissecting the passed get_*() object
  aoi <- data$aoi
  image <- data$data
  geom <- data$geom
  stat <- data$stat
  method <- data$method
  param <- data$param
  startDate <- data$startDate
  endDate <- data$endDate
  c.low <- data$c.low
  c.high <- data$c.high
  bbox <- data$bbox

  if(is.null(param) & is.null(band))stop({"Need to choose a band name."})

  if(is.null(param)){

    image = image$select(band)
    param <- band

  }

  if(length(param) > 1){stop("can only use one band, sorry.")}

  reducers <- reducers()

  if(is.null(geeFC)) {

    reg <- sf_setup(aoi)

  } else {

    reg <- geeFC_setup(aoi, geeFC)


  }

  stats <- image$reduceRegions(
    reducer = reducers,
    collection = reg$reg,
    scale = scale,
    tileScale = tileScale
  )

  # hijacked from rgee
  if(isTRUE(lazy)){
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({

      rgee::ee_as_sf(stats)

    }, lazy = TRUE)

  } else {

    region_df <- rgee::ee_as_sf(stats)

    region_df <- sf::st_as_sf(region_df) %>%
      dplyr::mutate(param = param, month_range = paste0(c.low," - ", c.high), year_range = paste0(stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"))) %>%
      dplyr::mutate(dplyr::across(c('max', 'mean', 'median', 'min','stdDev', 'sum'), as.numeric))

  if(leaflet == "TRUE") {

    # this is subjective and could be axed for sure.

    website <- function(){

      if (class(data) == "landsat_list"){

        '<a href = "https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-quality-assessment?qt-science_support_page_related_con=0#qt-science_support_page_related_con"> More Info </a>'

        } else if (class(data) == "met_list" & method %in% c('AN81m', 'AN81d', 'Norm81m')){

        '<a href = "https://prism.oregonstate.edu/documents/PRISM_datasets.pdf"> More Info </a>'

        } else if (class(data) == "met_list" & method == 'GRIDMET'){

          '<a href = "https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.3413"> More Info </a>'
        }
    }

    if(class(region_df$geometry[[1]])[[2]] != "POINT") {

      ldPal <- leaflet::colorNumeric(palette = Pal(palette, reverse, n_pal), region_df$mean)

      plot <-  viz_A() %>% leaflet::addPolygons(data = region_df, color = "black",
                                                fillOpacity = .75,
                                                fillColor = ~ldPal(mean),
                                                popup = paste0("<b>", "Parameter: ", "</b>",paste0(param, " by ", stat),
                                                               "<br>", "<b>", "Site ID: ", "</b>", ifelse(is.null(variable), paste("NULL"),region_df[[variable]]) ,
                                                               "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                               "<br>", "<b>", "Maximum: ", "</b>",round(region_df$max,3),
                                                               "<br>", "<b>", "Minimum: ", "</b>",round(region_df$min,3),
                                                               "<br>", "<b>", "Mean: ", "</b>",round(region_df$mean,3),
                                                               "<br>", "<b>", "Median: ", "</b>",round(region_df$median,3),
                                                               "<br>", "<b>", "Standard Deviation: ", "</b>",round(region_df$stdDev,3),
                                                               "<br>", "<b>", "Sum: ", "</b>",round(region_df$sum,3),
                                                               "<br>",
                                                               website()))



    } else {

      ldPal <- leaflet::colorNumeric(palette = Pal(palette, reverse, n_pal), region_df$mean)
      plot <-  viz_A() %>% leaflet::addCircleMarkers(data = region_df, color = "black",
                                                     fillOpacity = .75,
                                                     fillColor = ~ldPal(mean),
                                                     popup = paste0("<b>", "Parameter: ", "</b>",paste0(param, " by ", stat),
                                                                    "<br>", "<b>", "Date Range: ", "</b>",paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                                                                    "<br>", "<b>", "Value: ", "</b>",round(region_df$mean,3),
                                                                    "<br>",
                                                                    website()))


    } #ending ifelse

    print(plot)

  } #ending ifelse

      return(region_df)

  } #ending ifelse


} #end function
