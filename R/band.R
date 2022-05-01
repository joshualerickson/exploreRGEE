
#' Extract Regions by Date
#' @description This function allows the user to pass a previously created get_*() object to get
#' a time series from a collection, e.g. banding/toBands. This uses the \link[rgee]{ee_extract} function from rgee.
#' @param imageCol A previously created get_* object or ee.ImageCollection
#' @param geeFC A known GEE FeatureCollection or asset, e.g. "USGS/WBD/2017/HUC12"
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param band A \code{character} indicating what bands/type to use when you have more than one.
#' @param temporal A \code{character} indicating what temporal filter to use on the collection, e.g. 'yearly' (default), 'monthly', 'year_month', 'all'.
#' @param temporal_stat A \code{character} indicating what to reduce the ImageCollection when using temporal filtering, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param lazy \code{logical} whether to run a 'sequential' future in the background or not.
#' @param reducer A \code{character} 'median' (default) for reducing.
#' @param variable \code{character} indicating what to facet ggplot by. Need to know ahead of time.
#' @param ggplot \code{logical} TRUE/FALSE. Whether to print side-effect ggplot. See details.
#' @param save.plot \code{logical} TRUE/FALSE. Whether to save the plot in a list.
#' @param user_geom \code{logical} TRUE/FALSE.
#' @param startDate \code{character} format date, e.g. "2018-10-23". NULL (default)
#' @param endDate \code{character} format date, e.g. "2018-10-23". NULL (default)
#' @param c.low \code{numeric} lower month value for calendar range. NULL (default)
#' @param c.high \code{numeric} higher month value for calendar range. NULL (default)
#' @note If using a get_*() object band() doesn't use the image generated. It only uses the ImageCollection.
#' Additional arguments to pass if using a ImageCollection without get_*()'s are the startDate, endDate, c.low and c.high arguments.
#' Faster with points or centroids of polygons.
#' If lazy is TRUE, the function will be run in the background.
#' @return A \code{data.frame} and a side-effect plot (if ggplot = TRUE); unless using \code{save.plot} then a list with \code{data.frame} and ggplot.
#' @importFrom rlang .data
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
#' # without plotting save to object
#' ld8_ts <- ld8 %>% band(scale = 30, band = 'NDVI')
#'
#' # with plotting as side-effect
#' ld8 %>% band(scale = 30, band = 'NDVI', ggplot = TRUE, variable = 'name')
#'
#' # save both plot and data
#' ld8_ts <- ld8 %>% band(scale = 30, band = 'NDVI',
#'                        ggplot = TRUE, variable = 'name',
#'                        save.plot = TRUE)
#'
#' }

band <- function(imageCol, geeFC = NULL, scale, band = NULL,
                 temporal = 'yearly', temporal_stat = 'median', lazy = FALSE,
                 reducer = 'median', variable = NULL,
                 ggplot = FALSE, save.plot = F, user_geom = NULL, startDate = NULL,
                 endDate = NULL, c.low = NULL, c.high = NULL) {
  # error catching
  if(missing(imageCol)){stop("Need a get_* object or ImageCollection to use this function")}
  if(any(class(imageCol) == 'diff_list' | class(imageCol) == 'terrain_list' | class(imageCol) == 'ee.image.Image')){stop("Can't band with this type of list")}
  if(!temporal %in% c('yearly', 'monthly', 'year_month', 'all')){stop("Need correct temporal argument")}
  if(class(imageCol)[[1]] == "ee.imagecollection.ImageCollection" & is.null(user_geom)){stop("Need a user geom if using ImageCollection")}
  # dissecting the passed get_*() object
  if(class(imageCol)[[1]] == "ee.imagecollection.ImageCollection"){

    aoi <- user_geom %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
    geom <- setup(aoi)
    method <- NULL
    startDate <- startDate
    endDate <- endDate
    c.low <- c.low
    c.high <- c.high


  } else {
    aoi <- imageCol$aoi
    startDate <- imageCol$startDate
    endDate <- imageCol$endDate
    geom <- imageCol$geom
    method <- imageCol$method
    c.low <- imageCol$c.low
    c.high <- imageCol$c.high

  }

  if(is.null(band))stop({"Need to choose a band name."})

  if(class(imageCol)[[1]] == "ee.imagecollection.ImageCollection"){

    imageCol <- imageCol$select(band)

    proc <- ee_timeseries(imageCol,
                          geom = geom,
                          geeFC = geeFC,
                          scale = scale,
                          temporal = temporal,
                          temporal_stat = temporal_stat,
                          reducer = reducer,
                          lazy = lazy,
                          startDate = startDate,
                          endDate = endDate,
                          months = months)

  } else {

    imageCol$imageCol <- imageCol$imageCol$select(band)

    proc <- ee_timeseries(imageCol = imageCol,
                          geeFC = geeFC,
                          scale = scale,
                          temporal = temporal,
                          temporal_stat = temporal_stat,
                          reducer = reducer,
                          lazy = lazy)

  }


  if(ggplot == TRUE){

    proc_ggplot <- plot_proc(proc = proc, facet_col_var = variable)

    print(proc_ggplot +
            ggplot2::labs(title = paste0(method, " ", band, ' ', temporal_stat, " values for date range: "),
                          subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                          y = paste0(band, ' values'), color = "ID"))
  }


  if(save.plot == TRUE){
    return(list(proc = proc, proc_ggplot = proc_ggplot))

  } else {return(proc)}

}



# ggplot plot for proc data

plot_proc <- function(proc, facet_col_var){

  mapping <- ggplot2::aes(.data$date, .data[['value']], colour = .data[[facet_col_var]])

  if(is.null(facet_col_var)){

    mapping$colour <- NULL

  }

  if (is.null(facet_col_var)){

    facet <- NULL

  } else {

    facet <- ggplot2::facet_wrap(dplyr::vars(.data[[facet_col_var]]))
  }

  proc %>% ggplot2::ggplot(mapping) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(alpha = 0.3) +
    ggplot2::theme_bw()  +
    facet

}
