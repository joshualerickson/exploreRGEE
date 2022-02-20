
#' Extract Time Series
#' @description This function allows the user to pass a previously created get_*() object or ee ImageCollection to get
#' a time series. This uses the \link[rgee]{ee_extract} function from rgee and does some tidying to return a sf data.frame.
#' @param imageCol A previously created get_* object or ee ImageCollection
#' @param geeFC A known GEE FeatureCollection or asset, e.g. "USGS/WBD/2017/HUC12"
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param temporal A \code{character} indicating what temporal filter to use on the collection, e.g. 'yearly' (default), 'monthly', 'year_month', 'all'.
#' @param temporal_stat A \code{character} indicating what to filter the ImageCollection when using temporal filtering, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param reducer A \code{character} indicating what to reduce the ImageCollection when using \link[rgee]{ee_extract} function, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param lazy \code{logical} whether to run a 'sequential' future in the background or not.
#' @param ... Other arguments to pass to \link[rgee]{ee_extract} function.

#' @importFrom rlang .data
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
#' # save both plot and data
#' ld8_ts <- ld8 %>% ee_timeseries()
#'
#' }

ee_timeseries <- function(imageCol, ...) {

  UseMethod('ee_timeseries')
}


#' @name ee_timeseries
#' @param geom A sf data.frame object.
#' @param startDate \code{character} format date, e.g. "2018-10-23".
#' @param endDate \code{character} format date, e.g. "2018-10-23".
#' @param months \code{numeric} vector of monnths, e.g. c(1,12).
#' @param ... extra args to pass on
#' @export

ee_timeseries.ee.imagecollection.ImageCollection <- function(imageCol,
                                                             geom,
                                                             geeFC = NULL,
                                                             scale,
                                                             temporal = 'yearly',
                                                             temporal_stat = 'mean',
                                                             reducer = 'mean',
                                                             lazy = FALSE,
                                                             startDate = NULL,
                                                             endDate = NULL,
                                                             months = NULL,
                                                             ...){

  # error catching
  if(missing(imageCol)){stop("Need a get_* object or ImageCollection to use this function")}

  if(any(class(imageCol) == 'diff_list' | class(imageCol) == 'terrain_list' | class(imageCol) == 'ee.image.Image')){stop("Can't band with this type of list")}

  if(!temporal %in% c('yearly', 'monthly', 'year_month', 'all')){stop("Need correct temporal argument")}

  stopifnot(!is.null(imageCol), inherits(imageCol, "ee.imagecollection.ImageCollection"))

  #use filter functions
  if(temporal == 'yearly'){

    imageCol <- ee_year_filter(imageCol = imageCol,startDate = startDate, endDate = endDate, stat = temporal_stat)

  } else if (temporal == 'monthly'){

    imageCol <- ee_month_filter(imageCol = imageCol, months = months, stat = temporal_stat)

  } else if (temporal == 'year_month') {

    imageCol <- ee_year_month_filter(imageCol = imageCol,startDate = startDate, endDate = endDate,months = months,  stat = temporal_stat)

  } else if (temporal == 'all'){

  }

  if(is.null(geeFC)) {

    reg <- sf_setup(geom)

  } else {

    if (isTRUE(lazy)){

    reg <- geeFC_setup_aoi(geom, geeFC)

    } else {

    reg <- geeFC_setup(geom, geeFC)

  }}

  if(isTRUE(lazy)){
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({

      extract_time(imageCol = imageCol,
                reg = reg,
                scale = scale,
                reducer = reducer,
                ...
      )
    }, lazy = TRUE)

  } else {

    extract_time(imageCol = imageCol,
              reg = reg,
              scale = scale,
              reducer = reducer,
              ...
              )

  }

}



#' @name ee_timeseries
#' @param ... extra args to pass on
#' @export
ee_timeseries.exploreList <- function(imageCol,
                                      geeFC = NULL,
                                      scale,
                                      temporal = 'yearly',
                                      temporal_stat = 'mean',
                                      reducer = 'mean',
                                      lazy = FALSE,
                                      ...){

  #use exploreList data
  geom <- imageCol$aoi
  startDate <- imageCol$startDate
  endDate <- imageCol$endDate
  c.low <- imageCol$c.low
  c.high <- imageCol$c.high

  # error catching
  if(missing(imageCol)){stop("Need a get_* object or ImageCollection to use this function")}

  if(any(class(imageCol) == 'diff_list' | class(imageCol) == 'terrain_list' | class(imageCol) == 'ee.image.Image')){stop("Can't band with this type of list")}

  if(!temporal %in% c('yearly', 'monthly', 'year_month', 'all')){stop("Need correct temporal argument")}

  stopifnot(!is.null(imageCol), inherits(imageCol, "exploreList"))

  imageCol <- temporal_filter(temporal, imageCol, temporal_stat)


  if(is.null(geeFC)) {

    reg <- sf_setup(geom)

  } else {

    if (isTRUE(lazy)){

      reg <- geeFC_setup_aoi(geom, geeFC)

    } else {

      reg <- geeFC_setup(geom, geeFC)

    }}

  if(isTRUE(lazy)){
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({

      extract_time(imageCol = imageCol,
                reg = reg,
                scale = scale,
                reducer = reducer,
                ...
      )
    }, lazy = TRUE)

  } else {

    extract_time(imageCol = imageCol,
              reg = reg,
              scale = scale,
              reducer = reducer,
              ...
    )

  }
}


#' @title extract_time
#' @description Function used to get extraction of filtered IC and then returns a nice tidy
#' data.frame with time = date and .names as the band name, e.g. B1_mean, B1_median, etc.
#' @param imageCol ee ImageCollection
#' @param reg sf object
#' @param reducer what reducer to use in rgee::ee_extract
#' @param scale scale in meters
#' @param ... extra args to pass to rgee::ee_extract
extract_time <- function(imageCol, reg, reducer, scale,...){

  reducer_fun <- switch(
    reducer,
    "mean" = ee$Reducer$mean(),
    "max" = ee$Reducer$mean(),
    "min" = ee$Reducer$min(),
    "median"= ee$Reducer$median(),
    "sum"= ee$Reducer$sum(),
    "sd" = ee$Reducer$stdDev(),
    NULL
  )

  #tB <- imageCol$toBands()
  tB <-  imageCol |>
    map_date_to_bandname_ic()

  # cat("starting ee_extract\n")
  data_tb <- rgee::ee_extract(x = tB,
                              y = reg$reg,
                              sf = FALSE,
                              fun = reducer_fun,
                              scale = scale,
                              ...)
  # client side
  band_names_cli<- imageCol$first()$bandNames()$getInfo()

  # regex to be removed from name to create date col
  rm_rgx <- paste0(".*",band_names_cli)
  rm_rgx <- glue::glue_collapse(rm_rgx,sep = "|")

  # regex to extract parameter identifier
  # reorder so shorter names with common prefix to another band names wont replace string before longer version
  extract_rgx <- band_names_cli[stringr::str_order(band_names_cli,decreasing=T)]
  extract_rgx <- glue::glue_collapse(extract_rgx,sep = "|")


  data_tb |>
    tidyr::pivot_longer(cols = -c(1:(length(reg$aoi)-1)),names_to = ".names", values_to = 'value') |>
    mutate(
      parameter=str_extract(.data$.names, pattern=extract_rgx),
      date= str_remove(string = .data$.names, pattern = rm_rgx) |>
        str_replace_all("_","-") |> lubridate::ymd()

    ) |>
    select(-.data$.names)
}

#' @title map_date_to_bandname_ic
#' @description Slick helper function by Zack that get's the names
#' of the bands as well as the date.
#' @param ic ee ImageCollection
#'
map_date_to_bandname_ic <- function(ic){
  ic |>
    ee$ImageCollection$map(
      function(x){
        # can't use getInfo() in sever-side function
        bnames<- x$bandNames()
        date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')

        # since bnames is technically a list rather than a simple string I need to map over it
        # this should make it flexible fore when there are more bands I want to rename anyways
        bnames_date <- bnames$map(
          rgee::ee_utils_pyfunc(function(x){
            ee$String(x)$cat(ee$String("_"))$cat(date)

          })
        )
        x$select(bnames)$rename(bnames_date)
      }

    )

}


#' @title Filtering by Temporal Arguments
#' @description A helper function to distinguish time reducer
#' @param temporal time to reduce
#' @param imageCol ee ImageCollection
#' @param temporal_stat what to reduce time by.
#'
temporal_filter <- function(temporal, imageCol, temporal_stat){


    switch(temporal,
           'yearly' = ee_year_filter(imageCol, temporal_stat),
           'monthly' = ee_month_filter(imageCol, temporal_stat),
           'year_month' = ee_year_month_filter(imageCol, temporal_stat),
           'all' = imageCol$imageCol,
           NULL
    )
}



