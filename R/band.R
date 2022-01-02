
#' Extract Regions by Date
#' @description This function allows the user to pass a previously created get_*() object to get
#' a time series from a collection, e.g. banding/toBands. This uses the \link[rgee]{ee_extract} function from rgee.
#' @param data A previously created get_* object
#' @param geeFC A known GEE FeatureCollection or asset, e.g. "USGS/WBD/2017/HUC12"
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 800 (m) default.
#' @param band A \code{character} indicating what bands/type to use when you have more than one.
#' @param temporal A \code{character} indicating what temporal filter to use on the collection, e.g. 'yearly' (default), 'monthly', 'year_month', 'all'.
#' @param stat A \code{character} indicating what to reduce the ImageCollection when using temporal filtering, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param lazy \code{logical} whether to run a 'sequential' future in the background or not.
#' @param fun A earth engine reducer, e.g. ee$Reducer$median() (default).
#' @param variable \code{character} indicating what to facet ggplot by. Need to know ahead of time.
#' @param ggplot \code{logical} TRUE/FALSE. Whether to print side-effect ggplot. See details.
#' @param save.plot \code{logical} TRUE/FALSE. Whether to save the plot in a list, e.g. data + ggplot.
#' @param user_geom \code{logical} TRUE/FALSE.
#' @param startDate \code{character} format date, e.g. "2018-10-23". NULL (default)
#' @param endDate \code{character} format date, e.g. "2018-10-23". NULL (default)
#' @param c.low \code{numeric} lower month value for calendar range. NULL (default)
#' @param c.high \code{numeric} higher month value for calendar range. NULL (default)
#' @note Additional arguments to pass if using a ImageCollection without get_*()'s are the startDate, endDate, c.low and c.high arguments.
#' Faster with points or centroids of polygons.
#' If lazy is TRUE, the function will be run in the background using a for-loop.
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

band <- function(data, geeFC = NULL, scale, band = NULL,
                 temporal = 'yearly', stat = 'median', lazy = FALSE,
                 fun = ee$Reducer$median(), variable = NULL,
                 ggplot = FALSE, save.plot = F, user_geom = NULL, startDate = NULL,
                 endDate = NULL, c.low = NULL, c.high = NULL) {
# error catching
  if(missing(data)){stop("Need a get_* object or ImageCollection to use this function")}
if(any(class(data) == 'diff_list' | class(data) == 'terrain_list' | class(data) == 'ee.image.Image')){stop("Can't band with this type of list")}
if(!temporal %in% c('yearly', 'monthly', 'year_month', 'all')){stop("Need correct temporal argument")}
  if(class(data)[[1]] == "ee.imagecollection.ImageCollection" & is.null(user_geom)){stop("Need a user geom if using ImageCollection")}
  # dissecting the passed get_*() object
if(class(data)[[1]] == "ee.imagecollection.ImageCollection"){

  aoi <- user_geom %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  imageCol <- data
  geom <- setup(aoi)
  method <- NULL
  param <- NULL
  startDate <- startDate
  endDate <- endDate
  c.low <- c.low
  c.high <- c.high


} else {
    aoi <- data$aoi
    imageCol <- data$imageCol
    startDate <- data$startDate
    endDate <- data$endDate
    geom <- data$geom
    method <- data$method
    param <- data$param
    c.low <- data$c.low
    c.high <- data$c.high

}
  if(is.null(param) & is.null(band))stop({"Need to choose a band name."})

  if(is.null(param)){

    imageCol = imageCol$select(band)
    param <- band

  }

  if(temporal == 'yearly'){

    imageCol <- year_filter(startDate = startDate, endDate = endDate,imageCol = imageCol, stat = stat)

  } else if (temporal == 'monthly'){

    imageCol <- month_filter(c.low = c.low, c.high = c.high,imageCol = imageCol, stat = stat)

  } else if (temporal == 'year_month') {

    imageCol <- year_month_filter(startDate = startDate, endDate = endDate,c.low = c.low, c.high = c.high,imageCol = imageCol, stat = stat)

  } else if (temporal == 'all'){

  }

  if(is.null(geeFC)) {

    reg <- sf_setup(aoi)

  } else {

    if (isTRUE(lazy)){

    reg <- geeFC_setup_aoi(aoi, geeFC)

    } else {

      reg <- geeFC_setup(aoi, geeFC)

  }}

  if(isTRUE(lazy)){
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({

      fut_band_func(imageCol = imageCol, data = data, reg = reg, fun = fun, scale = scale, param = param, method = method, tmp_type = temporal)

    }, lazy = TRUE)

  } else {



    band_func(imageCol = imageCol, reg = reg, fun = fun, scale = scale, param = param,
                  method = method, data = data, startDate = startDate, endDate = endDate, stat = stat,
                  save.plot = save.plot, ggplot = ggplot, variable = variable, c.low = c.low, c.high = c.high, tmp_type = temporal)

  }

}


# function for getting the banding

fut_band_func <- function(imageCol, data, reg, fun, scale, param, method, tmp_type){


  n_lists <- nrow(reg$aoi)/10

  reggy <- reg$aoi %>%
    dplyr::group_by((dplyr::row_number()-1) %/% (dplyr::n()/n_lists))%>%
    tidyr::nest() %>% dplyr::pull(data)

  final_proc <- data.frame()

  for(i in 1:length(reggy)){

    aoi <- reggy[[i]]

    tB <- imageCol$toBands()

    data_tb <- rgee::ee_extract(tB, aoi, fun = fun, scale = scale)

    param_name <- paste0("_", param)

    proc <- data_tb %>% tidyr::pivot_longer(dplyr::contains(param_name), names_to = "Date", values_to = param)


    proc <- getting_proc(data = data, proc = proc, param_name = param_name, method = method, tmp_type = tmp_type)

    final_proc <- plyr::rbind.fill(proc, final_proc)

    Sys.sleep(1/100)

  }

  final_proc
}

band_func <- function(imageCol, reg, fun, scale, param, method, data,
                          startDate, endDate, stat, save.plot, ggplot, variable, c.low, c.high, tmp_type){

  tB <- imageCol$toBands()

  data_tb <- rgee::ee_extract(x = tB, y = reg$reg, fun = fun, scale = scale)

  param_name <- paste0("_",param)

  proc <- data_tb %>% tidyr::pivot_longer(dplyr::contains(param_name), names_to = "Date", values_to = param)


  proc <- getting_proc(data = data, proc = proc, param_name = param_name, method = method, tmp_type = tmp_type)


  if(ggplot == TRUE){

    proc_ggplot <- plot_proc(proc = proc, param_v = param, facet_col_var = variable)

    print(proc_ggplot +
            ggplot2::labs(title = paste0(method, " ", param, ' ', stat, " values for date range: "),
                 subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                 y = paste0(param, ' values'), color = "ID"))
  }


  if(save.plot == TRUE){
    return(list(proc = proc, proc_ggplot = proc_ggplot))

  } else {return(proc)}

}

# processing function for bands by 'class'


getting_proc <- function(data, proc, param_name, method, tmp_type){

  if(tmp_type == 'all'){

  if(class(data) == 'met_list'){

if(method == "AN81m" | method == "TERRACLIMATE"){

  proc <- proc %>% dplyr::mutate(Date = stringr::str_remove_all(.data$Date, "X"),
                                 Date = stringr::str_remove_all(.data$Date, param_name),
                                 Date = stringr::str_replace(.data$Date,"(\\d{4})", "\\1-"),
                                 Date = paste0(.data$Date, "-01"),
                                 Date = lubridate::as_date(.data$Date))

} else if (method == "AN81d" | method == 'GRIDMET' | method == 'DAYMET') {

  proc <- proc %>% dplyr::mutate(Date = stringr::str_remove_all(.data$Date, "X"),
                                 Date = stringr::str_remove_all(.data$Date,param_name),
                                 Date = stringr::str_replace(.data$Date,"(\\d{4})", "\\1-"),
                                 Date = lubridate::as_date(.data$Date))

} else if (method == 'TRMMh'){

  proc <- proc %>% dplyr::mutate(Date = stringr::str_remove_all(.data$Date, 'X'),
              Date = stringr::str_remove_all(.data$Date, param_name),
              Date = stringr::str_sub(.data$Date, start = 6),
              Date = stringr::str_sub(.data$Date, end = -3),
              Date = stringr::str_replace_all(.data$Date, "_", " "),
              Date = stringr::str_replace(.data$Date, "(\\d{6})", "\\1-"),
              Date = stringr::str_replace(.data$Date, "(\\d{4})", "\\1-"),
              Date = paste0(.data$Date, "00"),
              Date = lubridate::parse_date_time2(.data$Date, orders = '%Y/%m/%d %H:%M'))

} else if (method == 'TRMMm'){

  proc <- proc %>% dplyr::mutate(Date = stringr::str_remove_all(.data$Date, 'X'),
                                 Date = stringr::str_remove_all(.data$Date, param_name),
                                 Date = stringr::str_sub(.data$Date, start = 6),
                                 Date = stringr::str_sub(.data$Date, end = -3),
                                 Date = lubridate::as_date(.data$Date)
                                 )

}
  } else if (class(data) == 'landsat_list'){


    proc <- proc %>% dplyr::mutate(Date = stringr::str_remove(.data$Date, param_name),
                                   Date = stringr::str_sub(.data$Date, start = -8),
                                   Date = lubridate::as_date(.data$Date))

  } else if (class(data) == 'sent2_list'){


    proc <- proc %>% dplyr::mutate(Date = stringr::str_sub(.data$Date, end = 9),
                                   Date = stringr::str_remove(.data$Date, "X"),
                                   Date = lubridate::as_date(.data$Date))

  } else if (class(data) == 'npp_list'){


    proc <- proc %>% dplyr::mutate(Date = stringr::str_remove(.data$Date, "_annualNPP"),
                                   Date = stringr::str_remove(.data$Date, "X"),
                                   Date = as.numeric(.data$Date))

  } else if (class(data) == 'any_list' | class(data)[[1]] == 'ee.imagecollection.ImageCollection'){


  proc <- proc %>% dplyr::mutate(raw_date = .data$Date,
                                 Date = dplyr::row_number())

  }

  } else if (tmp_type == 'year_month'){

    proc <- proc %>% dplyr::mutate(Date = stringr::str_replace(.data$Date, "[^_]*_(.*)", "\\1"),
                                   Date = stringr::str_replace(.data$Date, '_', '-'),
                                   Date = lubridate::as_date(.data$Date))

  } else if (tmp_type == 'monthly' | tmp_type == 'yearly'){

    proc <- proc %>% dplyr::mutate(Date = stringr::str_remove(.data$Date, param_name),
                                   Date = stringr::str_remove(.data$Date, "X"),
                                   Date = as.numeric(.data$Date))

  }

  return(proc)

}

# ggplot plot for proc data

plot_proc <- function(proc, param_v, facet_col_var){

  mapping <- ggplot2::aes(.data$Date, .data[[param_v]], colour = .data[[facet_col_var]])

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
