
#' Get Landsat SR Products
#' @description This function gets 30-m USGS Landsat 8, 7 , 5 and 4 (Surface Reflectance Tier 1 (SR))
#' satellite images from various bands. The image processing for time series can either use methods by Roy et al. (2016) (e.g. harmonization) so that all three Landsat
#' missions will be scaled for a harmonized time series analysis ('harm_ts') or you can just get time series without harmonizing ('ts'). All the methods will use
#' Zhu et al. (2015) masking function and filter image properties by cloud cover less than 50% and Geometric RMSE Model less than 10.
#' @param aoi A sf object indicating the extent of the geom.
#' @param method A \code{character} indicating what method to use, e.g. 'ld8', 'ld7', 'ld5', 'ld4', 'ts', 'harm_ts'.
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue' or c('Green', 'Red', 'NIR') or NULL returns all bands.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'sd', 'first'.
#' @param cloud_mask \code{logical} whether to mask out certain cloud artifacts. TRUE (default).
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @note If you want to use this with \href{https://github.com/davemlz/eemont}{#eemont} functions then make sure \code{cloud_mask} is set to \code{FALSE}.
#' Also, 'harm_ts' and 'ts' can take a long time to process in other function \link[exploreRGEE]{viz}, \link[exploreRGEE]{rr} and \link[exploreRGEE]{band}  so be aware!
#' @return A list of Earth Engine Objects and arguments.
#' @details
#' The methods currently available (more to come):
#' \itemize{
#' \item  \strong{harm_ts}: Harmonizing Landsat Missions; 1984-01-01 - 2021-01-22.
#' \item  \strong{ts}: Combining Landsat Missions; 1984-01-01 - 2021-01-22.
#' \item \strong{ld8}: LANDSAT/LC08/C01/T1_SR; 2013-04-11 - 2021-01-22.
#' \item \strong{ld7}: LANDSAT/LC07/C01/T1_SR; 1999-01-01 - 2021-01-21.
#' \item \strong{ld5}: LANDSAT/LC05/C01/T1_SR; 1984-01-01 - 2012-05-05.
#' \item \strong{ld4}: LANDSAT/LC04/C01/T1_SR; 1982-08-22 - 1993-12-14.
#' }
#' The param (bands) currently available (more to come):
#' \itemize{
#' \item  \strong{Blue}, \strong{Green}, \strong{Red}, \strong{NIR},\strong{SWIR1},
#' \strong{SWIR2}, \strong{NDVI}, \strong{NDWI}, \strong{NBR}
#' }
#' @export
#'
#' @examples \dontrun{
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
#'
#' }
get_landsat <- function(aoi, method = "ld8", param = NULL, stat = "median", cloud_mask = TRUE, startDate = '1984-01-01', endDate = '2020-10-30',
                        mask = FALSE, m.low = NULL, m.high = NULL, c.low = 1, c.high = 12){


  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)

  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  col <- col_ld(c.low = c.low, c.high = c.high, geom = geom, startDate = startDate,
                endDate = endDate, method = method, cloud_mask = cloud_mask)

  if(!is.null(param)){

    col = col$select(param)

  }

  data <- data_stat(col, stat)

  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  landsat_list <- list(imageCol = col, image = data, geom = geom, method = method, param = param, stat = stat,
                       startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                       m.low = m.low, m.high = m.high,
                       aoi = aoi,
                       bbox = as.numeric(sf::st_bbox(aoi)))

  class(landsat_list) = c("landsat_list", "exploreList")
  return(landsat_list)

}


#' Get Meteorological Products
#'
#' @description This function gets you all the Meteorological data you want in a Earth Engine object. See details for available methods.
#'
#' @param aoi A sf object indicating the extent of the geom.
#' @param method \code{character}. 'Norm81m'(default), see details.
#' @param param \code{character}. NULL (default). Use the band names for appropriate dataset method, e.g. PRISM = 'ppt', GRIDMET = 'pr', DAYMET = 'prcp', etc.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#'
#' @details
#' The methods currently available (more to come):
#' \itemize{
#' \item  \strong{Norm81m}: PRISM 1981-2010; Monthly, annual average.
#' \item \strong{AN81m}: PRISM Jan 1895 - ongoing; Monthly annual time series.
#' \item \strong{AN81d}: PRISM Jan 1 1981 - ongoing; Daily time series.
#' \item \strong{GRIDMET}: 1979-01-01 - ongoing; The Gridded Surface Meteorological dataset provides high spatial resolution (~4-km) daily surface fields of temperature, precipitation, winds, humidity and radiation across the contiguous United States from 1979. \cr
#' \item  \strong{DAYMET}: 1980-01-01 - 2019-12-31; Daymet V3 provides gridded estimates of daily weather parameters for United States, Mexico, Canada, Hawaii, and Puerto Rico. It is derived from selected meteorological station data and various supporting data sources. \cr
#' \item \strong{TRMMh}: 1998-01-01 - 2020-12-31; The Tropical Rainfall Measuring Mission (TRMM) is a joint mission between NASA and the Japan Aerospace Exploration Agency (JAXA) designed to monitor and study tropical rainfall. The 34B2 product contains a gridded, TRMM-adjusted, merged infrared precipitation (mm/hr) and RMS precipitation-error estimate, with a 3-hour temporal resolution and a 0.25 degree spatial resolution. \cr
#' \item \strong{TRMMm}: 1998-01-01 - 2019-12-01; This collection is no longer being updated. See IMERG monthly. This dataset algorithmically merges microwave data from multiple satellites, including SSMI, SSMIS, MHS, AMSU-B and AMSR-E, each inter-calibrated to the TRMM Combined Instrument.
#' \item \strong{TERRACLIMATE}: 1958-01-01 - 2019-12-01; TerraClimate is a dataset of monthly climate and climatic water balance for global terrestrial surfaces.
#'}
#' @return A list of Earth Engine Objects and arguments.

#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' prism_monthly <- get_met(huc, method = 'AN81m', startDate = '2014-01-01',
#'                   endDate = '2018-12-31', c.low = 6, c.high = 11)
#'
#'
#' }

get_met <- function(aoi, method = "Norm81m", param = NULL, stat = "median", startDate = "1981-01-01", endDate = "2010-01-01",
                      mask = FALSE, m.low = NULL, m.high = NULL, c.low = 1, c.high = 12){

  if(method == "Norm81m") {

    startDate = "1981-01-01"
    endDate = "2010-01-01"
  }

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)
  }

  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

  collN <- coll_name(method = method)

  met <- ee$ImageCollection(collN)$filterBounds(geom)

  if(!is.null(param)){

    met <- ee$ImageCollection(collN)$select(param)

  }

  met <- met$filter(colFilter)

  met <- met$filterDate(startDate, endDate)

  data <- data_stat(met, stat)

  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }


  met_list <- list(imageCol = met, image = data, geom = geom, method = method, param = param, stat = stat,
                     startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                     m.low = m.low, m.high = m.high, bbox = as.numeric(sf::st_bbox(aoi)),
                   aoi = aoi)

  class(met_list) = c("met_list", "exploreList")
  return(met_list)
}


#' Get Sentinel-2 Products
#'
#' @description This function uses the Sentinel-2 missions.
#' @param aoi A sf object indicating the extent of the geom.
#' @param method A \code{character} indicating what method to use, e.g. 'S2_1C' (default) or 'S2_2A'.
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI', 'NBR', etc.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param cloud_mask \code{logical} whether to mask out certain cloud artifacts. TRUE (default).
#' @param startDate \code{character} format date, e.g. "2018-10-23"
#' @param endDate \code{character} format date, e.g. "2018-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @return A list of Earth Engine Objects and arguments.
#' @export
#'
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' sent2_2A_SCL <- get_sent2(huc, method = 'S2_2A', param = 'SCL', startDate = '2018-01-01',
#'                   endDate = '2020-12-31', c.low = 6, c.high = 11)
#'
#'
#' }
get_sent2 <- function (aoi, method = "S2_1C", param = NULL, stat = "median", cloud_mask = TRUE, startDate = "2015-04-01", endDate = "2020-01-01",
                       mask = FALSE, m.low = NULL, m.high = NULL, c.low = 1, c.high = 12) {

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)
  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  s2 <- col_s2(c.low, c.high, geom, startDate, endDate, cloud_mask, method)

  if(!is.null(param)){

    s2 = s2$select(param)

  }
  data <- data_stat(s2, stat)

  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  sent_list <- list(imageCol = s2, image = data, geom = geom, method = method, param = param, stat = stat,
                     startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                     m.low = m.low, m.high = m.high,
                     bbox = as.numeric(sf::st_bbox(aoi)), aoi = aoi)

  class(sent_list) = c("sent2_list", "exploreList")
  return(sent_list)
}

#' Get Net Annual NPP (CONUS)
#'
#' @description This function gets Net Primary Productivity products for selected time frames. Cloud masking will be at 50% if TRUE; however, for 'ld_NPP' 255 will be masked out.
#'
#' @param aoi A sf object indicating the extent of the geom.
#' @param method A \code{character} indicating what method to use, e.g. 'ld_NPP', 'modis_NPP', 'terra', 'aqua'.
#' @param param A \code{character} indicating what band to visualize, e.g. 'annualNPP', 'Npp', etc.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param cloud_mask \code{logical} whether to mask out certain cloud artifacts. TRUE (default).
#' @param startDate \code{character} format date, e.g. "2018-10-23"
#' @param endDate \code{character} format date, e.g. "2018-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @details
#' The methods currently available:
#' \itemize{
#' \item  \strong{ld_NPP}: UMT/NTSG/v2/LANDSAT/NPP; 1986-01-01 - 2019-01-01.
#' \item  \strong{modis_NPP}: UMT/NTSG/v2/MODIS/NPP; 2001-01-01 - 2019-01-01.
#' \item \strong{terra}: MODIS/006/MOD17A3HGF; 2001-01-01 - 2019-01-01.
#' \item \strong{aqua}: MODIS/006/MYD17A3HGF; 2002-01-01 - 2019-01-01.
#' }
#' @return A list of Earth Engine Objects and arguments.
#' @export
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' npp <- get_npp(huc, method = 'ld_NPP', param = 'annualNPP', startDate = '2014-01-01',
#'                   endDate = '2018-12-31')
#'
#'
#' }

get_npp <- function(aoi, method = "ld_NPP", param = NULL, stat = "median", cloud_mask = TRUE, startDate = "1986-01-01", endDate = "2019-01-01",
                    mask = FALSE, m.low = NULL, m.high = NULL, c.low = 1, c.high = 12){

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)
  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  if(is.null(param)){

    col_med <- npp_med(geom, startDate, endDate, method, cloud_mask)

  } else {

  col_med <- npp_med(geom, startDate, endDate, method, cloud_mask)$select(param)

  }
  data <- data_stat(col_med, stat)

  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  npp_list <- list(imageCol = col_med, image = data, geom = geom, method = method, param = param, stat = stat,
                    startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                    m.low = m.low, m.high = m.high,
                    bbox = as.numeric(sf::st_bbox(aoi)), aoi = aoi)

  class(npp_list) = c("npp_list", "exploreList")
  return(npp_list)


}



#' Get Image Difference
#' @description This function allows the user to get differences by subtracting two images. Only works for one-band selections and
#' from different time series.
#' @param data A previously create get_* object
#' @param startDate2 \code{character} format date, e.g. "1999-10-23". A second start date to use as the subtraction window.
#' @param endDate2 \code{character} format date, e.g. "1999-10-23". A second end date to use as the subtraction window.
#' @param band A \code{character} indicating what bands/type to use when you have more than one. Can \strong{only} select one, e.g. 'NDVI'.
#' @return A list of Earth Engine Objects and arguments.
#' @export
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' # get intitial image. Can by any get_*().
#'
#' ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
#'                   endDate = '2018-12-31', c.low = 6, c.high = 11)
#'
#' # now subtract from new one
#'
#' diff <- ld8 %>% get_diff(startDate2 = '2019-01-01', endDate2 = '2019-12-31', band = 'NDVI')
#'
#' # now viz, rr, band, etc.
#'
#' diff %>% viz(scale = 30)
#' }

get_diff <- function(data, startDate2 = '2010-01-01', endDate2 = '2018-10-30', band = NULL){

  if(missing(data))stop({"Need a previously created get_* object as 'data'."})
  if(class(data) == 'terrain_list')stop({"Can't use terrain for this function."})

  aoi <- data$aoi
  imageCol <- data$imageCol
  image <- data$data
  geom <- data$geom
  stat <- data$stat
  method <- data$method
  startDate <- data$startDate
  endDate <- data$endDate
  param <- data$param
  c.low <- data$c.low
  c.high <- data$c.high
  bbox <- data$bbox
  mask <- data$mask
  m.low <- data$m.low
  m.high <- data$m.high

  if(is.null(param) & is.null(band)){stop("need to select only one band!")}

  if(!is.null(band)){param <- band}


image_col2 <- class_type(data,aoi = aoi,method = method, param = param, stat = stat,
                        startDate = startDate2, endDate = endDate2, c.low = c.low, c.high = c.high, mask = mask,
                        m.low = m.low, m.high = m.high)

  image2 <- image_col2$data

    image = image$select(param)

    image2 = image2$select(param)

  final_image <- image2$subtract(image)

  diff_list <- list(imageCol = list(imageCol, image_col2$imageCol), image = final_image, geom = geom, method = method, param = param, stat = stat,
                     startDate = startDate, endDate = endDate, startDate2 = startDate2, endDate2 = endDate2, c.low = c.low, c.high = c.high,
                     bbox = as.numeric(sf::st_bbox(aoi)), aoi = aoi)

  class(diff_list) = c("diff_list", "exploreList")
  return(diff_list)

}
#' Get Terrain Products
#' @description This function takes the USGS NED (National Elevation Dataset)
#' or SRTM (Shuttle Radar Topography Mission) and gets a handful of terrain indices. This is good for
#' downloaded areas for further analysis or passing on for some quick stats.
#' @param aoi A sf object indicating the extent of the geom.
#' @param method \code{character} indicating what method to use, e.g. 'NED', 'SRTM'.
#' @param param \code{character} indicating terrain type, e.g. 'dem', 'FA', 'TWI', 'TRI', 'TPI', 'aspect', 'slope', 'cos', 'sin', 'hillshade', 'complete'.
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param az \code{numeric} The illumination azimuth in degrees from north.
#' @param el \code{numeric} The illumination elevation in degrees.
#' @return A list of Earth Engine Objects and arguments.
#' @export
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' # get terrain
#'
#' twi <- get_terrain(huc, method = 'NED', param = 'TWI')
#' }

get_terrain <- function(aoi, method = "NED", param = "slope",
                        mask = FALSE, m.low = NULL, m.high = NULL, az = 270, el = 45){

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)
  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)
  reducers <- reducers()

  if(method == "NED"){

    dem <- rgee::ee$Image("USGS/NED")$resample('bicubic')

  } else if (method == "SRTM") {

    dem = rgee::ee$Image("USGS/SRTMGL1_003")$resample('bicubic')
  }


  if(param == "slope"){

      data <- rgee::ee$Terrain$slope(dem)

  } else if (param == "sin"){


    #Get the aspect (in degrees).
    aspect = rgee::ee$Terrain$aspect(dem)

    #Convert to radians, compute the sin of the aspect.

    data = aspect$divide(180)$multiply(pi)$sin()



  }else if (param == "cos"){


    #Get the aspect (in degrees).
    aspect = rgee::ee$Terrain$aspect(dem)

    #Convert to radians, compute the cosine of the aspect.

    data = aspect$divide(180)$multiply(pi)$cos()



  } else if (param == "hillshade"){



    data = rgee::ee$Terrain$hillshade(dem, az)



  } else if (param == "TPI"){

    boxcar <- rgee::ee$Kernel$square(8, "pixels", T)

    # Smooth the image by convolving with the boxcar kernel.
    smooth <- dem$convolve(boxcar)

    data <- dem$subtract(smooth)


  } else if (param == "TRI"){

    mean = dem$reduceNeighborhood(
      reducer = rgee::ee$Reducer$mean(),
      kernel = rgee::ee$Kernel$square(3, "pixels", T)
    )

    sd = dem$reduceNeighborhood(
      reducer = rgee::ee$Reducer$stdDev(),
      kernel = rgee::ee$Kernel$square(3, "pixels", T)
    )

    data <- mean$divide(sd)

  } else if (param == "complete"){

    data <- rgee::ee$Terrain$products(dem)

  } else if (param == "aspect"){


    #Get the aspect (in degrees).
    data = rgee::ee$Terrain$aspect(dem)

  }else if (param == "TWI"){

    dem = rgee::ee$Image("USGS/SRTMGL1_003")

    slope <- rgee::ee$Terrain$slope(dem)

    fa <- rgee::ee$Image("MERIT/Hydro/v1_0_1")$select('upg')

    faProj <- fa$projection()

    slope <- rgee::ee$Terrain$slope(dem)

    #convert from degrees to radians

    slope <- slope$divide(180)$multiply(base::pi)

    slopeMean = slope$reduceResolution(
      reducer = rgee::ee$Reducer$mean()
    )$reproject(
      crs = faProj
    )

    image = fa$addBands(slopeMean)

    image = image$addBands(image$select('slope')$tan())
    image = image$addBands(image$select('upg')$divide(image$select('slope_1')))


    data = image$select('upg_1')$log()

  } else if (param == "FA") {

    data <- rgee::ee$Image("MERIT/Hydro/v1_0_1")$select('upg')


  } else if (param == "dem") {

    data = dem
  }

  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  terrain_list <- list(imageCol = NULL, image = data, geom = geom, method = method, param = param, stat = NULL,
                    startDate = NULL, endDate = NULL, c.low = NULL, c.high = NULL, mask = mask, m.low = m.low, m.high = m.high,
                    bbox = as.numeric(sf::st_bbox(aoi)), aoi = aoi)

  class(terrain_list) = c("terrain_list",, "exploreList")
  return(terrain_list)
}


#' Get Earth Engine Products
#' @description This function allows the user to provide a earth engine image/imageCollection \code{character} string which will help with simple processing.
#' @param aoi A sf object indicating the extent of the geom.
#' @param image_type A \code{character} indicating what type of image, e.g. 'ImageCollection' or 'Image'.
#' @param method A \code{character} indicating what imageCollection to use, e.g. "UMD/hansen/global_forest_change_2019_v1_7".
#' @param param A \code{character} indicating what band to select.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @note Use how you would normally call a GEE session online but with method as your collection snippet.
#' @return A list of Earth Engine Objects and arguments.

#' @export
#'
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' # get any Image or ImageCollection
#'
#' forest_cover_loss <- get_any(huc, image_type = "Image",
#'                              method = 'UMD/hansen/global_forest_change_2019_v1_7',
#'                              param = 'lossyear')
#' }
#'
get_any <- function(aoi, image_type = "ImageCollection", method, param = NULL, stat = "median", startDate = NULL, endDate = NULL,
                        mask = FALSE, m.low = NULL, m.high = NULL, c.low = NULL, c.high = NULL){


  if(missing(method)){stop("Need an Image/Image Collection to proceed")}

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)

  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  if(image_type == 'ImageCollection') {

  col = rgee::ee$ImageCollection(method)$filterBounds(geom)

  calRange = rgee::ee$Filter$calendarRange(c.low,c.high, 'month')

  if(!is.null(c.low) & !is.null(c.high)){
  col <- col$filter(calRange)
  }

  if(!is.null(startDate) & !is.null(endDate)){
   col <- col$filterDate(startDate, endDate)
  }

  if(!is.null(param)){

    col = col$select(param)

  }

  data <- data_stat(col, stat)

  } else if (image_type == 'Image'){

    if(!is.null(param)){

      data = ee$Image(method)$select(param)

    } else {

      data = ee$Image(method)

    }

  } else {

    stop("Need Image or ImageCollection")
  }


  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  any_list <- list(imageCol = col, image = data, geom = geom, method = method, param = param, stat = stat,
                       startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                       m.low = m.low, m.high = m.high,
                       aoi = aoi,
                       bbox = as.numeric(sf::st_bbox(aoi)))

  class(any_list) = c("any_list", "exploreList")
  return(any_list)

}

#' NASA Earth Exchange Products
#' @description This function allows the user to get NASA Earth Exchange products.
#' @param aoi A sf object indicating the extent of the geom.
#' @param method A \code{character} indicating what imageCollection to use, e.g. "ensemble" is equal to = "NASA/NEX-DCP30_ENSEMBLE_STATS".
#' @param scenario A \code{character} indicating what Representative Concentration Pathways (RCPs) to use, e.g. 'rcp85' (default).
#' @param model A \code{character} indicating what model to use (only for 'dcp' and 'gddp'). 'ACCESS1-0' (default).
#' @param param A \code{character} indicating what band to select.
#' @param stat A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param mask \code{logical} whether to mask out certain ranges
#' @param m.low \code{numeric} low value for mask, e.g. greater than 'm.low'
#' @param m.high \code{numeric} high value for mask, e.g. less than 'm.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @note \code{scenario} only uses \code{dcp} and \code{gddp} so disregard when using \code{ensemble} and be aware when using the others. In addition, it is up to the users to understand what model to use with
#' what scenario! Some scenarios and models don't jive so do your research.
#' @return A list of Earth Engine Objects and arguments.
#' @details
#' The \code{method}, \code{scenario} and \code{model} currently available:
#' \itemize{
#' \item  \strong{method}: ensemble, dcp, gddp.
#' \item  \strong{scenario}: 'historical', 'rcp26', 'rcp45', 'rcp60', 'rcp85'
#' \item \strong{model}: ACCESS1-0', 'bcc-csm1-1', 'bcc-csm1-1-m', 'BNU-ESM', 'CanESM2', 'CCSM4', 'CESM1-BGC', 'CESM1-CAM5', 'CMCC-CM', 'CNRM-CM5', 'CSIRO-Mk3-6-0', 'FGOALS-g2', 'FIO-ESM', 'GFDL-CM3', 'GFDL-ESM2G', 'GFDL-ESM2M', 'GISS-E2-H-CC', 'GISS-E2-R', 'GISS-E2-R-CC', 'HadGEM2-AO', 'HadGEM2-CC', 'HadGEM2-ES', 'inmcm4', 'IPSL-CM5A-LR', 'IPSL-CM5A-MR', 'IPSL-CM5B-LR', 'MIROC5', 'MIROC-ESM', 'MIROC-ESM-CHEM', 'MPI-ESM-LR', 'MPI-ESM-MR', 'MRI-CGCM3', 'NorESM1-M'
#' }
#' @export
#'
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' # get any Image or ImageCollection
#'
#' forest_cover_loss <- get_any(huc, method = 'ensemble',
#'                              scenario = 'rcp85',
#'                              model = 'ACCESS1-0',
#'                              method = 'NASA/NEX-DCP30_ENSEMBLE_STATS',
#'                              param = 'lossyear')
#' }
#'
get_nex <- function(aoi, method = 'ensemble', scenario = 'rcp85', model = 'ACCESS1-0', param = NULL, stat = "median", startDate = NULL, endDate = NULL,
                    mask = FALSE, m.low = NULL, m.high = NULL, c.low = NULL, c.high = NULL){


  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)

  }
  aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
  geom <- setup(aoi)

  if(method == 'ensemble') {

    col = ee$ImageCollection('NASA/NEX-DCP30_ENSEMBLE_STATS')$
      filter(ee$Filter$eq('scenario', scenario))

  } else if (method == 'dcp'){

    col = ee$ImageCollection("NASA/NEX-DCP30")$
      filter(ee$Filter$eq('scenario', scenario))$
      filter(ee$Filter$eq('model', model))

  } else if (method == 'gddp'){

    col = ee$ImageCollection('NASA/NEX-GDDP')$
      filter(ee$Filter$eq('scenario', scenario))$
      filter(ee$Filter$eq('model', model))
  }

    col = col$filterBounds(geom)

    calRange = rgee::ee$Filter$calendarRange(c.low,c.high, 'month')

    if(!is.null(c.low) & !is.null(c.high)){
      col <- col$filter(calRange)
    }

    if(!is.null(startDate) & !is.null(endDate)){
      col <- col$filterDate(startDate, endDate)
    }

    if(!is.null(param)){

      col = col$select(param)

    }

    data <- data_stat(col, stat)


  if(mask == TRUE) {

    data_m <- data$gt(m.low)$And(data$lt(m.high))

    data <- data$updateMask(data_m)

  }

  any_list <- list(imageCol = col, image = data, geom = geom, method = method, param = param, stat = stat,
                   startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                   m.low = m.low, m.high = m.high,
                   aoi = aoi,
                   bbox = as.numeric(sf::st_bbox(aoi)))

  class(any_list) = c("nex_list", "exploreList")
  return(any_list)

}

#' Get Linear Regression
#' @description This function allows the user to get a linearFit() from an ImageCollection.
#' @param data A previously create get_* object.
#' @param band A \code{character} indicating what bands/type to use when you have more than one. Can \strong{only} select one, e.g. 'NDVI'.
#' @param stat A \code{character} indicating what to reduce the imageCollection when using temporal filtering, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.
#' @param temporal A \code{character} indicating what temporal filter to use on the collection, e.g. 'yearly' (default), 'year_month', 'all'.
#' @return A list of Earth Engine Objects and arguments. Specifically an ImageCollection with
#' bands c('scale', 'offset').
#' @export
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc
#'
#' # get intitial image. Can by any get_*().
#'
#' ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
#'                   endDate = '2018-12-31', c.low = 6, c.high = 11)
#'
#' # now subtract from new one
#'
#' lr <- ld8 %>% get_linear(band = 'NDVI', temporal = 'yearly')
#'
#' lr %>% viz(band = 'scale')
#' }

get_linear <- function(data, method = 'lfit', band = NULL, stat = 'median', temporal = 'yearly'){

  if(missing(data))stop({"Need a previously created get_* object as 'data'."})

    aoi <- data$aoi
    imageCol <- data$imageCol
    geom <- data$geom
    param <- data$param
    startDate <- data$startDate
    endDate <- data$endDate
    c.low <- data$c.low
    c.high <- data$c.high
    bbox <- data$bbox


  if(is.null(param) & is.null(band)){stop("need to select only one band!")}

  if(!is.null(band)){param <- band}

  collection = imageCol$select(param)

  time_band = function(image){

    return(image$addBands(image$metadata('system:time_start')$divide(3.1536e10)))

  }
  collection = collection$map(time_band)

  if (temporal == 'yearly'){

      collection <- year_filter(startDate = startDate, endDate = endDate,
                            imageCol = collection, stat = stat)
      ind_dep <- c('system:time_start', param)

  } else if (temporal == 'year_month'){

    collection <- year_month_filter(startDate = startDate, endDate = endDate,
                                    imageCol = collection, stat = stat, c.low = c.low,
                                    c.high = c.high)
    ind_dep <- c(paste0('system:time_start_',stat), paste0(param, '_',stat))

  } else {

    ind_dep <- c('system:time_start', param)

  }


  trend <- collection$select(ind_dep)$reduce(ee$Reducer$linearFit())

  linear_list <- list(imageCol = collection, image = trend, geom = geom, method = method, param = NULL, stat = stat,
                    startDate = startDate, endDate = endDate,c.low = c.low, c.high = c.high,
                    bbox = as.numeric(sf::st_bbox(aoi)), aoi = aoi)

  class(linear_list) = c("linear_list", "exploreList")
  return(linear_list)

}
