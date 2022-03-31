#' ee_closest_distance_to_val
#' @param x ee$Image or ee$ImageCollection
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection, sfc or sf objects.
#' @param val \code{numeric} pixel value of interest
#' @param scale \code{numeric} scale in meters
#' @return data.frame containg y with closest distance column ("distance_to") containing
#' closest distance to pixel value specified for each record.
#' @export
#' @examples \dontrun{
#'
#' # Load Libraries
#'
#' library(rgee)
#' library(tidyverse)
#' library(sf)
#' ee_Initialize()
#' library(exploreRGEE)
#'
#' # Bring in data
#' huc <- exploreRGEE::huc |> st_centroid()
#' #GET YEARLY WATER OCCURENCE IMAGE COLLECTION FROM GEE - JRC
#' water_yearly <- rgee::ee$ImageCollection("JRC/GSW1_3/YearlyHistory")
#' # FILTER DATES 2000-2019
#' water_years_filtered <- water_yearly$
#'  filterDate("2000-01-01","2019-12-31")
#'
#' distance_to_water <- ee_closest_distance_to_val(x = water_years_filtered,
#'                                                  y = aoi,
#'                                                  boolean_cond = ">=",
#'                                                  val = 2,
#'                                                  scale = 30)
#'
#' }

ee_closest_distance_to_val <-  function(x,...){
  UseMethod('ee_closest_distance_to_val')


}




ee_closest_distance_to_val.ee.imagecollection.ImageCollection <-  function(x,
                                                                           y,
                                                                           boolean_cond="=",
                                                                           val=2,
                                                                           scale=30
){

  # stopifnot(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  boolean_mask_cond<-switch_boolean(boolean_cond=boolean_cond,val=val)

  # if(is.numeric(val)){}
  # cat(crayon::green("masking x image/imageCollection\n"))
  x_masked <- x$map(
    function(image){
      image_masked = boolean_mask_cond(image)$
        selfMask()
      return(ee$Image(image_masked$copyProperties(image,image$propertyNames())))

    }
  )

  cat(crayon::green("Generating distance raster(s)\n"))
  euclidean_distance_to_x <-  x_masked$
    map(
      function(image){
        distances = image$mask()$
          fastDistanceTransform({
            neighborhood=1024
          })$multiply(ee$Image$pixelArea())$sqrt()$rename("distance_to")$
          reproject(crs="EPSG:4326",scale=scale)
        return(ee$Image(distances$copyProperties(image,image$propertyNames())))

      }
    )
  cat(crayon::green("Extracting distance raster values to y\n"))
  res <- exploreRGEE::ee_timeseries(imageCol = euclidean_distance_to_x$select("distance_to"),geom=y,temporal="all",scale=scale)
  return(res)
}


ee_closest_distance_to_val.ee.image.Image<-  function(x,
                                                      y,
                                                      boolean_cond="=",
                                                      val=2,
                                                      scale=30
){

  # stopifnot(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.image.Image"))
  boolean_mask_cond<-switch_boolean(boolean_cond=boolean_cond,val=val)

  # if(is.numeric(val)){}
  cat(crayon::green("masking x image/imageCollection\n"))
  x_masked <-ee$Image(
    boolean_mask_cond(x)$
      selfMask()$
      copyProperties(x,x$propertyNames())
  )
  cat(crayon::green("Generating distance raster(s)\n"))

  FDT = x_masked$mask()$
    fastDistanceTransform({
      neighborhood=1024
    })

  distances= FDT$
    multiply(ee$Image$pixelArea())$
    sqrt()$
    rename("distance_to")$
    reproject(crs="EPSG:4326",scale=scale)

  euclidean_distance_to_x <-ee$Image(distances$copyProperties(x_masked,x_masked$propertyNames()))

  cat(crayon::green("Extracting distance raster values to y\n"))
  res <- exploreRGEE::ee_timeseries(imageCol = euclidean_distance_to_x$select("distance_to"),geom=y,temporal="all",scale=scale)
  return(res)
}
