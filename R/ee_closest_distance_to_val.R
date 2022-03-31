
ee_closest_distance_to_val <-  function(x,...){
  UseMethod('ee_closest_distance_to_val')


}


ee_closest_distance_to_val.ee.imagecollection.ImageCollection <-  function(x,
                                                                           y,
                                                                           boolean_cond="=",
                                                                           val=2,
                                                                           scale
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
  res <- exploreRGEE::ee_timeseries(imageCol = euclidean_distance_to_x$select("distance_to"),geom=sf,temporal="all",scale=30)
  return(res)
}

JRC_example$first() |> class()
ee_closest_distance_to_val.ee.image.Image<-  function(x,
                                                      y,
                                                      boolean="=",
                                                      val=2,
                                                      scale
){

  # stopifnot(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.image.Image"))
  boolean_mask_cond<-switch_boolean(boolean=boolean,val=val)

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
  res <- exploreRGEE::ee_timeseries(imageCol = euclidean_distance_to_x$select("distance_to"),geom=sf,temporal="all",scale=30)
  return(res)
}
