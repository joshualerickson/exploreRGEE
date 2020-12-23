
#' Visualize Sentinel-1 SAR GRD: C-band
#' @description The Sentinel-1 mission provides data from a
#'  dual-polarization C-band Synthetic Aperture Radar (SAR) instrument.
#' This collection includes the S1 Ground Range Detected (GRD) scenes,
#' processed using the Sentinel-1 Toolbox to generate a calibrated,
#' ortho-corrected product. The collection is updated daily. New assets are ingested within two days after they become available.
#' @param aoi A sf object
#' @param param A \code{character} indicating what band to visualize, e.g. 'NPOL', 'VV', 'VVsd'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} indicating in meters what to scale the statistics by, e.g. min/max.

#'
#' @examples
viz_Sent1 <- function(){


  bb <-  sf::st_bbox(aoi)
  geom <- ee$Geometry$Rectangle(bb)

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
  )

  #filter image by bounds and metadata

  s1 <- ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(geom)$filterMetadata('transmitterReceiverPolarisation', 'equals', c('VV', 'VH'))$filterMetadata('resolution_meters', 'equals' , 10)
  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
  s1 = s1$filter(calRange)
  s1 = s1$filterDate(startDate, endDate)


  if (param == "NPOL") {


  #Functions to mask out edges of images using using angle
  maskAngGT30 = function(image) {
    ang = image$select('angle')
    return(image$updateMask(ang$gt(30.63993)))
  }
  maskAngLT45 = function(image) {
    ang = image$select('angle')
    return(image$updateMask(ang$lt(45.53993)))
  }

 #Apply angle masking functions to image collection
 s1pol = s1$map(maskAngGT30)
  s1pol = s1pol$map(maskAngLT45)

  #Function to filter out windy days using climate forecasts
  pctWat = function(image){
   d = image$date()$format('Y-M-d')
    wx = ee$ImageCollection('NOAA/CFSV2/FOR6H')$filterDate(d)
    vWind = wx$select('v-component_of_wind_height_above_ground')
    a = vWind$max()
    uWind = wx$select('u-component_of_wind_height_above_ground')
    b = uWind$max()
    a = a$pow(2)
    b = b$pow(2)
    ab = a$add(b)
    ws = ab$sqrt()
    ws = ws$multiply(3.6)
    return(image$updateMask(ws$lt(12)))
  }

 #Apply windy day filter to image collection
  s1pol = s1pol$map(pctWat)

  #Function to perform angle correction
  toGamma0 = function(image) {
    vh = image$select('VH')$subtract(image$select('angle')$multiply(base::pi/180)$cos()$log10()$multiply(10))
    return(vh$addBands(image$select('VV')$subtract(image$select('angle')$multiply(base::pi/180)$cos()$log10()$multiply(10))))
  }
  #Apply angle correction
  s1pol = s1pol$map(toGamma0)

  #Function to add band containing normalized difference
 #between VH and VV
  adddiff = function(image) {
    return(image$addBands(image$expression(
      expression = '(VH - VV) / (VH + VV)', opt_map = list(VH = image$select('VH'), VV = image$select('VV')))))

  }

   # Add normalized difference band to image collection and
 #create new variable of this band
  s1pol = s1pol$map(adddiff)
  NPOL = s1pol$select('VH_1')

 #Function to create boxcar 3 x 3 pixel filter
 boxcar = ee$Kernel$circle(3,'pixels', T)

 #Function to apply boxcar filter
  fltr = function(image) {
    return(image$convolve(boxcar))
  }

  #Apply 3 x 3 pixel mean filter to Pol image
  NPOL = NPOL$map(fltr)
  NPOL = NPOL$median()


  stats <- NPOL$reduceRegions(
    reducer = reducers,
    collection = geom,
    scale = scale
  )

  cat("Band: ", param, "\n")

  print(stats$getInfo()$features[[1]]$properties)

  st_max <- stats$getInfo()$features[[1]]$properties$max

  st_min <- stats$getInfo()$features[[1]]$properties$min


  mlay <- Map$addLayer(NPOL$clip(geom), visParams = list(min = st_min, max = st_max))


  } else if (param == "VV") {


    #Function to mask out edges of images using using angle
   #(mask out angles >= 45.23993)
    maskAngLT452 = function(image) {
       ang = image$select('angle')
      return(image$updateMask(ang$lt(45.23993)))
    }
    maskAngGT30 = function(image) {
      ang = image$select('angle')
      return(image$updateMask(ang$gt(30.63993)))
    }
  #Apply angle masking functions to image collection
    s1vv = s1$map(maskAngGT30)
    s1vv = s1vv$map(maskAngLT452)

    #Function to apply angle correction (for VV)
    toGamma01 = function(image) {
      return(image$select('VV')$subtract(image$select('angle')$multiply(base::pi/180)$cos()$log10()$multiply(10L)))
    }
 #Apply angle correciton to image collection
   s1vv = s1vv$map(toGamma01)

     #Sigma Lee speckle filtering

      toNatural = function(img) {
        return(ee$Image(10)$pow(img$select(0)$divide(10)))
      }
    toDB = function(img) {
      return(ee$Image(img)$log10()$multiply(10))
    }
   #The RL speckle filter from
  #https://code.earthengine.google.com/2ef38463ebaf5ae133a478f173fd0ab5
   # by Guido Lemoine

    s1vv <- s1vv$filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$filterMetadata('resolution_meters', 'equals' , 10)

   RefinedLee = function(img) {
      # img must be in natural units, i.e. not in dB!
        # Set up 3x3 kernels
      weights3 = ee$List$'repeat'(ee$List$'repeat'(1,3),3)
      kernel3 = ee$Kernel$fixed(3,3, weights3, 1, 1, F)
      mean3 = img$reduceNeighborhood(ee$Reducer$mean(), kernel3)
     variance3 = img$reduceNeighborhood(ee$Reducer$variance(), kernel3)

      # Use a sample of the 3x3 windows inside a 7x7 windows to determine gradients
     #and directions
      sample_weights = ee$List(list(c(0,0,0,0,0,0,0), c(0,1,0,1,0,1,0),
                                    c(0,0,0,0,0,0,0), c(0,1,0,1,0,1,0), c(0,0,0,0,0,0,0), c(0,1,0,1,0,1,0),
                                    c(0,0,0,0,0,0,0)))
      sample_kernel = ee$Kernel$fixed(7,7, sample_weights, 3,3, F)

     #Calculate mean and variance for the sampled windows and store as 9 bands
      sample_mean = mean3$neighborhoodToBands(sample_kernel)
      sample_var = variance3$neighborhoodToBands(sample_kernel)

   # Determine the 4 gradients for the sampled windows
      gradients = sample_mean$select(1)$subtract(sample_mean$select(7))$abs()

      gradients = gradients$addBands(sample_mean$select(6)$subtract(sample_mean$select(2))$abs())
      gradients = gradients$addBands(sample_mean$select(3)$subtract(sample_mean$select(5))$abs())
      gradients = gradients$addBands(sample_mean$select(0)$subtract(sample_mean$select(8))$abs())

   # And find the maximum gradient amongst gradient band
      max_gradient = gradients$reduce(ee$Reducer$max())
 #Create a mask for band pixels that are the maximum gradient
      gradmask = gradients$eq(max_gradient)

       # duplicate gradmask bands: each gradient represents 2 directions
      gradmask = gradmask$addBands(gradmask)
  #Determine the 8 directions
      directions = sample_mean$select(1)$subtract(sample_mean$select(4))$gt(sample_mean$select(4)$subtract(sample_mean$select(7)))$multiply(1)
      directions = directions$addBands(sample_mean$select(6)$subtract(sample_mean$select(4))$gt(sample_mean$select(4)$subtract(sample_mean$select(2)))$multiply(2))
      directions = directions$addBands(sample_mean$select(3)$subtract(sample_mean$select(4))$gt(sample_mean$select(4)$subtract(sample_mean$select(5)))$multiply(3))
      directions = directions$addBands(sample_mean$select(0)$subtract(sample_mean$select(4))$gt(sample_mean$select(4)$subtract(sample_mean$select(8)))$multiply(4))

      #The next 4 are the not() of the previous 4
      directions = directions$addBands(directions$select(0)$Not()$multiply(5))
      directions = directions$addBands(directions$select(1)$Not()$multiply(6))
      directions = directions$addBands(directions$select(2)$Not()$multiply(7))
      directions = directions$addBands(directions$select(3)$Not()$multiply(8))

       # Mask all values that are not 1-8
      directions = directions$updateMask(gradmask)

      #"collapse" the stack into a singe band image (due to masking, each pixel
                                                 #has just one value (1-8) in it's directional band, and is otherwise masked)

      directions = directions$reduce(ee$Reducer$sum())

#Generate stats
         sample_stats = sample_var$divide(sample_mean$multiply(sample_mean))
# Calculate localNoiseVariance
       #sigmaV = sample_stats$toArray()$arraySort()$arraySlice(0,0,5)$arrayReduce(ee$Reducer$mean(), 0)
       sigmaV <- sample_stats$toArray()$arraySort()
       sigmaV <- sigmaV$arraySlice(0,0,5)
       sigmaV <- sigmaV$reduce(
         reducer = ee$Reducer$mean())

   #Set up the 7*7 kernels for directional statistics
      rect_weights = ee$List$'repeat'(ee$List$'repeat'(0,7),3)$cat(ee$List$'repeat'(ee$List$'repeat'(1,7),4))

#Set weights
        diag_weights = ee$List(list(c(1,0,0,0,0,0,0, c(1,1,0,0,0,0,0), c(1,1,1,0,0,0,0),c(1,1,1,1,0,0,0), c(1,1,1,1,1,0,0), c(1,1,1,1,1,1,0), c(1,1,1,1,1,1,1))))

 rect_kernel = ee$Kernel$fixed(7,7, rect_weights, 3, 3, F)
 diag_kernel = ee$Kernel$fixed(7,7, diag_weights, 3, 3, F)
# Create stacks for mean and variance using the original kernels.
# Mask with relevant direction.

 dir_mean = img$reduceNeighborhood(ee$Reducer$mean(), rect_kernel)$updateMask(directions$eq(1))

 dir_var = img$reduceNeighborhood(ee$Reducer$variance(), rect_kernel)$updateMask(directions$eq(1))

   #and add the bands for rotated kernels
for (i in 1:4) {
dir_mean = dir_mean$addBands(img$reduceNeighborhood(ee$Reducer$mean(),
rect_kernel$rotate(i))$updateMask(directions$eq(2*i+1)))

dir_var = dir_var$addBands(img$reduceNeighborhood(ee$Reducer$variance(),
rect_kernel$rotate(i))$updateMask(directions$eq(2*i+1)))

dir_mean = dir_mean$addBands(img$reduceNeighborhood(ee$Reducer$mean(),
diag_kernel$rotate(i))$updateMask(directions$eq(2*i+2)))

dir_var = dir_var$addBands(img$reduceNeighborhood(ee$Reducer$variance(),
diag_kernel$rotate(i))$updateMask(directions$eq(2*i+2)))
}
#"collapse" the stack into a single band image (due to masking, each pixel
#has just one value in it's directional band, and is otherwise masked)

      dir_mean = dir_mean$reduce(ee$Reducer$sum())
      dir_var = dir_var$reduce(ee$Reducer$sum())
     # A finally generate the filtered value
      varX = dir_var$subtract(dir_mean$multiply(dir_mean)$multiply(sigmaV))$divide(sigmaV$add(ee$Number(1)))
       b = varX$divide(dir_var)
     result = dir_mean$add(b$multiply(img$subtract(dir_mean)))
      return(result$arrayFlatten('sum'))
    }
 #----------------------------End Sigma Lee Filtering-------------------------//
 #Apply three functions as part of Sigma Lee filtering
  s1vv = s1vv$map(toNatural)
   s1vv = s1vv$map(RefinedLee)
   s1vv = s1vv$map(toDB)
#Extract mean VV and VVsd input variables
   VV = s1vv$mean()
   VVsd = s1vv$reduce(ee$Reducer$stdDev())
# Apply 3 x 3 pixel mean filter to VV and VVsd images
  fltr = function(image) {
    return(image$convolve(boxcar))
  }
    VVfltrd = VV$map(fltr)
   VVfltrd = fltr(VV)
   VVsdfltrd = fltr(VVsd)


  }
} #end curly
