
########## Sent-2 section ##################

#get sent 2 cleaned

col_s2 <- function(c.low, c.high, geom, startDate, endDate, cloud_mask, method){

  if(method == "S2_1C"){

    s2 <- rgee::ee$ImageCollection('COPERNICUS/S2')$filterBounds(geom)

  } else if (method == "S2_2A"){

    s2 <- rgee::ee$ImageCollection('COPERNICUS/S2_SR')$filterBounds(geom)

  }

  calRange = rgee::ee$Filter$calendarRange(c.low,c.high, 'month')

  s2 = s2$filter(calRange)

  s2 = s2$filterDate(startDate, endDate)

if(isTRUE(cloud_mask)){ s2 = s2$map(maskcloud1)}

  s2 = s2$map(addNDVIsent)

  s2 = s2$map(addNDWIsent)

  s2 = s2$map(calcNbrsent)

  if(method == "S2_1C"){

    s2 = s2$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI", "NBR"),
                   c("B1","Blue","Green","Red","B5","B6" ,"B7","NIR","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI", "NBR"))

  } else if (method == "S2_2A"){

    s2 = s2$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B11","B12","AOT","WVP","SCL","TCI_R","TCI_G", "TCI_B", "MSK_CLDPRB","MSK_SNWPRB",
                     "QA10","QA20","QA60","NDVI", "NDWI", "NBR"),
                   c("B1","Blue","Green","Red","B5","B6" ,"B7","NIR","B8A","B9","B11","B12","AOT","WVP","SCL","TCI_R","TCI_G", "TCI_B", "MSK_CLDPRB","MSK_SNWPRB",
                     "QA10","QA20","QA60","NDVI", "NDWI", "NBR"))

  }
}

# Function to mask cloud from built-in quality band
# information on cloud

maskcloud1 = function(image) {
  QA60 = image$select('QA60')
  return(image$updateMask(QA60$lt(1)))
}

# sent2 ndwi


addNDWIsent = function(image){
  return(image$addBands(image$normalizedDifference(c('B3', 'B8'))$rename('NDWI')))
}

# sent2 ndvi


addNDVIsent = function(image){
  return(image$addBands(image$normalizedDifference(c('B8', 'B4'))$rename('NDVI')))
}

calcNbrsent = function(image) {
  return(image$addBands(image$normalizedDifference(c('B8', 'B11'))$rename('NBR')))
}


############ Sent 1 (SAR) section ###############

#Functions to mask out edges of images using using angle
maskAngGT30 = function(image) {
  ang = image$select('angle')
  return(image$updateMask(ang$gt(30.63993)))
}
maskAngLT45 = function(image) {
  ang = image$select('angle')
  return(image$updateMask(ang$lt(45.53993)))
}

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

#Function to perform angle correction
toGamma0 = function(image) {
  vh = image$select('VH')$subtract(image$select('angle')$multiply(base::pi/180)$cos()$log10()$multiply(10))
  return(vh$addBands(image$select('VV')$subtract(image$select('angle')$multiply(base::pi/180)$cos()$log10()$multiply(10))))
}

#Function to add band containing normalized difference
#between VH and VV
adddiff = function(image) {
  return(image$addBands(image$expression(
    expression = '(VH - VV) / (VH + VV)', opt_map = list(VH = image$select('VH'), VV = image$select('VV')))))

}




