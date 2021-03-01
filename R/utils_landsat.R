

# Function to get and rename bands of interest from OLI.

renameOli = function(img) {
  img$select(c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'pixel_qa'),c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa'))
}

# Function to get and rename bands of interest from ETM+.
renameEtm = function(img) {
  img$select(c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'pixel_qa'),c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa'))
}

# Function to convert etm to oli using coefficients


etmToOli = function(img) {
  coefficients = list(
    itcps = ee$Image$constant(c(0.0003, 0.0088, 0.0061, 0.0412, 0.0254, 0.0172))$multiply(10000),
    slopes = ee$Image$constant(c(0.8474, 0.8483, 0.9047, 0.8462, 0.8937, 0.9071))
  )
  img = img$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2'))$multiply(ee$Image(coefficients$slopes))$add(coefficients$itcps)$round()$toShort()$addBands(img$select('pixel_qa'))

  return(img)
}

# This function gets NDVI to landsat.

addNDVI = function(image) {
  return(image$addBands(image$normalizedDifference(c('NIR', 'Red'))$rename('NDVI')))
}

# This function gets NDWI for landsat.


addNDWI = function(image) {
  return(image$addBands(image$normalizedDifference(c('Green', 'NIR'))$rename('NDWI')))
}

#get near burn index

calcNbr = function(image) {
  return(image$addBands(image$normalizedDifference(c('NIR', 'SWIR2'))$rename('NBR')))
}


# Mask out bad pixels (cloud masker)
fmask = function(img) {
  cloudShadowBitMask = base::bitwShiftL(1,3)
  cloudsBitMask = base::bitwShiftL(1,5)
  qa = img$select('pixel_qa')
  mask = qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$And(qa$bitwiseAnd(cloudsBitMask)$eq(0))

  return(img$updateMask(mask))
}

# Define function to prepare OLI images.


prepOli = function(img) {
  orig = img
  img = renameOli(img)
  img = fmask(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# Define function to prepare ETM+ images.


prepEtm = function(img) {
  orig = img
  img = renameEtm(img)
  img = fmask(img)
  img = etmToOli(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# without Roy harmonizing
prepEtm_cloud = function(img) {
  orig = img
  img = renameEtm(img)
  img = fmask(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# Define function to prepare OLI images.


prepOli_raw = function(img) {
  orig = img
  img = renameOli(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)
  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# Define function to prepare ETM+ images.


prepEtm_raw = function(img) {
  orig = img
  img = renameEtm(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)
  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}


# function for pre-processing landsat data

col_ld <- function(c.low, c.high, geom, startDate, endDate, method, cloud_mask){


# Bring in image collections from landsat 5,7,8
oliCol = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterBounds(geom)
etmCol = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterBounds(geom)
tmCol = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$filterBounds(geom)
ld4Col = ee$ImageCollection('LANDSAT/LT04/C01/T1_SR')$filterBounds(geom)
colFilter = ee$Filter$lt('CLOUD_COVER', 50)$lt('GEOMETRIC_RMSE_MODEL', 10)$Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

if(isTRUE(cloud_mask)){

  if(method == 'harm_ts'){
# Filter collections and prepare them for merging.
  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm))
  tmCol = tmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm))
  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

} else if (method == 'ts') {

  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))
  tmCol = tmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))
  ld4Col = ld4Col$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))
  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol$merge(ld4Col)))

} else {

  oliCol = oliCol$filter(colFilter)$map(prepOli)
  etmCol = etmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))
  tmCol = tmCol$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))
  ld4Col = ld4Col$filter(colFilter)$map(rgee::ee_utils_pyfunc(prepEtm_cloud))

}

} else {
  oliCol = oliCol$map(prepOli_raw)
  etmCol = etmCol$map(rgee::ee_utils_pyfunc(prepEtm_raw))
  tmCol = tmCol$map(rgee::ee_utils_pyfunc(prepEtm_raw))
  ld4Col = ld4Col$map(rgee::ee_utils_pyfunc(prepEtm_raw))
}

if(method == 'ld8'){

  collection <- oliCol

} else if (method == 'ld7'){

  collection <- etmCol

} else if (method == 'ld5'){

  collection <- tmCol

} else if (method == 'ld4'){

  collection <- ld4Col

}

#filter by cal range
calRange = ee$Filter$calendarRange(c.low,c.high, 'month')


collection <- collection$filter(calRange)


collection <- collection$filterDate(startDate, endDate)



}
