

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
  return(image$addBands(image$normalizedDifference(c('NIR', 'Red'))))
}

# This function gets NDWI for landsat.


addNDWI = function(image) {
  return(image$addBands(image$normalizedDifference(c('Green', 'NIR'))))
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

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}


# sent2 ndwi


addNDWIsent = function(image){
  return(image$addBands(image$normalizedDifference(c('B3', 'B8'))))
}

# sent2 ndvi


addNDVIsent = function(image){
  return(image$addBands(image$normalizedDifference(c('B8', 'B4'))))
}


# Function to mask cloud from built-in quality band
# information on cloud

maskcloud1 = function(image) {
  QA60 = image$select('QA60')
  return(image$updateMask(QA60$lt(1)))
}

# Mask cloud for NPP

maskcloud_npp = function(image) {
  QC255 = image$select('QC')
  return(image$updateMask(QC255$lt(255)))
}
