npp_med <- function(geom, startDate, endDate, method) {

  collection = rgee::ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

  collection <- collection$filterDate(startDate, endDate)

  #Create a mask for the QC (clouds)
if(method == 'cloud_mask'){
  collection = collection$map(maskcloud_npp)}

 collection
}

# Mask cloud for NPP

maskcloud_npp = function(image) {
  QC255 = image$select('QC')
  return(image$updateMask(QC255$lt(255)))
}

