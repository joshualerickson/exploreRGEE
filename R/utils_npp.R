npp_med <- function(geom, startDate, endDate, method, cloud_mask) {

  if(method == 'ld_NPP'){

    collection = rgee::ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

  } else if (method == 'aqua'){

    collection = rgee::ee$ImageCollection('MODIS/006/MYD17A3HGF')$filterBounds(geom)

  } else if (method == 'terra'){

    collection = rgee::ee$ImageCollection('MODIS/006/MOD17A3HGF')$filterBounds(geom)

  } else if (method == 'modis_NPP'){

    collection = rgee::ee$ImageCollection('UMT/NTSG/v2/MODIS/NPP')$filterBounds(geom)

  }

  collection <- collection$filterDate(startDate, endDate)


  #Create a mask for the QC (clouds)
  if(isTRUE(cloud_mask)){

    if(method == 'ld_NPP'){

      collection = collection$map(maskcloud_npp)

    } else if(method == 'aqua' || method == 'terra'){

      collection = collection$map(maskcloud_npp_aq_ter)

    } else if(method == 'modis_NPP'){

      collection = collection$map(maskcloud_npp_modis)

    }
  }

 collection
}

# Mask cloud for NPP

maskcloud_npp = function(image) {
  QC255 = image$select('QC')
  return(image$updateMask(QC255$lt(255)))
}

# Mask terra and aqua
maskcloud_npp_aq_ter = function(image) {
  Npp_QC50 = image$select('Npp_QC')
  return(image$updateMask(Npp_QC50$lt(50)))
}

# Mask terra and aqua
maskcloud_npp_modis = function(image) {
  QC50 = image$select('QC')
  return(image$updateMask(QC50$lt(50)))
}

