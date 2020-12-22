

#' Title
#'
#' @param aoi A sf object
#' @param method A \code{character} indicating what method to use, e.g. 'LANDSAT', 'PRISM', 'MODIS', 'SAR2'
#' @param param A \code{character} indicating what band to visualize. Check other functions to find out.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param startDate2 \code{character} format date, e.g. "1999-10-23". A second start date to use as the subtraction window.
#' @param endDate2 \code{character} format date, e.g. "1999-10-23". A second end date to use as the subtraction window.
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#'
#' @return
#' @export
#'
#' @examples
get_diff <- function(aoi,method = "LANDSAT", param = "NDVI", startDate = '2000-01-01', endDate = '2010-12-30',startDate2 = '2010-01-01', endDate2 = '2020-10-30',
                     c.low = 1, c.high = 12, scale = 250){
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
)$combine(
  reducer2 = ee$Reducer$median(),
  sharedInputs = TRUE
)

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

grp = "Hydrography"
opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)


if (method == "LANDSAT"){
# Bring in image collections from landsat 5,7,8
oliCol = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterBounds(geom)
etmCol = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterBounds(geom)
tmCol = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$filterBounds(geom)

colFilter = ee$Filter$lt('CLOUD_COVER', 50)$lt('GEOMETRIC_RMSE_MODEL', 10)$Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

# Filter collections and prepare them for merging.
oliCol = oliCol$filter(colFilter)$map(prepOli)
etmCol = etmCol$filter(colFilter)$map(prepEtm)
tmCol = tmCol$filter(colFilter)$map(prepEtm)

#Merge the collections.

collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

#filter by cal range
calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
collection <- collection$filter(calRange)


collection <- collection$filterDate(startDate, endDate)


col_med <- collection$median()

col_med <- col_med$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'nd', 'nd_1'), c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'NDVI', 'NDWI'))

col_med <- col_med$select(param)


collection2 = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))

collection2 <- collection2$filter(calRange)


collection2 <- collection2$filterDate(startDate2, endDate2)


col_med2 <- collection2$median()

col_med2 <- col_med2$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'nd', 'nd_1'), c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa', 'NDVI', 'NDWI'))

col_med2 <- col_med2$select(param)

final_image <- col_med2$subtract(col_med)

stats <- final_image$reduceRegions(
  reducer = reducers,
  collection = geom,
  scale = scale
)

cat("Band: ", param, "\n")

print(stats$getInfo()$features[[1]]$properties)

st_max <- stats$getInfo()$features[[1]]$properties$max

st_min <- stats$getInfo()$features[[1]]$properties$min
cen <- as.numeric(AOI::bbox_coords({{aoi}}))
Map$setCenter(cen[1], cen[2], 7)
sld_intervals <- paste0(
  "<RasterSymbolizer>",
  '<ColorMap  type="ramp" extended="false" >',
  '<ColorMapEntry color="#B22222" quantity="-0.9" />',
  '<ColorMapEntry color="#FF0000" quantity="-0.2" />',
  '<ColorMapEntry color="#000000" quantity="0" />',
  '<ColorMapEntry color="#008000" quantity="0.2" />',
  '<ColorMapEntry color="#0000CD" quantity="0.9" />',
  "</ColorMap>",
  "</RasterSymbolizer>"
)

mlay <- Map$addLayer(final_image$clip(geom)$sldStyle(sld_intervals), opacity = 0.75)
mlay <- mlay@map %>%
  leaflet::addWMSTiles(GetURL("USGSTopo"),
                       group = "Topo", layers = "0") %>%
  leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                       group = grp, options = opt, layers = "0") %>%
  leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                           "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                            overlayGroups = c(paste0("Landsat: ", param, "; " , startDate, " - ", endDate), grp))
return(mlay)

} else if (method == "PRISM"){

  coll_name <- paste0("OREGONSTATE/PRISM/AN81m")
  colFilter <- ee$Filter$calendarRange(c.low,c.high, 'month')

  prism <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate, endDate)
  prism <- prism$filter(colFilter)$median()

  prism2 <- ee$ImageCollection(coll_name)$select(param)$filterBounds(geom)$filterDate(startDate2, endDate2)
  prism2 <- prism2$filter(colFilter)$median()

  final_image <- prism2$subtract(prism)

  stats <- final_image$reduceRegions(
    reducer = reducers,
    collection = geom,
    scale = scale
  )

  cat("Band: ", param, "\n")

  print(stats$getInfo()$features[[1]]$properties)

  st_max <- stats$getInfo()$features[[1]]$properties$max

  st_min <- stats$getInfo()$features[[1]]$properties$min

  cen <- as.numeric(AOI::bbox_coords({{aoi}}))

  Map$setCenter(cen[1], cen[2], 7)


  sld_intervals <- paste0(
    "<RasterSymbolizer>",
    '<ColorMap  type="ramp" extended="false" >',
    '<ColorMapEntry color="#B22222" quantity="-1000" />',
    '<ColorMapEntry color="#FF0000" quantity="-50" />',
    '<ColorMapEntry color="#000000" quantity="0" />',
    '<ColorMapEntry color="#008000" quantity="50" />',
    '<ColorMapEntry color="#0000CD" quantity="1000" />',
    "</ColorMap>",
    "</RasterSymbolizer>"
  )

  mlay <- Map$addLayer(final_image$clip(geom)$sldStyle(sld_intervals), opacity = 0.75)
  mlay <- mlay@map %>%
    leaflet::addWMSTiles(GetURL("USGSTopo"),
                         group = "Topo", layers = "0") %>%
    leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                         group = grp, options = opt, layers = "0") %>%
    leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                             "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                              overlayGroups = c(paste0("Landsat: ", param, "; " , startDate, " - ", endDate), grp))
return(mlay)

 } else if (method == "NPP"){

   collection = ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

   collection <- collection$filterDate(startDate, endDate)

   #Create a mask for the QC (clouds)
   maskcloud1 = function(image) {
     QC255 = image$select('QC')
     return(image$updateMask(QC255$lt(255)))
   }

   collection = collection$map(maskcloud1)

   col_med <- collection$median()

   col_med <- col_med$select('annualNPP')


   collection2 = ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$filterBounds(geom)

   collection2 <- collection2$filterDate(startDate2, endDate2)

   collection2 = collection2$map(maskcloud1)

   col_med2 <- collection2$median()

   col_med2 <- col_med2$select('annualNPP')

   final_image <- col_med2$subtract(col_med)

   stats <- final_image$reduceRegions(
     reducer = reducers,
     collection = geom,
     scale = scale
   )

   cat("Band: ", "annualNPP", "\n")

   print(stats$getInfo()$features[[1]]$properties)

   st_max <- stats$getInfo()$features[[1]]$properties$max

   st_min <- stats$getInfo()$features[[1]]$properties$min

   cen <- as.numeric(AOI::bbox_coords({{aoi}}))

   Map$setCenter(cen[1], cen[2], 7)


   sld_intervals <- paste0(
     "<RasterSymbolizer>",
     '<ColorMap  type="ramp" extended="false" >',
     '<ColorMapEntry color="#B22222" quantity="-1000" />',
     '<ColorMapEntry color="#FF0000" quantity="-50" />',
     '<ColorMapEntry color="#000000" quantity="0" />',
     '<ColorMapEntry color="#008000" quantity="50" />',
     '<ColorMapEntry color="#0000CD" quantity="1000" />',
     "</ColorMap>",
     "</RasterSymbolizer>"
   )

   mlay <- Map$addLayer(final_image$clip(geom)$sldStyle(sld_intervals),  opacity = 0.75)
   mlay <- mlay@map %>%
     leaflet::addWMSTiles(GetURL("USGSTopo"),
                          group = "Topo", layers = "0") %>%
     leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                          group = grp, options = opt, layers = "0") %>%
     leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                              "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                               overlayGroups = c(paste0("Annual NPP", "; " , startDate, " - ", endDate), grp))
   return(mlay)

}else if (method == "SENT2") {


  s2 <- ee$ImageCollection('COPERNICUS/S2')$filterBounds(geom)
  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
  s2 = s2$filter(calRange)
  s2 = s2$filterDate(startDate, endDate)

  # Function to mask cloud from built-in quality band
  #information on cloud
  maskcloud1 = function(image) {
    QA60 = image$select('QA60')
    return(image$updateMask(QA60$lt(1)))
  }


  s2 = s2$map(maskcloud1)

  addNDVI = function(image){
    return(image$addBands(image$normalizedDifference(c('B8', 'B4'))))
  }


  s2 = s2$map(addNDVI)

  addNDWI = function(image){
    return(image$addBands(image$normalizedDifference(c('B3', 'B8'))))
  }

  s2 = s2$map(addNDWI)

  if (param == "NDVI"){
    s2 = s2$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","nd", "nd_1"),
                   c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI"))
    s2 = s2$select("NDVI")$median()
  }

  s22 <- ee$ImageCollection('COPERNICUS/S2')$filterBounds(geom)
  s22 = s22$filter(calRange)
  s22 = s22$filterDate(startDate2, endDate2)

  # Function to mask cloud from built-in quality band
  #information on cloud
  maskcloud1 = function(image) {
    QA60 = image$select('QA60')
    return(image$updateMask(QA60$lt(1)))
  }


  s22 = s22$map(maskcloud1)


  s22 = s22$map(addNDVI)


  s22 = s22$map(addNDWI)

  if (param == "NDVI"){
    s22 = s22$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","nd", "nd_1"),
                   c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI"))
    s22 = s22$select("NDVI")$median()
  }


  final_image <- s22$subtract(s2)

  stats <- final_image$reduceRegions(
    reducer = reducers,
    collection = geom,
    scale = scale
  )

  cat("Band: ", param, "\n")

  print(stats$getInfo()$features[[1]]$properties)

  st_max <- stats$getInfo()$features[[1]]$properties$max

  st_min <- stats$getInfo()$features[[1]]$properties$min
  cen <- as.numeric(AOI::bbox_coords({{aoi}}))
  Map$setCenter(cen[1], cen[2], 7)
  sld_intervals <- paste0(
    "<RasterSymbolizer>",
    '<ColorMap  type="ramp" extended="false" >',
    '<ColorMapEntry color="#B22222" quantity="-0.9" />',
    '<ColorMapEntry color="#FF0000" quantity="-0.2" />',
    '<ColorMapEntry color="#000000" quantity="0" />',
    '<ColorMapEntry color="#008000" quantity="0.2" />',
    '<ColorMapEntry color="#0000CD" quantity="0.9" />',
    "</ColorMap>",
    "</RasterSymbolizer>"
  )

  mlay <- Map$addLayer(final_image$clip(geom)$sldStyle(sld_intervals), opacity = 0.75)
mlay <-  mlay@map %>%
    leaflet::addWMSTiles(GetURL("USGSTopo"),
                         group = "Topo", layers = "0") %>%
    leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                         group = grp, options = opt, layers = "0") %>%
    leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                             "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                              overlayGroups = c(paste0("Sentinel 2: ", param, "; " , startDate, " - ", endDate), grp))
  return(mlay)
}

}








