

#' Visualise NDVI
#'
#' @param aoi
#' @param startDate
#' @param endDate
#'
#' @return
#'
#' @examples
get_Video <- function(aoi,collection = "MOD13", param = NULL, startDate = '2018-01-01', endDate = '2019-01-01'){

  if(collection == "MOD13"){
  col = ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')


  } else if (collection == "PRISM") {

    col = ee$ImageCollection("OREGONSTATE/PRISM/AN81m")$select(param)
  }



mask <- rgee::sf_as_ee(aoi)
region <- mask$geometry()$bounds()

Map$addLayer(mask)
col = col$map(function(img) {
  doy = ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})

distinctDOY = col$filterDate(startDate, endDate)


filter = ee$Filter$equals(leftField = 'doy', rightField = 'doy')

# Define a join.
join = ee$Join$saveAll('doy_matches')

#Apply the join and convert the resulting FeatureCollection to an
# ImageCollection.
joinCol = ee$ImageCollection(join$apply(distinctDOY, col, filter))


comp = joinCol$map(function(img) {
 doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches'))
  return(doyCol$reduce(ee$Reducer$median()))
})

if(collection == "MOD13"){
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = 'NDVI_median',
  palette = c('FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301')
)
} else if (collection == "PRISM"){

  visParams = list(
    min = 0.0,
    max = 200,
    bands = 'ppt_median',
    palette = c('black','red',
                'orange', 'white',
                'green', 'forestgreen',
                'blue')
  )
}

#Create RGB visualization images for use as animation frames.
rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})

gifParams = list(
  'region' =  region,
  'dimensions' =  600,
  'crs' = 'EPSG:4326',
  'framesPerSecond' = 5)

#Print the GIF URL to the console.
print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))

}



