
#filter by month
month_filter <- function(c.low, c.high, imageCol){

  months = ee$List$sequence(c.low, c.high)

byMonth = ee$ImageCollection$fromImages(
  months$map(rgee::ee_utils_pyfunc(function (m) {
    indexString = ee$Number(m)$format('%03d')
    return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$mean()$set('system:index', indexString))}
  )))
}
#filter by year
year_filter <- function(startDate, endDate, imageCol){

  startYear = lubridate::year(startDate)
  endYear = lubridate::year(endDate)

  years = ee$List$sequence(startYear, endYear)

byYear = ee$ImageCollection$fromImages(
  years$map(rgee::ee_utils_pyfunc(function (y) {
    indexString = ee$Number(y)$format('%03d')
    return(imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))$mean()$set('system:index', indexString))}
  )))
}

# function that will reduce the by month and year // might use later //

year_month_filter <- function(startDate, endDate, c.low, c.high, imageCol){

startYear = lubridate::year(startDate)
endYear = lubridate::year(endDate)

years = ee$List$sequence(startYear, endYear)

months = ee$List$sequence(c.low, c.high)

# load the image collection
byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

  yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

  byYear = ee$ImageCollection$fromImages(

    months$map(rgee::ee_utils_pyfunc(function (m) {

      statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$mean())
      indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
      return(statImage$set('system:index', indexString))

    }))
  )


  return(byYear)

})))$flatten())

}



