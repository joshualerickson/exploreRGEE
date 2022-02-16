
#filter by month
month_filter <- function(c.low, c.high, imageCol, stat){

months = ee$List$sequence(c.low, c.high)

  if(stat == "median"){

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$median()$set('system:index', indexString))}
      )))

  } else if (stat == "mean") {

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$mean()$set('system:index', indexString))}
      )))

  } else if (stat == "max") {

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$max()$set('system:index', indexString))}
      )))

  } else if (stat == "min") {

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$min()$set('system:index', indexString))}
      )))

  } else if (stat == "sum"){

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$sum()$set('system:index', indexString))}
      )))

  } else if (stat == "stdDev"){

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        statImage = imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$stdDev())
        return(statImage$set('system:index', indexString))}
      )))

  } else if (stat == 'first'){

    byMonth = ee$ImageCollection$fromImages(
      months$map(rgee::ee_utils_pyfunc(function (m) {
        indexString = ee$Number(m)$format('%03d')
        return(imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))$first()$set('system:index', indexString))}
      )))

  }

}
#filter by year
year_filter <-  function(startDate, endDate, imageCol, stat){

  startYear = lubridate::year(startDate)
  endYear = lubridate::year(endDate)
  years = ee$List$sequence(startYear, endYear)

  ee_reducer <-  convert_reducer(stat)

  ee$ImageCollection$fromImages(
    years$map(rgee::ee_utils_pyfunc(function (y) {
      indexString = ee$Number(y)$format('%03d')
      ic_temp_filtered <- imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))
      ee_reducer(ic_temp_filtered)$
        set('system:index', indexString)$
        set('year',y)$
        set('month',1)$
        set('date',ee$Date$fromYMD(y,1,1))$
        # set('system:time_start',ee$Date$fromYMD(y,m,1))$
        set('system:time_start',ee$Date$millis(ee$Date$fromYMD(y,1,1)))
    }

    ))
  )
}


# function that will reduce the month per year //

year_month_filter <- function(startDate, endDate, c.low, c.high, imageCol, stat){

startYear = lubridate::year(startDate)
endYear = lubridate::year(endDate)

years = ee$List$sequence(startYear, endYear)

months = ee$List$sequence(c.low, c.high)

if(stat == "median"){

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$median())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())


} else if (stat == "mean") {

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


} else if (stat == "max") {

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$max())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())


} else if (stat == "min") {

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$min())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())

} else if (stat == "sum"){

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$sum())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())


} else if (stat == "stdDev"){

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$stdDev())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())


} else if (stat == 'first'){

  byMonthYear = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    byYear = ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        statImage = yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))$reduce(ee$Reducer$first())
        indexString = ee$Date$fromYMD(y, m, 1)$format('YYYY_MM_dd')
        return(statImage$set('system:index', indexString))

      }))
    )


    return(byYear)

  })))$flatten())


}

}



