#' yearly filter
#' perhaps can replcae year_filter

year_filter2 <-  function(startDate, endDate, imageCol, stat){

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

