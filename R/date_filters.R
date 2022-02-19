#' @title Filter by Month
#' @param imageCol An earth engine ImageCollection
#' @param stat A \code{character} indicating what to reduce the imageCollection by,
#'  e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'sd', 'first'.
#' @export
#'
#'


ee_month_filter <- function(imageCol, stat,...){

  UseMethod('ee_month_filter')

}




#' @name ee_month_filter
#' @param months \code{numeric} vector, e.g. c(1,12).
#' @export

ee_month_filter.ee.imagecollection.ImageCollection <- function(imageCol,months, stat){

months = ee$List$sequence(months[1], months[2])

stopifnot(!is.null(imageCol), inherits(imageCol, "ee.imagecollection.ImageCollection"))

ee_reducer <- stat_to_reducer(stat)

ee$ImageCollection$fromImages(
  months$map(rgee::ee_utils_pyfunc(function (m) {
    indexString = ee$Number(m)$format('%03d')
    ic_temp_filtered <- imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))
    ee_reducer(ic_temp_filtered)$
      set('system:index', indexString)$
      set('year',0000)$
      set('month',m)$
      set('date',ee$Date$fromYMD(1,m,1))$
      # set('system:time_start',ee$Date$fromYMD(y,m,1))$
      set('system:time_start',ee$Date$millis(ee$Date$fromYMD(1,m,1)))
  }
  )))

}

#' @name ee_month_filter
#' @export
ee_month_filter.exploreList <- function(imageCol, stat){

  months = ee$List$sequence(imageCol$c.low, imageCol$c.high)

  stopifnot(!is.null(imageCol), inherits(imageCol, "exploreList"))

  imageCol <- imageCol$imageCol

  ee_reducer <- stat_to_reducer(stat)

  ee$ImageCollection$fromImages(
    months$map(rgee::ee_utils_pyfunc(function (m) {
      indexString = ee$Number(m)$format('%03d')
      ic_temp_filtered <- imageCol$filter(ee$Filter$calendarRange(m, m, 'month'))
      ee_reducer(ic_temp_filtered)$
        set('system:index', indexString)$
        set('year',0000)$
        set('month',m)$
        set('date',ee$Date$fromYMD(1,m,1))$
        # set('system:time_start',ee$Date$fromYMD(y,m,1))$
        set('system:time_start',ee$Date$millis(ee$Date$fromYMD(1,m,1)))
      }
    )))


}

#' @title Filter by Year
#' @param imageCol An earth engine ImageCollection
#' @param startDate \code{character} format date, e.g. "2018-10-23".
#' @param endDate \code{character} format date, e.g. "2018-10-23".
#' @param stat A \code{character} indicating what to reduce the imageCollection by,
#'  e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'sd', 'first'.
#'
#' @export
#'


ee_year_filter <- function(imageCol, stat,...){

  UseMethod('ee_year_filter')

}

#' @name ee_year_filter
#' @param startDate
#' @param endDate
#' @export
#'
#'

ee_year_filter.ee.imagecollection.ImageCollection <-  function(imageCol,
                                                            startDate,
                                                            endDate,
                                                            stat
                                                            ){

  stopifnot(!is.null(imageCol), inherits(imageCol, "ee.imagecollection.ImageCollection"))

  startYear = lubridate::year(startDate)
  endYear = lubridate::year(endDate)
  years = ee$List$sequence(startYear, endYear)

  ee_reducer <-  stat_to_reducer(stat)

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

#' @name ee_year_filter
#' @export
#'
#'

ee_year_filter.exploreList <-  function(imageCol,
                                     stat
                                     ) {


  stopifnot(!is.null(imageCol), inherits(imageCol, 'exploreList'))

  startYear = lubridate::year(imageCol$startDate)
  endYear = lubridate::year(imageCol$endDate)
  years = ee$List$sequence(startYear, endYear)

  imageCol <- imageCol$imageCol
  ee_reducer <-  stat_to_reducer(stat)

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

#' @title Filter by Year and Month
#' @param imageCol An earth engine ImageCollection
#' @param startDate \code{character} format date, e.g. "2018-10-23".
#' @param endDate \code{character} format date, e.g. "2018-10-23".
#' @param months \code{numeric} vector, e.g. c(1,12).
#' @param stat A \code{character} indicating what to reduce the imageCollection by,
#'  e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'sd', 'first'.
#'
#' @export
#'
#'


ee_year_month_filter <- function(imageCol, stat,...){

  UseMethod('ee_year_month_filter')

}

#' @name ee_year_month_filter
#' @param startDate
#' @param endDate
#' @param months
#' @export
#'
#'

ee_year_month_filter.ee.imagecollection.ImageCollection <-  function(imageCol,
                                                               startDate,
                                                               endDate,
                                                               months,
                                                               stat
){

  stopifnot(!is.null(imageCol), inherits(imageCol, "ee.imagecollection.ImageCollection"))


startYear = lubridate::year(startDate)
endYear = lubridate::year(endDate)

years = ee$List$sequence(startYear, endYear)

months = ee$List$sequence(months[1], months[2])

ee_reducer <-  stat_to_reducer(stat)

ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

  yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

  ee$ImageCollection$fromImages(

    months$map(rgee::ee_utils_pyfunc(function (m) {

      indexString = ee$Number(m)$format('%03d')
      ic_temp_filtered <- yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))
      ee_reducer(ic_temp_filtered)$
        set('system:index', indexString)$
        set('year',y)$
        set('month',m)$
        set('date',ee$Date$fromYMD(y,m,1))$
        # set('system:time_start',ee$Date$fromYMD(y,m,1))$
        set('system:time_start',ee$Date$millis(ee$Date$fromYMD(y,m,1)))

    }))
  )

})))$flatten())


}

#' @name ee_year_month_filter
#' @export
#'
#'

ee_year_month_filter.exploreList <-  function(imageCol,
                                        stat
){


  stopifnot(!is.null(imageCol), inherits(imageCol, 'exploreList'))

  startYear = lubridate::year(imageCol$startDate)
  endYear = lubridate::year(imageCol$endDate)
  years = ee$List$sequence(startYear, endYear)


  months = ee$List$sequence(imageCol$c.low, imageCol$c.high)

  imageCol <- imageCol$imageCol
  ee_reducer <-  stat_to_reducer(stat)

  ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (y) {

    yearCollection = imageCol$filter(ee$Filter$calendarRange(y, y, 'year'))

    ee$ImageCollection$fromImages(

      months$map(rgee::ee_utils_pyfunc(function (m) {

        indexString = ee$Number(m)$format('%03d')
        ic_temp_filtered <- yearCollection$filter(ee$Filter$calendarRange(m, m, 'month'))
        ee_reducer(ic_temp_filtered)$
          set('system:index', indexString)$
          set('year',y)$
          set('month',m)$
          set('date',ee$Date$fromYMD(y,m,1))$
          # set('system:time_start',ee$Date$fromYMD(y,m,1))$
          set('system:time_start',ee$Date$millis(ee$Date$fromYMD(y,m,1)))

      }))
    )

  })))$flatten())


}

