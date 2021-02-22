#set-up
setup <- function(aoi){

  bb <-  sf::st_bbox(aoi)
geom <- ee$Geometry$Rectangle(bb)
return(geom)
}

#reducers function
reducers <-  function() {

  ee$Reducer$mean()$combine(
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
}




# function for setting up the region and geom

sf_setup <- function(aoi = aoi){

  if(is.atomic(aoi)) {

    clng <- aoi[[1]]
    clat <- aoi[[2]]
    aoi <- data.frame(clat = clat, clng = clng)
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326)

    geom <- setup(aoi)

    reg <- aoi

  } else {

    aoi <- aoi %>% sf::st_transform(crs = 4326)
    bb <-  sf::st_bbox(aoi)
    geom <- ee$Geometry$Rectangle(bb)
    aoi <- aoi %>% dplyr::mutate(dplyr::across(lubridate::is.Date, as.character))
    reg <- rgee::sf_as_ee(aoi)
  }
return(list(reg = reg, geom = geom, aoi = aoi))
}

# function for setting up function and geom with a GEE FeatureCollection

geeFC_setup <- function(aoi = aoi, geeFC = geeFC){

  geom <- setup(aoi)

  geeFC_id <- paste0(geeFC)

  reg <- ee$FeatureCollection(geeFC_id)$filterBounds(geom)

  aoi <- rgee::ee_as_sf(reg)

  return(list(reg = reg, geom = geom, aoi = aoi))
}


# function for determining what reducer/stat to use

data_stat <- function(data,stat){

  if(stat == "median"){

    data$median()

  } else if (stat == "mean") {

    data$mean()

  } else if (stat == "max") {

    data$max()

  } else if (stat == "min") {

    data$min()

  } else if (stat == "sum"){

    data$sum()

  } else if (stat == "stdDev"){

    data$reduce(ee$Reducer$stdDev())

  } else if (stat == 'first'){

    data$first()

  }

}



# function to seperate out the different lists

class_type <- function(data, aoi, method, param, stat,
                       startDate, endDate, c.low, c.high, mask,
                       m.low, m.high,...){

  if (class(data) == "landsat_list"){

    get_landsat(aoi = aoi,method = method, param = param, stat = stat,
                startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
                m.low = m.low, m.high = m.high)

  } else if (class(data) == "met_list"){

    get_met(aoi = aoi,method = method, param = param, stat = stat,
              startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
              m.low = m.low, m.high = m.high)

  } else if (class(data) == "sent2_list"){

    get_sent2(aoi = aoi,method = method, param = param, stat = stat,
              startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
              m.low = m.low, m.high = m.high)

  } else if (class(data) == "npp_list"){

    get_npp(aoi = aoi,method = method, param = param, stat = stat,
              startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
              m.low = m.low, m.high = m.high)

  } else if (class(data) == "any_list"){

    get_npp(aoi = aoi,method = method, param = param, stat = stat,
            startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
            m.low = m.low, m.high = m.high)

  }
}

# function that will reduce the by month and year // might use later //
#
#   startYear = lubridate::year(startDate)
#   endYear = lubridate::year(endDate)
#
#   years = ee$List$sequence(1984, 1986)
#
#    startMonth = c.low
#    endMonth = c.high
#
#   months = ee$List$sequence(1, 12)
#
#   # load the image collection
#   byMonth = ee$ImageCollection(ee$FeatureCollection(years$map(rgee::ee_utils_pyfunc(function (year) {
#
#     yearCollection = collection$filter(ee$Filter$calendarRange(1984, 1986, 'year'))
#
#     byYear = ee$ImageCollection$fromImages(
#
#       months$map(rgee::ee_utils_pyfunc(function (month) {
#
#         medianImage = yearCollection$filter(ee$Filter$calendarRange(8, 8, 'month'))$reduce(ee$Reducer$median())
#         date = ee$Date$fromYMD(year, month, 1)$format("YYYY_MM_dd")
#         return(medianImage$reduce(ee$Reducer$median())$rename(date))
#
#       }))
#     )
#
#
#     return(byYear)
#   })))$flatten())
#
#   # filter the empty one out
#   collection = byMonth$filter(ee$Filter$listContains('system:band_names', 'constant')$Not())$sort('system:time_start')$toBands()
#

# viz stretching // might use later //

# viz_stretch <- function(image, n, geom, param, scale){
# i <- image
# n_std <- n
# stretch_std = function(i, n_std, geom) {
#
#
#  mean = i$reduceRegion(
#     reducer = ee$Reducer$mean(),
#     geometry = geom,
#     crs = "EPSG:4326",
#     scale = scale,
#     bestEffort = TRUE)
#
#  std = i$reduceRegion(
#     reducer = ee$Reducer$stdDev(),
#     geometry = geom,
#     crs = "EPSG:4326",
#     scale = scale,
#     bestEffort = TRUE)
#
#  min = mean$map(rgee::ee_utils_pyfunc(function(key, val){
#     return(ee$Number(val)$subtract(ee$Number(std$get(key))$multiply(n_std)))
#   }))$getInfo()
#
# max = mean$map(rgee::ee_utils_pyfunc(function(key, val){
#     return(ee$Number(val)$add(ee$Number(std$get(key))$multiply(n_std)))
#   }))$getInfo()
#
#   return(list(vmin = min, vmax = max))
# }
#
# s = stretch_std(i,3, geom)
#
# min = s$vmin %>% purrr::flatten() %>% as.data.frame() %>%  dplyr::select(min = all_of({{param}}))
# max = s$vmax %>% purrr::flatten() %>% as.data.frame() %>%  dplyr::select(max = all_of({{param}}))
#
# minmax <- data.frame(min = min, max = max)
#
# }
