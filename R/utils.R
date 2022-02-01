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
    aoi <- sf::st_as_sf(aoi, coords = c("clng", "clat")) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326")

    geom <- setup(aoi)

    reg <- aoi

  } else {

    aoi <- aoi %>% sf::st_transform(crs = 4326, proj4string = "+init=epsg:4326")
    bb <-  sf::st_bbox(aoi)
    geom <- ee$Geometry$Rectangle(bb)
    aoi <- aoi %>% dplyr::mutate(dplyr::across(c(lubridate::is.Date, lubridate::is.POSIXct, lubridate::is.POSIXlt, lubridate::is.POSIXt), as.character))
    reg <- rgee::sf_as_ee(aoi)
  }
return(list(reg = reg, geom = geom, aoi = aoi))
}

# function for setting up function and geom with a GEE FeatureCollection

geeFC_setup_aoi <- function(aoi, geeFC){

  geom <- setup(aoi)
  reg <- ee$FeatureCollection(geeFC)
  reg <- reg$filterBounds(geom)
  aoi <- rgee::ee_as_sf(reg)
  return(list(aoi = aoi, geom = geom))
}

geeFC_setup <- function(aoi, geeFC){

  geom <- setup(aoi)

  reg <- ee$FeatureCollection(geeFC)
  reg <- reg$filterBounds(geom)
  return(list(reg = reg, geom = geom))
}


# function for determining what reducer/stat to use

data_stat <- function(x,stat){
  switch(stat,

          "mean" = x$reduce(ee$Reducer$mean()),
          "max" = x$reduce(ee$Reducer$max()),
          "min" = x$reduce(ee$Reducer$min()),
          "median"= x$reduce(ee$Reducer$median()),
          "sum"= x$reduce(ee$Reducer$stdDev()),
          "sd" =  x$reduce(ee$Reducer$stdDev()),
          "first" = x$reduce(ee$Reducer$first()),
          NULL
  )
  }



# function to seperate out the different lists

class_type <- function(data, aoi, method, param, stat,
                       startDate, endDate, c.low, c.high, mask,
                       m.low, m.high){

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

    get_any(aoi = aoi,method = method, param = param, stat = stat,
            startDate = startDate, endDate = endDate, c.low = c.low, c.high = c.high, mask = mask,
            m.low = m.low, m.high = m.high)

  }
}


