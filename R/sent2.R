
#' Sentinel-2 MSI: MultiSpectral Instrument, Level-1C
#'
#'
#' @param aoi A sf object
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI'.
#' @param startDate \code{character} format date, e.g. "2018-10-23"
#' @param endDate \code{character} format date, e.g. "2018-10-23"
#' @param window \code{logical} whether to mask out certain ranges
#' @param w.low \code{numeric} low value for mask, e.g. greater than 'w.low'
#' @param w.high \code{numeric} high value for mask, e.g. less than 'w.high'
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#' @return A leaflet map
#' @export
#'
#' @examples
viz_Sent2 <- function (aoi, param = "NDVI", startDate = '2015-04-01', endDate = '2020-10-30',
                       window = FALSE, w.low = NULL, w.high = NULL, c.low = 1, c.high = 12, scale = 250) {


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

  s2 <- ee$ImageCollection('COPERNICUS/S2')$filterBounds(geom)

  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')

  s2 = s2$filter(calRange)

  s2 = s2$filterDate(startDate, endDate)

  s2 = s2$map(maskcloud1)

  s2 = s2$map(addNDVIsent)

s2 = s2$map(addNDWIsent)

# some reason i can't rename the bands
s2 = s2$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","nd", "nd_1"),
        c("B1","Blue","Green","Red","B5","B6" ,"B7","NIR","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI"))

s2 = s2$select(param)

s2 = s2$median()

stats <- s2$reduceRegions(
  reducer = reducers,
  collection = geom,
  scale = scale
)

cat("Band: ", param, "\n")

print(stats$getInfo()$features[[1]]$properties)

st_max <- stats$getInfo()$features[[1]]$properties$max

st_min <- stats$getInfo()$features[[1]]$properties$min

cen <- as.numeric(AOI::bbox_coords({{aoi}}))
Map$setCenter(cen[1], cen[2], 13)

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

grp = "Hydrography"
opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
mLayer <- Map$addLayer(s2$clip(geom),
                       visParams = list(min = st_min, max = st_max, palette = c(
                         "#d73027", "#f46d43", "#fdae61",
                         "#fee08b", "#d9ef8b", "#a6d96a",
                         "#66bd63", "#1a9850"
                       )), paste0("Sentinel 2: ", param, "; " ,startDate, " - ", endDate))

mLayer@map %>%
  leaflet::addWMSTiles(GetURL("USGSTopo"),
                       group = "Topo", layers = "0") %>%
  leaflet::addWMSTiles(GetURL("USGSHydroCached"),
                       group = grp, options = opt, layers = "0") %>%
  leaflet::addLayersControl(baseGroups = c("Topo","CartoDB.Positron", "CartoDB.DarkMatter",
                                           "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"),
                            overlayGroups = c(paste0("Landsat: ", param, "; " , startDate, " - ", endDate), grp))
  } #end curly




#' Extract Regions by Date
#' @description This function is similar to \link[exploreRGEE]{rr_Landsat} but instead of reducing the region
#' for entire date range this function keeps the dates selected ('year' and 'months') and returns the median of each month.
#'  This function is good if your wanting a time series within a region(s).
#' @param aoi A sf object.
#' @param wb A \code{character} of HUC id, e.g. HUC02-HUC12.
#' @param id A \code{vector} identifying each region. \code{optional} if using \code{wb}
#' @param param A \code{character} indicating what band to visualize, e.g. 'Blue', 'Green', 'Red', 'NIR', 'NDVI', 'NDWI'.
#' @param startDate \code{character} format date, e.g. "1999-10-23"
#' @param endDate \code{character} format date, e.g. "1999-10-23"
#' @param c.low \code{numeric} lower month value for calendar range
#' @param c.high \code{numeric} higher month value for calendar range
#' @param scale \code{numeric} value indicating what to reduce the regions by, e.g. 250 (m) default.
#' @param plot \code{logical} TRUE/FALSE.
#' @param save.plot \code{logical} TRUE/FALSE whether to save plot and data.frame
#' @importFrom stringr str_remove
#'
#' @return A \code{data.frame} and a side-effect plot; unless using \code{save.plot}.
#' @export
#'
#' @examples
band_Sent2 <- function(aoi, wb, id,
                         param = 'NDVI', startDate = "2019-04-01",
                         endDate = "2020-10-01",c.low = 1, c.high = 12,scale = 250, plot = TRUE, save.plot = "FALSE") {
  if(missing(aoi)){stop("Need aoi to use this function")}
  if(!missing(aoi) & missing(id) & missing(wb)){stop("Need at least id or wb with argument to use this function")}
  if(!missing(id) & !missing(wb)){stop("Can't have both id and wb set.")}

  bb <-  sf::st_bbox(aoi)
  geom <- ee$Geometry$Rectangle(bb)

  if(missing(wb)) {


    region <- rgee::sf_as_ee(aoi)

    reg <- region

  } else {

    h_size <- paste0("USGS/WBD/2017/", wb)

    reg <- ee$FeatureCollection(h_size)$filterBounds(geom)
    feat <- reg$getInfo()$features

  }
  #bring in sent2 collection
  s2 <- ee$ImageCollection('COPERNICUS/S2')$filterBounds(geom)
  calRange = ee$Filter$calendarRange(c.low,c.high, 'month')
  s2 = s2$filter(calRange)
  s2 = s2$filterDate(startDate, endDate)

  s2 = s2$map(maskcloud1)

  s2 = s2$map(addNDVIsent)

  s2 = s2$map(addNDWIsent)



  collection <- s2$select(c("B1","B2","B3","B4","B5","B6" ,"B7","B8","B8A","B9","B10","B11","B12","QA10","QA20","QA60","nd", "nd_1"),
                                          c("B1","Blue","Green","Red","B5","B6" ,"B7","NIR","B8A","B9","B10","B11","B12","QA10","QA20","QA60","NDVI", "NDWI"))

  collection <- collection$select(param)

  tB <- collection$toBands()

  ld_tb <- rgee::ee_extract(tB, reg, fun = ee$Reducer$median(), scale = scale)

  if(missing(wb)){

    ld_tb <- ld_tb %>% dplyr::mutate(ID = {{ id }})

  } else {
    vect_prtib <- vector()
    for(i in 1:length(feat)){

      prtib <- feat[[i]]$properties$name

      vect_prtib <- append(prtib, vect_prtib)

    }

    ld_tb <- ld_tb %>% mutate(ID = vect_prtib)
  }
  proc <- ld_tb %>% pivot_longer(-ID, names_to = "Date")

  proc <- proc %>% mutate(Date = stringr::str_sub(Date, start = 18),
                          Date = stringr::str_sub(Date, end = 8),
                          Date = lubridate::as_date(Date))
  proc <- proc %>%
    mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(ID, Date) %>% dplyr::summarise(value = mean(value, na.rm = TRUE))

  if(plot == "TRUE"){

    plot_proc <- proc %>% ggplot(aes(Date, value, color = ID)) +
      geom_line() +
      geom_smooth(alpha = 0.3) +
      geom_point() +
      theme_bw() +
      labs(title = paste0(param, " values for date range: "),
           subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
           y = paste0(param, ' values')) +
      facet_wrap(~ID)

    print(plot_proc)

  }
  if(save.plot == "TRUE"){
    return(list(proc = proc, plot_proc = plot_proc))

  }
  proc

}
