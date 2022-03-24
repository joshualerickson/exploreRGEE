#' ee_extract_long
#' @param ic ImageCollection
#' @param sf sf object for spatial disaggregation
#' @param sf_col sf column to include in output
#' @param scale scale (meters) of imageCollection
#' @param reducer sf_col level reducer
#' @return `rgee::ee_extract` data.frame with date & measurements pivoted long
#' @importFrom dplyr select mutate
#' @importFrom stringr str_remove str_replace_all str_extract




ee_extract_long <-  function(ic,
                             sf,
                             sf_col,
                             # parameter_name,
                             scale,
                             reducer
                             # via="rgee_backup"
){

  reducer_fun<- switch(
    reducer,
    "mean" = ee$Reducer$mean(),
    "max" = ee$Reducer$mean(),
    "min" = ee$Reducer$min(),
    "median"= ee$Reducer$median(),
    "sum"= ee$Reducer$sum(),
    "sd" = ee$Reducer$stdDev(),
    NULL
  )


  cat("renaming bands with dates\n")
  ic_renamed<- ic |>
    map_date_to_bandname_ic()

  cat("starting ee_extract\n")
  ic_extracted_wide_sf <- rgee::ee_extract(x = ic_renamed,
                                           y=sf[sf_col],
                                           scale=scale,
                                           fun= reducer_fun,
                                           via = "drive",
                                           sf=T)


  # client side
  band_names_cli<- ic$first()$bandNames()$getInfo()

  # regex to be removed from name to create date col
  rm_rgx <- paste0(".*",band_names_cli)
  rm_rgx <- glue::glue_collapse(rm_rgx,sep = "|")

  # regex to extract parameter identifier
  # reorder so shorter names with common prefix to another band names wont replace string before longer version
  extract_rgx <- band_names_cli[stringr::str_order(band_names_cli,decreasing=T)]
  extract_rgx <- glue::glue_collapse(extract_rgx,sep = "|")

  ic_extracted_wide_sf |>
    sf::st_drop_geometry() |>
    tidyr::pivot_longer(-1,names_to = "name") |>
    mutate(
      parameter=str_extract(.data$name, pattern=extract_rgx),
      date= str_remove(string = .data$name, pattern = rm_rgx) |>
        str_replace_all("_","-") |> lubridate::ymd()

    ) |>
    select(-.data$name)


}
