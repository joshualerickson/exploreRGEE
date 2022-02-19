map_date_to_bandname_ic <- function(ic){
  ic |>
    ee$ImageCollection$map(
      function(x){
        # can't use getInfo() in sever-side function
        bnames<- x$bandNames()
        date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')

        # since bnames is technically a list rather than a simple string I need to map over it
        # this should make it flexible fore when there are more bands I want to rename anyways
        bnames_date <- bnames$map(
          rgee::ee_utils_pyfunc(function(x){
            ee$String(x)$cat(ee$String("_"))$cat(date)

          })
        )
        x$select(bnames)$rename(bnames_date)
      }

    )

}
