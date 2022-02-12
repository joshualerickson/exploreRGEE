band_func2 <- function(imageCol, reg, fun, scale, param, method, data,
                       startDate, endDate, stat, save.plot, ggplot, variable, c.low, c.high, tmp_type){

  # tB <- imageCol$toBands()
  tB <-  imageCol |>
    map_date_to_bandname_ic()

  cat("starting ee_extract\n")
  data_tb <- rgee::ee_extract(x = tB, y = reg$reg, fun = fun, scale = scale)
  # client side
  band_names_cli<- imageCol$first()$bandNames()$getInfo()

  # regex to be removed from name to create date col
  rm_rgx <- paste0(".*",band_names_cli)
  rm_rgx <- glue::glue_collapse(rm_rgx,sep = "|")

  # regex to extract parameter identifier
  # reorder so shorter names with common prefix to another band names wont replace string before longer version
  extract_rgx <- band_names_cli[stringr::str_order(band_names_cli,decreasing=T)]
  extract_rgx <- glue::glue_collapse(extract_rgx,sep = "|")

  proc <-  data_tb |>
    # sf::st_drop_geometry() |>
    tidyr::pivot_longer(contains(param),names_to = "measurment_id",values_to = param) |>
    dplyr::mutate(
      # parameter=stringr::str_extract(.data$name, pattern=extract_rgx),
      Date= stringr::str_remove(string = .data$measurment_id, pattern = rm_rgx) |>
        stringr::str_replace_all("_","-") |> lubridate::ymd()

    ) |>
    dplyr::select(-.data$measurment_id)


  # proc <- data_tb %>% tidyr::pivot_longer(dplyr::contains(param_name), names_to = "Date", values_to = param)


  # proc <- getting_proc(data = data, proc = proc, param_name = param_name, method = method, tmp_type = tmp_type)


  if(ggplot == TRUE){

    proc_ggplot <- plot_proc(proc = proc, param_v = param, facet_col_var = variable)

    print(proc_ggplot +
            ggplot2::labs(title = paste0(method, " ", param, ' ', stat, " values for date range: "),
                          subtitle = paste0("Years: ",stringr::str_remove(startDate,"(-).*"), " - ", stringr::str_remove(endDate,"(-).*"), "; Months: ", c.low, " - ", c.high),
                          y = paste0(param, ' values'), color = "ID"))
  }


  if(save.plot == TRUE){
    return(list(proc = proc, proc_ggplot = proc_ggplot))

  } else {return(proc)}

}

