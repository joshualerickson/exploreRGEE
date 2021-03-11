coll_name <- function(method){

  if(method == 'Norm81m'){

  paste0("OREGONSTATE/PRISM/",method)

  } else if (method == 'AN81m'){

    paste0("OREGONSTATE/PRISM/",method)

  } else if (method == 'AN81d'){

    paste0("OREGONSTATE/PRISM/",method)

  } else if (method == 'GRIDMET'){

    "IDAHO_EPSCOR/GRIDMET"

  } else if (method == 'DAYMET'){

    "NASA/ORNL/DAYMET_V3"

  } else if (method == 'TRMMh'){

    "TRMM/3B42"

  } else if (method == 'TRMMm'){

    "TRMM/3B43V7"

  } else if (method == 'TERRACLIMATE'){

    "IDAHO_EPSCOR/TERRACLIMATE"
  }

}
