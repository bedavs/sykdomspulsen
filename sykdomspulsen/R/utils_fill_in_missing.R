fill_in_missing <- function(d){
  if(!"granularity_geo" %in% names(d)){
    d[, granularity_geo:=dplyr::case_when(
      stringr::str_detect(location_code,"municip") ~ "municip",
      stringr::str_detect(location_code,"county") ~ "county",
      stringr::str_detect(location_code,"norge") ~ "nation",
    )]
  }
  if(!"border" %in% names(d)){
    d[, border := config$border]
  }
  if(!"age" %in% names(d)){
    d[, age := "total"]
  }
  if(!"sex" %in% names(d)){
    d[,sex := "total"]
  }
  # only providing season
  if(!"date" %in% names(d) & !"yrwk" %in% names(d) & "season" %in% names(d)){
    x <- copy(fhidata::days)
    x[,season:=fhi::season(yrwk)]
    setorder(x,-yrwk)
    x[,row:=1:.N,by=.(season)]
    x <- x[row==1]
    d[
      x,
      on="season",
      date:=sun
    ]
  }

  # only providing yrwk
  if(!"date" %in% names(d) & "yrwk" %in% names(d)){
    d[
      fhidata::days,
      on="yrwk",
      date:=sun
      ]
  }

  if(!"date" %in% names(d) & !"yrwk" %in% names(d)){
    d[,date:=as.Date("1900-01-01")]
  }
  if(!"yrwk" %in% names(d)){
    d[,yrwk := fhi::isoyearweek(date)]
  }
  if(!"season" %in% names(d)){
    d[, season := fhi::season(yrwk)]
  }
  if(!"year" %in% names(d)){
    d[, year := fhi::isoyear_n(date)]
  }
  if(!"week" %in% names(d)){
    d[, week := fhi::isoweek_n(date)]
  }
  if(!"x" %in% names(d)){
    d[, x := fhi::x(week)]
  }
}
