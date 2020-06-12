fill_in_missing <- function(d){
  if(!"granularity_geo" %in% names(d)){
    d[, granularity_geo:=dplyr::case_when(
      stringr::str_detect(location_code,"municip") ~ "municip",
      stringr::str_detect(location_code,"county") ~ "county",
      stringr::str_detect(location_code,"norge") ~ "nation",
      stringr::str_detect(location_code,"ward") ~ "ward"
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
    dates <- unique(d[, "date"])
    dates[,yrwk := fhi::isoyearweek(date)]
    d[dates,on="date",yrwk := yrwk]
  }
  if(!"season" %in% names(d)){
    dates <- unique(d[, "yrwk"])
    dates[,season := fhi::season(yrwk)]
    d[dates,on="yrwk",season := season]
  }
  if(!"year" %in% names(d)){
    dates <- unique(d[, "date"])
    dates[,year := fhi::isoyear_n(date)]
    d[dates, on="date",year := year]
  }
  if(!"week" %in% names(d)){
    dates <- unique(d[, "date"])
    dates[,week := fhi::isoweek_n(date)]
    d[dates,on="date", week := week]
  }
  if(!"x" %in% names(d)){
    dates <- unique(d[, "week"])
    dates[,x := fhi::x(week)]
    d[dates, on="week", x := x]
  }
}
