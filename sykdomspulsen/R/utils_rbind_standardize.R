rbind_standardize <- function(..., use.names = T){
  dots <- list(...)
  for(i in seq_along(dots)){
    for(j in seq_along(names(dots[[i]]))){
      if(inherits(dots[[i]][[j]],"Date")){
        set(dots[[i]], j=j, value = data.table::as.IDate(dots[[i]][[j]]))
      }
      if(inherits(dots[[i]][[j]],"glue")){
        set(dots[[i]], j=j, value = as.character(dots[[i]][[j]]))
      }
    }
  }
  rbindlist(dots, use.names=use.names)
}
