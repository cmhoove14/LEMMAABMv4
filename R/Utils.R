#----------------------------
# Utils 
# Chris Hoover
#----------------------------

#' @title no tests
#' 
#' @description No tests on miscellaneous day
#' 
#' @return 0
#' @export
#' 

tests0 <- function(date_num){
  return(0)
}

#' @title Not in 
#' 
#' @description Opposite of `%in%`
#' 
#' @return function for opposite of `%in%``
#' @export

`%!in%` <- Negate(`%in%`)

#' @title Summarize Infection
#' 
#' @description Function to summarize infection vector with individual states into aggregate
#' 
#' @param x infections status vector from `ABM` run
#' 
#' @return vector of length 9 with integer counts of number of agents in each class
#' @export

sum.inf <- function(x){
  
  S = sum(x == "S")
  E = sum(x == "E")
  Ip = sum(x == "Ip")
  Ia = sum(x == "Ia")
  Im = sum(x == "Im")
  Imh = sum(x == "Imh")
  Ih = sum(x == "Ih")
  D = sum(x == "D")
  R = sum(x == "R")
  
  return(c(S, E, Ip, Ia, Im, Imh, Ih, D, R))
}


#' @title Unpack elements of list into global environment
#' 
#' @description Taken from https://stackoverflow.com/questions/26168592/recursively-send-list-variables-to-the-global-environment and turned quiet
#' 
#' @param l list (or list of lists) to unpack
#' 
#' @return assigns elements of list(s) to global environment
#' @export

unpack_list <- function(l) {
  if(is.list(l)) { 
    list2env(l, envir = .GlobalEnv)
    invisible(lapply(l, unpack_list))
  }
}
