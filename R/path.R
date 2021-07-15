#' build file.path
#'
#' @param x a path
#' @param y a path extention
#'
#' @return
#' @export
#'
#' @examples x %//% y
`%//%` <- function(x, y){
  tryCatch({
    !is.character(x)
  },
    error=function(e){
      TRUE
    }) -> flag_notCharacter
  if(flag_notCharacter){
    sym_x <- rlang::ensym(x)
    if(sym_x == as.name(".")){
      .root <- rprojroot::is_rstudio_project$make_fix_file()
      x <- .root()
    }
  }
  newpath <- file.path(x,y)
  flag_file <-
    stringr::str_detect(
      basename(newpath),
      "\\.[:alnum:]*$")
  if(!flag_file){
    if(!dir.exists(newpath)) dir.create(newpath, recursive = T)
  }
  if(flag_file){
    basedir <- dirname(newpath)
    if(!dir.exists(basedir)) dir.create(basedir, recursive = T)
  }
  return(newpath)
}
