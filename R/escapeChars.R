#'
#' @title Escape special characters in character vector.
#'
#' @description Function to escape special characters in character vector.
#'
#' @param  x - character vector in which to escape special characters
#' @param env - environment for escaped characters
#'
#' @return character vector with special characters escaped.
#'
#' @details Currently escapes the following characters: &, %.
#'
#' @export
#'
escapeChars<-function(x,env="latex"){
  x=gsub("&","\\&",x,fixed=TRUE);
  x=gsub("%","\\%",x,fixed=TRUE);
  return(x);
}

