#'
#' @title Escape special characters in character vector.
#'
#' @description Function to escape special characters in character vector.
#'
#' @param  x - character vector in which to escape special characters
#' @param env - environment for escaping characters (default="latex")
#'
#' @return character vector with special characters escaped.
#'
#' @details Currently escapes the following characters: "&", "%", "_" in
#' a latex environment.
#'
#' @export
#'
escapeChars<-function(x,env="latex"){
  if (env=="latex"){
    x=gsub("\\&","&",x,fixed=TRUE); #--unescape first
    x=gsub("&","\\&",x,fixed=TRUE);

    x=gsub("\\%","%",x,fixed=TRUE); #--unescape first
    x=gsub("%","\\%",x,fixed=TRUE);

    x=gsub("\\_","_",x,fixed=TRUE); #--unescape first
    x=gsub("_","\\_",x,fixed=TRUE);
  }
  return(x);
}

