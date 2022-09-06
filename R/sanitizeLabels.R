#'
#' @title Sanitize labels by replacing illegal characters with "."
#'
#' @description Function to sanitize labels by replacing illegal characters with ".".
#'
#' @param  x - character vector to sanitize
#' @param env - environment for sanitizing characters (default="latex")
#'
#' @return character vector with sanitized labels.
#'
#' @details Currently sanitizes the following characters: "&", "%", "_" in
#' a latex environment.
#'
#' @export
#'
sanitizeLabels<-function(x,env="latex"){
  if (env=="latex"){
    x=gsub("\\&","&",x,fixed=TRUE); #--unescape first
    x=gsub("&",".",x,fixed=TRUE);

    x=gsub("\\%","%",x,fixed=TRUE); #--unescape first
    x=gsub("%",".",x,fixed=TRUE);

    x=gsub("\\_","_",x,fixed=TRUE); #--unescape first
    x=gsub("_",".",x,fixed=TRUE);
  }
  return(x);
}
