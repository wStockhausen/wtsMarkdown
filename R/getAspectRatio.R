#'
#' @title Get the aspect ratio of an image
#'
#' @description Function to get the aspect ratio of an image.
#'
#' @param img - an image object or a filename (as a string)
#'
#' @return the aspect ratio (height/width)
#'
#' @details Uses [getImageDims()] to obtain the aspect ratio.
#'
#' @md
#'
#' @export
#'
getAspectRatio<-function(img){
  return(getImageDims(img)$asp);
}
