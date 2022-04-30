#'
#' @title Get the dimensions of an image
#'
#' @description Function to get the dimensions of an image.
#'
#' @param img - an image object or a filename (as a string)
#'
#' @return list with elements w (width in inches), h (height in inches), and asp (aspect=h/w)
#'
#' @details Uses [magick::image_read()] to read an image file (if \code{img} is a character string) and
#' [magick::image_read()] to obtain size information on the image object. Width/height units are given in
#' inches.
#'
#' @importFrom magick image_read image_info
#' @importFrom stringr str_split_fixed
#'
#' @md
#'
#' @export
#'
getImageDims<-function(img){
  if (is.character(img)){
    img = magick::image_read(img);
  }
  inf = magick::image_info(img);
  dpi = as.numeric(stringr::str_split_fixed(inf$density,"x",2));
  return(list(w=inf$width/dpi[1],h=inf$height/dpi[2],asp=(inf$height/dpi[2])/(inf$width/dpi[1])));
}
