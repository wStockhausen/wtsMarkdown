#'
#' @title Create a figInfo object (a figure information tibble), or append figure information to one
#'
#' @description Function to create a figInfo object (a figure information tibble), or append figure information to one.
#'
#' @param .x - NULL, or an existing figure information dataframe (see [@details])
#' @param label - the latex label for the figure
#' @param path - path to file (not including filename)
#' @param fn - the filename for a figure or image
#' @param width - figure width (in inches)
#' @param height - figure height (in inches)
#' @param orientation - the orientation for the figure ("portrait" or "landscape")
#' @param caption - the caption for the figure
#'
#' @return A figure information tibble (with a single row or the input dataframe with an added row of information).
#'
#' @details A figure information dataframe has columns label, path, fn, width, height, orientation, and caption.
#'
#' @import dplyr
#' @import tibble
#'
#' @md
#'
#' @export
#'
figInfo<-function(.x,label="",path="",fn="",width=NA_real_,height=NA_real_,orientation=c("portrait","landscape"),caption=""){
  y = tibble::tibble(label=label,path=path,fn=fn,width=width,height=height,orientation=orientation[1],caption=caption);
  if (!missing(.x)) y = dplyr::bind_rows(.x,y);
  return(y);
}


