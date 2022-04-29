#'
#' @title Create a figure information dataframe, or append figure information to one
#'
#' @description Function to create a figure information dataframe, or append figure information to one.
#'
#' @param .x - NULL, or an existing figure information dataframe (see [@details])
#' @param cap - the caption for the figure
#' @param label - the latex label for the figure
#' @param file - the path to a file containing a figure or image
#' @param width - figure width (in inches)
#' @param height - figure height (in inches)
#' @param orientation - the orientation for the figure ("portrait" or "landscape")
#'
#' @return A figure information dataframe (with a single row or the input dataframe with an added row of information).
#'
#' @details A figure information dataframe has columns label, file, caption, width, height, and orientation.
#'
#' @export
#'
figureInfo<-function(.x,caption="",label="",file="",width=6.5,height=6.5,orientation=c("portrait","landscape")){
  .x = rbind(.x,
             data.frame(label=label,file=file,caption=caption,width=width,height=height,orientation=orientation[1],stringsAsFactors=FALSE));
  return(.x);
}

