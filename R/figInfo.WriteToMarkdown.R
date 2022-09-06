#'
#' @title Process a figInfo object or file for inclusion in a markdown file
#'
#' @description Function to process a figInfo object or file for inclusion in a markdown file.
#'
#' @param figInfo - figInfo dataframe or filename for csv file in figInfo format
#' @param env - "environment" in which to write markdown (default="latex")
#' @param includeLabels - include label caption (default=FALSE)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return nothing
#'
#' @details Creates text in markdown format to insert figures in a figInfo object
#' into a markdown file. Handles figures represented as pdf files or as latex code in text files.
#'
#' The \code{env} parameter is used to specify the output format for escaping characters in captions.
#' See [escapeChars()].
#'
#' Setting \code{includeLabels} adds the label to the caption to make referencing easier.
#'
#' @import magrittr
#' @import readr
#'
#' @md
#'
#' @export
#'
figInfo.WriteToMarkdown<-function(figInfo,env="latex",includeLabels=FALSE,verbose=FALSE){
    if (is.character(figInfo)) figInfo = readr::read_csv(figInfo);
    nf = nrow(figInfo);
    for (f in 1:nf){
      fi = figInfo[f,];
      isLandscape = tolower(fi$orientation)=="landscape";
      if (verbose) message(fi$label," is landscape? ",isLandscape," ",fi$orientation);
      if (isLandscape)  cat("\n\\blandscape\n");
      fn = file.path(fi$path,fi$fn);
      if (!file.exists(fn)) stop("Figure file '",fn,"' was not found!");
      label = sanitizeLabels(fi$label,env=env);
      caption = escapeChars(fi$caption,env=env);
      if (includeLabels) caption = paste0("'**",label,"**'. ",caption);
      dims = getImageDims(fn);
      maxW = 6.5; maxH = 8.0;  #--leave room for caption
      if (isLandscape){ maxW = 9.0; maxH = 5.5;}
      width = dims$w; height = dims$h;
      if (width<maxW) {
        #--width ok, check height
        if (dims$h>maxH){
          height = maxH;              #set hight to max
          width = width*(maxH/dims$h);#--need to shrink width by maxH/dims$h
        }#--else height ok too
      } else {  #--width>maxW
        height = dims$h*(maxW/width); #--scale height by maxW/width
        width  = maxW;                #--set width to maxW
        if (height>maxH){
          width = width*(maxH/height);#--scale width by maxH/height
          height = maxH;              #--set height to maxH
        }#---else height ok
      }
      if (verbose) {
        message("table image size was ",dims$w," x ",dims$h);
        message("table image size is  ",width ," x ", height);
      }
      wtsMarkdown::insertImage(fn,
                               cap=caption,
                               lbl=label,
                               width=width,
                               height=height);
      cat("\n\n<!--\\FloatBarrier-->\n");
      if (fi$orientation=="landscape") cat("\n\\elandscape\n")
    }
}
