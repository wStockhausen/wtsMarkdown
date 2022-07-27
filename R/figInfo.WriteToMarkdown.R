#'
#' @title Process a figInfo object or file for inclusion in a markdown file
#'
#' @description Function to process a figInfo object or file for inclusion in a markdown file.
#'
#' @param figInfo - figInfo dataframe or filename for csv file in figInfo format
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return nothing
#'
#' @details Creates text in markdown format to insert figures in a figInfo object
#' into a markdown file. Handles figures represented as pdf files or as latex code in text files.
#'
#' @import magrittr
#' @import readr
#'
#' @md
#'
#' @export
#'
figInfo.WriteToMarkdown<-function(figInfo,verbose=FALSE){
    if (is.character(figInfo)) figInfo = readr::read_csv(figInfo);
    nf = nrow(figInfo);
    for (f in 1:nf){
      fi = figInfo[f,];
      if (fi$orientation=="landscape") cat("\n\\blandscape\n")
      fn = file.path(fi$path,fi$fn);
      if (!file.exists(fn)) stop("Figure file '",fn,"' was not found!")
      wtsMarkdown::insertImage(fn,cap=fi$caption,lbl=fi$label,width=fi$width,height=fi$height);
      cat("\n\n<!--\\FloatBarrier-->\n");
      if (fi$orientation=="landscape") cat("\n\\elandscape\n")
    }
}
