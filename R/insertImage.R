#'
#' @title Create the latex to insert an image into a latex document
#'
#' @description Function create the latex to insert an image into a latex document.
#'
#' @param fn - filename (including path) of image to insert
#' @param cap - the caption for the image
#' @param lbl - the latex label for the image
#' @param width - figure width (in inches)
#' @param height - figure height (in inches)
#' @param align - character string indicating latex alignment
#'
#' @return nothing.
#'
#' @details Intended for use in a pdf-bound RMD script to create the latex
#' code to insert an image into a pdf document.
#'
#' @export
#'
insertImage<-function(fn,cap,lbl=cap,width=6.5,height=6,align="ht!"){
  if (!is.na(width)) w=paste0("width=",width,"in");
  if (!is.na(height)) h=paste0("height=",height,"in")
  if (is.na(height)) {
    ig = paste0("\\includegraphics[",w,"]");
   } else if (is.na(width)) {
    ig = paste0("\\includegraphics[",h,"]");
   } else {
    ig = paste0("\\includegraphics[",w,",",h,"]");
   }
  #message("insertImage: fn = ",fn)
  s   = paste0(ig,"{",fn,"} \\caption{",cap,"}\\label{fig:",lbl,"}");
  all = c(paste0("\\begin{figure}[",align,"]"),
          s,
          "\\end{figure}")
  #message("all: ",all)
  cat(all,sep="\n");
}
