#'
#' @title Process a table info object or file
#'
#' @description Function to process a table info object or file.
#'
#' @param tblInfo - tblInfo dataframe or filename
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return nothing
#'
#' @details nothing.
#'
#' @import magrittr
#' @import readr
#' @import stringr
#'
#' @md
#'
#' @export
#'
processTableInfo<-function(tblInfo,verbose=FALSE){
    if (is.character(tblInfo)) tblInfo = readr::read_csv(tblInfo);
    nf = nrow(tblInfo);
    for (f in 1:nf){
      ti = tblInfo[f,];
      maxW = 6.5;
      maxH = 8.0;#--leave room for caption
      if (ti$orientation=="landscape") {
        cat("\n\\blandscape\n")
      }
      if ((is.null(ti$type))|(ti$type=="latex")){
        #--table is in latex text format
        lns = readLines(con=ti$file);#--read latex code
        cat(lns,sep="\n");           #--insert code
      } else {
        #--table is in pdf format
        fn = ti$file;
        if (!stringr::str_ends(fn,stringr::fixed(".pdf"))) fn = paste0(fn,".pdf");
        dims = getImageDims(fn);
        maxW = 6.5; maxH = 8.0;  #--leave room for caption
        if (ti$orientation=="landscape"){ maxW = 9.0; maxH = 5.5;}
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
        size=paste0("width=",width);
        if (verbose) {
          message("table image size was ",dims$w," x ",dims$h);
          message("table image size is  ",width ," x ", height);
        }
        str = paste0("\\begin{table} \n",
                     "  \\caption{",ti$caption,"}",
                     "  \\label{",ti$label,"} \n",
                     "    \\includegraphics[width=",width,"in]{",fn,"} \n",
                     " \\end{table}\n");
        if (verbose) message("inserting pdf table: ",str);
        cat(str);
      }
      if (ti$orientation=="landscape") cat("\n\\elandscape\n")
      if (ti$orientation!="landscape") cat("\n\\clearpage\n");
    }
}

