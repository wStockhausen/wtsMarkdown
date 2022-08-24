#'
#' @title Process a tableInfo object or file for inclusion in a RMD file
#'
#' @description Function to process a tableInfo object or file for inclusion in a markdown file.
#'
#' @param tblInfo - a tableInfo object (dataframe) or filename for csv file in tableInfo format
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return nothing
#'
#' @details Creates text in markdown format to insert tables in a tableInfo object
#' into a markdown file. Handles tables represented as pdf files or as latex code in text files.
#' See [tableInfo()].
#'
#' @import magrittr
#' @import readr
#' @import stringr
#'
#' @md
#'
#' @export
#'
tableInfo.WriteToRMD<-function(tblInfo,docformat="latex",verbose=FALSE){
    if (is.character(tblInfo)) tblInfo = readr::read_csv(tblInfo);
    lst = list(); ctr=0;
    nf = nrow(tblInfo);
    for (f in 1:nf){
      if (verbose) message("Creating table ",f," of ",nf," tables")
      ti = tblInfo[f,];
      if (verbose) message("\tlabel = '",ti$label,"'. fn= '",f,"'.");
      maxW = 6.5;
      maxH = 8.0;#--leave room for caption
      isLandscape = tolower(ti$orientation)=="landscape";
      if (verbose) message(ti$label," is landscape? ",isLandscape," ",ti$orientation);
      if (isLandscape) {
        if (docformat=="latex"){
          lst[[ctr%<>%+1]]=paste0("\n\\blandscape\n");
        }
      }
      if ((is.null(ti$type))|(ti$type %in% c("","latex"))){
        #--table is in latex text format
        if (ti$latex==""){
          #--code contained in separate file
          fn = file.path(ifelse(ti$path=="",".",ti$path),ti$fn);
          lns = readLines(con=fn);     #--read latex code
          lst[[ctr%<>%+1]]=paste(lns,sep="\n");           #--insert code
        } else {
          #--code contained in ti$latex
          lst[[ctr%<>%+1]]=paste0(ti$latex);               #--insert code
        }
      } else {
        #--table is in image format
        fn = file.path(ifelse(ti$path=="",".",ti$path),ti$fn);
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
        size=paste0("width=",width);
        if (verbose) {
          message("table image size was ",dims$w," x ",dims$h);
          message("table image size is  ",width ," x ", height);
        }
        str=paste0("```{r ",ti$label,",echo=FALSE}\n",
                   '  knitr::kable("![](',fn,')"),',"caption='",ti$caption,'",format="',docformat,'")\n',
                   "```\n");
        lst[[ctr%<>%+1]]=str;
      }
      if (isLandscape) {
        if (docformat=="latex"){
          lst[[ctr%<>%+1]]=paste0("\n\\elandscape\n\n")
          lst[[ctr%<>%+1]]=paste0("\n\\clearpage\n\n");
        }
      }
    }
    str = "";
    for (l in lst) str %<>% paste0(l,sep="\n");
    str %<>% paste0("\n")
    return(str)
}

