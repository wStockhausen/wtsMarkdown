#'
#' @title Parse figure info text (from a latex md file)
#'
#' @description Function to parse figure info text (from a latex md file).
#'
#' @param figtxt the figure text (i.e., text between \\begin\{figure\} and \\end\{figure\} delimiters)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return a figInfo object (a tibble with columns label, path, fn, width, height, orientation and caption)
#'
#' @details Parsing assumes the input file is in markdown format for Latex. See [figInfo()].
#'
#' @import stringr
#'
#' @export
#'
figInfo.ParseText<-function(figtxt,verbose=FALSE){
  if (verbose) message("#---------Starting figInfo.ParseText")
  #--extract includegraphics[...]
  t1 = stringr::str_remove(figtxt,fixed("\\includegraphics["));
  sizeInfo = stringr::str_remove(t1,stringr::regex("\\].*"));
  t2 = stringr::str_remove(t1,stringr::fixed(paste0(sizeInfo,"]")));
  splt = stringr::str_split(t2,"\\{")[[1]]
  path = stringr::str_remove(splt[2],stringr::regex("\\}.*$"));#--this is the full path name
  captn = stringr::str_remove(splt[3],stringr::regex("\\}.*$"));
  label = stringr::str_remove(stringr::str_remove(splt[4],stringr::regex("\\}.*$")),fixed("fig:"));
  #--extract width, height info from sizeInfo
  splts = stringr::str_split(sizeInfo,",")[[1]]; width="";height="";
  for (splt in splts){
    if (stringr::str_detect(splt,stringr::fixed("width")))  width =stringr::str_extract_all(splt[1],regex("[+-]?(\\d*\\.)?\\d+"))[[1]];#--"(\\d+\\.\\d+)|(\\d+)" also works, but would ignore negative sign
    if (stringr::str_detect(splt,stringr::fixed("height"))) height=stringr::str_extract_all(splt[1],regex("[+-]?(\\d*\\.)?\\d+"))[[1]];
  }
  if (verbose) message("figInfo.ParseText: ",figtxt,"\n",paste0("'",paste(label,captn,path,sizeInfo,width,height,sep="'\n\'"),"'"));
  fi = figInfo(label=label,path=dirname(path),fn=basename(path),width=width,height=height,orientation="portrait",caption=captn);#--using default orientation
  return(fi)
}
