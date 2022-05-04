#'
#' @title Parse figure info text from a latex md file
#'
#' @description Function to parse figure info text from a latex md file.
#'
#' @param figtxt the figure text (i.e., text between \\begin\{figure\} and \\end\{figure\} delimiters)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @return list with elements label, path, width, height, and caption
#'
#' @details Parsing assumes the input file is in markdown format for Latex.
#'
#' @import stringr
#'
#' @export
#'
parseFigInfoText<-function(figtxt,verbose=FALSE){
  if (verbose) message("#---------Starting parseFigInfoText")
  #--extract includegraphics[...]
  t1 = stringr::str_remove(figtxt,fixed("\\includegraphics["));
  sizeInfo = stringr::str_remove(t1,stringr::regex("\\].*"));
  t2 = stringr::str_remove(t1,stringr::fixed(paste0(sizeInfo,"]")));
  splt = stringr::str_split(t2,"\\{")[[1]]
  path = stringr::str_remove(splt[2],stringr::regex("\\}.*$"));
  captn = stringr::str_remove(splt[3],stringr::regex("\\}.*$"));
  label = stringr::str_remove(stringr::str_remove(splt[4],stringr::regex("\\}.*$")),fixed("fig:"));
  #--extract width, height info from sizeInfo
  splts = stringr::str_split(sizeInfo,",")[[1]]; width="";height="";
  for (splt in splts){
    if (stringr::str_detect(splt,stringr::fixed("width")))  width =stringr::str_extract_all(splt[1],regex("[+-]?(\\d*\\.)?\\d+"))[[1]];#--"(\\d+\\.\\d+)|(\\d+)" also works, but would ignore negative sign
    if (stringr::str_detect(splt,stringr::fixed("height"))) height=stringr::str_extract_all(splt[1],regex("[+-]?(\\d*\\.)?\\d+"))[[1]];
  }
  if (verbose) message("parseTextInfo: ",figtxt,"\n",paste0("'",paste(label,captn,path,sizeInfo,width,height,sep="'\n\'"),"'"));
  return(list(label=label,path=path,size=sizeInfo,caption=captn,width=width,height=height))
}
