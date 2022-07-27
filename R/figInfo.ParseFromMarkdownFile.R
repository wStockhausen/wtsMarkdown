#'
#' @title Parse a markdown (.md) file to extract information on figures by document section
#'
#' @description Function to parse a markdown (.md) file to extract information on figures by document section.
#'
#' @param fn - the markdown (.md) filename, including path
#'
#' @return a figInfo tibble (see [figInfo()])
#'
#' @details The markdown file is assumed to be in markdown format for Latex.
#'
#' @importFrom stringr str_remove str_split fixed regex
#'
#' @export
#'
figInfo.ParseFromMarkdownFile<-function(fn,verbose=FALSE){
  if (verbose) message("#---------Starting figInfo.ParseTextFromMarkdownFile")
  #--read lines from file
  txt = readLines(con=fn);

  #--loop through text lines, identify sections, extract figure information
  lst = list(); ctr = 0;
  i = 0;
  sections=vector(mode="character",length=10);
  while (i < length(txt)){
    lvl<-stringr::str_count(txt[i<-i+1],fixed('#'));
    if (verbose) message("line ",i,": ",txt[i]);
    if (lvl>0){
      sections[lvl] = stringr::str_trim(stringr::str_remove_all(txt[i],fixed("#")));
      if (verbose){
        message("section '",sections[lvl],"' found at line ",i,": '",txt[i],"'");
        message("\tlevel = ",lvl," sections = ",paste(sections[1:lvl],collapse="::"))
      }
      lvlc = lvl;#--keep track of "current" level
    } else {
      if (stringr::str_detect(txt[i],fixed("\\begin{figure}"))){
        lst[[ctr<-ctr+1]] = figInfo();
        #--need to capture text until "\end{figure}"
        ibeg = i+1;
        while(!stringr::str_detect(txt[i<-i+1],fixed("\\end{figure}"))){}
        iend=i-1;
        figtxt=txt[ibeg:iend];
        if (verbose) message("figInfo.ParseTextFromMarkdownFile: found ",ctr,"th figure starting at line ",i,":\n",paste("\t",figtxt,"\n"))
        figInfo=figInfo.ParseText(figtxt,verbose=verbose);
        if (verbose) message("figInfo.ParseTextFromMarkdownFile: assigining figInfo values");
        lst[[ctr]]$label   = figInfo$label;
        lst[[ctr]]$path    = figInfo$path;
        lst[[ctr]]$fn      = figInfo$fn;
        lst[[ctr]]$width   = figInfo$width;
        lst[[ctr]]$height  = figInfo$height;
        lst[[ctr]]$caption = figInfo$caption;
        #--TDOO: capture current orientation also
        if (verbose) message("figInfo.ParseTextFromMarkdownFile: assigined figInfo values");
      }
    }
  }#--while
  tbl = dplyr::bind_rows(lst);
  return(tbl);
}

#figInfo = figInfo.ParseTextFromMarkdownFile("modelComparisons.knit.md")

