#'
#' @title Write a tabular object in latex format to a connection
#'
#' @description Function to write a tabular object in latex format to a connection.
#'
#' @param tbl - a tabular object
#' @param cap - table caption
#' @param label - label for referencing
#' @param conn - connection to write to
#' @param justification - character indicating table justification ("l","r")
#' @param type - table type ("","longtable")
#' @param size - font size ("","small")
#'
#' @return string version of table in latex format (invisibly)
#'
#' @details If \code{conn} is "", the created latex string is written to the standard output connection (in most cases, the console).
#' If \code{conn} is not "", the connection is opened for writing to text and the string is written to it. If \code{conn=NULL}, nothing is written.
#' In all cases, the string is returned invisibly
#' to the calling function.
#'
#' This uses [tables::toLatex()] to create the latex representation of the tabular object.
#'
#' @import dplyr
#' @import tables
#'
#' @md
#'
#' @export
#'
writeTabularToLatex<-function(tbl,
                              cap,
                              conn="",
                              label=NULL,
                              justification="l",
                              type=c("","longtable"),
                              size=c("","small")){
  type = type[1];
  size = size[1];
  add<-function(str,add){return(paste0(str,add))}

  str = "";
  if (size=="small") str %<>% add("\\begin{small}\n");
  if (type=="") {
    str %<>% add("\\begin{table}[H]\n");
    str %<>% add(paste0("\\caption{",cap,"}\n"));
    if (!is.null(label)) str %<>% add(paste0("\\label{",label,"}"));
    str %<>% add("\\centering\n");
  }

  if (type=="")
    res = tables::toLatex(tbl,options=list(tabular="tabular",justification=justification));
  if (type=="longtable"){
    toprule = paste0("\\caption{",cap,"} \\\\\n\\hline");
    if (!is.null(label)) toprule = paste0("\\caption{",cap,"} \\label{",label,"} \\\\\n\\hline");
    res = tables::toLatex(tbl,options=list(tabular="longtable",
                                           toprule=toprule));
  }

  str %<>% add(res$text);

  if (type=="") str %<>% add("\\end{table}");
  if (size=="small") str %<>% add("\\end{small}");

  if (!is.null(conn)){
    if (conn!="") conn=file(conn,open="wt");
    cat(str,file=conn);
    if (!is.character(conn)) close(conn);
  }

  return(invisible(str));
}
