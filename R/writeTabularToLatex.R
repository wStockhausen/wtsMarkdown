#'
#' @title Write a tabular object in latex format to a connection
#'
#' @description Function to write a tabular object in latex format to a connection.
#'
#' @param tbl - a tabular object
#' @param cap - caption
#' @param conn - connection to write to
#' @param label - label for latex
#' @param justification - character indicating table justification ("l","r")
#' @param type - table type ("","longtable")
#' @param size - font size ("","small")
#'
#' @return nothing.
#'
#' @details If \code{conn} is not "", the connection is opened for writing to text.
#' Uses [tables::toLatex()] to create the latex representation of the tabular object.
#'
#' @import tables
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
  if (conn!="") conn=file(conn,open="wt");

  if (size=="small") catp("\\begin{small}")
  if (type=="") {
    catp("\\begin{table}[H]",conn);
    catp(paste0("\\caption{",cap,"}"),conn);
    if (!is.null(label)) catp(paste0("\\label{",label,"}"),conn);
    catp("\\centering",conn);
  }

  if (type=="")
    res = tables::toLatex(tbl,options=list(tabular="tabular",justification=justification));
  if (type=="longtable"){
    toprule = paste0("\\caption{",cap,"} \\\\\n\\hline");
    if (!is.null(label)) toprule = paste0("\\caption{",cap,"} \\label{",label,"} \\\\\n\\hline");
    res = tables::toLatex(tbl,options=list(tabular="longtable",
                                           toprule=toprule));
  }

  cat(res$text,file=conn);

  if (type=="") catp("\\end{table}",conn);
  if (size=="small") catp("\\end{small}",conn);
  close(conn);
}
