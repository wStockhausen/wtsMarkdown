#'
#' @title Create a table information dataframe, or append table information to one
#'
#' @description Function to create a table information dataframe, or append table information to one.
#'
#' @param .x - NULL, or an existing table information dataframe (see [Details])
#' @param cap - the caption for the table
#' @param label - the latex label for the table
#' @param path - the path to a file containing a latex table
#' @param fn - the name of the file containing a latex table
#' @param typesize - the type size for the table ("normal" or "small")
#' @param orientation - the orientation for the table ("portrait" or "landscape")
#' @param type - table format ("latex" or "pdf")
#' @param latex - latex-formatted table as character string or ""
#'
#' @return A tableInfo tibble (with a single row or the input dataframe with an added row of information).
#'
#' @details A tableInfo tibble has columns label, path, fn, caption, typesize, orientation, (format) type, and latex.
#'
#' @export
#'
tableInfo<-function(.x,caption="",label="",path="",fn="",typesize=c("normal","small"),orientation=c("portrait","landscape"),type=c("latex","pdf"),latex=""){
  y = tibble::tibble(label=label,path=path,fn=fn,caption=caption,typesize=typesize[1],orientation=orientation[1],type=type[1],latex=latex)
  if (!missing(.x)) y = dplyr::bind_rows(.x,y);
  return(y);
}

