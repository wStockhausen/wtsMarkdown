#'
#' @title Create a table information dataframe, or append table information to one
#'
#' @description Function to create a table information dataframe, or append table information to one.
#'
#' @param .x - NULL, or an existing table information dataframe (see [@details])
#' @param cap - the caption for the table
#' @param label - the latex label for the table
#' @param file - the path to a file containing a latex table
#' @param typesize - the type size for the table ("normal" or "small")
#' @param orientation - the orientation for the table ("portrait" or "landscape")
#' @param type - table format ("latex" or "pdf")
#'
#' @return A table information dataframe (with a single row or the input dataframe with an added row of information).
#'
#' @details A table information dataframe has columns label, file, caption, typesize, orientation, and format type.
#'
#' @export
#'
tableInfo<-function(.x,caption="",label="",file="",typesize=c("normal","small"),orientation=c("portrait","landscape"),type=c("latex","pdf")){
  .x = rbind(.x,
             data.frame(label=label,file=file,caption=caption,typesize=typesize[1],orientation=orientation[1],type=type[1],stringsAsFactors=FALSE));
  return(.x);
}

