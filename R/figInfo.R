#'
#' @title Create a figInfo object (a figure information tibble), or append figure information to one
#'
#' @description Function to create a figInfo object (a figure information tibble), or append figure information to one.
#'
#' @param .x - NULL, or an existing figure information dataframe (see [@details])
#' @param label - the latex label for the figure
#' @param path - path to file (not including filename)
#' @param fn - the filename for a figure or image
#' @param width - figure width (in inches)
#' @param height - figure height (in inches)
#' @param orientation - the orientation for the figure ("portrait" or "landscape")
#' @param caption - the caption for the figure
#'
#' @return A figure information tibble (with a single row or the input dataframe with an added row of information).
#'
#' @details A figure information dataframe has columns label, path, fn, width, height, orientation, and caption.
#'
#' @import dplyr
#' @import tibble
#'
#' @md
#'
#' @export
#'
figInfo<-function(.x,label="",path="",fn="",width=NA_real_,height=NA_real_,orientation=c("portrait","landscape"),caption=""){
  y = tibble::tibble(label=label,path=path,fn=fn,width=width,height=height,orientation=orientation[1],caption=caption);
  if (!missing(.x)) y = dplyr::bind_rows(.x,y);
  return(y);
}

#'
#' @title Read figInfo from a csv file and add it to an eexisting figInfo object
#'
figInfo.ReadCSV<-function(allfi,fncsv){
  if (is.character(fncsv)) {
    if (file.exists(fncsv)){
      dfr = readr::read_csv(fncsv);
    } else {
      stop("figInfo.ReadCSV: input csv file '",fncsv,"' not found!")
    }
  } else {
    dfr = fncsv;#--fncsv should be a dataframe
  }

  for (i in 1:nrow(dfr)){
    #--testing: i=1;
    rw = dfr[i,];
    fn = rw$fn;
    if (!stringr::str_ends(fn,stringr::regex("([^\\s]+(\\.(?i)(jpe?g|png|gif|bmp|pdf))$)"))) fn = paste0(fn,".pdf");#--see: https://www.geeksforgeeks.org/how-to-validate-image-file-extension-using-regular-expression/
    if (!file.exists(file.path(rw$path,fn))) {
      stop("figInfo.ReadCSV: figure file '",file.path(rw$path,fn),"' not found!")
    }
    orientation = "portrait"; #--default orientation
    if (rw$width>6.5) orientation = "landscape";
    allfi %<>% wtsMarkdown::figInfo(label=rw$label,
                                    path = rw$path,
                                    fn   = fn,
                                    caption=rw$caption,
                                    width=rw$width,
                                    height=wtsMarkdown::getAspectRatio(file.path(rw$path,fn))*rw$width,
                                    orientation=orientation);
  }
  return(allfi);
}
