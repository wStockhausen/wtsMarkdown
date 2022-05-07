#'
#' @title Read figInfo from a csv file and add it to an existing figInfo object
#'
#' @description Function to read figInfo from a csv file and add it to an existing figInfo object.
#'
#' @param allfi - existing figInfo object, or NULL
#' @param fncsv - name of csv file in figInfo format, or a dataframe in figInfo format
#'
#' @return a dataframe in figInfo format with information from the csv file appended
#'
#' @details Filenames in the csv file ending in a blank are assumed to be pdf files (under the assumption
#' the csv file was crated by [parseFigInfoFromMarkdownFile()]. See [figInfo()] for format information on returned dataframe.
#'
#' @import readr
#' @import stringr
#'
#' @md
#'
#' @export
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

