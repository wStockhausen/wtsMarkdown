#'
#' @title Fix column labels for row factors in a \code{tabular} table
#'
#' @description Function to fix column labels for row factors in a \code{tabular} table
#'
#' @param tbr - tabular table
#' @param labelsForRowFactors - character vector of desired column names for row factors
#' @param dim - dimension of the tbr's dimnames attribute in which the row labels reside
#' @param - verbose - flag (T/F) to print diagnostic information
#'
#' @return \code{tbr}, with labels for row factors updated to the input \code{labelsForRowFactors}.
#'
#' @details The [tables::tabular()] function seems to have a problem with row factors that have one value, and
#' that is that the resulting tabular object is lacking column headings for some (most?) of those factors--something
#' which the conversion to pdf does not like. This function gives a quick way for assiging the "correct" headings (or new ones,
#' for that matter) to a tabular object. At this point, the tables I've dealt with have all had the row factor column headings in the 2nd dimension
#' of the rorwLabel object's "dimnames" attribute, but I suspect this may vary with the dimension of the factor crossing (hence \code{dim},
#' which default is 2).
#'
#' Note also that this function may provide an easy way to fix collisions with latex constraints on column headings (it doesn't like underscores in headings, for one),
#' at least for those reflecting row factors.
#'
#' @importFrom tables rowLabels
#'
#' @md
#'
#' @export
#'
fixColLabelsForRowFactors<-function(tbr,labelsForRowFactors,dim=2,verbose=FALSE){
    rwLbls = tables::rowLabels(tbr);
    dimnames = attr(rwLbls,"dimnames");
    if (verbose) {
        for (i in 1:length(dimnames)) message("dimnames[[",i,"]] = ",dimnames[[i]],"\n")
        message("labels = ",labelsForRowFactors)
    }
    if (length(dimnames[[dim]])!=length(labelsForRowFactors)){
        str = paste0("length(dimnames[[",dim,"]])!=length(labelsForRowFactors): ",length(dimnames[[dim]])," ",length(labelsForRowFactors),"\n",
                     "dimnames[[",dim,"]]       =",dimnames[[dim]],"\n",
                     "labelsForRowFactors = ",labelsForRowFactors,"\n")
        stop(str);
    }
    dimnames[[dim]]<-labelsForRowFactors;
    attr(rwLbls,"dimnames") = dimnames;
    tables::rowLabels(tbr) = rwLbls;
    return(tbr);
}
