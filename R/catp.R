#'
#' @title Print a vector of character strings to a connection.
#'
#' @description Function to print a vector of character strings to a connection.
#'
#' @param str - vector of character strings
#' @param conn - connection to print to
#' @param sep - separator to use between vector elements
#'
#' @details Shortcut for \code{cat(str,sep=sep,file=conn)}.
#'
#' @export
#'
catp<-function(str,conn,sep="\n"){cat(str,sep=sep,file=conn);}
