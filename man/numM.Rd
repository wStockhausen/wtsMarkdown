% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numberFunctions.R
\name{numM}
\alias{numM}
\title{Apply a pretty format for numbers in millions}
\usage{
numM(x)
}
\arguments{
\item{x}{- vector (numeric or character) of numbers to apply pretty format to}
}
\value{
character vector
}
\description{
Function to apply a pretty format for numbers in millions.
}
\details{
Values >= 10000 are rounded to single ones,
values >= 1000 are formatted with 1 decimal place,
values >= 100 are formatted with 2 decimal places,
values < 100 are formatted with 3 decimal places.
}
