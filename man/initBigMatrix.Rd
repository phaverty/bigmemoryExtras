\name{.initBigMatrix}
\alias{.initBigMatrix}
\title{Create a new BigMatrix-derived class}
\usage{
  .initBigMatrix(x = NULL,
    class = c("BigMatrix", "BigMatrixFactor"), backingfile,
    nrow, ncol, dimnames = NULL, type = "double", ...)
}
\arguments{
  \item{x}{NULL, matrix, or big.matrix. Optional data or
  big.matrix for new BigMatrix}

  \item{class}{character, class name.  BigMatrix or
  BigMatrixFactor currently.}

  \item{backingfile}{character, full path to the file that
  will contain the data matrix}

  \item{nrow}{integer, number of rows in the matrix we are
  about to create}

  \item{ncol}{integer, number of columns in the matrix we
  are about to create}

  \item{dimnames}{list, list(rownames,colnames), as for a
  typical matrix}

  \item{type}{character, can be double, integer, or char}

  \item{...}{other args to pass to "new" method of
  generator object, like levels}
}
\value{
  BigMatrix
}
\description{
  Create a new BigMatrix-derived class
}
\keyword{internal}

