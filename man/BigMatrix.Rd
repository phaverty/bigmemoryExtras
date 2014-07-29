\name{BigMatrix}
\alias{BigMatrix}
\title{Create a new BigMatrix}
\usage{
  BigMatrix(x = NA_real_, backingfile, nrow, ncol,
    dimnames = NULL, type = "double")
}
\arguments{
  \item{x}{scalar numeric, NULL, matrix, or big.matrix.
  Optional data or big.matrix for new BigMatrix. A scalar
  numeric can be used to initalize the whole matrix. NULL
  gives the bigmemory feature of initializing to all zeros
  instantly.}

  \item{backingfile}{character, full path to the file that
  will contain the data matrix}

  \item{nrow}{integer, number of rows in the matrix we are
  about to create}

  \item{ncol}{integer, number of columns in the matrix we
  are about to create}

  \item{dimnames}{list, list(rownames,colnames), as for a
  typical matrix}

  \item{type}{character type of big.matrix (double,
  integer, char)}
}
\value{
  BigMatrix
}
\description{
  Create a new BigMatrix
}
\examples{
dnames = dimnames=list(letters[1:3],LETTERS[1:3])
x = matrix(1:9,ncol=3,dimnames=dnames)
ds = BigMatrix(x,tempfile())
ds = BigMatrix(backingfile=tempfile(),nrow=3,ncol=3,dimnames=dnames)
}

