\name{BigMatrixFactor}
\alias{BigMatrixFactor}
\title{Create a new BigMatrixFactor}
\usage{
  BigMatrixFactor(x = NA_character_, backingfile, nrow,
    ncol, dimnames = NULL, levels)
}
\arguments{
  \item{x}{scalar or matrix to be treated as character.}

  \item{backingfile}{character, full path to the file that
  will contain the data matrix}

  \item{nrow}{integer, number of rows in the matrix we are
  about to create}

  \item{ncol}{integer, number of columns in the matrix we
  are about to create}

  \item{dimnames}{list, list(rownames,colnames), as for a
  typical matrix}

  \item{levels}{character, as for a typical factor}
}
\value{
  BigMatrixFactor
}
\description{
  Create a new BigMatrixFactor
}
\examples{
dnames = dimnames=list(letters[1:3],LETTERS[1:3])
levels=c("AA","AB","BB")
x = matrix( sample( levels, 9, replace=TRUE), ncol=3, dimnames=dnames)
ds = BigMatrixFactor(x,tempfile(),levels=levels)
ds = BigMatrixFactor(backingfile=tempfile(),nrow=3,ncol=3,dimnames=dnames,levels=levels)
}

