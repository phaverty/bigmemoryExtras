\name{BigMatrixFactor-class}
\Rdversion{1.1}
\docType{class}
\alias{BigMatrixFactor-class}
\alias{coerce,BigMatrixFactor,factor-method}
\alias{coerce,BigMatrixFactor,matrix-method}
\alias{levels,BigMatrixFactor-method}
\alias{levels<-,BigMatrixFactor-method}
\alias{nlevels,BigMatrixFactor-method}

\title{Class \code{"BigMatrixFactor"}}
\description{
The BigMatrixFactor class extends the BigMatrix class to add factor
functionality. Data are stored as 32-bit or 8-bit integers on-disk
depending on the number of levels. Reference Class methods are also
provided in S4 style to give the familiar matrix and factor API (please
see the examples below). Subsetting returns a integer matrix with the S3 class "factor" and
an appropriate levels attribute.
}
\section{Extends}{
Class \code{"\linkS4class{BigMatrix}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    S4-style methods supplement ReferenceClass methods to provide a
    standard factor API.
    \item{nlevels}{\code{signature(x = "BigMatrixFactor")}: Query the number of levels, as in a typical factor. }
    \item{levels}{\code{signature(x = "BigMatrixFactor")}: Access the levels, as in a typical factor. }
    \item{levels<-}{\code{signature(x = "BigMatrixFactor")}: Set the levels, as in a typical factor. Disallowed for now. }
  }
}
\author{
Peter M. Haverty
}
\seealso{
See Also \code{\link{ReferenceClasses}}, \code{\link{BigMatrix}}, \code{\link{factor}},
\code{\linkS4class{BigMatrix}}, \code{\link{makeActiveBinding}},\code{\link[bigmemory]{filebacked.big.matrix}}
}
\examples{
showClass("BigMatrixFactor")
back.file = tempfile()
x = matrix( sample( 1:3, 9, replace=TRUE), ncol=3, dimnames=list(letters[1:3],LETTERS[1:3]))
ds = BigMatrixFactor(x,back.file,levels=c("AA","AB","BB"))
ds[1,3]
ds[3,3] = "AB"
ds[,]
nlevels(ds)
ds$nlevels()
levels(ds)
ds$levels
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{bigmat}:}{Object of class \code{activeBindingFunction} Public access to the big.matrix object. }
    \item{\code{datapath}:}{Object of class \code{activeBindingFunction} Full file path to the big.matrix backing file. }
    \item{\code{descpath}:}{Object of class \code{character} Full file path to the big.matrix metadata file. }
    \item{\code{.bm}:}{Object of class \code{big.matrix} Private storage of the big.matrix object. }
    \item{\code{levels}:}{Object of class \code{activeBindingFunction} Public access to the levels. }
    \item{\code{.levels}:}{Object of class \code{character} Levels as in a typical factor. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{nlevels()}:}{ Check number of levels, as in a typical factor. }
    \item{\code{getValues(i, j, drop)}:}{ Getter for on-disk data.
      Returns a factor with the appropriate number of dimensions. }
    \item{\code{setValues(i, j, value)}:}{ Setter for on-disk data. Data
      converted to integer, matching to levels for character input. }
  }

The following methods are inherited (from the corresponding class):
setValues ("BigMatrix"), getValues ("BigMatrix"), attach ("BigMatrix"), save ("BigMatrix")
}
