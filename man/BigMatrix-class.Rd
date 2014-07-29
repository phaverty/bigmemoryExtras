\name{BigMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{BigMatrix-class}
\alias{[,BigMatrix,ANY,ANY,ANY-method}
\alias{[<-,BigMatrix,ANY,ANY-method}
\alias{[<-,BigMatrix,ANY,ANY,ANY-method}
\alias{annotatedDataFrameFrom,BigMatrix-method}
\alias{coerce,BigMatrix,matrix-method}
\alias{as.matrix,BigMatrix-method}
\alias{dim,BigMatrix-method}
\alias{dimnames,BigMatrix-method}
\alias{dimnames<-,BigMatrix,ANY-method}
\alias{length,BigMatrix-method}
\alias{ncol,BigMatrix-method}
\alias{nrow,BigMatrix-method}
\alias{apply,BigMatrix-method}

\title{Class \code{"BigMatrix"}}
\description{
This package defines a "BigMatrix" ReferenceClass which adds
safety and convenience features to the filebacked.big.matrix class from the
bigmemory package. BigMatrix protects against segfaults by monitoring and
gracefully restoring the connection to on-disk data. We provide
utilities for using BigMatrix-derived classes as
assayData matrices within the Biobase package's eSet family of classes.
BigMatrix provides some optimizations related to attaching to, and indexing
into, file-backed matrices with dimnames.

A big.matrix object is backed by two files, a data file (backingfile)
and a text metadata file (descriptor file). BigMatrix replaces
the text metadata file with a binary "rds" file, for faster access. The location of this file
is stored in the \code{descpath} field. The data in the metadata file is used to "attach" a
big.matrix object its on-disk data. BigMatrix stores the big.matrix object and checks for a
valid connection to the on-disk data each time it is necessary to access the big.matrix object.
Re-attachment, using the \code{attach} method and the \code{descpath} field is performed
as necessary. This avoids a segfault caused by accessing what could otherwise
be a nil pointer. For example, A nil pointer occurs when a big.matrix data is saved and loaded
using \code{save()} and \code{load()}.

Reference classes are a relatively new and unused feature of R. They
represent a new object model that is quite similar to OOP you might have
seen in java or python. The ?ReferenceClasses help page is an excellent
resource on this topic. As ReferenceClasses are an extension of R's S4
class system, S4-style methods can also be defined. In order to give the
familiar matrix API, BigMatrix's ReferenceClass methods have a
corresponding S4-style method.  Please see the examples below.
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
S4-style methods supplement ReferenceClass methods to provide a standard matrix API.
  \describe{
    \item{[}{\code{signature(x = "BigMatrix", i = "ANY", j = "ANY", drop = "ANY")}: Access matrix data. }
    \item{[<-}{\code{signature(x = "BigMatrix", i = "ANY", j = "ANY")}: Set matrix data. }
    \item{[<-}{\code{signature(x = "BigMatrix", i = "ANY", j = "ANY", NA = "ANY")}: Set matrix data }
    \item{annotatedDataFrameFrom}{\code{signature(object = "BigMatrix")}: Used in creation of a Biobase eSet with a BigMatrix as an assayDataElement. }
    \item{coerce}{\code{signature(from = "BigMatrix", to = "matrix")}:
      Coerce BigMatrix to a base matrix. }
    \item{as.matrix}{\code{signature(x = "BigMatrix")}: Coerce BigMatrix to a base matrix. }
    \item{dim}{\code{signature(x = "BigMatrix")}: Get dimensions of the matrix. }
    \item{dimnames}{\code{signature(x = "BigMatrix")}: Get dimension
      names of the matrix. }
    \item{dimnames<-}{\code{signature(x = "BigMatrix", value = "ANY")}:
      Set dimension names }
    \item{length}{\code{signature(x = "BigMatrix")}: Get length of the vector used as a matrix (nrow * ncol). }
    \item{ncol}{\code{signature(x = "BigMatrix")}: Get the number of columns in the matrix. }
    \item{nrow}{\code{signature(x = "BigMatrix")}: Get the number of
      rows in the matrix. }
    \item{apply}{\code{signature(X = "BigMatrix")}: Apply a function
      over one margin of the data in a BigMatrix, borrowing from biganalytics. }
	 }
}
\author{
Peter M. Haverty
}

\seealso{
See Also \code{\link{ReferenceClasses}}, \code{\link{BigMatrix}}, \code{\linkS4class{BigMatrixFactor}},  \code{\link{makeActiveBinding}},\code{\link[bigmemory]{filebacked.big.matrix}}
}
\examples{
showClass("BigMatrix")
back.file = tempfile()
x = matrix(1:9,ncol=3,dimnames=list(letters[1:3],LETTERS[1:3]))
ds = BigMatrix(x,back.file)
ds[1,3]
ds[3,3] = 5
as(ds,"matrix")
ds[,]
rownames(ds)
ds$rownames()
colnames(ds)
ds$colnames()
dimnames(ds)
ds$dimnames()
dim(ds)
ds$dim()
nrow(ds)
ds$nrow()
ncol(ds)
ds$ncol()
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{bigmat}:}{Object of class \code{activeBindingFunction} Public access to the big.matrix object. }
    \item{\code{datapath}:}{Object of class \code{activeBindingFunction} Full file path the big.matrix backing file. }
    \item{\code{descpath}:}{Object of class \code{character} Full file path the big.matrix metadata file. }
    \item{\code{.bm}:}{Object of class \code{big.matrix} Private storage of the big.matrix object. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{save()}:}{ Saves metadata about big.matrix in .bm field to file at location given by descpath field. }
    \item{colnames}{\code{signature(x = "BigMatrix")}: Get column names of the matrix. }
    \item{rownames}{\code{signature(x = "BigMatrix")}: Get row names of the matrix. }
    \item{dimnames}{\code{signature(x = "BigMatrix")}: Get dimension names of the matrix. }
    \item{dim}{\code{signature(x = "BigMatrix")}: Get the dimensions of the matrix. }
    \item{nrow}{\code{signature(x = "BigMatrix")}: Get the number of rows in the matrix. }
    \item{ncol}{\code{signature(x = "BigMatrix")}: Get the number of columns in the matrix. }
    \item{length}{\code{signature(x = "BigMatrix")}: Get the number of cells in the matrix. }
    \item{\code{setValues(i, j, value)}:}{ Setter for on-disk data. }
    \item{\code{getValues(i, j, drop)}:}{ Getter for on-disk data. }
    \item{\code{attach()}:}{ Uses path from descpath field to connect
      object to on-disk data. 'force' argument will force re-attachment
      of an object that is already attached.}
  }
}
