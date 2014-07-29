\docType{package}
\name{bigmemoryExtras-package}
\alias{bigmemoryExtras-package}
\title{An extension of the bigmemory package with added safety, convenience, and a factor class.}
\description{
  This package defines a "BigMatrix" ReferenceClass which
  adds safety and convenience features to the
  filebacked.big.matrix class from the bigmemory package.
  BigMatrix protects against segfaults by monitoring, and
  gracefully restoring, the connection to on-disk data. It
  also protects against accidental data modification with a
  filesystem-based permissions system. We provide
  functionality for using BigMatrix-derived classes as
  assayData matrices within the Biobase package's eSet
  family of classes. BigMatrix provides some optimizations
  related to attaching to, and indexing into, file-backed
  matrices with dimnames. Additionally, the package
  provides a "BigMatrixFactor" class, a file-backed matrix
  with factor properties.
}
\details{
  BigMatrix stores the filesystem path to its on-disk data.
  When a big.matrix object is saved, e.g. with "save" the
  external pointer to the on-disk data becomes "nil". Any
  access to this nil pointer causes a segfault. A BigMatrix
  object will gracefully re-attach itself to its on-disk
  components any time use of the "nil" pointer is
  attempted.
}
\seealso{
  BigMatrix-class BigMatrixFactor-class BigMatrix
  BigMatrixFactor filebacked.big.matrix ReferenceClasses
}

