\name{attachAssayDataElements}
\alias{attachAssayDataElements}
\title{Attach on-disk matrices into assayData}
\usage{
  attachAssayDataElements( aData )
}
\arguments{
  \item{aData}{assayData object (environment or list
  style). Usually from the assayData slot of an eSet.}
}
\value{
  assayData matching the supplied aData argument. Any
  BigMatrix objects will be attached. Re-assignment back
  original eSet only necessary if using a list type
  assayData, which is rare.
}
\description{
  Biobase's eSet-derived objects (e.g. ExpressionSet,
  GenoSet) can hold one or more BigMatrix objects in their
  assayData slot. After re-loading an eSet from disk, these
  objects will each need to be re-attached to their on-disk
  component using their stored path information. This
  function checks each assayDataElement to see if it is an
  un-attached BigMatrix object, re-attaching if necessary.
  All other assayDataElements are left untouched. In later
  releases this function may also handle other on-disk
  types, like HDF5-based matrices.
}
\details{
  Environment type assayData objects, even
  "lockedEnvironment" objects, will be updated in place
  (same pointer).
}
\examples{
\dontrun{ assayData(eset) = attachAssayDataElements( assayData(eset) ) }
}

