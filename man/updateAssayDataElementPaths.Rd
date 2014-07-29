\name{updateAssayDataElementPaths}
\alias{updateAssayDataElementPaths}
\title{Update directory for BigMatrix assayDataElement to new location}
\usage{
  updateAssayDataElementPaths(ds, dir)
}
\arguments{
  \item{ds}{eSet}

  \item{dir}{character, path to directory holding all
  BigMatrix files}
}
\value{
  eSet
}
\description{
  Update directory for BigMatrix assayDataElement to new
  location. Assumes files have already been moved on the
  filesystem. Assumes names of description and data files
  are the same.
}
\details{
  If you have renamed specific backing files or descriptor
  files, you will want to update the datapath and descpath
  fields of the relevant BigMatrix objects.
}
\examples{
\dontrun{ eset = updateAssayDataElementPaths(eset, "/new/path/to/bigmat/dir") }
}
\author{
  Peter M. Haverty \email{phaverty@gene.com}
}

