#####  Functions to use BigMatrix with Biobase's eSet-derived objects
##' @importFrom Biobase annotatedDataFrameFrom assayDataElementNames assayDataElement
##' @importClassesFrom Biobase AssayData eSet
NULL

##' Use BigMatrix in eSet assayData
##'
##' The Biobase package's eSet objects hold one or more matrices within an environment stored
##' in their assayData slot. Use of classes other than a base matrix require a method to
##' create an annotatedDataFrame from that class.  Users are unlikely to ever call this
##' method.
##'
##' @exportMethod annotatedDataFrameFrom
##' @rdname annotatedDataFrameFrom
##' @param object a BigMatrix
setMethod("annotatedDataFrameFrom", signature(object="BigMatrix"),
          function(object) {
              .Deprecated("nothing", msg="All eSet-related content is being deprecated.")
              return(Biobase:::annotatedDataFrameFromMatrix(object))
          })

##' Attach on-disk matrices into assayData
##'
##' Biobase's eSet-derived objects (e.g. ExpressionSet, GenoSet) can hold one or more BigMatrix
##' objects in their assayData slot. After re-loading an eSet from disk,
##' these objects will each need to be re-attached to their on-disk component using
##' their stored path information. This function checks each assayDataElement to
##' see if it is an un-attached
##' BigMatrix object, re-attaching if necessary. All other assayDataElements are left
##' untouched. In later releases this function may also handle other on-disk types,
##' like HDF5-based matrices.
##'
##' Environment type assayData objects, even "lockedEnvironment" objects, will be
##' updated in place (same pointer).
##'
##' @examples \dontrun{ assayData(eset) = attachAssayDataElements( assayData(eset) ) }
##' @param aData assayData object (environment or list style). Usually from the assayData slot of an eSet.
##' @return assayData matching the supplied aData argument.
##' Any BigMatrix objects will be attached. Re-assignment back
##' original eSet only necessary if using a list type assayData, which is rare.
##' @export
attachAssayDataElements <- function(aData) {
    .Deprecated("nothing", msg="All eSet-related content is being deprecated.")
    for( ad.name in assayDataElementNames(aData)) {
        if ( is( aData[[ad.name]], "BigMatrix" ) ) {
            aData[[ad.name]]$attach()
        }
    }
    return(invisible(aData))
}

##' Update directory for BigMatrix assayDataElement to new location
##'
##' Update directory for BigMatrix assayDataElement to new location. Assumes files have already
##' been moved on the filesystem. Assumes names of description and data files are the same.
##'
##' If you have renamed specific backing files or descriptor files, you will want to update the
##' datapath and descpath fields of the relevant BigMatrix objects.
##'
##' @param ds eSet
##' @param dir character, path to directory holding all BigMatrix files
##' @return eSet
##' @examples \dontrun{ eset = updateAssayDataElementPaths(eset, "/new/path/to/bigmat/dir") }
##' @export
##' @author Peter M. Haverty \email{phaverty@@gene.com}
updateAssayDataElementPaths <- function(ds, dir) {
    .Deprecated("updateBackingfiles", "eSet-specific features are now deprecated.")
    for (ad.name in assayDataElementNames(ds)) {
        if (is(assayDataElement(ds,ad.name),"BigMatrix")) {
            ad = assayDataElement(ds,ad.name)
            ad$backingfile = file.path( dir, basename(assayDataElement(ds,ad.name)$backingfile))
        }
    }
    return(invisible(ds))
}
