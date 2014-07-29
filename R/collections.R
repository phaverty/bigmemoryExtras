##########################################################
### Functions for working with groups of BigMatrix objects
##########################################################

##' Update directory for BigMatrix objects in a collection to new location
##'
##' Update directory for BigMatrix objects in a collection to new location. Assumes files have already
##' been moved on the filesystem. Assumes names of description and data files are the same. The
##' collection can contain a mix of BigMatrix objects and other types. The other types will be
##' not be touched and will be returned as they are.
##'
##' If you have renamed specific backing files, you will want to update the
##' backingfile field of the relevant BigMatrix objects.
##'
##' @param x list, SimpleList, environment or something with names and [[ methods
##' @param dir character, path to directory holding all BigMatrix files
##' @return x param, with modified BigMatrix objects.
##' @examples
##' \dontrun{ list = updateBackingfiles(list, "/new/path/to/bigmat/dir") }
##' \dontrun{ assays(se) = updateBackingfiles(assays(se), "/new/path/to/bigmat/dir") }
##' \dontrun{ assayData(eset) = updateBackingfiles(assayData(eset), "/new/path/to/bigmat/dir") }
##' @export
updateBackingfiles <- function(x, dir) {
    relock = FALSE
    if (is.environment(x)) {
        if (environmentIsLocked(x)) {
            relock = TRUE
        }
        item.names = ls(x)
    } else {
        item.names = names(x)
    }
    for (item in item.names) {
        if (is(x[[item]],"BigMatrix")) {
            if (relock) { unlockBinding(item, x) }
            x[[item]]$backingfile = file.path( dir, basename(x[[item]]$backingfile))
        }
    }
    if (relock) { lockEnvironment(x, bindings=TRUE) }
    return(invisible(x))
}
