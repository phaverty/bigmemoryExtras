##' An extension of the bigmemory package with added safety, convenience, and a factor class.
##'
##' This package defines a "BigMatrix" ReferenceClass which adds
##' safety and convenience features to the filebacked.big.matrix class from the
##' bigmemory package. BigMatrix protects against segfaults by monitoring, and
##' gracefully restoring, the connection to on-disk data. It also protects
##' against accidental data modification with a filesystem-based permissions
##' system. We provide functionality for using BigMatrix-derived classes
##' as assayData matrices within the Biobase package's eSet family of classes.
##' BigMatrix provides some optimizations related to attaching to,
##' and indexing into, file-backed matrices with dimnames. Additionally, the package
##' provides a "BigMatrixFactor" class, a file-backed matrix with factor
##' properties.
##'
##' BigMatrix stores the
##' filesystem path to its on-disk data. When a big.matrix object is saved, e.g. with
##' "save" the external pointer to the on-disk data becomes "nil". Any access
##' to this nil pointer causes a segfault. A BigMatrix object will
##' gracefully re-attach itself to its on-disk components any time use of the
##' "nil" pointer is attempted.
##'
##' @docType package
##' @name bigmemoryExtras-package
##' @aliases bigmemoryExtras-package
##' @seealso BigMatrix-class BigMatrixFactor-class BigMatrix BigMatrixFactor filebacked.big.matrix ReferenceClasses
##' @import methods
##' @import bigmemory
NULL

### Modifications to big.matrix object from bigmemory to help with saving to and restoring from disk and to prevent usage of a nil address

# Initialize can't require args or have side-effects, so can not create on-disk elements with initialize. Have to do "BigMatrix" function (and/or method?) for users to initialize

########################
###  Class BigMatrix ###
########################
##' @exportClass BigMatrix
setClassUnion("characterOrNull",c("character","NULL"))
BigMatrixGenerator <- setRefClass("BigMatrix",
                         fields=list(
                             .backingfile="character",
                             .rownames="characterOrNull",
                             .colnames="characterOrNull",
                             .description="list",
                             .bm="big.matrix",
                           bigmat=function(value) {
                             if (missing(value)) {
                               if (is.nil(.self$.bm@address)) {
                                 .self$attach()
                               }
                               return(.self$.bm)
                             } else {
                               if (is.big.matrix(value)) {
                                 .self$.bm = value
                               } else {
                                 stop("Replacement value for bigmat must be a big.matrix\n")
                               }
                             }
                           },
                           backingfile=function(value) {
                               if (missing(value)) {
                                   return(.self$.backingfile)
                               } else {
                                   if (file.exists(value)) {
                                       .self$.description$filename = basename(value)
                                       .self$.backingfile=value
                                   } else {
                                       stop("Specified 'backingfile' does not exist. Please move/copy the file before re-setting 'backingfile'.")
                                   }
                               }
                           },
                           description=function(value) {
                               if (missing(value)) {
                                   return(.self$.description)
                               } else {
                                   .self$backingfile = file.path(dirname(.self$.backingfile), value$filename)
                                   .self$.description = value
                               }
                           },
                           datafile = function(value) {
                             .Defunct(new=backingfile, msg="The datafile is now called 'backingfile' like in bigmemory proper.")
                           }),
                         methods=list(
                           rownames = function(value) {
                             if (missing(value)) {
                               return(.self$.rownames)
                             } else {
                               if (base::length(value) != .self$nrow()) { stop("Length of rownames must match the number of rows.") }
                             .self$.rownames = value
                             }
                           },
                           colnames = function(value) {
                             if (missing(value)) {
                               return(.self$.colnames)
                             } else {
                               if (base::length(value) != .self$ncol()) { stop("Length of colnames must match the number of columns.") }
                             .self$.colnames = value
                             }
                           },
                           dimnames = function(value) {
                             if (missing(value)) {
                               if (is.null(.self$.rownames) && is.null(.self$.rownames) ) {
                                 return(NULL)
                               } else {
                                 return(list(.self$.rownames, .self$.colnames))
                               }
                             } else {
                               if (!is.null(value) && base::length(value) != 2) { stop("dimnames must be set with a list of length 2.") }
                               .self$.rownames = value[[1]]
                               .self$.colnames = value[[2]]
                             }
                           },
                           dim  = function() {
                             return( c( .self$description$nrow, .self$description$ncol ) )
                           },
                           nrow = function() {
                             return(.self$description$nrow)
                           },
                           ncol = function() {
                             return(.self$description$ncol)
                           },
                           length = function() {
                             return( .self$description$nrow * .self$description$ncol )
                           },
                           attach=function(force=FALSE) {
                             if (force == FALSE && ! all( vapply(.self, class, character(1)) %in% c("refMethodDef", "activeBindingFunction"))) {  # All data in private (dot prefix) member variables not seen by *apply or ls
                                 stop("Attempting to attach an older type of BigMatrix. Please use the updateBigMatrix function first.")
                             }
                             if (force == FALSE && ! is.nil(.self$.bm@address)) {
                               message("Already attached to on-disk data. To re-attach, use force=TRUE.\n")
                             } else {
                               message("Attaching to on-disk data:", .self$backingfile, "...\n")
                               if ( ! file.exists(backingfile) ) {
                                 stop("Backing file ",backingfile," does not exist.")
                               }
                               if ( file.access(.self$backingfile,4) != 0 ) {
                                 stop("Can not attach to descriptor file without read permissions.")
                               }
                               tryCatch({
                                 desc = new('big.matrix.descriptor',  description=.self$description)
                                 .self$.bm = attach.big.matrix(desc,path=dirname(.self$backingfile))
                               },
                                        error = function(e) { stop("Failed to attach big.matrix on disk component.\n") } )
                             }
                           },
                           getValues=function(i,j,drop=TRUE, withDimnames=TRUE) {
                             object = .self$bigmat
                             if (!missing(i) && is.character(i)) { i = match(i,.self$.rownames) }
                             if (!missing(j) && is.character(j)) { j = match(j,.self$.colnames) }
                             if (missing(i)) {
                               if (missing(j)) {
                                 x = object[,,drop=drop]
                               } else {
                                 x = object[,j,drop=drop]
                                 }
                             } else {
                               if (missing(j)) {
                                 x = object[i,,drop=drop]
                               } else {
                                 x = object[i,j,drop=drop]
                               }
                             }
                             if (withDimnames == TRUE && base::length(x) > 1) {
                               if (is.matrix(x)) {
                                 if (!is.null(.self$rownames()) && !is.null(.self$colnames())) {
                                   if (missing(i)) { i = seq.int(1, .self$nrow())}
                                   if (missing(j)) { j = seq.int(1, .self$ncol())}
                                   dimnames(x) = list(.self$.rownames[i], .self$.colnames[j])
                                 }
                               } else {
                                 if (missing(i) && !is.null(.self$rownames())) { names(x) = .self$.rownames }
                                 if (missing(j) && !is.null(.self$colnames())) { names(x) = .self$.colnames }
                               }
                             }
                             return(x)
                           },
                           setValues=function(i,j,value) {
                             object = .self$bigmat
                             bigmemory:::checkReadOnly(object)
                             if (!missing(i) && is.character(i)) { i = match(i,.self$.rownames) }
                             if (!missing(j) && is.character(j)) { j = match(j,.self$.colnames) }
                             if (missing(i)) {
                               if (missing(j)) {
                                 object[,] <- value
                               } else {
                                 object[,j] <- value
                                 }
                             } else {
                               if (missing(j)) {
                                 object[i,] <- value
                               } else {
                                 object[i,j] <- value
                               }
                             }
                           },
                           save=function(rdsfile) {
                             saveRDS( .self, rdsfile )
                           },
                           show=function() {
                             message( class(.self),
                                     "\nbackingfile: ", .self$backingfile, "\n",
                                     "dim: ", paste(.self$dim(), collapse=", ")
                                     )
                             if (!is.null(.self$rownames())) {
                               message("rownames: ", paste(head(.self$rownames()), collapse=", "), " ...")
                             } else {
                               message("rownames: NULL")
                             }
                             if (!is.null(.self$colnames())) {
                               message("colnames: ", paste(head(.self$colnames()), collapse=", "), " ...")
                             } else {
                               message("colnames: NULL")
                             }
                             if (is.nil(.self$.bm@address)) {
                               message("Object is not currently attached to on-disk data.\n")
                             } else {
                               message("Object is currently attached to on-disk data.\n")
                             }
                           }
                           )
                         )

setValidity("BigMatrix", function(object) {
  if (!file.exists(object$backingfile)) {
    return("Backingfile does not exist")
  }
  if (file.access(object$backingfile,4) < 0) {
    return("Backingfile is not readable")
  }
  return(TRUE)
})

## Maintain Generic Function and Method illusion for certain matrix API functions
## No logic other than passing to the right R5 method

##' @exportMethod '['
setMethod('[', signature(x = "BigMatrix"),
          function(x,i,j,..., drop=TRUE) {
            return(x$getValues(i,j,..., drop=drop))
          })

##' @exportMethod '[<-'
setMethod('[<-', signature(x = "BigMatrix",i="ANY",j="ANY",value="ANY"),
          function(x,i,j,value) {
            x$setValues(i,j,value)
            return(x)
          })

##' @exportMethod dimnames
setMethod('dimnames', signature(x="BigMatrix"),
          function(x) {
            return(x$dimnames())
          })

##' @exportMethod 'dimnames<-'
setMethod('dimnames<-', signature(x="BigMatrix",value="ANY"),
          function(x,value) {
            x$dimnames(value)
            return(x)
          })

##' @exportMethod nrow
setMethod('nrow', signature(x="BigMatrix"),
          function(x) {
            return(x$nrow())
          })

##' @exportMethod ncol
setMethod('ncol', signature(x="BigMatrix"),
          function(x) {
            return(x$ncol())
          })

##' @exportMethod dim
setMethod('dim', signature(x="BigMatrix"),
          function(x) {
            return(x$dim())
          })

##' @exportMethod length
setMethod('length', signature(x="BigMatrix"),
          function(x) {
            return(x$length())
          })

##' @exportMethod as.matrix
setMethod("as.matrix",signature(x="BigMatrix"), function(x) { return(x[,]) })
setAs("BigMatrix","matrix", function(from) { return(from[,]) })

##' @exportMethod apply
setMethod("apply",signature(X="BigMatrix"),
	function(X, MARGIN, FUN, ...) {
		    if (! require("biganalytics", quietly=TRUE)) { stop("Failed to require 'biganalytics'\n.") }
			     biganalytics::apply(X$bigmat, MARGIN, FUN, ...)
	})

##' Create a new BigMatrix-derived class
##'
##' Create a new BigMatrix-derived class
##' @param x NULL, matrix, or big.matrix. Optional data or big.matrix for new BigMatrix
##' @param class character, class name.  BigMatrix or BigMatrixFactor currently.
##' @param backingfile character, full path to the file that will contain the data matrix
##' @param nrow integer, number of rows in the matrix we are about to create
##' @param ncol integer, number of columns in the matrix we are about to create
##' @param dimnames list, list(rownames,colnames), as for a typical matrix
##' @param type character, can be double, integer, or char
##' @param ... other args to pass to "new" method of generator object, like levels
##' @return BigMatrix
##' @keywords internal
##' @rdname initBigMatrix
.initBigMatrix = function(x=NULL, class=c("BigMatrix", "BigMatrixFactor"), backingfile, nrow, ncol, dimnames=NULL, type="double", ...) {
  class = match.arg(class)
  backingpath = dirname(backingfile)
  dir.create(backingpath,showWarnings=FALSE,recursive=TRUE)
  backingpath = normalizePath(backingpath)
  descriptorfile = paste(backingfile,".desc",sep="")
  if (is.matrix(x)) {
    if (is.null(dimnames)) {
      dimnames = dimnames(x)
      dimnames(x) = NULL
    }
    new.matrix = as.big.matrix(x, backingpath=backingpath, descriptorfile=basename(descriptorfile), backingfile=basename(backingfile))
  } else if (is.big.matrix(x)) {
    if( is.nil(x@address) ) {
      tryCatch( { x = attach.big.matrix(descriptorfile) },
               error = function(e) { stop("Failed to attach big.matrix on disk component.\n") } )
    }
    new.matrix = x
  } else if (is.null(x) || (is.numeric(x) && length(x) == 1)) {
    new.matrix = filebacked.big.matrix(
      init=x,
      nrow=nrow,ncol=ncol,
      type=type,
      backingfile=basename(backingfile),
      descriptorfile=basename(descriptorfile),
      backingpath=normalizePath(backingpath),
      dimnames=NULL)
  } else {
    stop("Argument x must be a scalar numeric, matrix, or big.matrix.\n")
  }
  unlink(file.path(backingpath,descriptorfile))  # Delete bigmemory version of desc file until they stop using dput/dget
  description = describe(new.matrix)@description # description method is not exported and it just does this anyway
  bm = getRefClass(class)$new(.bm=new.matrix, .description=description, .backingfile=backingfile, .rownames=dimnames[[1]], .colnames=dimnames[[2]], ...)
  return( bm )
}

##' Create a new BigMatrix
##'
##' Create a new BigMatrix
##' @param x scalar numeric, NULL, matrix, or big.matrix. Optional data or big.matrix for new BigMatrix. A scalar numeric can be used to initalize the whole matrix. NULL gives the bigmemory feature of initializing to all zeros instantly.
##' @param backingfile character, full path to the file that will contain the data matrix
##' @param nrow integer, number of rows in the matrix we are about to create
##' @param ncol integer, number of columns in the matrix we are about to create
##' @param dimnames list, list(rownames,colnames), as for a typical matrix
##' @param type character type of big.matrix (double, integer, char)
##' @return BigMatrix
##' @examples
##' x <- big.matrix(10,  2,  type='integer',  init = -5)
##' dnames = dimnames=list(letters[1:3],LETTERS[1:3])
##' x = matrix(1:9,ncol=3,dimnames=dnames)
##' ds = BigMatrix(x,tempfile())
##' ds = BigMatrix(backingfile=tempfile(),nrow=3,ncol=3,dimnames=dnames)
##' @export
BigMatrix <- function(x=NA_real_,backingfile,nrow,ncol,dimnames=NULL,type="double") {
  bm = .initBigMatrix(x=x, class="BigMatrix",backingfile=backingfile, nrow=nrow, ncol=ncol, dimnames=dimnames, type=type)
  return( bm )
}

##' Update previous BigMatrix type to new BigMatrix
##'
##' BigMatrix has changed some of its internal storage to eliminate the descriptor file and to keep the dimnames on the R side. This function will take a
##' Version <= 1.3 type and update it to the Version >=1.4 type.
##' @param object BigMatrix
##' @export
##' @examples
##' x <- big.matrix(10,  2,  type='integer',  init = -4)
##' @return BigMatrix
updateBigMatrix <- function(object) {
    if ("descpath" %in% ls(object)) {
        path = object$descpath
        tryCatch(
            { desc = readRDS(path) },
            error = function(e) { stop(sprintf("Failed to read descriptor file when updating BigMatrix.\n%s\n", e))}
            )

        desc.list = desc@description
        dimnames = list(desc.list$rowNames, desc.list$colNames)
        desc.list$rowNames = desc.list$colNames = NULL
        new.desc = new('big.matrix.descriptor',  description=desc.list)
        bm = attach.resource(new.desc, path=dirname(object$descpath))
        backingfile = file.path( dirname(object$descpath), desc.list$filename)
    } else {
        dimnames = list(object$.rownames, object$.colnames)
        backingfile = object$backingfile
        object$attach(force=TRUE)
        bm = object$bigmat
    }
    if (class(object) == "BigMatrixFactor") {
        bigmat = .initBigMatrix(x=bm, class="BigMatrixFactor", backingfile=backingfile, dimnames=dimnames, .levels=object$levels)
    } else {
        bigmat = BigMatrix(x=bm, backingfile=backingfile, dimnames=dimnames)
    }
    return(bigmat)
}

# benchmark(x = theta$getValues(,  1),  y = theta2$getValues(,  1),  z = theta2$getValues(,  1,  withDimnames=FALSE),  replications=5)
