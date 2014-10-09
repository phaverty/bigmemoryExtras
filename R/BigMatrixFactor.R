

##############################
###  Class BigMatrixFactor ###
##############################
##' @exportClass BigMatrixFactor
BigMatrixFactorGenerator <- setRefClass("BigMatrixFactor",
                             contains="BigMatrix",
                               fields=list(
                                   levels=function(value) {
                                       if (missing(value)) {
                                           return(.self$.levels)
                                       } else {
                                           stop("BigMatrixFactor levels are read-only.")
                                       }
                                   },
                                   .levels="character"
                                 ),
                               methods=list(
                                 getValues=function(i, j, drop=TRUE) {
                                   mat = callSuper(i, j, drop=TRUE)
                                   att.list = attributes(mat)
                                   if (is.matrix(mat)) {
                                     att.list = c(att.list, list(class=c("matrix", "factor"), levels=.self$levels))
                                   } else {
                                     att.list = c(att.list, list(class=c("factor"), levels=.self$levels))
                                   }
                                   attributes(mat) = att.list # This is the second time we set dimnames, so that could be improved.
                                   return(mat)
                                 },
                                 setValues=function(i,j,value) {
                                   if (!is.character(value)) { value = as.character(value) }
                                   value = match(value,.self$levels)
                                   callSuper(i,j,value)
                                 },
                                 nlevels=function() {
                                   return(base::length(.self$levels))
                                 },
                                 show=function() {
                                   callSuper()
                                   message("Levels:", paste(.self$levels, collapse=" "), "\n")
                                 }
                                )
                               )

##' Create a new BigMatrixFactor
##'
##' Create a new BigMatrixFactor
##' @param x scalar or matrix to be treated as character.
##' @param backingfile character, full path to the file that will contain the data matrix
##' @param nrow integer, number of rows in the matrix we are about to create
##' @param ncol integer, number of columns in the matrix we are about to create
##' @param dimnames list, list(rownames,colnames), as for a typical matrix
##' @param levels character, as for a typical factor
##' @return BigMatrixFactor
##' @examples
##' dnames = dimnames=list(letters[1:3],LETTERS[1:3])
##' levels=c("AA","AB","BB")
##' x = matrix( sample( levels, 9, replace=TRUE), ncol=3, dimnames=dnames)
##' ds = BigMatrixFactor(x,tempfile(),levels=levels)
##' ds = BigMatrixFactor(backingfile=tempfile(),nrow=3,ncol=3,dimnames=dnames,levels=levels)
##' @export
BigMatrixFactor <- function(x=NA_character_,backingfile,nrow,ncol,dimnames=NULL,levels) {
  if (! (is.matrix(x) || length(x) == 1) ) { stop("Initial value for BigMatrixFactor must be a matrix or of length 1.") }
  if (missing(levels) && is.character(x)) { levels = sort(unique(as.vector(x))) }
  if ( length(levels) > 127 ) {
    type = "integer"
  } else {
    type = "char"
  }
  att.list = attributes(x)
  if (!is.character(x)) { x = as.character(x) }
  x = match(x, levels)
  attributes(x) = att.list
  bm = .initBigMatrix(x=x,class="BigMatrixFactor",backingfile=backingfile, nrow=nrow, ncol=ncol, dimnames=dimnames, .levels=levels, type=type)
  return( bm )
}

## Maintain Generic Function and Method illusion for certain matrix API functions
## No logic other than passing to the right R5 method
##' @exportMethod levels
setMethod("levels",signature=signature(x="BigMatrixFactor"),
          function(x) {
            return(x$levels)
          })

##' @exportMethod 'levels<-'
setMethod("levels<-",signature=signature(x="BigMatrixFactor"),
          function(x) {
            stop("Levels are read-only.")
          })

##' @exportMethod nlevels
setMethod("nlevels",signature=signature(x="BigMatrixFactor"),
          function(x) {
            return(x$nlevels())
          })

setAs("BigMatrixFactor","matrix", function(from) { return(from[, ]) })
