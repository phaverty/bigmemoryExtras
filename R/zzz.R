.onLoad <- function(libname,pkgname) {
  options(bigmemory.typecast.warning=FALSE)
}

.onUnLoad <- function(libpath) {
  options(bigmemory.typecast.warning=NULL)
}
