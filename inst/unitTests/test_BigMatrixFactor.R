library(bigmemoryExtras)
library(RUnit)

rownames = letters[1:3]
colnames = LETTERS[1:3]
back.dir = tempdir()

fs.data.file = file.path(back.dir,"bigmat","fs")
char.mat = matrix(c(rep("AA",5),rep("BB",4)),ncol=3,dimnames=list(rownames,colnames))
factor.mat = factor(char.mat, levels=c("AA", "BB"))
attributes(factor.mat) = c(attributes(char.mat), list(class=c("matrix", "factor"), levels=levels(factor.mat)))
int.mat = matrix(c(rep(1L,5),rep(2L,4)),ncol=3,dimnames=list(rownames,colnames))
levels = c("AA","BB")
fs = BigMatrixFactor(char.mat, backingfile=fs.data.file,nrow=3,ncol=3,dimnames=list(rownames,colnames),levels=levels)

test_creation <- function() {
  back.dir = tempdir()

  fs = BigMatrixFactor(char.mat, backingfile=tempfile(), levels=levels)
  checkTrue(validObject(fs))
  checkEquals(fs[, ], factor.mat, "Initialize with char mat, gets right chars")

  na_bmf = BigMatrixFactor(backingfile=tempfile(), nrow=3, ncol=3, dimnames=NULL, levels=LETTERS[1:3])
  checkTrue( all(is.na(na_bmf[, ])), "Default init is to all NA for BigMatrixFactor")
  checkTrue( all(BigMatrixFactor("B", backingfile=tempfile(), ncol=3, nrow=4, levels=LETTERS[1:4])[, ] == "B"), "Scalar character init value in levels is OK.")
  checkTrue( all(is.na( BigMatrixFactor("X", backingfile=tempfile(), ncol=3, nrow=4, levels=LETTERS[1:4])[, ])), "Bad scalar init value becomes NA")
  checkTrue( all(is.na( BigMatrixFactor(1, backingfile=tempfile(), ncol=3, nrow=4, levels=LETTERS[1:4])[, ])), "Bad scalar init value becomes NA")
  checkTrue( all(is.na( BigMatrixFactor(matrix(1:9,ncol=3), backingfile=tempfile(), levels=LETTERS[1:4])[, ])), "Bad scalar init value becomes NA")
  bad.char.mat = matrix( c('a', 3, 'b', 'e'), ncol=2)
  expected.bad.char.mat = matrix( c('a', 3, 'b', NA), ncol=2)
  checkIdentical( as.character(expected.bad.char.mat), as.character(BigMatrixFactor(bad.char.mat, backingfile=tempfile(), levels=c('a', '3', 'b', 'c'))[, ]), "Bad values in init mat become NA")
  checkIdentical( c("AA", "BB"), levels(BigMatrixFactor(char.mat, backingfile=tempfile())), "Levels set from character if missing")
}

test_coercion <- function() {
  checkEquals( fs[,], as.matrix(fs) )
}

test_subset <- function() {
  char.mat = matrix(c(rep("AA",5),rep("BB",4)),ncol=3,dimnames=list(rownames,colnames))
  fac1 = factor(char.mat[, 1], levels=c("AA", "BB"))
  fac2 = factor(char.mat[2, ], levels=c("AA", "BB"))
  checkIdentical( fs[,], factor.mat )
  checkIdentical( fs[,1], fac1 )
  checkIdentical( fs[2,], fac2 )
}

test_write <- function() {
  onecol = factor(c(2,NA,1), levels=c("AA", "BB"))
  names(onecol) = rownames
  fs[,1] = onecol
  checkIdentical(fs[,1], onecol, "Setting BigMatrixFactor with characters")

  fs = BigMatrixFactor("AA",fs.data.file,5,5,levels=levels)
  fs[, 1] = c("AA", "SHOE", "GOO", "AA", "BB")
  retmat = factor(c("AA", NA, NA, "AA", "BB", rep("AA", 20)), level=c("AA", "BB"))
  dim(retmat) = c(5, 5)
  attributes(retmat) = attributes(retmat)[c("dim", "class", "levels")]
  attr(retmat, "class") = c("matrix", "factor")
  fs_return = fs[,]; dimnames(fs_return) = NULL  # Hack for bad handling of dimnames when they are NULL by bigmemory < 4.4.4
  checkIdentical( fs_return, retmat, "Setting BMF with bad characters gives NAs")

  fs2 = BigMatrixFactor(backingfile=tempfile(), nrow=3, ncol=3, levels=c('A', '9', 'B', '12'))
  fs2[, ] = "A"
  fs2[2,2] = 9
  fs2[3,3] = 1
  retmat2 = factor( c(rep("A", 4), "9", rep("A", 3), NA_character_), level=c("A", "9", "B", "12"))
  dim(retmat2) = c(3, 3)
  attributes(retmat2) = attributes(retmat2)[c("dim", "class", "levels")]
  attr(retmat2, "class") = c("matrix", "factor")
  fs2_return = fs2[,]; dimnames(fs2_return) = NULL  # Hack for bad handling of dimnames when they are NULL by bigmemory < 4.4.4
  checkIdentical( retmat2, fs2_return)
}

test_levels <- function() {
  checkIdentical( levels(fs), c("AA","BB") )
  checkIdentical( fs$levels, c("AA","BB") )
  checkException( fs$levels <- c("AB","QQ"), silent=TRUE )
  checkException( levels(fs) <- c("AB","QQ"), silent=TRUE )
}

test_nlevels <- function() {
  checkEquals( nlevels(fs), 2L )
}
