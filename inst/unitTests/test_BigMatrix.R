library(bigmemoryExtras)
library(RUnit)

rownames = letters[1:3]
colnames = LETTERS[1:3]
mat = matrix(as.numeric(1:9),ncol=3,dimnames=list(rownames,colnames))
int.mat = matrix(c(rep(1L,5),rep(2L,4)),ncol=3,dimnames=list(rownames,colnames))
levels = c("AA","BB")
char.mat = matrix()

back.dir = tempdir()
ds.data.file = file.path(back.dir,"bigmat","ds")
ds = BigMatrix(mat,ds.data.file,3,3,list(rownames,colnames))

test_creation <- function() {
  back.dir = tempdir()
  back.file = tempfile()
  ds = BigMatrix(mat,back.file)
  checkTrue(validObject(ds))
  checkEquals(ds[,],mat)

  ds = BigMatrix(ds$bigmat,back.file, dimnames=dimnames(mat))
  checkTrue(validObject(ds))
  checkEquals(ds[,],mat)

  ds = BigMatrix(x=NULL,tempfile(),3,3,list(rownames,colnames))
  ds[,] = mat
  checkTrue(validObject( ds ))
  checkEquals(ds[,],mat)
  checkException( BigMatrix(x=1:4, tempfile()), "x must be NULL, scalar numeric, matrix, or bigmatrix.", silent=TRUE)
  checkEquals( BigMatrix(x=12, ncol=2, nrow=2, dimnames=list(LETTERS[1:2], LETTERS[1:2]), tempfile())[2, 2], 12, "BigMatrix creation with a scalar init value.")
  na_bm = BigMatrix(backingfile=tempfile(), nrow=3, ncol=3, dimnames=NULL)
  checkTrue( all(is.na(na_bm[, ])), "Default init is to all NA for BigMatrix")
}

test_coercion <- function() {
  checkEquals( ds[,], as(ds,"matrix") )
  checkEquals( ds[,], as.matrix(ds) )
}

test_subset <- function() {
  ds = BigMatrix(mat,ds.data.file,3,3,list(rownames,colnames))
  checkIdentical( ds[1,3], mat[1,3] )
  checkIdentical( ds[1,], mat[1,] )
  checkIdentical( ds[,2], mat[,2] )
  checkIdentical( ds[,], mat )
  checkIdentical( ds["a",], mat["a",] )
  checkIdentical( ds[,"B"], mat[,"B"] )
  ds["b","B"] = 3
  mat["b","B"] = 3
  checkIdentical( ds[,], mat,"After re-setting some values" )
  mat_nonames = mat
  dimnames(mat_nonames) = NULL
  ds_nonames = BigMatrix(mat_nonames,tempfile(),3,3)
  ds_nonames_return = ds_nonames[,];   dimnames(ds_nonames_return) = NULL  # Hack for bad handling of dimnames when they are NULL by bigmemory < 4.4.4
  checkIdentical( ds_nonames_return, mat_nonames, "Get full matrix from BM w/o dimnames" )
}

test_write <- function() {
  ds[1,1] = 5
  checkIdentical(ds[1,1],5,"Writing to a BigMatrix")
  ds[2,] = 5
  checkIdentical(ds[2,],c(A=5,B=5,C=5),"Writing row to a BigMatrix")
  ds[,2] = 7
  checkIdentical(ds[,2],c(a=7,b=7,c=7),"Writing col to a BigMatrix")
  ds[,] = mat
  checkIdentical(ds[,],mat,"Writing whole matrix to a BigMatrix")
  Sys.chmod(ds$backingfile,"0444")
  ds$attach(force=TRUE)
  checkException( ds[1,1] <- 5, silent=TRUE, "Writing to a BigMatrix with a non-writeable data file")
  Sys.chmod(ds$backingfile,"0644")
  ds$attach(force=TRUE)
  checkIdentical(ds[1, 1], 1, "OK read after chmod to readable and attach.")
  return(TRUE)
}

test_describing <- function() {
  mat = matrix(as.numeric(1:9),ncol=3,dimnames=list(rownames,colnames))
  ds = BigMatrix(mat,ds.data.file,3,3,list(rownames,colnames))
  # Getting
  checkEquals(nrow(ds),nrow(mat))
  checkEquals(ncol(ds),ncol(mat))
  checkEquals(dim(ds),dim(mat))
  checkEquals(length(ds),length(mat))
  checkIdentical(dimnames(ds),dimnames(mat))
  # Setting
  new.dimnames = list(letters[4:6], LETTERS[4:6])
  dimnames(ds) = new.dimnames
  checkIdentical(dimnames(ds), new.dimnames)

  ds = BigMatrix(mat,ds.data.file,3,3,list(rownames,colnames))
  colnames(ds) = new.dimnames[[2]]
  checkIdentical(colnames(ds), new.dimnames[[2]])

  ds = BigMatrix(mat,ds.data.file,3,3,list(rownames,colnames))
  rownames(ds) = new.dimnames[[1]]
  checkIdentical(rownames(ds), new.dimnames[[1]])
}

test_reattach <- function() {
  mat = matrix(as.numeric(1:9),ncol=3,dimnames=list(rownames,colnames))
  data.file = tempfile()
  old.ds = BigMatrix(mat,data.file,3,3,list(rownames,colnames))
  object.file = paste(data.file,".rds",sep="")
  saveRDS(old.ds,file=object.file)
  new.ds = readRDS(object.file)
  checkEquals( new.ds[,], mat )
  file.rename(old.ds$backingfile,file.path(tempdir(),"shoe2"))
  checkException(old.ds$attach(force=TRUE),silent=TRUE,"Missing descriptor file")
}

test_paths <- function() {
  checkEquals(normalizePath(ds$backingfile),normalizePath(ds.data.file))
  new.data.file = tempfile()
  new.ds = BigMatrix(mat,new.data.file,3,3,list(rownames,colnames))
  even.newer.data.file = tempfile()
  checkException({new.ds$backingfile = even.newer.data.file},silent=TRUE,"backingfile must exist before datapath is replaced.")
  file.copy(ds.data.file,even.newer.data.file)
  new.ds$backingfile = even.newer.data.file
  even.newer.data.file = normalizePath(even.newer.data.file)
  checkEquals(normalizePath(new.ds$backingfile),even.newer.data.file)
  checkEquals(new.ds[,], ds[,])
  checkEquals(basename(new.ds$backingfile), new.ds$.description$filename, "filename in description gets set by backingfile")
  desc = new.ds$description
  desc$filename = basename(new.data.file)
  new.ds$description = desc
  checkEquals(basename(new.ds$backingfile), new.ds$.description$filename, "filename in backingfile gets set by description")
}

test_update <- function() {
  object.file = system.file("unitTests/tdata/old.ds.rda", package="bigmemoryExtras")
  desc.file = system.file("unitTests/tdata/ds.desc.rds", package="bigmemoryExtras")
  ds = get(load(object.file))
  ds@.xData$descpath = desc.file
  checkException(ds[1, 1], "Should fail to attach an old-style object.", silent=TRUE)
  newds = updateBigMatrix(ds)
  checkTrue(validObject(newds))
  checkIdentical(rownames, rownames(newds))
  checkIdentical(colnames, colnames(newds))
}
