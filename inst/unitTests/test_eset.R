### Tests for stuff related to use of BigMatrix in eSets
require(Biobase)

rownames = letters[1:3]
colnames = LETTERS[1:3]
mat = matrix(as.numeric(1:9),ncol=3,dimnames=list(rownames,colnames))
int.mat = matrix(c(rep(1L,5),rep(2L,4)),ncol=3,dimnames=list(rownames,colnames))
  
back.dir = tempdir()
data.file = file.path(back.dir,"bigmat","ds.eset")
ds.eset = BigMatrix(mat,data.file,3,3,list(rownames,colnames))

test_use_in_eset <- function() {
  eset = ExpressionSet()
  assayDataElement(eset,"exprs") = ds.eset
  checkEquals( exprs(eset)[,1], ds.eset[,1], "Subset should give back column" )
}
