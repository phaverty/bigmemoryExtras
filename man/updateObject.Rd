\name{updateObject}
\alias{updateObject}
\title{Update previous BigMatrix type to new BigMatrix}
\arguments{
  \item{object}{BigMatrix}
}
\value{
  BigMatrix
}
\description{
  BigMatrix has changed some of its internal storage to
  eliminate the descriptor file and to keep the dimnames on
  the R side. This function will take a Version <= 1.3 type
  and update it to the Version >=1.4 type.
}

