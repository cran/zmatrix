\name{zmatrix}

\alias{zmatrix}
\alias{as.zmatrix}
\alias{as.matrix.zmatrix}
\alias{is.zmatrix}
\alias{[.zmatrix}
\alias{[<-.zmatrix}
\alias{print.zmatrix}

\title{Zero-offset matrices}

\usage{
m[i], m[i, j], m[, j], m[i, j, drop=TRUE]
zmatrix(...)
as.zmatrix(m)
as.matrix.zmatrix(m)
is.zmatrix(m)
print.zmatrix(m, ...)}

\arguments{
 \item{i, j}{Indices for rows and columns}
 \item{m}{A matrix, possibly of class `zmatrix'}
 \item{drop}{Lose dimensions of length 1?}}

\description{
Allows operations with zero-offset matrices.}

\value{
With a zero-offset matrix the rows and columns are numbered
from zero.  \code{zmatrix} creates a zero-offset matrix of class
`zmatrix', \code{as.zmatrix} converts an existing matrix to a
zero-offset matrix, and \code{as.matrix} converts it back again.
Subscripting a zero-offset matrix does not return a zero-offset
matrix.}

\note{The print method is provided to give zero-index row and
column names in the absence of specified names.}

\author{Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk},
following assistance from Brian Ripley.}

\seealso{\code{\link{matrix}}}

\examples{
fred <- matrix(1:12, 3, 4)
is.zmatrix(fred)		# Not yet!
fred <- as.zmatrix(fred)
fred; print.default(fred)	# note the labels

fred[2, ] <- NA; fred		# sets the third row
fred[, -(0:2)] <- NA; fred	# all bar the first three cols

fred[0, ]			# a vector
fred[0, , drop=FALSE]		# a matrix
rownames(fred) <- LETTERS[1:3]
fred["A", 0:2]			# mixture of names and indices
}

\keyword{array}
