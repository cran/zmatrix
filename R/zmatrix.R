"[.zmatrix" <-
function(x, i, j, drop = TRUE)
{
  if(!missing(i) && is.numeric(i))
    if (all(i>=0))
      i <- i + 1
    else if (all(i<=0))
      i <- i - 1
    else stop("Subscripts of mixed sign")

  if(!missing(j) && is.numeric(j))
    if (all(j>=0))
      j <- j + 1
    else if (all(j<=0))
      j <- j - 1
    else stop("Subscripts of mixed sign")

  NextMethod("[")
}

"[<-.zmatrix"  <-
function(x, i, j, value)
{
  if(!missing(i) && is.numeric(i))
    if (all(i>=0))
      i <- i + 1
    else if (all(i<=0))
      i <- i - 1
    else stop("Subscripts of mixed sign")

  if(!missing(j) && is.numeric(j))
    if (all(j>=0))
      j <- j + 1
    else if (all(j<=0))
      j <- j - 1
    else stop("Subscripts of mixed sign")

  y <- unclass(x)
  if(missing(i)) {
    if(missing(j)) y[] <- value else y[, j] <- value
  } else {
    if(missing(j)) y[i,] <- value else y[i, j] <- value
  }
  structure(y, class="zmatrix")
}

"zmatrix" <-
function (...) 
{
    structure(matrix(...), class = "zmatrix")
}

"as.zmatrix" <-
function(m)
{
    structure(as.matrix(m), class = "zmatrix")
}

"as.matrix.zmatrix" <- 
function(m)
{
  m <- unclass(m)
  NextMethod("as.matrix")
}

"is.zmatrix" <-
function(m) inherits(m, "zmatrix")

"print.zmatrix" <-
function(m, ...)
{
    dm <- dimnames(m)
    if (is.null(dm))
      dm <- rep(list(NULL), 2)
    if (is.null(dm[[1]]))
      dm[[1]] <- paste("[", 1:nrow(m)-1, ",]", sep="")
    if (is.null(dm[[2]]))
      dm[[2]] <- paste("[,", 1:ncol(m)-1, "]", sep="")
    dimnames(m) <- dm
    NextMethod("print")
}

