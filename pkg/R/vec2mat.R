"vec2mat" <-
function (x, sep = "-") 
{
    n.na <- sum(is.na(x))
    x.is <- deparse(substitute(x))
    if (n.na > 0) 
        stop(n.na, " NAs not allowed, found in ", x.is)
    dimx <- dim(x)
    l.dimx <- length(dimx)
    clpse <- function(x, collapse = ", ") paste(x, collapse = collapse)
    x.not.sq <- ((l.dimx > 2) || ((l.dimx == 2) && (dimx[1] != 
        dimx[2])))
    if (x.not.sq) 
        stop("Array of dim(", clpse(dimx), ") not allowed for ", 
            x.is)
    if (l.dimx == 2) {
        if (any(x != t(x))) 
            stop("Matrix not symmetric ", x.is)
        return(x)
    }
    namx <- names(x)
    if (length(namx) != length(x)) 
        stop("Names required for ", deparse(substitute(x)))
    x.lvls <- vec2mat2(namx, sep)
    Lvls <- unique(as.vector(x.lvls))
    n.lvls <- length(Lvls)
    x0 <- {
        if (is.numeric(x)) 
            1
        else if (is.logical(x)) 
            FALSE
        else if (is.character(x)) 
            ""
        else stop("Must be class numeric, logical or ", "character;  instead is ", 
            class(x))
    }
    X <- array(x0, dim = c(n.lvls, n.lvls), dimnames = list(Lvls, 
        Lvls))
    i.lvls <- 1:n.lvls
    names(i.lvls) <- Lvls
    ix.lvls <- array(i.lvls[x.lvls], dim = dim(x.lvls))
    rev.ix <- (ix.lvls[, 1] > ix.lvls[, 2])
    if (any(rev.ix)) 
        ix.lvls[rev.ix, ] <- ix.lvls[rev.ix, 2:1]
    X[ix.lvls] <- x
    X[lower.tri(X)] <- t(X)[lower.tri(X)]
    X
}
