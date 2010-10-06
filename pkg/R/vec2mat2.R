"vec2mat2" <-
function (x, sep = "-") 
{
    splits <- strsplit(x, sep)
    n.spl <- sapply(splits, length)
    if (any(n.spl != 2)) 
        stop("Names must contain exactly one '", sep, "' each;  instead got ", 
            paste(x, collapse = ", "))
    x2 <- t(as.matrix(as.data.frame(splits)))
    dimnames(x2) <- list(x, NULL)
    x2
}
