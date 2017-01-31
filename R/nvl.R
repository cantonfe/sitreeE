nvl <-
function (x, change) 
{
    i <- !is.finite(x)
    x[i] <- change
    return(x)
} 

