extract.fylkene <-
function (kom) 
{
    kom <- as.character(kom, lenght = 4)
    komnr.n <- nchar(kom) == 3
    kom[komnr.n] <- paste("0", kom[komnr.n], sep = "")
    return(substr(kom, start = 1, stop = 2))
}
