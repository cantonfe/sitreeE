
height.of.X.tallest.trees <- function(height, uplotID, num.trees){

  ## order by uplotID all heights
  mean.height <- aggregate(height ~ uplotID, FUN = function(x){
    x <- x[order(x, decreasing = TRUE)]
    mean(x[1:(min(c(length(x), num.trees)))])
  }
  )
  
  return(mean.height)
}

