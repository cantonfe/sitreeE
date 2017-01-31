development.phase <- function(log.class = NA, deadwood.class = NA)
{
  
  ## logging class and dead wood must be valid
  ##if(any(!log.class %in% c(1,2,3,4,5,6)))
  ##  stop("log.class value not valid!")
  
  ##if(any(!dead.wood.class %in% c(1,2,3)))
  ##  stop("deadwood value not valid!")  
  
  dev.phase <- rep(NA, length(log.class))
  dev.phase[log.class %in% 1:3 & deadwood.class %in% 1:2] <- 1
  dev.phase[log.class %in% 4:5 & deadwood.class %in% 1:2] <- 2
  dev.phase[log.class %in% 6 & deadwood.class %in% 1:2] <- 3
  dev.phase[log.class %in% 1:3 & deadwood.class %in% 3] <- 4
  dev.phase[log.class %in% 4:6 & deadwood.class %in% 3] <- 5
  return(dev.phase)
}
