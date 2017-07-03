 

PBAL <-  function(BA){

  ord.x <- order(BA)
  BAo <- sum(BA[ord.x]) - cumsum(BA[ord.x])
  ## ties
  is.tie <- rev(duplicated(BA[order(BA, decreasing = TRUE)]))
  BAo[is.tie] <- BAo[is.tie] - BA[ord.x][is.tie]
  ## return it to the original form
  BAo <- BAo[match(1:length(BAo), ord.x)]
  return(BAo)
}

