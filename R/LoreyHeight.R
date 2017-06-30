## The weighted mean height whereby individual trees are weighted in proportion to their basal area
lorey.height <- function(BA, height, group.id = NULL){

  if (is.null(group.id) ){
    lh <- weighted.mean( x = height, w= BA)
  } else{
    is.num <- is.numeric(group.id)
    lh <- by( data.frame(BA = BA, height = height),
             INDICES = list(group.id),
             FUN =   function(x){
               weighted.mean( x = x$height, w = x$BA)
               
             }
             )
    lh <- data.frame(group.id = names(lh), lorey.height = as.vector(lh))
    if (is.num) lh$group.id <- as.numeric(lh$group.id)
    
  }

  return(lh)
  
}
