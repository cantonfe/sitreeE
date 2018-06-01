

top.height <- function(thickness, height, num.trees.per.ha,
                       plot.id, plot.size.m2){

  ## number of trees is per ha
  ## Calculate number of trees per plot
  dt <- data.frame(thickness, height, plot.id, plot.size.m2)
  dt$num.trees.per.plot <- round(dt$plot.size.m2  * num.trees.per.ha/10000)

  dt <- dt[order(dt$plot.id, dt$thickness, decreasing = TRUE),]
  top.heights <- by(dt,
                   dt$plot.id,
                   function(x) {
                     if (nrow(x) < x$num.trees.per.plot[1]) {
                       tp <- mean(x$height)
                     } else {
                       tp <- mean(x$height[1:x$num.trees.per.plot[1]])
                     }
                     return(tp)
                   }
                   )
  ## putting it back to the expected order
  my.plot.id <- attr(top.heights, "dimnames")[[1]]

  top.heights <- data.frame(top.heights = as.vector(top.heights),
                            plot.id = my.plot.id)

  return(top.heights)
}
