drought.risk.func <- function(veg.type)
{
  risk.value <- ifelse(veg.type %in% c(51,52,54,61,71,72,73),1,
                       ifelse(veg.type %in% c(12,15,16,24,33,34,46,93),2,
                              ifelse(veg.type %in% c(14,26,41,43,92),3,
                                     ifelse(veg.type %in% c(11,13,21,22,42,44,45,81,91,00),4,NA))))
  return(risk.value)
}