library(colorspace)
library(data.table)

color_check <- function(parameters) {
  l_value <- parameters[1]
  h_value <- parameters[2]
  
  if(l_value > 100 | l_value < 0) {
    return(0)
  }
  
  if(h_value < 0 | h_value > 360) {
    return(0)
  }
  
  c_value <- max_chroma(h = h_value, l = l_value)
  return(-c_value)
}

values <- optim(par = c(50, 180),
                fn = color_check,
                method = "SANN") # have to try a couple times
values

color_check(c(50, 100))

color_check(c(89, 165))
