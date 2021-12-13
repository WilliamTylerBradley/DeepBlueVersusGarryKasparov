
h_angle <- 15
c_value <- 15
l_values <- seq(10, 90, 5)
size_values <- seq(10, 30, 2.5)

dt <- CJ(h_value = seq(0, 300, 60) + h_angle,
         c_value = c_value,
         l_value = l_values,
         size = size_values)

dt <- dt[l_value - size > 0 & 
           l_value + size < 100]

dt[, size_check := color_grid(h_value, c_value, l_value, size), 
   by = seq_len(nrow(dt))]

ggplot(data = dt, aes(size, l_value, color = size_check)) +
  geom_point()




dt <- dt[size == size_check]
dt_check <- unique(dt[, .(h_value, size)])
dt_check <- dt_check[, .(size_group = .N), by = size][size_group == 6][
  size == max(size)]
# get median l_value
dt <- dt[size == dt_check$size, ]
dt <- dt[, .(l_value = median(l_value)), by = .(h_value, c_value, size)]

##########

color_grid <- CJ(y = seq(-1, 1, length.out = 8) * size,
                 l_values = seq(-1, 1, length.out = 8) * size + l_value)

if(any(color_grid$l_value < 0 | color_grid$l_value > 100)) {
  return(0)
}

color_grid[, x := c_value]
color_grid[, ':='(x_new = x * cos(h_value * pi/180) - y * sin(h_value * pi/180),
                  y_new = x * sin(h_value * pi/180) + y * cos(h_value * pi/180))]
color_grid[, ':=' (c_values = sqrt(x_new^2 + y_new^2),
                   h_values = (atan2(y_new, x_new) * 180/pi) %% 360)]
color_grid[, h_check := ifelse((180 - abs(abs(h_values - h_value) - 180)) * 
                                 sign(180 - abs(h_values - h_value)) * 
                                 sign(h_values - h_value) > 60, 
                               TRUE, FALSE)]

if(any(color_grid$h_check)) {
  return(0)
}

color_grid[, hex_color := hcl(h_values, c_values, l_values, fixup = FALSE)]

if(any(is.na(color_grid$hex_color))) {
  return(0)
}

return(size)


####

color_grid <- function(h_value, c_value, l_value, size) {
  
  color_grid <- CJ(y = seq(-1, 1, length.out = 8) * size,
                   l_values = seq(-1, 1, length.out = 8) * size + l_value)
  
  if(any(color_grid$l_value < 0 | color_grid$l_value > 100)) {
    return(0)
  }
  
  color_grid[, x := c_value]
  color_grid[, ':='(x_new = x * cos(h_value * pi/180) - y * sin(h_value * pi/180),
                    y_new = x * sin(h_value * pi/180) + y * cos(h_value * pi/180))]
  color_grid[, ':=' (c_values = sqrt(x_new^2 + y_new^2),
                     h_values = (atan2(y_new, x_new) * 180/pi) %% 360)]
  color_grid[, h_check := ifelse((180 - abs(abs(h_values - h_value) - 180)) * 
                                   sign(180 - abs(h_values - h_value)) * 
                                   sign(h_values - h_value) > 60, 
                                 TRUE, FALSE)]
  
  if(any(color_grid$h_check)) {
    return(0)
  }
  
  color_grid[, hex_color := hcl(h_values, c_values, l_values, fixup = FALSE)]
  
  if(any(is.na(color_grid$hex_color))) {
    return(0)
  }
  
  return(-size)
}

values <- optim(par = c(15, 15),
                fn = color_grid,
                method = "SANN")


