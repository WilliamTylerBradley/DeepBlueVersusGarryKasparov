library(colorspace)
library(data.table)
library(ggplot2)

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

# build out grid
color_grid <- function(parameters) {
  h_value <- parameters[1]
  c_value <- parameters[2]
  l_value <- parameters[3]
  size <- parameters[4]
  
  color_grid <- CJ(y = seq(-1, 1, length.out = 8) * size,
                 l_values = seq(-1, 1, length.out = 8) * size + l_value)
  
  color_grid[, x := c_value]
  color_grid[, ':='(x_new = x * cos(h_value * pi/180) - y * sin(h_value * pi/180),
                  y_new = x * sin(h_value * pi/180) + y * cos(h_value * pi/180))]
  color_grid[, ':=' (c_values = sqrt(x_new^2 + y_new^2),
                   h_values = (atan2(y_new, x_new) * 180/pi) %% 360)]
  color_grid[, h_check := ifelse(h_values < h_value - 30 | h_values > h_value + 30, 
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


h_value <- 120
c_value <- 75
l_value <- 75
size <- 15
color_grid()


values <- optim(par = c(h_value, c_value, l_value, size),
                fn = color_grid,
                method = "SANN") # have to try a couple times
values

h_value <- values$par[1]
c_value <- values$par[2]
l_value <- values$par[3]
size <- values$par[4]

x_y_grid <- CJ(y = seq(-1, 1, length.out = 8) * size,
               l_values = seq(-1, 1, length.out = 8) * size + l_value)
x_y_grid[, x := c_value]
x_y_grid[, ':='(x_new = x * cos(h_value * pi/180) - y * sin(h_value * pi/180),
                y_new = x * sin(h_value * pi/180) + y * cos(h_value * pi/180))]
x_y_grid[, ':=' (c_values = sqrt(x_new^2 + y_new^2),
                 h_values = (atan2(y_new, x_new) * 180/pi) %% 360)]
x_y_grid[, hex_color := hcl(h_values, c_values, l_values, fixup = FALSE)]

ggplot(data = x_y_grid,
       aes(y, l_values, 
           color = hex_color, 
           fill = hex_color)) +
  geom_tile() + 
  scale_color_identity() +
  scale_fill_identity() +
  theme(legend.position = "none")

ggplot(data = x_y_grid,
       aes(h_values, c_values, 
           color = hex_color, 
           fill = hex_color)) +
  geom_point() + 
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 360),
                     breaks = seq(45, 360, 45),
                     minor_breaks = seq(0, 315, 45) + 45/2,
                     labels = c('45', '90', '135', '180', 
                                '225', '270', '315', '0|360')) +
  scale_y_continuous(limits = c(0, 180)) +
  coord_polar(start = 270 * pi / 180,
              direction = -1) +
  theme(legend.position = "none")

# I think I can just max maximize over a grid
# h angle goes from 0 to 59.5
# l values are 6 times 0 to 100
# sizes goes from 0 to 50 ish?
# c values go from 1 to 180


