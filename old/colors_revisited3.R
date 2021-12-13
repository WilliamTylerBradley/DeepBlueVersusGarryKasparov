library(colorspace)
library(data.table)
library(ggplot2)

# build out grid
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
  
  return(size)
}

color_grid_set <- function(h_angle, c_value, l_values, size_values) {

  dt <- CJ(h_value = seq(0, 300, 60) + h_angle,
           c_value = c_value,
           l_value = l_values,
           size = size_values)
  
  dt <- dt[l_value - size > 0 & 
             l_value + size < 100]
  
  dt[, size_check := color_grid(h_value, c_value, l_value, size), 
     by = seq_len(nrow(dt))]
  
  dt <- dt[size == size_check]
  dt_check <- unique(dt[, .(h_value, size)])
  dt_check <- dt_check[, .(size_group = .N), by = size][size_group == 6][
    size == max(size)]
  # get median l_value
  dt <- dt[size == dt_check$size, ]
  dt <- dt[, .(l_value = median(l_value)), by = .(h_value, c_value, size)]
  
  return(dt)
}

return_color_grid_set_size <- function(h_angle, c_value, l_values, size_values){
  dt <- color_grid_set(h_angle, c_value, l_values, size_values)
  
  return(unique(dt$size))
}

# save output in loop
for(c_value in seq(10, 49)) {
  print(c_value)
  dt <- CJ(h_angle = seq(1, 59),
           c_value = c_value)
  dt[, size := return_color_grid_set_size(h_angle, c_value, 
                                          l_values = seq(10, 90, 5),
                                          size_values = seq(10, 30, 2.5)),
     by = seq_len(nrow(dt))]
  write.csv(dt, here::here("color_grid_sizes", 
                           paste0("c_value_", c_value, ".csv")),
            row.names = FALSE)
}

# Read in and see results
size_results <- data.table(h_angle = integer(),
                           c_value = numeric(),
                           size = numeric())
for(c_value in seq(10, 49)) {
  print(c_value)
  dt <- read.csv(here::here("color_grid_sizes", 
                           paste0("c_value_", c_value, ".csv")),
                 )
  
  size_results <- rbind(size_results, dt)
  
}

ggplot(data = size_results,
       aes(h_angle, c_value, color = size, fill = size)) +
  geom_tile() # kinda neat

## now maximize on a finer grain for
# h values of 0 to 59 again
# c values of 12 to 25
t1 <- Sys.time()
for(c_value in seq(15, 25)) {
  print(c_value)
  
  dt <- CJ(h_angle = seq(1, 59),
           c_value = c_value)
  dt[, size := return_color_grid_set_size(h_angle, c_value,
                                          l_values = seq(10, 90, 1),
                                          size_values = seq(15, 30, 1)),
     by = seq_len(nrow(dt))]
  write.csv(dt, here::here("color_grid_sizes_smaller",
                           paste0("c_value_", 
                                  gsub("\\.", "_", as.character(c_value)),
                                  ".csv")),
            row.names = FALSE)
}
t2 <- Sys.time()
t2 - t1

size_results <- data.table(h_angle = integer(),
                           c_value = numeric(),
                           size = numeric())
for(c_value in seq(12, 25)) {
  print(c_value)
  dt <- read.csv(here::here("color_grid_sizes_smaller", 
                            paste0("c_value_", 
                                   gsub("\\.", "_", as.character(c_value)),
                                   ".csv")),
  )
  
  size_results <- rbind(size_results, dt)
  
}

ggplot(data = size_results,
       aes(h_angle, c_value, color = size, fill = size)) +
  geom_tile()

## now maximize on a finer grain for
# h values of 25 to 35 again
# c values of 16 to 20
t1 <- Sys.time()
for(c_value in seq(16.5, 16.75, by = .25)) { # probably add in 16
  print(c_value)
  
  dt <- CJ(h_angle = seq(25, 35, by = .25),
           c_value = c_value)
  dt[, size := return_color_grid_set_size(h_angle, c_value,
                                          l_values = seq(10, 90, .25),
                                          size_values = seq(20, 27, .25)),
     by = seq_len(nrow(dt))]
  write.csv(dt, here::here("color_grid_sizes_smallest",
                           paste0("c_value_", 
                                  gsub("\\.", "_", as.character(c_value)),
                                  ".csv")),
            row.names = FALSE)
}
t2 <- Sys.time()
t2 - t1

size_results <- data.table(h_angle = integer(),
                           c_value = numeric(),
                           size = numeric())
for(c_value in seq(16, 20, by = .25)) {
  print(c_value)
  dt <- read.csv(here::here("color_grid_sizes_smallest", 
                            paste0("c_value_", 
                                   gsub("\\.", "_", as.character(c_value)),
                                   ".csv")),
  )
  
  size_results <- rbind(size_results, dt)
  
}

ggplot(data = size_results,
       aes(h_angle, c_value, color = size, fill = size)) +
  geom_tile()

# I think I can just max maximize over a grid # maybe switch to use binary search or something?
# h angle goes from 0 to 59.5
# l values are 6 times 0 to 100
# sizes goes from 0 to 50 ish?
# c values go from 1 to 180

# need:
# h angle
# l values (six)
# c value
# size
  
# max size and max c_value, any h_angle
size_results <- size_results[size == max(size), 
                              ][c_value == max(c_value), 
                                ][h_angle == max(h_angle), ]

dt <- color_grid_set(size_results$h_angle,
                     size_results$c_value,
                     l_values = seq(10, 90, .25),
                     size_values = seq(20, 27, .25))

size <- unique(dt$size)

x_y_grid <- CJ(y = seq(-1, 1, length.out = 8) * size,
               l_values = seq(-1, 1, length.out = 8) * size,
               h_value = dt$h_value)
x_y_grid <- merge(x_y_grid, dt, by = "h_value")

x_y_grid[, ':='(x = c_value,
                l_values = l_values + l_value)]
x_y_grid[, ':='(x_new = x * cos(h_value * pi/180) - y * sin(h_value * pi/180),
                y_new = x * sin(h_value * pi/180) + y * cos(h_value * pi/180))]
x_y_grid[, ':=' (c_values = sqrt(x_new^2 + y_new^2),
                 h_values = (atan2(y_new, x_new) * 180/pi) %% 360)]
x_y_grid[, hex_color := hcl(h_values, c_values, l_values, fixup = FALSE)]

# geom_tile doesn't work https://github.com/tidyverse/ggplot2/issues/849

ggplot(data = x_y_grid,
       aes(xmin = y - size/7, 
           xmax = y + size/7,
           ymin = l_values - size/7,
           ymax = l_values + size/7,
           color = hex_color, 
           fill = hex_color)) +
  geom_rect() + 
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(. ~ h_value) +
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
# ok

# Save results
color_grids <- x_y_grid
color_grids[, ':=' (grid = frank(h_value, ties.method = "dense"))]
color_grids[, ':=' (col_num = frank(y, ties.method = "dense"),
                    row_num = frank(l_values, ties.method = "dense")),
            by = "grid"]
color_grids[, ':=' (y = NULL,
size = NULL,
x = NULL,
x_new = NULL,
y_new = NULL)]
setnames(color_grids,
         c("h_value", "c_value", "l_value"),
         c("base_h", "base_c", "base_l"))
setcolorder(color_grids,
            c("grid", "col_num", "row_num",
              "base_h", "base_c", "base_l",
              "h_values", "c_values", "l_values",
              "hex_color"))

ggplot(data = color_grids,
       aes(xmin = col_num - .5, 
           xmax = col_num + .5,
           ymin = row_num - .5,
           ymax = row_num + .5,
           color = hex_color, 
           fill = hex_color)) +
  geom_rect() + 
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(. ~ grid) +
  theme(legend.position = "none")

write.csv(color_grids,
          here::here("color_grids.csv"),
          row.names = FALSE)
