library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

color_list <- c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF")
color_list_LUV <- as(hex2RGB(color_list), "polarLUV")
color_list <- c(color_list[1],
                hex(mixcolor(alpha = .5, 
                             color1 = color_list_LUV[1], 
                             color2 = color_list_LUV[2]),
                    fixup = TRUE),
                color_list[2],
                color_list[3],
                hex(mixcolor(alpha = .5, 
                             color1 = color_list_LUV[4], 
                             color2 = color_list_LUV[5]),
                    fixup = TRUE),
                color_list[6])
h_set <- coords(as(hex2RGB(color_list), "polarLUV"))[, 3]
h_set <- c(h_set[length(h_set)] - 360, h_set, h_set[1] + 360)

i <- 2
h_values <- seq((h_set[i] * .75 + h_set[i + 1] * .25), 
                (h_set[i + 1] * .25 + h_set[i + 2] * .75), length.out = 8)

# So lock H values, just min max h values (for top/bottom)
# Moves along L? and L differences?
# Yeah, this, then just build a 3d grid with constraints, then convert to color

# yeah, grid of starting L values vs L differences
# set top/bottom based on min/max H /max chroma
# figure out grid (based on max chroma?)
# given min/max L/H, chroma is defined, right?

# Set up plane/rectangle based on grid
# hinge at top or bottom
# calculate distance to hitting outside

# Set L = 80
h_values <- c(h_set[i] * .75 + h_set[i + 1] * .25, 
                (h_set[i + 1] * .25 + h_set[i + 2] * .75))
starting_chroma <- max_chroma(h = h_values, l = 80, floor = TRUE)

starting_line <- data.table(x_values = seq(starting_chroma[1] * cos(h_values[1] * pi/180), 
                                           starting_chroma[2] * cos(h_values[2] * pi/180),
                                           length.out = 8),
                            y_values = seq(starting_chroma[1] * sin(h_values[1] * pi/180), 
                                           starting_chroma[2] * sin(h_values[2] * pi/180),
                                           length.out = 8))
starting_line[, ':=' (h_values = atan2(y_values, x_values) * 180/pi,
                      c_values = sqrt(x_values^2 + y_values^2),
                      l_values = 80)]
starting_line[, ':=' (color_values = hcl(h_values,
                                        c_values,
                                        l_values))]
ggplot(data = starting_line,
       aes(x = x_values,
           y = y_values,
           color = color_values)) +
  geom_point(size = 5) +
  scale_color_identity() + 
  xlim(c(-100, 100)) +
  ylim(c(-100, 100)) +
  coord_equal()

distance <- sqrt((starting_line[1, 1] - starting_line[2, 1])^2 + 
                   (starting_line[1, 2] - starting_line[2, 2])^2)
  
# sqrt((starting_line[1, 1] - starting_line[8, 1])^2 + 
#        (starting_line[1, 2] - starting_line[8, 2])^2) / 7

color_set <- starting_line[rep(1:.N, each = 8)][,
                                                ':=' (l_values = l_values + -cumsum(c(0, rep(distance, 7))))]
color_set[, max_chroma_values := max_chroma(h_values, l_values, floor = TRUE)]
color_set[, ':=' (color_values = hcl(h_values,
                                         c_values,
                                         l_values))]

ggplot(data = color_set,
       aes(x = x_values,
           y = y_values,
           color = color_values)) +
  geom_point(size = 5) +
  scale_color_identity() + 
  coord_equal() +
  facet_wrap(l_values ~ .)

ggplot(data = color_set,
       aes(x = x_values,
           y = l_values,
           fill = color_values)) +
  geom_tile() +
  scale_fill_identity() +
  coord_cartesian()

## reverse all this instead
# start with grid then
# scale
# rotate 
# translate

## one point, two angles, size (is max from top and bottom?)
## Scale up
## Rotation on x
## Rotation on z
## Rotate to face point (H value?)
## Move to center point

# Test with starting line

starting <- matrix(c(seq(1, 8), rep(0, 8), rep(0, 8)),
                   nrow = 3, byrow = TRUE)
turn_1 <- matrix(c(1, 0, 0,
                   0, cos(pi/2), -sin(pi/2),
                   0, sin(pi/2), cos(pi/2)),
                 nrow = 3)
turn_1 <- matrix(c(cos(pi/2), -sin(pi/2), 0,
                   sin(pi/2), cos(pi/2), 0,
                   0, 0, 1),
                 nrow = 3)
turn_1 %*% starting

# never rotate around the y to keep l_values evenly spaced
# never rotate around x to keep h_values evenly spaced
# rotate around z to move sides

distance <- 10
angle_side <- 5
h_starting <- h_values[1] * .5 + h_values[2] * .5
c_starting <- 55
l_starting <- 60
starting <- matrix(unlist(expand.grid(x = seq(-distance * 3.5, distance * 3.5, by = distance),
                               y = 0,
                               z = seq(-distance * 3.5, distance * 3.5, by = distance))),
                   nrow = 3, byrow = TRUE)
angle_side <- angle_side * pi/180
side_turn <- matrix(c(cos(angle_side), -sin(angle_side), 0,
                   sin(angle_side), cos(angle_side), 0,
                   0, 0, 1),
                 nrow = 3)
h_turn <- matrix(c(cos(h_starting * pi/180), -sin(h_starting * pi/180), 0,
                      sin(h_starting * pi/180), cos(h_starting * pi/180), 0,
                      0, 0, 1),
                    nrow = 3)
# starting <- side_turn %*% top_turn %*% starting # order important?
starting <- side_turn %*% starting
starting <- h_turn %*% starting

starting <- data.table(x = starting[1, ],
                       y = starting[2, ],
                       z = starting[3, ])
# ggplot(data = starting,
#        aes(x = x,
#            y = y)) +
#   geom_point() +
#   facet_wrap(. ~ z)
# ggplot(data = starting,
#        aes(x = x,
#            y = z)) +
#   geom_point()

## move to point
starting[, ':=' (h_starting = h_starting,
                 c_starting = c_starting,
                 l_starting = l_starting)]
starting[, ':=' (x_starting = c_starting * cos(h_starting * pi/180),
                 y_starting = c_starting * sin(h_starting * pi/180),
                 z_starting = l_starting)]
starting[, ':=' (x_values = x + x_starting,
                 y_values = y + y_starting,
                 z_values = z + z_starting)]
starting[, ':=' (h_values = atan2(y_values, x_values) * 180/pi,
                      c_values = sqrt(x_values^2 + y_values^2),
                      l_values = z_values)]
starting[, ':=' (color_values = hcl(h_values,
                                         c_values,
                                         l_values))]

ggplot(data = starting,
       aes(x = x_values,
           y = y_values,
           color = color_values)) +
  geom_point(size = 5) +
  scale_color_identity() + 
  coord_equal() +
  facet_wrap(l_values ~ .)

ggplot(data = starting,
       aes(x = h_values,
           y = l_values,
           fill = color_values)) +
  geom_tile() +
  scale_fill_identity() +
  coord_cartesian()

# fails check for h_values
# write in checks for h_values, c_values
# Simualated Annealing 