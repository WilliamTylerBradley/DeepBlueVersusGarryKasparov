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

## try to redo through transformation matrix
m_old <- matrix(data = c(1, 0, 0,
                         0, 1, 0,
                         0, 0, 1),
                nrow = 3)

# New basis
# V1 = top right to top left
# V2 = top center to center center
# V3 = perpendicular
h_left <- h_values[2]
h_right <- h_values[1]
h_center <- (h_left + h_right) / 2
c_left <- 54
c_right <- 88
l_top <- 80
# angle for slant
angle <- -5

x_left <- c_left * cos(h_left * pi/180)
y_left <- c_left * sin(h_left * pi/180)
z_left <- l_top
x_right <- c_right * cos(h_right * pi/180)
y_right <- c_right * sin(h_right * pi/180)
z_right <- l_top

x_bottom <- x_left
y_bottom <- y_left
z_bottom <- z_left - sqrt((x_left - x_right)^2 +
                            (y_left - y_right)^2)



## move top left to origin
x_move <- -x_left
y_move <- -y_left
z_move <- -z_left
x_left <- x_left + x_move
y_left <- y_left + y_move
x_right <- x_right + x_move
y_right <- y_right + y_move
z_left <- z_left + z_move
z_right <- z_right + z_move

# Get bottom point
# rotate top right 90 around Z then 90-angle down?

V1 <- c(x_right, y_right, z_right)
V2 <- c((x_right + x_left) - x_center,
        (y_right + y_left) - y_center,
        (z_right + z_left) - z_center)
V2 <- c(x_left, y_left, z_left) + V2

V1 %*% V2

library(pracma)
V3 <- cross(V1, V2)

m_new <- matrix(data = c(V1, V2, V3),
                nrow = 3)
change_basis <- solve(m_new) %*% m_old

test <- c(7, 0, 0) %*% change_basis
test

## What if start from outside, Then moved in until all points are inside?
# Yes, this is easier to find distance needed to move?
# Actually, build max_chroma table to be x, y
# Then try for multiple values of starting L.

## New max chroma table
max_c <- data.table(c_max_value = max_chroma_table,
                    location = names(max_chroma_table))
max_c[, ':=' (h_value = as.numeric(sub("-.*", "", location)),
              l_value = as.numeric(sub(".*-", "", location)),
              location = NULL)]

# setup grid with end four points
l_top <- seq(50, 80, 2)
h_left <- seq(h_values[1], h_values[1] * .75 + h_values[2] * .25, length.out = 15)
h_right <- seq(h_values[1] * .25 + h_values[2] * .75, h_values[2], length.out = 15)
c_left <- seq(20, 70, by = 2)
c_right <- seq(20, 70, by = 2)
c_center <- seq(20, 70, by = 2)

get_points <- function(l_top, h_left, h_right, c_left, c_right, c_center) {
  
  h_center <- mean(c(h_left, h_right))
  
  x_left <- c_left * cos(h_left * pi/180)
  y_left <- c_left * sin(h_left * pi/180)
  x_right <- c_right * cos(h_right * pi/180)
  y_right <- c_right * sin(h_right * pi/180)
  x_center <- c_center * cos(h_center * pi/180)
  y_center <- c_center * sin(h_center * pi/180)
  
  point_distance <- sqrt((x_left - x_right)^2 + (y_left - y_right)^2) / 7
  l_center <- l_top - 3.5 * point_distance
  
  # Get plane
  a1 <- x_left - x_right
  b1 <- y_left - y_right
  c1 <- l_top - l_top
  a2 <- x_center - x_right
  b2 <- y_center - y_right
  c2 <- l_center - l_top
  a <- b1 * c2 - b2 * c1
  b <- a2 * c1 - a1 * c2
  c <- a1 * b2 - b1 * a2
  d <- (- a * x_right - b * y_right - c * l_top)
  
}

# # # # # # # #

# Determine transformation matrix, then apply to all?









