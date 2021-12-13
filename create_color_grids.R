# Libraries
library(colorspace)
library(data.table)
library(ggplot2)

## Function to check if the colors are valid
# Given hue, chroma1, chroma2, and top luminance values, 
# return size or 0 if failed
return_distance <- function(hue_1, hue_2, chroma_1, chroma_2, luminance) {
  
  x_1 <- chroma_1 * cos(hue_1 * pi/180)
  y_1 <- chroma_1 * sin(hue_1 * pi/180)
  x_2 <- chroma_2 * cos(hue_2 * pi/180)
  y_2 <- chroma_2 * sin(hue_2 * pi/180)
  
  distance <- sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2)
  
  if(luminance - distance < 0) {
    return(0)
  }
  
  dt <- data.table(x = rep(seq(x_1, x_2, length.out = 8), 8),
                   y = rep(seq(y_1, y_2, length.out = 8), 8),
                   luminance = rep(seq(luminance, luminance - distance, 
                                       length.out = 8), each = 8))
  dt[, ':=' (hue = (atan2(y, x) * 180/pi) %% 360,
             chroma = sqrt(x^2 + y^2))]
  dt[, ':=' (color = hcl(hue, chroma, luminance, fixup = FALSE))]
  
  if(any(is.na(dt$color))) {
    return(0)
  }
  
  # # Check
  # ggplot(data = dt,
  #        aes(y, luminance, color = color)) +
  #   geom_point(size = 5) +
  #   scale_color_identity()
  # 
  # ggplot(data = dt,
  #        aes(hue, chroma, color = color)) +
  #   geom_point() + 
  #   scale_color_identity() +
  #   scale_fill_identity() +
  #   scale_x_continuous("Hue",
  #                      limits = c(0, 360),
  #                      breaks = seq(0, 360, 60),
  #                      minor_breaks = seq(0, 360, 60) + 30) +
  #   scale_y_continuous("Chroma",
  #                      limits = c(0, 180)) +
  #   coord_polar(start = 270 * pi / 180,
  #               direction = -1) +
  #   theme(legend.position = "none")
  
  return(distance)
}

## Function to get the color values
# Returns the whole table of colors, not just distance check
return_table <- function(hue_1, hue_2, chroma_1, chroma_2, luminance) {
  
  x_1 <- chroma_1 * cos(hue_1 * pi/180)
  y_1 <- chroma_1 * sin(hue_1 * pi/180)
  x_2 <- chroma_2 * cos(hue_2 * pi/180)
  y_2 <- chroma_2 * sin(hue_2 * pi/180)
  
  distance <- sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2)
  
  dt <- data.table(x = rep(seq(x_1, x_2, length.out = 8), 8),
                   y = rep(seq(y_1, y_2, length.out = 8), 8),
                   luminance = rep(seq(luminance, luminance - distance, 
                                       length.out = 8), each = 8))
  dt[, ':=' (hue = (atan2(y, x) * 180/pi) %% 360,
             chroma = sqrt(x^2 + y^2))]
  dt[, ':=' (color = hcl(hue, chroma, luminance, fixup = FALSE))]
  
  return(dt)
}

## Set up parameter grid to check
color_parameters <- CJ(hue_1 = seq(0, 330, 30),
                       chroma_1 = seq(22.5, 120, 2.5),
                       chroma_2 = seq(22.5, 120, 2.5),
                       luminance = seq(20, 98, 1))

# Keep the ones with the two chroma values different enough
color_parameters <- color_parameters[abs(chroma_1 - chroma_2) > 15]
# Add turn amount to hue
color_parameters[, hue_2 := hue_1 + 45]

## Run through all the options to see which ones produce a full set of colors
t1 <- Sys.time()
color_parameters[, distance := return_distance(hue_1, hue_2,
                                              chroma_1, chroma_2,
                                              luminance),
                 by = seq_len(nrow(color_parameters))]
t2 <- Sys.time()
t2 - t1

# Drop the parameter sets that fail, drop unneeded columns
check_colors <- color_parameters[distance > 0, ][, ':=' (luminance = NULL,
                                                             hue_2 = NULL)]

# Find the set with max distance (so largest region)
check_colors <- check_colors[, .(distance = max(distance)),
                 by = .(hue_1, chroma_1, chroma_2)]
# Need to make sure all twelve hues are there
check_colors <- check_colors[, .(distance = min(distance),
                                         hues = length(unique(hue_1))),
                                     by = .(chroma_1, chroma_2)][hues == 12, ]
check_colors <- check_colors[
  check_colors[, .I[distance == max(distance)]]]
check_colors <- check_colors[1] # make sure to only get one row

final_parameters <- color_parameters[chroma_1 == check_colors$chroma_1 &
                                     chroma_2 == check_colors$chroma_2][
                                       distance > 0]

## closest middle luminance
# can't use median because that breaks when ties
final_parameters[, luminance_quantile := frank(luminance) / .N, by = .(hue_1)]
final_parameters[, luminance_quantile_diff := abs(.5 - luminance_quantile)]
final_parameters <- final_parameters[
  final_parameters[, 
                   .I[luminance_quantile_diff == min(luminance_quantile_diff)], 
                   by = hue_1]$V1]
final_parameters <- final_parameters[
  final_parameters[, .I[luminance == min(luminance)], by = hue_1]$V1]

# Built out full set
dt <- data.table(x = numeric(),
                 y = numeric(),
                 luminance = numeric(),
                 hue = numeric(),
                 chroma = numeric(),
                 color = character(),
                 col_num = integer(),
                 row_num = integer(),
                 grid = integer(),
                 base_h = numeric(),
                 base_c = numeric(),
                 base_l = numeric())
for(grid in seq(1, 12)) {
  dt_now <- return_table(final_parameters[grid, ]$hue_1,
                      final_parameters[grid, ]$hue_2,
                      final_parameters[grid, ]$chroma_1,
                      final_parameters[grid, ]$chroma_2,
                      final_parameters[grid, ]$luminance)
  distance <- max(dt_now$luminance) - min(dt_now$luminance)
  
  dt_now[, ':=' (col_num = frank((180 - abs(abs(hue - final_parameters[grid, ]$hue_1) - 180)) * 
                                   sign(180 - abs(hue - final_parameters[grid, ]$hue_1)) * 
                                   sign(hue - final_parameters[grid, ]$hue_1), ties.method = "dense"),
                      row_num = frank(luminance, ties.method = "dense"),
                                      grid = grid,
                 base_h = final_parameters[grid, ]$hue_1,
                 base_c = final_parameters[grid, ]$chroma_1,
                 base_l = final_parameters[grid, ]$luminance - (distance / 2))]
  dt <- rbind(dt, dt_now)
}

ggplot(data = dt,
       aes(xmin = col_num - .5,
           xmax = col_num + .5,
           ymin = row_num - .5,
           ymax = row_num + .5,
           color = color,
           fill = color)) +
  geom_rect() +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(. ~ grid)

ggplot(data = dt,
       aes(hue, chroma, color = color)) +
  geom_point() +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous("Hue",
                     limits = c(0, 360),
                     breaks = seq(0, 360, 60),
                     minor_breaks = seq(0, 360, 60) + 30) +
  scale_y_continuous("Chroma",
                     limits = c(0, 50)) +
  coord_polar(start = 270 * pi / 180,
              direction = -1) +
  theme(legend.position = "none")


## set up to be color_grids output
dt[, ":=" (x = NULL,
           y = NULL)]

setnames(dt,
         c("hue", "chroma", "luminance", "color"),
         c("h_values", "c_values", "l_values", "hex_color"))
setcolorder(dt,
            c("grid", "col_num", "row_num",
              "base_h", "base_c", "base_l",
              "h_values", "c_values", "l_values",
              "hex_color"))
write.csv(dt,
          here::here("color_grids.csv"),
          row.names = FALSE)
