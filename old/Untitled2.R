#install.packages("rchess")
library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

source(here("create_game_dataset.R"))

game_names <- list.files(pattern = ".*\\.pgn$")

hue_turns <- (seq(1, 360, length.out = 7)[1:6] + 1996) %% 360
# ggplot(data = data.frame(x = cos(hue_turns * pi/180),
#                          y = sin(hue_turns * pi/180))) +
#   geom_point(aes(x = x,
#                  y = y)) +
#   coord_equal()

games <- vector(mode = "list", length = length(game_names))
turns <- vector(mode = "list", length = length(game_names))
for(i in seq_along(game_names)) {
  games[[i]] <- create_game_dataset(game_names[i], hue_turns[i])
  turns[[i]] <- max(games[[i]][, 'turn'])
}

max_turns <- max(as.numeric(turns))

titles <- rep(c("Deep Blue (Computer) vs Garry Kasparov 1996\nGame ",
                "Garry Kasparov vs Deep Blue (Computer) 1996\nGame "), 3)
titles <- paste0(titles, seq(1, 6))

gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                         24.5, 26.5, 28.5, 30.5, 31.5))

#
#
#
# #
# #
# # #
# # #
# # #
# #
# #
#
#
#

library(imager)
pixels <- CJ(x = seq(1, 5 * 300),
             y = seq(8 * 300),
             cc = c(1, 2, 3),
             value = 1)
setkey(pixels, x, y, cc)

game_pixels <- games[[2]]
color_value <- data.frame(t(col2rgb(game_pixels$color_value)))
game_pixels[, c("R", "G", "B") := data.frame(t(col2rgb(game_pixels$color_value)))]

moves <- game_pixels[, .(x, turn, R, G, B)]
moves[, turn := turn + 1]
setnames(moves, "turn", "y")
moves <- melt(moves, 
              id.vars = c("x", "y"),
              measure.vars = c("R", "G", "B"),
              variable.name = "cc")
moves[, cc := ifelse(cc == "R", 1,
                     ifelse(cc == "G", 2, 3))]
moves[, value := value / 225]

## move to correct spot
# x = 17 needs to be center = 300 * 5 / 2 = 750
# x = 1 needs to be left most = 750 - (13 * 16) = 542
# y = 1 needs to be top = .5 inch down = .5 * 300 = 150 = 151
# all needs to be 13 spaces

moves[, ':=' (x = (x * 13) - 12 + 542,
              y = (y * 13) - 12 + 150)]
setkey(moves, x, y, cc)

# unique(moves$y)
# unique(moves$x)
# 300 * 5 - (max(unique(moves$x)) + 13)

# stretch out boxes
moves <- moves[rep(1:.N, each = (13 * 13))][, 
                                      ':=' (x = x + rep(seq(0, 12), 13),
                                            y = y + rep(seq(0, 12), each = 13)),
                                      by = list(x, y, cc)]
setkey(moves, x, y, cc)
pixels[moves, `:=`(value = i.value)]

## capture square
# similar deal
captures_square <- game_pixels[!is.na(captured_x), .(captured_x, turn, R, G, B)]
captures_square[, turn := turn + 1]
setnames(captures_square, c("captured_x", "turn"), c("x", "y"))
captures_square <- melt(captures_square, 
              id.vars = c("x", "y"),
              measure.vars = c("R", "G", "B"),
              variable.name = "cc")
captures_square[, cc := ifelse(cc == "R", 1,
                     ifelse(cc == "G", 2, 3))]
captures_square[, value := value / 225]
captures_square[, ':=' (x = (x * 13) - 12 + 542,
                        y = (y * 13) - 12 + 150)]
setkey(captures_square, x, y, cc)
captures_square <- captures_square[rep(1:.N, each = (9 * 9))][, 
                                            ':=' (x = x + rep(seq(2, 10), 9),
                                                  y = y + rep(seq(2, 10), each = 9)),
                                            by = list(x, y, cc)]
setkey(captures_square, x, y, cc)
pixels[captures_square, `:=`(value = i.value)]

## captured lines
captures_lines <- game_pixels[!is.na(captured_x), .(captured_x, x, turn, R, G, B)]
captures_lines[, turn := turn + 1]
captures_lines[, ':=' (x = ifelse(x < captured_x, x, captured_x),
                       x_length = abs(x - captured_x))]
setnames(captures_lines, "turn", "y")
captures_lines <- melt(captures_lines, 
              id.vars = c("x", "y", "x_length"),
              measure.vars = c("R", "G", "B"),
              variable.name = "cc")
captures_lines[, cc := ifelse(cc == "R", 1,
                     ifelse(cc == "G", 2, 3))]
captures_lines[, value := value / 225]
captures_lines[, ':=' (x = (x * 13) - 12 + 542,
                      y = (y * 13) - 12 + 150,
                      x_length = x_length * 13)]
setkey(captures_lines, x, y, cc)

captures_lines <- captures_lines[rep(1:.N, (x_length * 5))][, 
                                            ':=' (x = x + rep(seq(7, 7 + min(x_length) - 1), 5),
                                                  y = y + rep(seq(4, 8), each = min(x_length))),
                                            by = list(x, y, cc)]
setkey(captures_lines, x, y, cc)
pixels[captures_lines, `:=`(value = i.value)]

749/751

## forgot the gaps!!!!!!!!!

test <- as.cimg(pixels, dims = c(5 * 300,
                             8 * 300,
                             1,
                             3))
test <- implot(test, 
               text(x = (300 * 5) - (300 * .5) ,
                  y = (300 * 8) - (300 * .5),
                  labels = titles[[1]],
                  adj = c(1, 0),
                  cex=3))

par(family = "mono")
test <- implot(test, 
               text(x = (300 * 5) - (300 * .5) ,
                    y = (300 * 8) - (300 * 1.5),
                    labels = titles[[1]],
                    adj = c(1, 0),
                    cex=3,
                    family = 'mono'))

save.image(test, file = here("test.png"))

plot(test)

?quartzFont
?text

?implot


## https://stackoverflow.com/questions/23807021/how-to-do-in-r-load-an-image-file-print-text-on-image-save-modified-image
#open new file for output
png(here("out.png"), width=300*5, height=300*8)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")    
rasterImage(test, usr[1], usr[3], usr[2], usr[4])

#add text
text(x = (5 - .5) / 5,
     y = (.5) / 8,
     labels = titles[[1]],
     adj = c(1, 0),
     cex=3,
     family = "Courier New")

#close image
dev.off()

