"station.symbol" <-
function (cx, cy, direction = 0, speed = 0, fill = 0, color = "green",
    temp = NULL, press = NULL, dewpt = NULL, circle=TRUE, cex = 2)
{
#
# Copyright 2001,2002 Eric Gilleland, and Doug Nychka
#
# This file is part of the RadioSonde library for R and related languages.
#
# RadioSonde is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# RadioSonde is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RadioSonde; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
    tpar <- par()
    size <- tpar$csi
    scalex <- (tpar$usr[2] - tpar$usr[1])/(tpar$pin[1])
    scaley <- (tpar$usr[4] - tpar$usr[3])/(tpar$pin[2])
    scalex <- (cex * (scalex * size))/5
    scaley <- (cex * (scaley * size))/5
    xs <- if (speed > 0) {
        X1 <- 0
        X2 <- 0
        Y1 <- 0
        Y2 <- 5
        if (speed >= 5 & speed < 10) {
            X1 <- c(X1, 0)
            X2 <- c(X2, 1)
            Y1 <- c(Y1, 5)
            Y2 <- c(Y2, 5)
        }
        if (speed >= 10 & speed < 15) {
            X1 <- c(X1, 0)
            X2 <- c(X2, 2)
            Y1 <- c(Y1, 5)
            Y2 <- c(Y2, 5)
        }
        if (speed >= 15 & speed < 20) {
            X1 <- c(X1, 0, 0)
            X2 <- c(X2, 1, 2)
            Y1 <- c(Y1, 4, 5)
            Y2 <- c(Y2, 4, 5)
        }
        if (speed >= 20 & speed < 25) {
            X1 <- c(X1, 0, 0)
            X2 <- c(X2, 2, 2)
            Y1 <- c(Y1, 4, 5)
            Y2 <- c(Y2, 4, 5)
        }
        if (speed >= 25 & speed < 30) {
            X1 <- c(X1, 0, 0, 0)
            X2 <- c(X2, 1, 2, 2)
            Y1 <- c(Y1, 3, 4, 5)
            Y2 <- c(Y2, 3, 4, 5)
        }
        if (speed >= 30 & speed < 35) {
            X1 <- c(X1, 0, 0, 0)
            X2 <- c(X2, 2, 2, 2)
            Y1 <- c(Y1, 3, 4, 5)
            Y2 <- c(Y2, 3, 4, 5)
        }
        if (speed >= 35 & speed < 40) {
            X1 <- c(X1, 0, 0, 0, 0)
            X2 <- c(X2, 1, 2, 2, 2)
            Y1 <- c(Y1, 2, 3, 4, 5)
            Y2 <- c(Y2, 2, 3, 4, 5)
        }
        if (speed >= 40 & speed < 45) {
            X1 <- c(X1, 0, 0, 0, 0)
            X2 <- c(X2, 2, 2, 2, 2)
            Y1 <- c(Y1, 2, 3, 4, 5)
            Y2 <- c(Y2, 2, 3, 4, 5)
        }
        if (speed >= 45 & speed < 50) {
            X1 <- c(X1, 0, 0, 0, 0, 0)
            X2 <- c(X2, 1, 2, 2, 2, 2)
            Y1 <- c(Y1, 1, 2, 3, 4, 5)
            Y2 <- c(Y2, 1, 2, 3, 4, 5)
        }
        if (speed >= 50 & speed < 55) {
            X1 <- c(X1, 0, 0)
            X2 <- c(X2, 2, 2)
            Y1 <- c(Y1, 4, 5)
            Y2 <- c(Y2, 4.5, 4.5)
        }
        if (speed >= 55 & speed < 60) {
            X1 <- c(X1, 0, 0, 0)
            X2 <- c(X2, 1, 2, 2)
            Y1 <- c(Y1, 3, 4, 5)
            Y2 <- c(Y2, 3, 4.5, 4.5)
        }
        if (speed >= 60 & speed < 65) {
            X1 <- c(X1, 0, 0, 0)
            X2 <- c(X2, 2, 2, 2)
            Y1 <- c(Y1, 3, 4, 5)
            Y2 <- c(Y2, 3, 4.5, 4.5)
        }
        direction <- (direction/360) * 2 * pi
        rot <- cbind(c(cos(direction), -sin(direction)), c(sin(direction),
            cos(direction)))
        S1 <- rbind(X1, Y1)
        S2 <- rbind(X2, Y2)
        S1 <- rot %*% S1
        S2 <- rot %*% S2
        S1 <- S1 * c(scalex, scaley) + c(cx, cy)
        S2 <- S2 * c(scalex, scaley) + c(cx, cy)
    }
    if (speed > 0) {
        segments(S1[1, ], S1[2, ], S2[1, ], S2[2, ])
    }

    #
    # Add circle to base of station annotation symbol if desired.
    #

    if(circle) {
        ts <- seq(0, 2 * pi, , 200)
        RX <- sin(ts) * scalex
        X1 <- RX + cx
        RY <- cos(ts) * scaley
        Y1 <- RY + cy
        if (speed == 0) {
            lines(RX * 2 + cx, RY * 2 + cy)
        }
        if (fill > 0) {
            lim <- c(51, 101, 151, 200)
            polygon(c(cx, X1[1:lim[fill]]), c(cy, Y1[1:lim[fill]]),
                density = -1, col = color)
        }
        lines(RX + cx, RY + cy)
        if (!is.null(temp)) {
            temp.text <- paste(temp, sep = "")
            temp.x <- cx - 3.25 * scalex
            temp.y <- cy + 1.25 * scaley
            text(temp.x, temp.y, labels = temp.text)
        }
        if (!is.null(press)) {
            press.text <- paste( press, sep = "")
            press.x <- cx + 4.75 * scalex
            press.y <- cy + 1.25 * scaley
            text(press.x, press.y, labels = press.text)
        }
        if (!is.null(dewpt)) {
            dewpt.text <- paste( dewpt, sep = "")
            dewpt.x <- cx - 3.25 * scalex
            dewpt.y <- cy - 2.25 * scaley
            text(dewpt.x, dewpt.y, labels = dewpt.text)
        }
    } # end of if circle stmt

invisible()
}
