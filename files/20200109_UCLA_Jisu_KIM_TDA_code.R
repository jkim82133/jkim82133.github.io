##########################################################################
# installing R package TDA
##########################################################################
if (!require(package = "TDA")) {
  install.packages(pkgs = "TDA")
}



##########################################################################
# installing required packages
##########################################################################
if (!require(package = "FNN")) {
  install.packages(pkgs = "FNN")
}
if (!require(package = "igraph")) {
  install.packages(pkgs = "igraph")
}
if (!require(package = "scales")) {
  install.packages(pkgs = "scales")
}



##########################################################################
# loading R package TDA
##########################################################################
library(package = "TDA")



##########################################################################
# uniform sample on the circle
##########################################################################
circleSample <- circleUnif(n = 20, r = 1)
plot(circleSample, xlab = "", ylab = "", pch = 20)



##########################################################################
# uniform sample on the circle, and grid of points
##########################################################################
X <- circleUnif(n = 400, r = 1)

lim <- c(-1.7, 1.7)
by <- 0.05
margin <- seq(from = lim[1], to = lim[2], by = by)
Grid <- expand.grid(margin, margin)



##########################################################################
# distance to measure
##########################################################################
m0 <- 0.1
DTM <- dtm(X = X, Grid = Grid, m0 = m0)

par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
persp(x = margin, y = margin,
      z = matrix(DTM, nrow = length(margin), ncol = length(margin)),
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
      main = "DTM")



##########################################################################
# kernel density estimator
##########################################################################
h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)

par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
persp(x = margin, y = margin,
    z = matrix(KDE, nrow = length(margin), ncol = length(margin)),
    xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
    expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
    main = "KDE")



##########################################################################
# persistent homology of a function over a grid
##########################################################################
DiagGrid <- gridDiag(X = X, FUN = kde, lim = c(lim, lim), by = by,
    sublevel = FALSE, library = "Dionysus", location = TRUE,
    printProgress = FALSE, h = h)



##########################################################################
# plotting persistence diagram
##########################################################################
par(mfrow = c(1,3))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
one <- which(DiagGrid[["diagram"]][, 1] == 1)
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagGrid[["cycleLocation"]][[one[i]]])[1])) {
    lines(DiagGrid[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
        col = i + 1)
  }
}
persp(x = margin, y = margin,
    z = matrix(KDE, nrow = length(margin), ncol = length(margin)),
    xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
    expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.9,
    main = "KDE")
plot(x = DiagGrid[["diagram"]], main = "KDE Diagram")



##########################################################################
# rips persistence diagram
##########################################################################
DiagRips <- ripsDiag(X = X, maxdimension = 1, maxscale = 0.5,
    library = c("GUDHI", "Dionysus"), location = TRUE)



##########################################################################
# plotting persistence diagram
##########################################################################
par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
plot(x = DiagRips[["diagram"]], main = "Rips Diagram")



##########################################################################
# computing landscape function
##########################################################################
tseq <- seq(0, 0.2, length = 1000)
Land <- landscape(DiagGrid[["diagram"]], dimension = 1, KK = 1, tseq = tseq)



##########################################################################
# plotting landscape function
##########################################################################
par(mfrow = c(1,2))
plot(x = DiagGrid[["diagram"]], main = "KDE Diagram")
plot(tseq, Land, type = "l", xlab = "(Birth+Death)/2",
     ylab = "(Death-Birth)/2", asp = 1, axes = FALSE, main = "Landscape")
axis(1); axis(2)



##########################################################################
# bootstrap confidence band for kde function
##########################################################################
bandKDE <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100,
    parallel = FALSE, alpha = 0.1, h = h)
print(bandKDE[["width"]])



##########################################################################
# bootstrap confidence band for persistent homology over a grid
##########################################################################
par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
plot(x = DiagGrid[["diagram"]], band = 2 * bandKDE[["width"]],
    main = "KDE Diagram")



##########################################################################
# bootstrap confidence band for landscape
##########################################################################
par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
plot(tseq, Land, type = "l", xlab = "(Birth+Death)/2",
     ylab = "(Death-Birth)/2", asp = 1, axes = FALSE, main = "200 samples")
axis(1); axis(2)
polygon(c(tseq, rev(tseq)), c(Land - bandKDE[["width"]],
        rev(Land + bandKDE[["width"]])), col = "pink", lwd = 1.5,
        border = NA)
lines(tseq, Land)