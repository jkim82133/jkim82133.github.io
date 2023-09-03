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
# installing R package TDA
##########################################################################
if (!require(package = "TDA")) {
  install.packages(pkgs = "TDA")
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
# rips filtration
##########################################################################
FltRips <- ripsFiltration(X = X, maxdimension = 1, maxscale = 0.5,
    library = "GUDHI")



##########################################################################
# filtration from function values
##########################################################################
h <- 0.3
KDEx <- kde(X = X, Grid = X, h = h)
FltFun <- funFiltration(FUNvalues = KDEx, cmplx = FltRips[["cmplx"]],
    sublevel = FALSE)



##########################################################################
# persistence diagram from filtration
##########################################################################
DiagFltFun <- filtrationDiag(filtration = FltFun, maxdimension = 1,
    library = "Dionysus", location = TRUE, printProgress = FALSE)



##########################################################################
# plotting persistence diagram
##########################################################################
par(mfrow = c(1,3))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
plot(x = DiagGrid[["diagram"]], main = "KDE Diagram over grid")
plot(x = DiagFltFun[["diagram"]], diagLim = c(0, 0.27),
    main = "KDE Diagram over Rips filtration")



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