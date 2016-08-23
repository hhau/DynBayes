# neat-o visualsations
dat <- as.data.frame(read.csv("data.2013.through.r18.2015.csv"))

all.team.names <- matrix(
  c(
    "Adelaide",
    "Brisbane",
    "Carlton",
    "Collingwood",
    "Essendon",
    "Fremantle",
    "GWS",
    "Geelong",
    "Gold Coast",
    "Hawthorn",
    "Melbourne",
    "North Melbourne",
    "Port Adelaide",
    "Richmond",
    "St Kilda",
    "Sydney",
    "West Coast",
    "Western Bulldogs"
  ),
  nrow = 18,
  ncol = 1
)

dat$goal.diff <- dat$home.goals - dat$away.goals
dat$behinds.diff <- dat$home.behinds - dat$away.behinds

#
home.list <- lapply(X = all.team.names, function(x){which(dat$home.name %in% x)})
away.list <- lapply(X= all.team.names, function(x){which(dat$away.name %in% x)})

n.time.points <- length(home.list[[1]]) + length(away.list[[1]])

#
goal.data.by.team <- matrix(0, nrow = 18, ncol = n.time.points)
behind.data.by.team <- matrix(0, nrow = 18, ncol = n.time.points)

goal.diff.data.by.team <- matrix(0, nrow = 18, ncol = n.time.points)
behind.diff.data.by.team <- matrix(0, nrow = 18, ncol = n.time.points)

library(data.table)
#
for (i in 1:18) {
  index.vec <- c(home.list[[i]], away.list[[i]])
  # goals
  goal.vec <- c((dat$home.goals)[c(home.list[[i]])], (dat$away.goals)[c(away.list[[i]])])
  # have to sort it by the index vec first pls
  # behinds
  behind.vec <- c((dat$home.behinds)[c(home.list[[i]])], (dat$away.behinds)[c(away.list[[i]])])

  combined <- as.data.frame(matrix(c(index.vec, goal.vec, behind.vec), ncol = 3))

  data.table <- data.table(combined, key = "V1")

  goal.data.by.team[i,] <- (data.table$V2)[c(1:(n.time.points))]
  behind.data.by.team[i,] <-(data.table$V3)[c(1:(n.time.points))]

  # goal diff
  # negative the second argument as goal diff is home - away
  # and we want all the same things here
  goal.diff.vec <- c((dat$goal.diff)[c(home.list[[i]])], -(dat$goal.diff)[away.list[[i]]])
  behinds.diff.vec <- c((dat$behinds.diff)[c(home.list[[i]])], -(dat$behinds.diff)[away.list[[i]]])
  other.comb <- data.frame(cbind(index.vec, goal.diff.vec, behinds.diff.vec))
  other.data.table <- data.table(other.comb, key="index.vec")

  goal.diff.data.by.team[i,] <- (other.data.table$goal.diff.vec)[c(1:n.time.points)]
  behind.diff.data.by.team[i,] <- (other.data.table$behinds.diff.vec)[c(1:n.time.points)]
}
# these are really really rough, not suprising model is having a hard time fitting them
matplot(1:n.time.points, t(goal.data.by.team), type = "l")
matplot(1:n.time.points, t(behind.data.by.team), type = "l")

matplot(1:n.time.points, t(goal.diff.data.by.team), type = "l")
matplot(1:n.time.points, t(behind.diff.data.by.team), type = "l")


# smoothed
library(TTR)
q <- 7
ma <- function (x, n = q , ...)
{
  ma <- runMean(x, n)
  if (!is.null(dim(ma))) {
    colnames(ma) <- "SMA"
  }
  return(ma)
}


matplot(1:n.time.points, apply(goal.data.by.team, 1, ma), type = "l", xlim = c(q,75), xlab = "Round number, consecutive",
        ylab = "Moving Average of Raw Goals Scored", col = 1:18, lty = 1:6)
legend("right", all.team.names, col = 1:18, lty = 1:6,cex=0.8,fill=1:18 )
matplot(1:n.time.points, apply(behind.data.by.team, 1, ma), type = "l")


matplot(1:n.time.points, apply(goal.diff.data.by.team, 1, ma), type = "l", xlim=c(q,75), xlab = "Round number, consecutive",
        ylab = "Moving Average of Goal difference", col = 1:18, lty = 1:6)
legend("right", all.team.names, col = 1:18, lty = 1:6,cex=0.8,fill=1:18)


matplot(1:n.time.points, apply(behind.diff.data.by.team, 1, ma), type = "l",xlim =c(q,60))


# skellam dist visual

library(Bessel)
lambda1 <- 1
lambda2 <- 1
x <- -10:10
normalising.vec.1 <- matrix(NA, nrow = 1, ncol = length(x))

q <- 1
for (i in min(x):max(x)) {
  normalising.vec.1[q] <- BesselI(z = (2*sqrt(lambda1*lambda2)), nu = i, nSeq = 1)
  q <- q + 1
}
y <- exp(-(lambda1+lambda2)) * (lambda1/lambda2)^(x/2) * normalising.vec.1
#plot(x = x, y = y, type = "o", pch = "+", col =1, xlab = "z", ylab= expression(f[Z](z)))

lambda3 <- 3
lambda4 <- 4
normalising.vec.2 <- matrix(NA, nrow = 1, ncol = length(x))

q <- 1
for (i in min(x):max(x)) {
  normalising.vec.2[q] <- BesselI(z = (2*sqrt(lambda3*lambda4)), nu = i, nSeq = 1)
  q <- q + 1
}
y.2 <- exp(-(lambda3+lambda4)) * (lambda3/lambda4)^(x/2) * normalising.vec.2

#lines(x = x, y = y.2, type = "o", col = "red")

lambda5 <- 2
lambda6 <- 2
normalising.vec.3 <- matrix(NA, nrow = 1, ncol = length(x))

q <- 1
for (i in min(x):max(x)) {
  normalising.vec.3[q] <- BesselI(z = (2*sqrt(lambda5*lambda6)), nu = i, nSeq = 1)
  q <- q + 1
}
y.3 <- exp(-(lambda5+lambda6)) * (lambda5/lambda6)^(x/2) * normalising.vec.3

#lines(x = x, y = y.3, type = "o", col = "black")

mat <- t(rbind(y, y.2, y.3))
matplot(x, mat, type = "b", lty = 1:3, col = 1, pch = 4:6, xlab = "z", ylab = expression(f[Z](z)), cex = 0.7 )
leg.1 <- expression(paste(lambda[1], " = ", lambda[2], " = ", "1", sep = "   "))
leg.2 <- expression(paste(lambda[1], " = ", "3, " , lambda[2], " = ", "4", sep = "   "))
leg.3 <- expression(paste(lambda[1], " = ", lambda[2], " = ", "2", sep = "   "))
legend("left", legend = c(leg.1, leg.2, leg.3), lty = c(1:3), col = 1, bty = "n", pch = 4:6, ncol = 1, merge = F,
                          pt.cex = 0.8, cex = 1.3, y.intersp = 1.7)
# still looks ugly

#legend("topleft", legend = c("λ1 = λ2 = 1", "λ1 = 3, λ2 = 4", "λ1 =  λ2 = 2"),
       col = c("blue", "red", "black"), lty = 1, cex = 1.3, text.width = 5.4)

library(Bessel)
x = seq(from = 0, to = 8, by = 0.1)
plot(x = x, y =BesselI(x, nu = 0), type = "l", col = "blue", xlim = c(0,9), ylab = "BesselI(z,2(λ1λ2))", xlab = "2(λ1λ2)", cex.lab = 1.3)
lines(x=x, y= BesselI(x, nu = 1), type = "l", col ="black")
lines(x=x, y =BesselI(x, nu = 2), type = "l", col = "red")
lines(x=x, BesselI(x, nu = -3), type = "l", col = "green")
legend(x = 0, y = 400, legend = c("z = 0", "z = 1", "z = 2", "z = 3"), col = c("blue", "black", "red", "green"), lty = 1, cex = 1.5, text.width = 2.5)

all.dat <- as.data.frame(read.csv("data.2013.through.2015.csv"))

library(hexbin)
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
tempdf <- data.frame(dat$home.goals - dat$away.goals, dat$home.behinds - dat$away.behinds)
plot(hexbin(tempdf, xbins = 15), colramp = rf, xlab = "Goal difference", ylab = "Behind difference")
