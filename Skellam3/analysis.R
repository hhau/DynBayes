# analysis of all the fits
library(xtable)
library(hexbin)
library(RColorBrewer)
#### This will probably need changing
setwd("/Users/hilary/Desktop/Uni/y4/HonThesis/Berwin/Fittingcode/hon-thesis/Skellam3/")
####

base.dir <- getwd()
tt <- as.character(1:23)
dir.list <- ifelse(nchar(tt)==2,tt,paste0("0",tt))
# data holding structure.

results <- list()
num.correct <- 0
percent.correct <- matrix(NA, nrow = 23, ncol = 1 )

goal.attack.mean <- matrix(0, nrow = 23, ncol = 18)
behind.attack.mean <- matrix(0, nrow = 23, ncol = 18)
def.mean <- matrix(0, nrow = 23, ncol = 18)

other.params <- list()

g.att.str <- list()
b.att.str <- list()
def.str <- list()
for(i in 1:23) {

  wd <- paste(base.dir, dir.list[i], sep = "/")
  #print(wd)
  setwd(wd)
  results[[i]] <- read.csv("internal predictions.csv")
  #print(results.file.str)

  # temp true/false loop
  temp <- as.matrix(results[[i]])[7,c(-1,-2)]
  n <- nlevels(as.factor(temp))
  num.temp <- as.numeric(as.factor(temp)) - (n - 1)
  num.correct <- num.correct + sum(num.temp)
  percent.correct[i] <- mean(num.temp)

  # get the delta/phi's/sigmas through time
  other.params[[i]] <- read.csv("parameter estimates.csv")

  # get the means through time
  temp.mat <- as.matrix(read.csv("AR mean estimates.csv"))
  g.att.mean.temp <- temp.mat[-1,c(-1,-3,-4)]
  b.att.mean.temp <- temp.mat[-1,c(-1,-2,-4)]
  def.mean.temp <- temp.mat[-1, c(-1,-2,-3)]
  goal.attack.mean[i,] <- g.att.mean.temp
  behind.attack.mean[i,] <- b.att.mean.temp
  def.mean[i,] <- def.mean.temp
}

# temp true/false loop

mean.through.time <- matrix(NA, nrow = 23, ncol =1)

residuals <- matrix(NA)

diff <- 0

delta.vec <- matrix(0, nrow = 1, ncol = 23)
phi.att.goal.vec <- matrix(0, nrow = 1, ncol = 23)
phi.att.behind.vec <- matrix(0, nrow = 1, ncol = 23)
phi.def.vec <- matrix(0, nrow = 1, ncol = 23)
sigma.att.vec <- matrix(0, nrow = 1, ncol = 23)
sigma.def.vec <- matrix(0, nrow = 1, ncol = 23)
sigma.mu.vec <- matrix(0, nrow = 1, ncol = 23)
for(i in 1:23) {
  mean.through.time[i] <- sum(percent.correct[c(1:i)])/i

  # drop the first two columns, as they are descriptive
  temp <- as.matrix(results[[i]])[,c(-1,-2)]
  #  take the predicted - actual
  temp.diff <- as.numeric(temp[3,]) - as.numeric(temp[6,])
  diff <- c(diff, temp.diff)

  delta.vec[i] <- other.params[[i]][1,2]
  phi.att.goal.vec[i] <- other.params[[i]][2,2]
  phi.att.behind.vec[i] <- other.params[[i]][3,2]
  phi.def.vec[i] <- other.params[[i]][4,2]
  sigma.att.vec[i] <- other.params[[i]][5,2]
  sigma.def.vec[i] <- other.params[[i]][6,2]
  sigma.mu.vec[i] <- other.params[[i]][7,2]
}

hist(diff[-1], xlim = c(-80,80), breaks = 50)


# performace through time
matplot(1:23, cbind(mean.through.time, percent.correct),
        type = "l",
        ylim = c(0,1), xlim = c(1,23),
        xlab ="Round number", ylab = "Percent correct",
        col=c("black", "red"), lty = c(2,1)
        )
legend("bottom", legend = c("round", "mean"), col = c("red","black"), lty=c(1,2), cex = 1.1, text.width = 2.5)

param.mat <- rbind(delta.vec, phi.att.goal.vec, phi.att.behind.vec, phi.def.vec, sigma.att.vec, sigma.def.vec, sigma.mu.vec)
matplot(1:23,t(param.mat), type = "o", lty=1:6, pch = "+",
        col = 1:5, xlab = "Round number", ylab = "Parameter value", ylim = c(0.0,1.03))
# again, see sigma.atta.vec is getting very close to prior bounds
# a similar story to it's traceplot
legend(x = 0.95, y = 0.36,c(expression(delta), expression(phi[alpha]), expression(phi[gamma]), expression(phi[beta]),
                         expression(sigma[a]), expression(sigma[beta]), expression(sigma[mu])),
       lty = 1:6 ,col = 1:5, cex = 0.8, text.width = 0.9, ncol = 2)


# means, through time, don't know if any of these are any good ????
matplot(x = 1:23, y = goal.attack.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[alpha]))

matplot(x = 1:23, y = behind.attack.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[gamma]))

matplot(x = 1:23, y = def.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[beta]))

# singular attack / def strength plots

setwd("/Users/hilary/Desktop/Uni/y4/HonThesis/Berwin/Fittingcode/hon-thesis/sepPhi/23/")

# def str

def.str <- as.matrix(read.csv(file = "def.str.csv"))
def.str <- def.str[,-1]

matplot(1:68, def.str[,c(10,11)], type = "l") # freo and Melbourne

# goal att str
goal.att.str <-as.matrix(read.csv(file = "goal.attack.str.csv"))[,-1]
matplot(1:68, goal.att.str[,c(3,10)], type= "l")

# not actually sure this is a good idea

# ci's

#N.CALC
predicted.points <- 0
actual.points <- 0
optimal.z <- function(quantile) {
  inside.int <- matrix(0, nrow = 1, ncol = (23*9 - 9))
  z <- 1
  # pick "c"i level
  ci <- qnorm(quantile)
  for (i in 1:23) {
    list.len <- length(results[[i]])
    for (q in 3:list.len) {
      mean <- as.numeric(as.matrix(results[[i]][, q])[3])
      predicted.points <- c(predicted.points, mean)
      sd.est <- as.numeric(as.matrix(results[[i]][, q])[11])
      actual <- as.numeric(as.matrix(results[[i]][, q])[6])
      actual.points <- c(actual.points, actual)
      if(actual < (mean + ci * sd.est) &  actual > (mean - ci*sd.est)) {
        inside.int[z] <- 1
      }
      z <- z + 1
    }
  }
  mean(inside.int) # not exactly close to 95%, but not that far either ?
}


x.vals <- seq(from = 0.5, to = 0.999, by = 0.001)

y.vals <- matrix(0, nrow = 1, ncol = length(x.vals))

for(i in 1:length(x.vals)) {y.vals[i] <- optimal.z(x.vals[i])}

plot(x=x.vals, y = y.vals, type = "l", xlab = "Normal Quantile", ylab = "Coverage", cex.lab = 1.2)
segments(0.5, 0, 1, 1, col = "blue", lty = 6)
# abline(a = -1, b = 2, col = "blue") # not the line i am looking for
legend(x = 0.9, y = 0.4, c("Ideal", "Actual"), col = c("blue", "black"), lty = c(6,1), cex = 1.2, text.width = 2)

# actual mat


all.team.names <- matrix(c("Adelaide",
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
), nrow = 18, ncol = 1)

team.nums <- 1:18
lookup.frame <- as.data.frame(all.team.names, stringsAsFactors = FALSE)
names(lookup.frame)[names(lookup.frame)=="V1"] <- "team.names"
lookup.frame$team.number <- team.nums

lookup.table <- lookup.frame$team.number
names(lookup.table) <- lookup.frame$team.names


points.at.t <- matrix(0, nrow = 23, ncol = 18)

pred.at.t <- matrix(0, nrow = 23, ncol = 18)

for(i in 1:23) {
  current.round <- as.matrix(results[[i]])
  list.len <- length(results[[i]])
  for (q in 3:list.len) {
    expected.home <- sum(as.numeric(current.round[8:10,q]) * c(4,2,0))
    expected.away <- 4 - expected.home
    home.team.num <- lookup.table[current.round[4,q]]
    away.team.num <- lookup.table[current.round[5,q]]
    pred.at.t[i, home.team.num] <- expected.home
    pred.at.t[i, away.team.num] <- expected.away

    if(as.numeric(current.round[6, q]) < 0) { # away team win
      points.at.t[i, home.team.num] <- 0
      points.at.t[i, away.team.num] <- 4
    } else if (as.numeric(current.round[6, q]) == 0) { # draw
      points.at.t[i, home.team.num] <- 2
      points.at.t[i, away.team.num] <- 2
    } else { # home win
      points.at.t[i, home.team.num] <- 4
      points.at.t[i, away.team.num] <- 0
    }
  }
}
# this bit is to deal with the null result game
geelong <- lookup.table["Geelong"]
ad <- lookup.table["Adelaide"]
points.at.t[14, geelong ] <- points.at.t[14, ad]  <- 2

points.over.t <- apply(points.at.t, 2, cumsum)
pred.over.t <- apply(pred.at.t,2,cumsum)
colnames(points.over.t) <- lookup.frame$team.names
colnames(pred.over.t) <- lookup.frame$team.names

library(reshape)
library(lattice)
library(latticeExtra)

plot(1:23, points.over.t[, "GWS"], type = "l", ylim = c(0, 60))
points(1:23, pred.over.t[, "GWS"], type = "l", col = "blue")

actual <- melt(points.over.t)
pred <- melt(pred.over.t)
index <- order(points.over.t[23,])
pred$X2 <- factor(pred$X2, levels = lookup.frame$team.names[index])
actual$X2 <- factor(actual$X2, levels = lookup.frame$team.names[index])

temp2 <- rbind(cbind(pred,X3 = 1), cbind(actual,X3 = 2))
temp2$X3 <- factor(temp2$X3, labels = c("Predicted", "Actual"))
xyplot(value~X1|X2, temp2, groups = X3, type = "l", lty = c(1,5),layout = c(6,3), xlab = "Round", ylab = "Points",
       par.settings = simpleTheme(lty=c(1,5)),
       auto.key=list(columns = 2, lines = TRUE, points = FALSE))


# temp <- xyplot(value ~ X1|X2, actual, type = "l", col = "Red", layout = c(6,3), xlab = "Round", ylab = "Points", auto.key = TRUE)
# final <- temp + xyplot(value ~ X1|X2, pred, type = "l", col = "blue",layout = c(6,3))

# UWA tipping comparison

# predicted vs actual data
# some funny code excuting has to occur to get this to work

predicted.points <- predicted.points[-1]
actual.points <- actual.points[-1]
plot(x = predicted.points, y = actual.points, xlim = c(-250, 250), ylim = c(-150,150))
tempdf <- data.frame(predicted.points, actual.points)
templm <- lm(actual.points ~ predicted.points - 1, data = tempdf)
abline(0, coef(templm))

plot(x =  predicted.points, resid(templm))

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

tempdf2 <- tempdf[-23,]
plot(hexbin(tempdf2, xbins = 15, xbnds = c(-260,260), ybnds = c(-150,150)) ,xlab = "Predicted point difference", ylab = "Actual point difference difference")

# monash footy tipping stuff
# frame all outcomes from the home team

# boldness & calibration
calib.sum <- 0
n <- 0
bold.total <- 0
for(i in 1:23) {
  n.games <- length(results[[i]])
  for(q in 3:n.games){
    score.diff <- as.numeric(as.matrix(results[[i]])[6,q])
    if (score.diff > 0 ) { # home team win
      r.temp <- 1
    } else if (score.diff < 0 ) { # home team lose
      r.temp <- 0
    } else { # drawn
      r.temp <- 1/2
    }
    o.temp <- as.numeric(as.logical(as.matrix(results[[i]])[7,q]))
    p.temp <- as.numeric((as.matrix(results[[i]])[8,q]))
    b.temp <- (p.temp - r.temp) * log2(p.temp/(1-p.temp))
    bold.total <- bold.total + b.temp
    calib.sum <- calib.sum + (o.temp - p.temp)^2
    n <- n + 1
  }
}

calib <- sqrt(calib.sum / (n - 1))
# calibration suggests this is garbage


