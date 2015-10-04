# analysis of all the fits
library(xtable)

setwd("/Users/hilary/Desktop/aam/")
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

  wd <- paste(base.dir, dir.list[i], sep = "/roundfits/")
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
phi.att.vec <- matrix(0, nrow = 1, ncol = 23)
phi.def.vec <- matrix(0, nrow = 1, ncol = 23)
sigma.att.vec <- matrix(0, nrow = 1, ncol = 23)
sigma.def.vec <- matrix(0, nrow = 1, ncol = 23)
for(i in 1:23) {
  mean.through.time[i] <- sum(percent.correct[c(1:i)])/i

  # drop the first two columns, as they are descriptive
  temp <- as.matrix(results[[i]])[,c(-1,-2)]
  #  take the predicted - actual
  temp.diff <- as.numeric(temp[3,]) - as.numeric(temp[6,])
  diff <- c(diff, temp.diff)

  delta.vec[i] <- other.params[[i]][1,2]
  phi.att.vec[i] <- other.params[[i]][2,2]
  phi.def.vec[i] <- other.params[[i]][3,2]
  sigma.att.vec[i] <- other.params[[i]][4,2]
  sigma.def.vec[i] <- other.params[[i]][5,2]
}

hist(diff[-1], xlim = c(-80,80), breaks = 50)


# performace through time
matplot(1:23, cbind(mean.through.time, percent.correct),
        type = "l",
        ylim = c(0,1), xlim = c(1,23),
        xlab ="Round number", ylab = "Percent correct",
        col=c("black", "red"), lty = 1
        )
legend("bottom", legend = c("round", "mean"), col = c("red","black"), lty=1, cex = 1.1, text.width = 2.5)

# other params through time
param.mat <- rbind(delta.vec, phi.att.vec, phi.def.vec, sigma.att.vec, sigma.def.vec)
matplot(1:23,t(param.mat), type = "o", lty=1, pch = "+",
        col = 1:5, xlab = "Round number", ylab = "Parameter value")
# again, see sigma.atta.vec is getting very close to prior bounds
# a similar story to it's traceplot
legend(x = 1, y = 0.49,c(expression(delta), expression(phi[a]), expression(phi[beta]),
                         expression(sigma[a]), expression(sigma[beta])),
       lty = 1,col = 1:5, cex = 1.5, text.width = 1.5)


# means, through time, don't know if any of these are any good ????
matplot(x = 1:23, y = goal.attack.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[alpha]))

matplot(x = 1:23, y = behind.attack.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[gamma]))

matplot(x = 1:23, y = def.mean, type="o", pch="+", xlab = "Round number", ylab = expression(mu[beta]))

# singular attack / def strength plots

setwd("/Users/hilary/Desktop/aam/roundfits/23/")

# def str

def.str <- as.matrix(read.csv(file = "def.str.csv"))
def.str <- def.str[,-1]

matplot(1:68, def.str[,c(10,11)], type = "l") # freo and Melbourne

# goal att str
goal.att.str <-as.matrix(read.csv(file = "goal.attack.str.csv"))[,-1]
matplot(1:68, goal.att.str[,c(10,11)], type= "l")

# not actually sure this is a good idea

# ci's

#N.CALC

optimal.z <- function(quantile) {
  inside.int <- matrix(0, nrow = 1, ncol = (23*9 - 9))
  z <- 1
  # pick "c"i level
  ci <- qnorm(quantile)
  for (i in 1:23) {
    list.len <- length(results[[i]])
    for (q in 3:list.len) {
      mean <- as.numeric(as.matrix(results[[i]][, q])[3])
      sd.est <- as.numeric(as.matrix(results[[i]][, q])[11])
      actual <- as.numeric(as.matrix(results[[i]][, q])[6])
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

plot(x=x.vals, y = y.vals, type = "l", xlab = "Normal Quantile", ylab = "Coverage")
segments(0.5, 0, 1, 1, col = "blue", lty = 1)
# abline(a = -1, b = 2, col = "blue") # not the line i am looking for, ask berwin ? / think harder
legend(x = 0.9, y = 0.4, c("Ideal", "Actual"), col = c("blue", "black"), lty = 1, cex = 1, text.width = 2)
