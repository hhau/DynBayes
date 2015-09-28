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

other.params <- list()
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

plot(x=1:23, y = delta.vec, ylim = c(0,1)) # delta dead on through time

param.mat <- rbind(delta.vec, phi.att.vec, phi.def.vec, sigma.att.vec, sigma.def.vec)
matplot(1:23,t(param.mat), type = "l", lty=1, col = 1:5) # again, see sigma.atta.vec is getting very close to prior bounds
  # a similar story to it's traceplot

# performace through time
matplot(1:23, cbind(mean.through.time, percent.correct),
        type = "l",
        ylim = c(0,1), xlim = c(1,23),
        xlab ="Round number", ylab = "Percent correct",
        col=c("black", "red"), lty = 1
        )
legend("bottom", legend = c("round", "mean"), col = c("red","black"), lty=1, cex = 1.1, text.width = 2.5)


# means, sds, phis, other parameters through time
