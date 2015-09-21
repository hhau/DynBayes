# analysis of all the fits
setwd("/Users/hilary/Desktop/aam/")
base.dir <- getwd()
tt <- as.character(1:23)
dir.list <- ifelse(nchar(tt)==2,tt,paste0("0",tt))
# data holding structure.

results <- list()

percent.correct <- matrix(NA, nrow = 23, ncol = 1 )

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
  percent.correct[i] <- mean(num.temp)

}
# temp true/false loop

mean.through.time <- matrix(NA, nrow = 23, ncol =1)

residuals <- matrix(NA)

diff <- 0

for(i in 1:23) {
  mean.through.time[i] <- sum(percent.correct[c(1:i)])/i

  # drop the first two columns, as they are descriptive
  temp <- as.matrix(results[[i]])[,c(-1,-2)]
  #  take the predicted - actual
  temp.diff <- as.numeric(temp[3,]) - as.numeric(temp[6,])
  diff <- c(diff, temp.diff)
}

hist(diff[-1], xlim = c(-80,80), breaks = 50)

matplot(1:23, mean.through.time, type = "l", ylim = c(0,1), xlim = c(1,12))
