library(rstan)
library(coda)
library(parallel)
library(Bessel)

#### Change this value for each fit, will differ ####
# doens't matter any more, sys.glob takes care of everything, what a function.
dat.str <- Sys.glob("data.*.csv")
dat <- as.data.frame(read.csv(file = dat.str))
#### ####

#### change these values for each fit, will be different ####
matchup.str <- Sys.glob("matchup.*.csv")
matchup <- as.data.frame(read.csv(matchup.str))
n.matches <- length(matchup$home.number)
#### ####

home.name <- as.matrix(dat$home.name)
away.name <- as.matrix(dat$away.name)
home.numb <- dat$home.number
away.numb <- dat$away.number


home.Goals <- dat$home.goals
home.Behinds <- dat$home.behinds
away.Goals <- dat$away.goals
away.Behinds <- dat$away.behinds


period <- dat$period
n.teams <- 18
n.games <- length(home.numb)
n.periods <- max(period)
n.bps <- 3
bps <- c(1,24,47)


# matricies for the Owen paper centering
ones <- matrix(1, nrow = (n.teams-1), ncol = 1)
S_t <-  n.teams / (n.teams-1) * (diag(n.teams-1) + ones %*% t(ones))

# Jmat is n * n-1
J.up.left <- diag(n.teams-1) - (1/n.teams)*(ones %*% t(ones))
J.bot.left <- - t(ones) * 1/n.teams

Jmat <- rbind(J.up.left, J.bot.left)

dat.list <- list(N = n.teams,
                 G = n.games,
                 T = n.periods,
                 period = period,
                 home = home.numb,
                 away = away.numb,
                 homeGoals = home.Goals,
                 homeBehinds = home.Behinds,
                 awayGoals = away.Goals,
                 awayBehinds = away.Behinds,
                 BP = n.bps,
                 Breaks = bps,
                 Smat = S_t,
                 JmatTrans = t(Jmat),
                 N_Matches = n.matches,
                 nextHome = matchup$home.number,
                 nextAway = matchup$away.number)
#inits needs to be a list of lists
# something in this is not right and needs changing, just exclude
# for now as stan will give random inital values for default

print("beggining stanfit")

prefit <- stan("Skellam goal and behind difference.stan", data = dat.list, chains = 0)

print("sampling")

sflist <-
  mclapply(1:4, mc.cores = 4,
           function(i) stan(fit = prefit, data = dat.list,
                            chains = 1, chain_id = i, iter = 2500,
                            refresh = -1, init = "0"))

print("finished stanfit, combining data")
fit <- sflist2stanfit(sflist)
write.csv(print(summary(fit)), file = "fitproper.csv")


print("extracting parameters")
fit.samples <- extract(fit)

write.csv(fit.samples, file = "samples.csv")

# parameter estimate extractor
delta <- mean (fit.samples$delta)
muAttGoals.vec <- apply(fit.samples$muAttGoals, 2, mean) #
muAttBehinds.vec <- apply(fit.samples$muAttBehinds, 2, mean)
muDef.vec <- apply(fit.samples$muDef, 2, mean)

mu.ests <- matrix(c( "Goal attack mean estimates", muAttGoals.vec,"Behind attack mean estimates", muAttBehinds.vec,
                     "defend means estimates", muDef.vec), ncol = 3, byrow = F)
write.csv(mu.ests, file = "AR mean estimates.csv")

phiAtt.vec <- mean(fit.samples$phiAtt)
phiDef.vec <- mean(fit.samples$phiDef)
std.attack.vec <- mean(fit.samples$stdAtt)
std.def.vec <- mean(fit.samples$stdDef)


goal.attack.strenghts.mat <- apply(fit.samples$attackGoals, 2:3, mean)
write.csv(goal.attack.strenghts.mat, file = "goal.attack.str.csv")

behind.attack.strengths.mat <- apply(fit.samples$attackBehinds, 2:3, mean)
write.csv(behind.attack.strengths.mat, file = "behind.attack.str.csv")

defence.strengths.mat <- apply(fit.samples$defend, 2:3, mean)
write.csv(defence.strengths.mat, file = "def.str.csv")

par.vec <- c(delta, "delta",
             phiAtt.vec, "phi.attack",
             phiDef.vec, "phi.defend",
             std.attack.vec, "std.attack",
             std.def.vec, "std.defend")
par.vec.est <- matrix(par.vec, ncol = 2, byrow = T)
write.csv(par.vec.est,  file = "parameter estimates.csv")

avg.pred.goal.diff <- apply(fit.samples$goalDiff, 2, mean)
avg.pred.behind.diff <- apply(fit.samples$behindDiff, 2, mean)
avg.pred.total.diff <- apply(fit.samples$totalDiff, 2, mean)

# see if we got it right
actual.total.diff <- matchup$home.score - matchup$away.score
correct.vec <- ((avg.pred.total.diff * actual.total.diff)>0)

# get the percentage of win/loss

winDrawLossPer <- function(x.vec) {
  home.win.per <- mean(as.numeric(x.vec > 0))
  draw.per <- mean(as.numeric(x.vec == 0))
  away.win.per <- mean(as.numeric(x.vec < 0))
  c(home.win.per, draw.per ,away.win.per)
}

per.chance.mat <- apply(fit.samples$totalDiff, 2, winDrawLossPer)

sd.ests <- round(apply(fit.samples$totalDiff,2,sd),2)

# i still think we are over predicting the draw chance, but this one
# is significantly better than before.

sim.pred.mat <- matrix(c("behind diff", avg.pred.behind.diff,
                         "goal diff", avg.pred.goal.diff,
                         "total diff", avg.pred.total.diff,
                         "home team", as.character(matchup$home.name),
                         "away team", as.character(matchup$away.name),
                         "actual result", actual.total.diff,
                         "prediction correct?", correct.vec,
                         "home win prob", per.chance.mat[1,],
                         "draw prob", per.chance.mat[2,],
                         "home loss prob", per.chance.mat[3,],
                         "simulated sd ests", sd.ests
                         ), nrow = 11, byrow = T)

write.csv(sim.pred.mat, file = "internal predictions.csv")

# some quick plots
pdf("goal attack str.pdf", width = 10, height = 5)
matplot(1:n.periods, goal.attack.strenghts.mat, type = "l" , xlab = "time period", ylab = "goal attack str")
dev.off()

pdf("behind attack str.pdf", width = 10, height = 5)
matplot(1:n.periods, behind.attack.strengths.mat, type = "l" , xlab = "time period", ylab = "behind attack str")
dev.off()

pdf("def str.pdf", width = 10, height = 5)
matplot(1:n.periods, defence.strengths.mat, type = "l", xlab = "time periods", ylab = "defence str")
dev.off()
