functions {
  # difference of two random variable by skellam for points spread
  real skellam_log(int k, real mu1, real mu2){
    real total;
    total <- (- mu1 - mu2) + (log(mu1) - log(mu2)) * k / 2;
    return total + log(modified_bessel_first_kind(k, 2*sqrt(mu1*mu2)));
  }
}


data {
  # Quantity of teams. Every team is tagged by an unique number between 1 to N.
  int<lower=1> N;

  # Quantity of all games played. Ordered by playing order.
  int<lower=1> G;

  # Quantity of all periods. A week as a period.
  int<lower=1> T;

  # the corresponding period of each game.
  # For example, 1 as week 1, 2 as week 2.
  int<lower=1, upper=T> period[G];

  # home and away teams in each game
  int<lower=1, upper=N> home[G];
  int<lower=1, upper=N> away[G];

  # home score and away score of each game.
  # int homeScore[G];
  # int awayScore[G];

  # home and away goals / behinds?
  int<lower=0> homeGoals[G];
  int<lower=0> homeBehinds[G];
  int<lower=0> awayGoals[G];
  int<lower=0> awayBehinds[G];

  # number of break points
  int <lower=1> BP;

  # break points
  int <lower=1> Breaks[BP];

  # read in the created S and J matricies from R
  matrix[N-1, N-1] Smat;

  #Jmat might only need to be N x N-1 ?? don't really care about last col anyway
  # Owen paper
  matrix[N-1, N] JmatTrans;

  # Things for predicting the next round
  int <lower = 6, upper = 9> N_Matches;
  int <lower = 1, upper = N> nextHome[N_Matches];
  int <lower = 1, upper = N> nextAway[N_Matches];

}

transformed data {
  real lmaxScore;
  int diffGoals[G];
  int diffBehinds[G];

  row_vector[N-1] Zeros;

  for(q in 1:G){
    diffGoals[q] <- homeGoals[q] - awayGoals[q];
    diffBehinds[q] <- homeBehinds[q] - awayBehinds[q];
  }

  lmaxScore <- log(400*max(homeBehinds));

  for (i in 1:(N-1)){
    Zeros[i] <- 0.0;
  }
}

parameters {
  # home advantage of scoring
  real<lower=0, upper=lmaxScore> delta;

  # dynamic time vary parameters
  # attack and defend parameters
  matrix<lower=-lmaxScore, upper=lmaxScore>[T, N-1] attackGoals_raw;
  matrix<lower=-lmaxScore, upper=lmaxScore>[T, N-1] attackBehinds_raw;

  matrix<lower=-lmaxScore, upper=lmaxScore>[T, N-1] defend_raw;

  # Autoregressive parameters for dynamic time vary parameters
  # Owen constraints require this to sum to zero
  row_vector<lower=-lmaxScore, upper=lmaxScore>[N-1] muAttGoals_raw;
  row_vector<lower=-lmaxScore, upper=lmaxScore>[N-1] muAttBehinds_raw;

  row_vector<lower=-lmaxScore, upper=lmaxScore>[N-1] muDef_raw;

  # varaince for mu prior
  # means should all have the same variance
  real<lower = 0.005, upper = 1> muStd;

  # same phi for both kinds of attack, and all teams
  real<lower=0, upper=1> phiAtt;
  real<lower=0, upper=1> phiDef;

  # Constrain variances
  real<lower=0.02, upper=0.8> stdAtt;
  real<lower=0.02, upper=0.8> stdDef;
}

transformed parameters {
  # setup up to zero constraints on attack and defence parameters

  # maybe we have to adjust the lower and upper parameters here
  matrix<lower=-10*lmaxScore, upper = 10*lmaxScore>[T, N] attackGoals;
  matrix<lower=-10*lmaxScore, upper = 10*lmaxScore>[T, N] attackBehinds;
  matrix<lower=-10*lmaxScore, upper = 10*lmaxScore>[T, N] defend;

  # set up sum to zero constraints on process means
  row_vector<lower = -lmaxScore, upper = lmaxScore>[N] muAttGoals;
  row_vector<lower = -lmaxScore, upper = lmaxScore>[N] muAttBehinds;
  row_vector<lower = -lmaxScore, upper = lmaxScore>[N] muDef;

  # not sure the loop directions here are correct
  # for (i in 1:T) { # at each of the time points
  #  for (q in 1:(N-1)) { # for each one of the teams
  #      attack[i,q] <- attack_raw[i,q]; # copy the attack and defence values for each team
  #      defend[i,q] <- defend_raw[i,q];
  #  } # happy for the above
  #  attack[i, N] <- -sum(attack_raw[i]); # at each time point ensure the attack and defence sum to zero
  #  defend[i, N] <- -sum(defend_raw[i]);
  #}


  attackGoals <- attackGoals_raw * JmatTrans;
  attackBehinds <- attackBehinds_raw * JmatTrans;
  defend <- defend_raw * JmatTrans;

  muAttGoals <- muAttGoals_raw * JmatTrans;
  muAttBehinds <- muAttBehinds_raw * JmatTrans;
  muDef <- muDef_raw * JmatTrans;

}
model {

  # can now stick priors on the means to ensure centring
  # note that these processes inhertently describe the score difference between
  # two teams, due to the fact we are working with the goals/behinds difference
  # hence the lambda parameters loose their interpritation as expected values?

  muAttGoals_raw ~ multi_normal(Zeros, muStd * Smat);
  muAttBehinds_raw ~ multi_normal(Zeros, muStd * Smat);
  muDef_raw ~ multi_normal(Zeros, muStd * Smat);

  phiAtt ~ beta(100,20);
  phiDef ~ beta(100,20);

  for(k in 1:T-1){

    # between seasonality shocks
    real decayFactor;
    int timeDelta;
    real inflationFactor;
    decayFactor <- 1;
    inflationFactor <- 5;

    if ((k % 23 < 7)  && ( k % 23 > 0)) { # mod by 23 for start of season variance
        timeDelta <- k % 23;
        decayFactor <- (inflationFactor - 1) * exp(-(timeDelta)) + 1;
    }
    # time series updating
    # allow for start of season shocks, at specified breakpoints.
    attackGoals_raw[k+1] ~ multi_normal(phiAtt * attackGoals_raw[k] + muAttGoals_raw, decayFactor * stdAtt * Smat);
    attackBehinds_raw[k+1] ~ multi_normal(phiAtt * attackBehinds_raw[k] + muAttBehinds_raw, decayFactor * stdAtt * Smat);
    defend_raw[k+1] ~ multi_normal(phiDef * defend_raw[k] + muDef_raw, decayFactor * stdDef * Smat);
  }

  for(q in 1:G){
    int k;
    int hm;
    int aw;
    real p1;
    real p2;
    real p3;
    real p4;

    k <- period[q];
    hm <- home[q];
    aw <- away[q];

    # perhaps we should have seperate deltas on each one ?
    p1 <- exp(attackGoals[k, hm] - defend[k, aw] + delta);
    p2 <- exp(attackGoals[k, aw] - defend[k, hm]);

    p3 <- exp(attackBehinds[k, hm] - defend[k, aw] + delta);
    p4 <- exp(attackBehinds[k, aw] - defend[k, hm]);

    diffGoals[q] ~ skellam(p1, p2);
    diffBehinds[q] ~ skellam(p3, p4);
  }
}

generated quantities{

#   # log likelihood for calculating Watanabe-Akaike information criterion (WAIC)
#   vector[G] log_like;
#
#   for(q in 1:G){
#     int k;
#     int hm;
#     int aw;
#     real p1;
#     real p2;
#     real p3;
#
#     k <- period[q];
#     hm <- home[q];
#     aw <- away[q];
#     p1 <- exp(attack[k, hm] - defend[k, aw] + delta);
#     p2 <- exp(attack[k, aw] - defend[k, hm]);
#     log_like[q] <- skellam_log(diffScore[q], p1, p2);
#   }
  vector[N_Matches] goalDiff;
  vector[N_Matches] behindDiff;
  vector[N_Matches] totalDiff;

  for (i in 1:N_Matches) {
    int k;
    int hm;
    int aw;
    int hmGoals;
    int awGoals;
    int hmBehinds;
    int awBehinds;
    real p1;
    real p2;
    real p3;
    real p4;

    hm <- nextHome[i];
    aw <- nextAway[i];

    p1 <- exp(attackGoals[T, hm] - defend[T, aw] + delta);
    p2 <- exp(attackGoals[T, aw] - defend[T, hm]);

    p3 <- exp(attackBehinds[T, hm] - defend[T, aw] + delta);
    p4 <- exp(attackBehinds[T, aw] - defend[T, hm]);

    # feel like this should be skellam_rng
    # but that probably means i need to write the skellam_rng
   hmGoals <- poisson_rng(p1);
   awGoals <- poisson_rng(p2);

   hmBehinds <- poisson_rng(p3);
   awBehinds <- poisson_rng(p4);

   goalDiff[i] <- hmGoals - awGoals;
   behindDiff[i] <- hmBehinds - awBehinds;
   totalDiff[i] <- 6 * (hmGoals - awGoals) + (hmBehinds - awBehinds);
  }
}
