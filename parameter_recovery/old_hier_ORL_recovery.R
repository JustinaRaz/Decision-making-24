install.packages("pacman")
pacman::p_load(R2jags, parallel, ggpubr, extraDistr, truncnorm)

setwd('/work/JustinaRazanauskaite#2891/DM/')

# Function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#------ Create task environment -------------------

set.seed(2001)

ntrials <- 100 # total number of trials in our payoff structure
nstruct <- 10 # size of our subdivisions for pseudo randomization

freq <- 0.5 # probability of our frequent losses (we have losses half of the time)
infreq <- 0.1 # probability of our infrequent losses (we have losses 1/10th of the time)

bad_r <- c(80, 90, 100, 110, 120, 80, 90, 100, 110, 120) # possible "bad" winnings
bad_freq_l <- c(-150, -200, -250, -300, -350, 0, 0, 0, 0, 0) # possible "bad" frequent loss
bad_infreq_l <- -1250 # "bad" infrequent loss
good_r <- c(40, 45, 50, 55, 60, 40, 45, 50, 55, 60) # "good" winnings
good_freq_l <- -50 # "good" frequent loss
good_infreq_l <- -250 # "good" infrequent loss

# Bad frequent
A_R <- sample(bad_r, nstruct, replace = FALSE) # we win on every trials
A_L <- sample(bad_freq_l, nstruct, replace = FALSE) # samples losses

# Bad infrequent
B_R <- sample(bad_r, nstruct, replace = FALSE) # we win on every trials
B_L <- sample(c(rep(bad_infreq_l, nstruct * infreq), rep(0, nstruct * (1 - infreq))))

# Good frequent
C_R <- sample(good_r, nstruct, replace = FALSE)
C_L <- sample(c(rep(good_freq_l, nstruct * freq), rep(0, nstruct * (1 - freq))))

# Good infrequent
D_R <- sample(good_r, nstruct, replace = FALSE)
D_L <- sample(c(rep(good_infreq_l, nstruct * infreq), rep(0, nstruct * (1 - infreq))))

# Create the pseudo randomized full payoff structure
A <- array(NA,ntrials) # setting up and empty array to be filled
B <- array(NA,ntrials)
C <- array(NA,ntrials)
D <- array(NA,ntrials)

for (i in 1:(ntrials/nstruct)) {
  A[(1+(i-1)*nstruct):(i*nstruct)] <- (A_R + A_L) # randomly shuffling the loss-array for every ten trials (and adding those losses to the winnings)
  B[(1+(i-1)*nstruct):(i*nstruct)] <- (B_R + B_L)
  C[(1+(i-1)*nstruct):(i*nstruct)] <- (C_R + C_L)
  D[(1+(i-1)*nstruct):(i*nstruct)] <- (D_R + D_L)
}

payoff <- cbind(A,B,C,D)/100 # combining all four decks as columns with each 100 trials - dividing our payoffs by 100 to make the numbers a bit easier to work with

# let's look at the payoff
colSums(payoff)

###--------------Run full parameter recovery -------------
niterations <- 100 # fewer because it takes too long
nsubs <- 63
ntrials_all <- rep(100, 63)

# mu
true_mu_a_rew <- array(NA,c(niterations))
true_mu_a_pun <- array(NA,c(niterations))
true_mu_K <- array(NA,c(niterations))
#true_mu_theta <- array(NA,c(niterations))
true_mu_omega_f <- array(NA,c(niterations))
true_mu_omega_p <- array(NA,c(niterations))

infer_mu_a_rew <- array(NA,c(niterations))
infer_mu_a_pun <- array(NA,c(niterations))
infer_mu_K <- array(NA,c(niterations))
#infer_mu_theta <- array(NA,c(niterations))
infer_mu_omega_f <- array(NA,c(niterations))
infer_mu_omega_p <- array(NA,c(niterations))

# sigma (SD for R) / lambda (precision for JAGS)
true_lambda_a_rew <- array(NA,c(niterations))
true_lambda_a_pun <- array(NA,c(niterations))
true_lambda_K <- array(NA,c(niterations))
#true_lambda_theta <- array(NA,c(niterations))
true_lambda_omega_f <- array(NA,c(niterations))
true_lambda_omega_p <- array(NA,c(niterations))

infer_lambda_a_rew <- array(NA,c(niterations))
infer_lambda_a_pun <- array(NA,c(niterations))
infer_lambda_K <- array(NA,c(niterations))
#infer_lambda_theta <- array(NA,c(niterations))
infer_lambda_omega_f <- array(NA,c(niterations))
infer_lambda_omega_p <- array(NA,c(niterations))

start_time = Sys.time()
for (i in 1:niterations) {
  print(i) # tracking progress
  
  ntrials <- ntrials_all
  
  # let's see how robust the model is. Does it recover all sorts of values?
  mu_a_rew <- runif(1,0,1)
  mu_a_pun <- runif(1,0,1)
  mu_K <- runif(1,0,2)
  # mu_theta <- 1 #runif(1,.2,2) # could also just be a set value (e.g. 1) to simplify the model a bit
  mu_omega_f <- runif(1,-2,2)
  mu_omega_p <- runif(1,-2,2)
  
  # sigma_a_rew <- runif(1,0,0.1)
  # sigma_a_pun <- runif(1,0,0.1)
  # sigma_K <- runif(1,0,0.2)
  # # sigma_theta <- runif(1,0,0.2) # if theta is just a set value (e.g. 1), then this isn't relevant anymore
  # sigma_omega_f <- runif(1,0,0.4)
  # sigma_omega_p <- runif(1,0,0.4)
  
  sigma_a_rew <- runif(1,0,.5)
  sigma_a_pun <- runif(1,0,.5)
  sigma_K <- runif(1,0,.5)
  # sigma_theta <- runif(1,0,0.2) # if theta is just a set value (e.g. 1), then this isn't relevant anymore
  sigma_omega_f <- runif(1,0,.5)
  sigma_omega_p <- runif(1,0,.5)
  
  source('ORL_sim.R')
  ORL_sims <- hier_ORL_sim(payoff,nsubs,ntrials,mu_a_rew,mu_a_pun,
                           mu_K,mu_omega_f,mu_omega_p,
                           sigma_a_rew,sigma_a_pun,sigma_K,
                           sigma_omega_f,sigma_omega_p)
  
  x <- ORL_sims$x
  X <- ORL_sims$X
  
  # set up jags and run jags model
  data <- list("x","X","ntrials","nsubs") 
  params<-c("mu_a_rew","mu_a_pun",
            "mu_K","mu_omega_f","mu_omega_p","lambda_a_rew",
            "lambda_a_pun","lambda_K","lambda_omega_f","lambda_omega_p")
  samples <- jags.parallel(data, inits=NULL, params,
                           model.file ="hier_ORL_no_theta.txt", n.chains=3, 
                           n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)
  
  # mu
  true_mu_a_rew[i] <- mu_a_rew
  true_mu_a_pun[i] <- mu_a_pun
  true_mu_K[i] <- mu_K
  true_mu_omega_f[i] <- mu_omega_f
  true_mu_omega_p[i] <- mu_omega_p
  
  # find maximum a posteriori
  Y <- samples$BUGSoutput$sims.list
  infer_mu_a_rew[i] <- MPD(Y$mu_a_rew)
  infer_mu_a_pun[i] <- MPD(Y$mu_a_pun)
  infer_mu_K[i] <- MPD(Y$mu_K)
  infer_mu_omega_f[i] <- MPD(Y$mu_omega_f)
  infer_mu_omega_p[i] <- MPD(Y$mu_omega_p)
  
  # lambda
  true_lambda_a_rew[i] <- sigma_a_rew
  true_lambda_a_pun[i] <- sigma_a_pun
  true_lambda_K[i] <- sigma_K
  true_lambda_omega_f[i] <- sigma_omega_f
  true_lambda_omega_p[i] <- sigma_omega_p
  
  # find maximum a posteriori
  infer_lambda_a_rew[i] <- MPD(Y$lambda_a_rew)
  infer_lambda_a_pun[i] <- MPD(Y$lambda_a_pun)
  infer_lambda_K[i] <- MPD(Y$lambda_K)
  infer_lambda_omega_f[i] <- MPD(Y$lambda_omega_f)
  infer_lambda_omega_p[i] <- MPD(Y$lambda_omega_p)
  
}

end_time = Sys.time()
end_time - start_time


# let's look at some scatter plots
# plotting code courtesy of Lasse
source('recov_plot.R')
pl1 <- recov_plot(true_mu_a_rew, infer_mu_a_rew, c("true mu_a_rew", "infer mu_a_rew"), 'smoothed linear fit')
pl2 <- recov_plot(true_mu_a_pun, infer_mu_a_pun, c("true mu_a_pun", "infer mu_a_pun"), 'smoothed linear fit')
pl3 <- recov_plot(true_mu_K, infer_mu_K, c("true mu_K", "infer mu_K"), 'smoothed linear fit')
pl4 <- recov_plot(true_mu_omega_f, infer_mu_omega_f, c("true mu_omega_f", "infer mu_omega_f"), 'smoothed linear fit')
pl5 <- recov_plot(true_mu_omega_p, infer_mu_omega_p, c("true mu_omega_p", "infer mu_omega_p"), 'smoothed linear fit')
mu_plot <- ggarrange(pl1, pl2, pl3, pl4, pl5)
ggsave('hier_ORL_recovery_no_theta_mu_old.png', mu_plot)

ggsave('pl1_mu.png', pl1)
ggsave('pl2_mu.png', pl2)
ggsave('pl3_mu.png', pl3)
ggsave('pl4_mu.png', pl4)
ggsave('pl5_mu.png', pl5)

print('saving mu plot')

pl1_1 <- recov_plot(true_lambda_a_rew, infer_lambda_a_rew, c("true lambda_a_rew", "infer lambda_a_rew"), 'smoothed linear fit')
pl2_2 <- recov_plot(true_lambda_a_pun, infer_lambda_a_pun, c("true lambda_a_pun", "infer lambda_a_pun"), 'smoothed linear fit')
pl3_3 <- recov_plot(true_lambda_K, infer_lambda_K, c("true lambda_K", "infer lambda_K"), 'smoothed linear fit')
pl4_4 <- recov_plot(true_lambda_omega_f, infer_lambda_omega_f, c("true lambda_omega_f", "infer lambda_omega_f"), 'smoothed linear fit')
pl5_5 <- recov_plot(true_lambda_omega_p, infer_lambda_omega_p, c("true lambda_omega_p", "infer lambda_omega_p"), 'smoothed linear fit')

lambda_plot <- ggarrange(pl1_1, pl2_2, pl3_3, pl4_4, pl5_5)

ggsave('hier_ORL_recovery_no_theta_lambda_old.png', lambda_plot)
ggsave('pl1_lambda.png', pl1_1)
ggsave('pl2_lambda.png', pl2_2)
ggsave('pl3_lambda.png', pl3_3)
ggsave('pl4_lambda.png', pl4_4)
ggsave('pl5_lambda.png', pl5_5)
print('saving lambda plot')

# save data for future use
save(ntrials_all, 
     niterations, 
     true_mu_a_rew,
     infer_mu_a_rew,
     true_mu_a_pun,
     infer_mu_a_pun,
     true_mu_K,
     infer_mu_K,
     true_mu_omega_f,
     infer_mu_omega_f,
     true_mu_omega_p,
     infer_mu_omega_p,
     true_lambda_a_rew,
     infer_lambda_a_rew,
     true_lambda_a_pun,
     infer_lambda_a_pun, 
     true_lambda_K, 
     infer_lambda_K,
     true_lambda_omega_f,
     infer_lambda_omega_f, 
     true_lambda_omega_p,
     infer_lambda_omega_p,
     samples,
     file="JAGS_data/JAGS_hier_ORL_recovery_no_theta_old.RData")
print('saving recovery data')
