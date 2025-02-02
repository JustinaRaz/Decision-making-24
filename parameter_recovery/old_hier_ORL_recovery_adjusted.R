install.packages("pacman")
pacman::p_load(R2jags, parallel, ggpubr, extraDistr, truncnorm, tidyverse)

setwd('/work/JustinaRazanauskaite#2891/DM/parameter_recovery/')

# Function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#------ Create task environment -------------------
random_seed <- round(runif(1, 1, 10000)) 
seed <- random_seed
set.seed(seed)

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
niterations <- 10 # fewer because it takes too long
nsubs <- 63
ntrials_all <- rep(100, 63)

# mu
true_mu_a_rew <- array(NA,c(niterations))
true_mu_a_pun <- array(NA,c(niterations))
true_mu_K <- array(NA,c(niterations))
true_mu_omega_f <- array(NA,c(niterations))
true_mu_omega_p <- array(NA,c(niterations))

infer_mu_a_rew <- array(NA,c(niterations))
infer_mu_a_pun <- array(NA,c(niterations))
infer_mu_K <- array(NA,c(niterations))
infer_mu_omega_f <- array(NA,c(niterations))
infer_mu_omega_p <- array(NA,c(niterations))

# sigma (SD for R) / lambda (precision for JAGS)
true_lambda_a_rew <- array(NA,c(niterations))
true_lambda_a_pun <- array(NA,c(niterations))
true_lambda_K <- array(NA,c(niterations))
true_lambda_omega_f <- array(NA,c(niterations))
true_lambda_omega_p <- array(NA,c(niterations))

infer_lambda_a_rew <- array(NA,c(niterations))
infer_lambda_a_pun <- array(NA,c(niterations))
infer_lambda_K <- array(NA,c(niterations))
infer_lambda_omega_f <- array(NA,c(niterations))
infer_lambda_omega_p <- array(NA,c(niterations))

#For tracking what's inside the loop:
track_X <- array(NA,c(niterations))
track_x <- array(NA,c(niterations))
track_Y <- array(NA,c(niterations))
track_samples <- array(NA,c(niterations))

start_time = Sys.time()
for (i in 1:niterations) {
  print(i) # tracking progress
  
  ntrials <- ntrials_all
  
  # let's see how robust the model is. Does it recover all sorts of values?
  mu_a_rew <- runif(1,0,1)
  mu_a_pun <- runif(1,0,1)
  mu_K <- runif(1,0,2)
  mu_omega_f <- runif(1,-2,2)
  mu_omega_p <- runif(1,-2,2)
  
  sigma_a_rew <- runif(1,0,.5)
  sigma_a_pun <- runif(1,0,.5)
  sigma_K <- runif(1,0,.5)
  sigma_omega_f <- runif(1,0,.5)
  sigma_omega_p <- runif(1,0,.5)
  
  source('ORL_sim.R')
  ORL_sims <- hier_ORL_sim(payoff,nsubs,ntrials,mu_a_rew,mu_a_pun,
                           mu_K,mu_omega_f,mu_omega_p,
                           sigma_a_rew,sigma_a_pun,sigma_K,
                           sigma_omega_f,sigma_omega_p)
  
  x <- ORL_sims$x
  X <- ORL_sims$X
  
  # Set up jags and run jags model
  
  data <- list("x","X","ntrials","nsubs") 
  params<-c("mu_a_rew","mu_a_pun",
            "mu_K","mu_omega_f","mu_omega_p","lambda_a_rew",
            "lambda_a_pun","lambda_K","lambda_omega_f","lambda_omega_p")
  samples <- jags.parallel(data, inits=NULL, params,
                           model.file ="hier_ORL_no_theta.txt", n.chains=4, 
                           n.iter=8000, n.burnin=1600, n.thin=1, n.cluster=4)
  
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
  
  track_X[i] <- X
  track_x[i] <- x
  track_Y[i] <- Y
  track_samples[i] <- samples
  
  #save output
  if (i ==1) {
    output_df <- tibble(
      #general
      n_iteration = i,
      # mu
      true_mu_a_rew = mu_a_rew,
      true_mu_a_pun = mu_a_pun,
      true_mu_K = mu_K,
      true_mu_omega_f = mu_omega_f,
      true_mu_omega_p = mu_omega_p,
      #inferred mu
      infer_mu_a_rew = MPD(Y$mu_a_rew),
      infer_mu_a_pun = MPD(Y$mu_a_pun),
      infer_mu_K = MPD(Y$mu_K),
      infer_mu_omega_f = MPD(Y$mu_omega_f),
      infer_mu_omega_p = MPD(Y$mu_omega_p),
      # lambda
      true_lambda_a_rew = sigma_a_rew,
      true_lambda_a_pun = sigma_a_pun,
      true_lambda_K = sigma_K,
      true_lambda_omega_f = sigma_omega_f,
      true_lambda_omega_p = sigma_omega_p,
      # find maximum a posteriori
      infer_lambda_a_rew = MPD(Y$lambda_a_rew),
      infer_lambda_a_pun = MPD(Y$lambda_a_pun),
      infer_lambda_K =  MPD(Y$lambda_K),
      infer_lambda_omega_f = MPD(Y$lambda_omega_f),
      infer_lambda_omega_p = MPD(Y$lambda_omega_p)
    )
  }else {
    temporary_df <- tibble(
      #general
      n_iteration = i,
      # mu
      true_mu_a_rew = mu_a_rew,
      true_mu_a_pun = mu_a_pun,
      true_mu_K = mu_K,
      true_mu_omega_f = mu_omega_f,
      true_mu_omega_p = mu_omega_p,
      #inferred mu
      infer_mu_a_rew = MPD(Y$mu_a_rew),
      infer_mu_a_pun = MPD(Y$mu_a_pun),
      infer_mu_K = MPD(Y$mu_K),
      infer_mu_omega_f = MPD(Y$mu_omega_f),
      infer_mu_omega_p = MPD(Y$mu_omega_p),
      # lambda
      true_lambda_a_rew = sigma_a_rew,
      true_lambda_a_pun = sigma_a_pun,
      true_lambda_K = sigma_K,
      true_lambda_omega_f = sigma_omega_f,
      true_lambda_omega_p = sigma_omega_p,
      # find maximum a posteriori
      infer_lambda_a_rew = MPD(Y$lambda_a_rew),
      infer_lambda_a_pun = MPD(Y$lambda_a_pun),
      infer_lambda_K =  MPD(Y$lambda_K),
      infer_lambda_omega_f = MPD(Y$lambda_omega_f),
      infer_lambda_omega_p = MPD(Y$lambda_omega_p)
    )
    
    output_df <- rbind(output_df,temporary_df)
    
  }
  
  print(paste("Progress:",i/niterations*100,"%",sep = " "))
  current_time <- Sys.time()
  time_spent <-current_time - start_time
  print(paste("Since start:",time_spent,sep = " "))
  
}


#write output to folder
name <- paste("seed", seed, sep = "_")
out_f <- paste("./results/",name,".rds", sep = "")
write_rds(output_df,out_f)


end_time = Sys.time()
end_time - start_time
formatted_time <- format(end_time, "%Y-%m-%d_%H-%M-%S")
file_name <- paste0("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted", seed, ".RData")


print("Saving the data")

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
     track_X,
     track_x,
     track_samples,
     file = file_name)
print('saving recovery data')