install.packages("pacman")
pacman::p_load(R2jags, parallel, ggplot2)

set.seed(2001)

setwd('/work/JustinaRazanauskaite#2891/DM/parameter_estimation/PPC/')

MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#load control data
young_data <- read.csv("../data/younger_group.csv")

subIDs <- unique(young_data$subjID)
nsubs <- length(subIDs)
ntrials_max <- 100

# all choices (x) and outcomes (X)
x_raw <- young_data$deck
X_raw <- young_data$gain + young_data$loss

ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[young_data$subjID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[young_data$subjID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[young_data$subjID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}

X_all <- X_all/100

# Let's run this on all subjects
pred_success <- array(nsubs)

start_time = Sys.time()

for (s in 1:nsubs) {
  
  x <- x_all[s, ]
  X <- X_all[s, ]
  
  ntrials <- ntrials_all[s]
  
  data <- list("x","X","ntrials") 
  params<-c("a_rew","a_pun","K","omega_f","omega_p","p")
  samples <- jags.parallel(data, inits=NULL, params,
                                model.file ="ORL_no_theta.txt",
                                n.chains=4, n.iter=8000, n.burnin=1600, n.thin=1, n.cluster=3)
  
  p_post <- samples$BUGSoutput$sims.list$p
  
  x_predict <- array(ntrials)
  
  for (t in 1:ntrials) {
    p_predict <- c(
      MPD(p_post[,t,1]),
      MPD(p_post[,t,2]),
      MPD(p_post[,t,3]),
      MPD(p_post[,t,4])
    )
    
    x_predict[t] <- which.max(p_predict)
    
  }
  
  pred_success[s] <- sum(x_predict==x[1:ntrials]) # only comparing with trials for which we have choices
  print(s)
  
}

save(samples,
     file = "output/ppc_samples_young.RData")

end_time = Sys.time()
end_time - start_time

pred_success_adjust <- pred_success/ntrials_all

avg_pred <- mean(pred_success_adjust)

# plotting code courtesy of Mia
pred_df <- data.frame(pred_success_adjust)

save(pred_df,
     file = "output/ppc_pred_young.RData")

pred_df$sub <- 1:length(pred_success_adjust) # rownames(pred_df) # creating a subject index
pred_df$avg <- mean(pred_df$pred_success)
pred_df$std <- sd(pred_df$pred_success)
pred_df$chance <- .25
ggplot(pred_df, aes(sub, pred_success_adjust)) +
  geom_point() +
  geom_line(aes(y=chance), linetype="dashed", color = "black") +
  geom_ribbon(aes(xmin = -Inf, xmax = Inf, ymin = avg - std, ymax = avg + std), fill = "pink", alpha = 0.6) + 
  geom_line(aes(y=avg)) + 
  ylim(0,1)