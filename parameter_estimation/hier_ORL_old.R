install.packages("pacman")
pacman::p_load(R2jags, parallel)


set.seed(2001)

setwd('/work/JustinaRazanauskaite#2891/DM/parameter_estimation/')

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#load data
old_data <- read.csv("data/older_group.csv")

#----------prepare data for jags models - want trial x subject arrays for choice and gain & loss ----
# identify and count unique subject IDs
subIDs <- unique(old_data$subjID)
nsubs <- length(subIDs)
ntrials_max <- 100

# all choices (x) and outcomes (X)
x_raw <- old_data$deck
X_raw <- old_data$gain + old_data$loss #note the sign!

#--- assign choices and outcomes in trial x sub matrix

#different number of trials across subjects. We'll need to fix this by padding arrays of < 100
#this is just so we can make the array
#then we'll also need to record number of valid trials for each sub, 
#then run the JAGS model on only valid trials

# empty arrays to fill
ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[old_data$subjID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[old_data$subjID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[old_data$subjID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}

# Scaling the payoffs (cuz the learning parameter becomes less relevant for very large payoffs/losses)
X_all <- X_all/100


#----------testing our data curation by running JAGS on one subject

###########################################################
#---------- run the hierarchical model on controls --------
###########################################################

x <- x_all
X <- X_all

ntrials <- ntrials_all

# set up jags and run jags model
data <- list("x","X","ntrials","nsubs") 
# NB! we're not tracking theta cuz we're not modelling it in order reduce complexity a bit (hence, we're just setting it to 1 in "hier_ORL.txt")
params<-c("mu_a_rew","mu_a_pun","mu_K","mu_theta","mu_omega_f","mu_omega_p") 

start_time = Sys.time()
samples <- jags.parallel(data, inits=NULL, params,
                         model.file ="hier_ORL_no_theta.txt",
                         n.chains=4, n.iter=8000, n.burnin=1600, n.thin=1, n.cluster=4)
end_time = Sys.time()
end_time - start_time

save(samples,
     file = "output_data/hier_est_old_all_subs_adjusted.RData")